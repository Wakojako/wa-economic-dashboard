library(readxl)
library(rvest)
library(xml2)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)
library(glue)
library(jsonlite)
library(httr2)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

find_latest_xlsx <- function(page_url, filename_regex) {
  page <- read_html(page_url)
  hrefs <- page |> html_elements("a") |> html_attr("href") |> discard(is.na)
  hit <- hrefs[str_detect(hrefs, regex(filename_regex, ignore_case = TRUE))][1]
  if (is.na(hit)) stop("Could not locate ABS workbook: ", filename_regex)
  url_absolute(hit, page_url)
}

download_abs_xlsx <- function(page_url, filename_regex, destination) {
  url <- find_latest_xlsx(page_url, filename_regex)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  download.file(url, destination, mode = "wb", quiet = TRUE, method = "libcurl")
  message("Downloaded: ", url)
  destination
}

parse_abs_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))

  x_chr <- str_trim(as.character(x))
  out <- as.Date(rep(NA_character_, length(x_chr)))

  # Fallback for Excel serial dates if readxl supplies them as text/numbers.
  x_num <- suppressWarnings(as.numeric(x_chr))
  is_excel_serial <- !is.na(x_num) & x_num > 10000 & x_num < 100000
  if (any(is_excel_serial)) {
    out[is_excel_serial] <- as.Date(
      x_num[is_excel_serial],
      origin = "1899-12-30"
    )
  }

  remaining <- is.na(out) & !is.na(x_chr) & nzchar(x_chr)
  if (any(remaining)) {
    parsed <- suppressWarnings(
      parse_date_time(
        x_chr[remaining],
        orders = c(
          "Y-m-d H:M:S", "Y-m-d", "Y/m/d",
          "d/m/Y", "m/d/Y", "d b Y",
          "b Y", "B Y", "b-Y", "Y-m",
          "Y b", "Y B"
        ),
        tz = "UTC",
        quiet = TRUE
      )
    )
    out[remaining] <- as.Date(parsed)
  }

  out
}

find_abs_series <- function(file, header_regex, series_type = NULL) {
  for (sheet in excel_sheets(file)) {
    if (!str_detect(sheet, "^Data")) next

    # ABS workbooks use rows 1-10 for series metadata and row 11 onward
    # for observations. Reading these separately prevents the date column
    # being coerced to character because it also contains metadata labels.
    meta <- read_excel(
      file,
      sheet = sheet,
      range = cell_rows(1:10),
      col_names = FALSE,
      .name_repair = "minimal"
    )

    headers <- as.character(unlist(meta[1, ], use.names = FALSE))
    types <- as.character(unlist(meta[3, ], use.names = FALSE))

    matches <- which(
      str_detect(headers, regex(header_regex, ignore_case = TRUE))
    )

    if (!is.null(series_type)) {
      matches <- matches[types[matches] == series_type]
    }

    if (length(matches) == 1) {
      observations <- read_excel(
        file,
        sheet = sheet,
        skip = 10,
        col_names = FALSE,
        .name_repair = "minimal"
      )

      date <- parse_abs_date(observations[[1]])
      value <- suppressWarnings(as.numeric(observations[[matches]]))

      result <- tibble(date = date, value = value) |>
        filter(!is.na(date), !is.na(value)) |>
        arrange(date)

      if (nrow(result) == 0) {
        stop(
          "ABS series was found, but no dated observations could be read: ",
          header_regex
        )
      }

      return(result)
    }
  }

  stop("ABS series not uniquely identified: ", header_regex)
}

lookup_value <- function(data, target_date) {
  target <- if (inherits(target_date, "Date")) {
    as.Date(target_date)
  } else if (inherits(target_date, "POSIXt")) {
    as.Date(target_date, tz = "UTC")
  } else {
    parse_abs_date(target_date)[1]
  }

  if (is.na(target)) {
    stop("Comparison date could not be parsed: ", target_date)
  }

  value <- data |> filter(date == target) |> pull(value)
  if (length(value) != 1) stop("Comparison date missing: ", target_date)
  value
}

signed_number <- function(x, digits = 1, suffix = "") {
  if (is.na(x)) return("n/a")
  paste0(ifelse(x > 0, "+", ""), format(round(x, digits), nsmall = digits, trim = TRUE), suffix)
}

change_status <- function(x, lower_is_better = FALSE) {
  if (is.na(x) || x == 0) return("neutral")
  if (lower_is_better) ifelse(x < 0, "good", "bad") else ifelse(x > 0, "good", "bad")
}

inflation_level_status <- function(x) {
  if (is.na(x)) return("neutral")
  if (x >= 2 && x <= 3) return("good")
  if (x >= 1.5 && x <= 3.5) return("neutral")
  "bad"
}

annualised_rate <- function(data, periods = 6, periods_per_year = 12) {
  data |> arrange(date) |> mutate(value = 100 * ((value / lag(value, periods)) ^ (periods_per_year / periods) - 1)) |> filter(!is.na(value))
}

safe_table_update <- function(page_url, predicate, output_file) {
  tryCatch({
    tables <- read_html(page_url) |> html_elements("table") |> html_table(fill = TRUE)
    hit <- keep(tables, predicate)
    if (length(hit) == 0) stop("table not found")
    write_csv(hit[[1]], output_file)
    TRUE
  }, error = function(e) {
    warning("Keeping cached detail table: ", basename(output_file), " (", conditionMessage(e), ")")
    FALSE
  })
}

get_lowest_fuel_price <- function(fuel = c("ulp", "diesel"), location = "Perth") {
  fuel <- match.arg(fuel)
  url <- paste0("https://wafuelfinder.com/", fuel, "/", URLencode(location, reserved = TRUE), "/today")
  page_text <- read_html(url) |> html_element("body") |> html_text2()
  lines <- str_split(page_text, "\\n")[[1]] |> str_trim()
  lines <- lines[nzchar(lines)]

  effective <- lines[str_detect(lines, regex("^Prices effective", ignore_case = TRUE))][1]
  price_idx <- which(str_detect(lines, "^[0-9]{2,3}\\.[0-9]$"))
  if (length(price_idx) == 0) stop("No fuel prices found at ", url)

  prices <- parse_double(lines[price_idx])
  stations <- map_chr(price_idx, function(i) if (i < length(lines)) lines[i + 1] else "")
  keep <- is.finite(prices) & prices >= 50 & prices <= 600
  prices <- prices[keep]; stations <- stations[keep]
  if (length(prices) == 0) stop("No plausible fuel prices found at ", url)

  i <- which.min(prices)
  tibble(
    fuel = ifelse(fuel == "ulp", "ULP", "Diesel"),
    location = location,
    lowest_price_cpl = prices[i],
    station = stations[i],
    effective = str_remove(effective %||% "", regex("^Prices effective\\s*", ignore_case = TRUE)),
    source_url = url
  )
}

extract_monthly_items <- function(obj) {
  candidates <- list(
    obj$data$items, obj$items, obj$data, obj$result$items, obj$result
  )
  for (x in candidates) {
    if (is.data.frame(x) && nrow(x) > 0) return(x)
    if (is.list(x) && length(x) > 0 && all(map_lgl(x, is.list))) {
      out <- tryCatch(bind_rows(x), error = function(e) NULL)
      if (is.data.frame(out) && nrow(out) > 0) return(out)
    }
  }
  NULL
}

get_fuelwatch_monthly_parse <- function(api_key, fuel_type, region = "Metro", date_from = Sys.Date() %m-% years(1), date_to = Sys.Date()) {
  endpoint <- "https://api.parse.bot/scraper/94dcac47-54ed-4b3d-940d-015801b89a29/get_monthly_average_prices"
  response <- request(endpoint) |>
    req_headers(`X-API-Key` = api_key) |>
    req_url_query(
      fuel_type = fuel_type,
      region = region,
      date_from = format(date_from, "%d %b %Y"),
      date_to = format(date_to, "%d %b %Y")
    ) |>
    req_retry(max_tries = 3) |>
    req_perform()

  obj <- fromJSON(resp_body_string(response), simplifyDataFrame = TRUE)
  items <- extract_monthly_items(obj)
  if (is.null(items)) stop("Monthly FuelWatch connector returned no usable records")
  names(items) <- names(items) |> str_to_lower() |> str_replace_all("[^a-z0-9]+", "_")
  month_col <- names(items)[str_detect(names(items), "^month$|month_date|date")][1]
  avg_col <- names(items)[str_detect(names(items), "^average$|average_price|avg")][1]
  if (is.na(month_col) || is.na(avg_col)) stop("Monthly connector response columns were not recognised")
  tibble(
    date = as.Date(floor_date(parse_date_time(as.character(items[[month_col]]), orders = c("Y-m-d", "d b Y", "b Y", "B Y", "Y-m")), "month")),
    value = parse_number(as.character(items[[avg_col]]))
  ) |> filter(!is.na(date), !is.na(value)) |> distinct(date, .keep_all = TRUE) |> arrange(date)
}

update_fuelwatch_monthly <- function(output_file = "data/fuel_monthly.csv") {
  key <- Sys.getenv("PARSE_API_KEY", unset = "")
  if (!nzchar(key)) {
    message("PARSE_API_KEY not set: keeping cached FuelWatch monthly table.")
    return(FALSE)
  }

  tryCatch({
    start <- floor_date(Sys.Date() %m-% months(14), "month")
    ulp <- get_fuelwatch_monthly_parse(key, "ULP", "Metro", start, Sys.Date()) |> rename(ulp_cpl = value)
    diesel <- get_fuelwatch_monthly_parse(key, "DSL", "Metro", start, Sys.Date()) |> rename(diesel_cpl = value)
    out <- full_join(ulp, diesel, by = "date") |> arrange(desc(date)) |>
      mutate(month = format(date, "%B %Y")) |> select(date, month, ulp_cpl, diesel_cpl)
    write_csv(out, output_file)
    TRUE
  }, error = function(e) {
    warning("FuelWatch monthly refresh failed; cached values retained. ", conditionMessage(e))
    FALSE
  })
}
