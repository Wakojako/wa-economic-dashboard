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

find_latest_xlsx <- function(page_url, filename_regex) {
  page <- read_html(page_url)

  hrefs <- page |>
    html_elements("a") |>
    html_attr("href") |>
    discard(is.na)

  hit <- hrefs[
    str_detect(
      hrefs,
      regex(filename_regex, ignore_case = TRUE)
    )
  ][1]

  if (is.na(hit)) {
    stop(
      "Could not find an ABS spreadsheet matching: ",
      filename_regex
    )
  }

  url_absolute(hit, page_url)
}

download_abs_xlsx <- function(
    page_url,
    filename_regex,
    destination) {

  url <- find_latest_xlsx(
    page_url,
    filename_regex
  )

  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  download.file(
    url,
    destination,
    mode = "wb",
    quiet = TRUE
  )

  message("Downloaded ", url)
  invisible(destination)
}

parse_abs_dates <- function(x) {
  # The time-series section is normally imported by readxl as
  # POSIXct/Date after the metadata rows are skipped.
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  # Fallback for Excel serial dates.
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  # Fallback for character dates such as:
  # "2026-06-01", "2026-06-01 00:00:00", or "Jun-2026".
  x <- trimws(as.character(x))
  result <- rep(as.Date(NA), length(x))

  iso_match <- grepl(
    "^\\d{4}-\\d{2}-\\d{2}",
    x
  )

  result[iso_match] <- as.Date(
    substr(x[iso_match], 1, 10),
    format = "%Y-%m-%d"
  )

  remaining <- is.na(result) & nzchar(x)

  if (any(remaining)) {
    parsed <- suppressWarnings(
      parse_date_time(
        x[remaining],
        orders = c(
          "Ymd HMS",
          "Ymd",
          "b-Y",
          "bY",
          "Y-b",
          "Y b",
          "dmy HMS",
          "dmy"
        ),
        quiet = TRUE
      )
    )

    result[remaining] <- as.Date(parsed)
  }

  serial_match <- is.na(result) &
    grepl("^\\d+(\\.\\d+)?$", x)

  if (any(serial_match)) {
    result[serial_match] <- as.Date(
      as.numeric(x[serial_match]),
      origin = "1899-12-30"
    )
  }

  result
}

find_abs_series <- function(
    file,
    header_regex,
    series_type = NULL) {

  for (sheet in excel_sheets(file)) {
    if (!str_detect(sheet, "^Data")) {
      next
    }

    # Read only the ten metadata rows as text.
    metadata <- read_excel(
      file,
      sheet = sheet,
      range = cell_rows(1:10),
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    )

    headers <- as.character(
      unlist(metadata[1, ], use.names = FALSE)
    )

    types <- as.character(
      unlist(metadata[3, ], use.names = FALSE)
    )

    matches <- which(
      str_detect(
        headers,
        regex(
          header_regex,
          ignore_case = TRUE
        )
      )
    )

    if (!is.null(series_type)) {
      matches <- matches[
        types[matches] == series_type
      ]
    }

    if (length(matches) == 1) {
      # Skip the metadata so readxl can recognise column 1
      # as a real Excel date column.
      observations <- read_excel(
        file,
        sheet = sheet,
        skip = 10,
        col_names = FALSE,
        na = c("", "na", "n.p.", "..", "—"),
        .name_repair = "minimal"
      )

      date <- parse_abs_dates(
        observations[[1]]
      )

      value <- suppressWarnings(
        as.numeric(
          observations[[matches]]
        )
      )

      result <- tibble(
        date = date,
        value = value
      ) |>
        filter(
          !is.na(date),
          !is.na(value)
        ) |>
        arrange(date)

      if (nrow(result) == 0) {
        stop(
          "The ABS series was found, but no dated ",
          "observations could be read: ",
          header_regex
        )
      }

      return(result)
    }
  }

  stop(
    "Series was not uniquely identified: ",
    header_regex
  )
}

pick_html_table <- function(
    page_url,
    required_column,
    required_value) {

  tables <- read_html(page_url) |>
    html_elements("table") |>
    html_table(fill = TRUE)

  hits <- keep(
    tables,
    function(x) {
      any(
        str_detect(
          names(x),
          fixed(required_column)
        )
      ) &&
        any(
          str_detect(
            as.character(x[[1]]),
            fixed(required_value)
          )
        )
    }
  )

  if (length(hits) == 0) {
    stop(
      "Could not identify the required table on ",
      page_url
    )
  }

  hits[[1]]
}

signed_number <- function(
    x,
    digits = 1,
    suffix = "") {

  paste0(
    ifelse(x > 0, "+", ""),
    format(
      round(x, digits),
      nsmall = digits
    ),
    suffix
  )
}

change_status <- function(
    x,
    lower_is_better = FALSE) {

  if (is.na(x) || x == 0) {
    return("neutral")
  }

  if (lower_is_better) {
    ifelse(x < 0, "good", "bad")
  } else {
    ifelse(x > 0, "good", "bad")
  }
}
