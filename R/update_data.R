source("R/helpers.R")

labour_url <- "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release"
cpi_url <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release"
dwelling_url <- "https://www.abs.gov.au/statistics/industry/building-and-construction/building-approvals-australia/latest-release"

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# Filenames are stable across monthly ABS release folders.
download_abs_xlsx(labour_url, "62020010\\.xlsx$", "data/raw/labour_states.xlsx")
download_abs_xlsx(cpi_url, "640109\\.xlsx$", "data/raw/cpi_index_monthly.xlsx")
download_abs_xlsx(cpi_url, "6401011\\.xlsx$", "data/raw/cpi_annual.xlsx")
download_abs_xlsx(cpi_url, "6401012\\.xlsx$", "data/raw/cpi_monthly_change.xlsx")
download_abs_xlsx(cpi_url, "6401017\\.xlsx$", "data/raw/cpi_quarterly.xlsx")
download_abs_xlsx(dwelling_url, "8731005\\.xlsx$", "data/raw/dwellings_wa.xlsx")

unemployment <- find_abs_series(
  "data/raw/labour_states.xlsx",
  "^Unemployment rate ;\\s+Persons ;\\s+> Western Australia",
  "Seasonally Adjusted"
)

cpi_annual <- find_abs_series(
  "data/raw/cpi_annual.xlsx",
  "Percentage Change from Corresponding Month of Previous Year ;\\s+All groups CPI ;\\s+Perth",
  "Original"
)

cpi_monthly <- find_abs_series(
  "data/raw/cpi_monthly_change.xlsx",
  "Percentage Change from Previous Period ;\\s+All groups CPI ;\\s+Perth",
  "Original"
)

cpi_quarterly_index <- find_abs_series(
  "data/raw/cpi_quarterly.xlsx",
  "Index Numbers ;\\s+All groups CPI ;\\s+Perth",
  "Original"
)

dwellings <- find_abs_series(
  "data/raw/dwellings_wa.xlsx",
  "Total number of dwelling units ;\\s+Western Australia ;\\s+Total \\(Type of Building\\) ;\\s+Total Sectors",
  "Seasonally Adjusted"
)

latest_unemp <- tail(unemployment, 1)
latest_cpi <- tail(cpi_annual, 1)
latest_cpi_month <- tail(cpi_monthly, 1)
latest_dwelling <- tail(dwellings, 1)

lookup_value <- function(data, target_date) {
  value <- data |> filter(date == as.Date(target_date)) |> pull(value)
  if (length(value) != 1) stop("Missing comparison date: ", target_date)
  value
}

unemp_month <- latest_unemp$value - unemployment$value[nrow(unemployment) - 1]
unemp_year <- latest_unemp$value - lookup_value(unemployment, latest_unemp$date %m-% years(1))
unemp_covid <- latest_unemp$value - lookup_value(unemployment, "2020-03-01")

latest_cpi_index_date <- max(
  cpi_quarterly_index$date,
  na.rm = TRUE
)

cpi_covid <- 100 * (
  lookup_value(cpi_quarterly_index, latest_cpi_index_date) /
    lookup_value(cpi_quarterly_index, "2020-03-01") - 1
)

dwelling_month <- 100 * (latest_dwelling$value / dwellings$value[nrow(dwellings) - 1] - 1)
dwelling_year <- 100 * (
  latest_dwelling$value / lookup_value(dwellings, latest_dwelling$date %m-% years(1)) - 1
)
dwelling_covid <- 100 * (
  latest_dwelling$value / lookup_value(dwellings, "2020-03-01") - 1
)

summary <- tibble(
  indicator = c("WA unemployment rate", "Perth CPI", "WA dwelling approvals"),
  group = c("Labour market", "Prices", "Housing"),
  latest_display = c(
    paste0(round(latest_unemp$value, 1), "%"),
    paste0(round(latest_cpi$value, 1), "%"),
    format(round(latest_dwelling$value), big.mark = ",", scientific = FALSE)
  ),
  month_display = c(
    signed_number(unemp_month, 1, " ppt"),
    signed_number(latest_cpi_month$value, 1, "%"),
    signed_number(dwelling_month, 1, "%")
  ),
  year_display = c(
    signed_number(unemp_year, 1, " ppt"),
    signed_number(latest_cpi$value, 1, "%"),
    signed_number(dwelling_year, 1, "%")
  ),
  base_display = c(
    signed_number(unemp_covid, 1, " ppt"),
    signed_number(cpi_covid, 1, "%"),
    signed_number(dwelling_covid, 1, "%")
  ),
  latest_status = c("bad", "bad", "good"),
  month_status = c(
    change_status(unemp_month, lower_is_better = TRUE),
    change_status(latest_cpi_month$value, lower_is_better = TRUE),
    change_status(dwelling_month)
  ),
  year_status = c(
    change_status(unemp_year, lower_is_better = TRUE),
    "bad",
    change_status(dwelling_year)
  ),
  base_status = c(
    change_status(unemp_covid, lower_is_better = TRUE),
    "bad",
    change_status(dwelling_covid)
  ),
  spark_values = c(
    paste(tail(unemployment$value, 36), collapse = "|"),
    paste(tail(cpi_quarterly_index$value, 13), collapse = "|"),
    paste(tail(dwellings$value, 36), collapse = "|")
  ),
  spark_colour = c("#0759a6", "#7dbb19", "#d88600"),
  note = c(
    paste0("Seasonally adjusted · ", format(latest_unemp$date, "%B %Y")),
    paste0("Annual inflation · ", format(latest_cpi$date, "%B %Y")),
    paste0("Seasonally adjusted · ", format(latest_dwelling$date, "%B %Y"))
  )
)

write_csv(summary, "data/dashboard_summary.csv")

# Detail tables from the latest ABS webpages.
labour_table <- pick_html_table(labour_url, "Western Australia", "Unemployment rate")
cpi_table <- pick_html_table(cpi_url, "Perth", "Food & non-alcoholic beverages")
dwelling_table <- pick_html_table(dwelling_url, "Total dwelling units approved", "Western Australia")

write_csv(labour_table, "data/labour_market.csv")
write_csv(cpi_table, "data/cpi_capital_cities.csv")
write_csv(dwelling_table, "data/dwelling_approvals.csv")

metadata <- list(
  updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  labour_reference_period = format(latest_unemp$date, "%B %Y"),
  cpi_reference_period = format(latest_cpi$date, "%B %Y"),
  dwelling_reference_period = format(latest_dwelling$date, "%B %Y"),
  sources = list(labour = labour_url, cpi = cpi_url, dwelling = dwelling_url)
)

write_json(metadata, "data/metadata.json", pretty = TRUE, auto_unbox = TRUE)