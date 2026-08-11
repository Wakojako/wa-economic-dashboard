source("R/helpers.R")

labour_url <- "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release"
cpi_url <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release"
dwelling_url <- "https://www.abs.gov.au/statistics/industry/building-and-construction/building-approvals-australia/latest-release"

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# Stable ABS table workbooks.
download_abs_xlsx(labour_url, "62020006\\.xlsx$", "data/raw/labour_wa.xlsx")
download_abs_xlsx(cpi_url, "640106\\.xlsx$", "data/raw/cpi_analytical.xlsx")
download_abs_xlsx(cpi_url, "6401011\\.xlsx$", "data/raw/cpi_capital_annual.xlsx")
download_abs_xlsx(cpi_url, "6401012\\.xlsx$", "data/raw/cpi_capital_monthly.xlsx")
download_abs_xlsx(dwelling_url, "8731005\\.xlsx$", "data/raw/dwellings_wa.xlsx")

unemployment <- find_abs_series("data/raw/labour_wa.xlsx", "^Unemployment rate ;\\s+Persons ;$", "Seasonally Adjusted")
perth_cpi_annual <- find_abs_series("data/raw/cpi_capital_annual.xlsx", "Percentage Change from Corresponding Month of Previous Year ;\\s+All groups CPI ;\\s+Perth", "Original")
perth_cpi_monthly <- find_abs_series("data/raw/cpi_capital_monthly.xlsx", "Percentage Change from Previous Period ;\\s+All groups CPI ;\\s+Perth", "Original")
trimmed_mean_annual <- find_abs_series("data/raw/cpi_analytical.xlsx", "Percentage Change from Corresponding Month of Previous Year ;\\s+Trimmed Mean ;\\s+Australia", "Seasonally Adjusted")
trimmed_mean_monthly <- find_abs_series("data/raw/cpi_analytical.xlsx", "Percentage Change from Previous Period ;\\s+Trimmed Mean ;\\s+Australia", "Seasonally Adjusted")
australia_cpi_sa_index <- find_abs_series("data/raw/cpi_analytical.xlsx", "Index Numbers ;\\s+All groups CPI, seasonally adjusted ;\\s+Australia", "Seasonally Adjusted")
dwellings <- find_abs_series("data/raw/dwellings_wa.xlsx", "Total number of dwelling units ;\\s+Western Australia ;\\s+Total \\(Type of Building\\) ;\\s+Total Sectors", "Seasonally Adjusted")

cpi_momentum <- annualised_rate(australia_cpi_sa_index, periods = 6, periods_per_year = 12)

latest_unemp <- tail(unemployment, 1)
latest_perth_cpi <- tail(perth_cpi_annual, 1)
latest_perth_month <- tail(perth_cpi_monthly, 1)
latest_trimmed <- tail(trimmed_mean_annual, 1)
latest_trimmed_month <- tail(trimmed_mean_monthly, 1)
latest_momentum <- tail(cpi_momentum, 1)
latest_dwelling <- tail(dwellings, 1)

unemp_month <- latest_unemp$value - unemployment$value[nrow(unemployment)-1]
unemp_year <- latest_unemp$value - lookup_value(unemployment, latest_unemp$date %m-% years(1))
unemp_covid <- latest_unemp$value - lookup_value(unemployment, "2020-03-01")

momentum_month <- latest_momentum$value - cpi_momentum$value[nrow(cpi_momentum)-1]
momentum_year_date <- latest_momentum$date %m-% years(1)
momentum_year <- if (momentum_year_date %in% cpi_momentum$date) latest_momentum$value - lookup_value(cpi_momentum, momentum_year_date) else NA_real_

dwelling_month <- 100 * (latest_dwelling$value / dwellings$value[nrow(dwellings)-1] - 1)
dwelling_year <- 100 * (latest_dwelling$value / lookup_value(dwellings, latest_dwelling$date %m-% years(1)) - 1)
dwelling_covid <- 100 * (latest_dwelling$value / lookup_value(dwellings, "2020-03-01") - 1)

# Perth monthly CPI begins in 2024, so the March 2020 cumulative price change is not calculated here.
base_summary <- tibble(
  indicator = c("WA unemployment rate","Perth headline CPI","Australia underlying CPI","Australia CPI short-term trend","WA dwelling approvals"),
  group = c("Labour market","Prices","Prices","Prices","Housing"),
  latest_display = c(paste0(round(latest_unemp$value,1),"%"), paste0(round(latest_perth_cpi$value,1),"%"), paste0(round(latest_trimmed$value,1),"%"), paste0(round(latest_momentum$value,1),"%"), format(round(latest_dwelling$value),big.mark=",",scientific=FALSE)),
  month_display = c(signed_number(unemp_month,1," ppt"), signed_number(latest_perth_month$value,1,"%"), signed_number(latest_trimmed_month$value,1,"%"), signed_number(momentum_month,1," ppt"), signed_number(dwelling_month,1,"%")),
  year_display = c(signed_number(unemp_year,1," ppt"), signed_number(latest_perth_cpi$value,1,"%"), signed_number(latest_trimmed$value,1,"%"), signed_number(momentum_year,1," ppt"), signed_number(dwelling_year,1,"%")),
  base_display = c(signed_number(unemp_covid,1," ppt"), "n/a", "n/a", "n/a", signed_number(dwelling_covid,1,"%")),
  latest_status = c("bad",inflation_level_status(latest_perth_cpi$value),inflation_level_status(latest_trimmed$value),inflation_level_status(latest_momentum$value),"good"),
  month_status = c(change_status(unemp_month,TRUE),change_status(latest_perth_month$value,TRUE),change_status(latest_trimmed_month$value,TRUE),change_status(momentum_month,TRUE),change_status(dwelling_month)),
  year_status = c(change_status(unemp_year,TRUE),inflation_level_status(latest_perth_cpi$value),inflation_level_status(latest_trimmed$value),change_status(momentum_year,TRUE),change_status(dwelling_year)),
  base_status = c(change_status(unemp_covid,TRUE),"neutral","neutral","neutral",change_status(dwelling_covid)),
  spark_values = c(paste(tail(unemployment$value,36),collapse="|"),paste(tail(perth_cpi_annual$value,36),collapse="|"),paste(tail(trimmed_mean_annual$value,36),collapse="|"),paste(tail(cpi_momentum$value,36),collapse="|"),paste(tail(dwellings$value,36),collapse="|")),
  spark_colour = c("#0759a6","#7dbb19","#5c43a5","#bf4b7a","#d88600"),
  note = c(paste0("Seasonally adjusted · ",format(latest_unemp$date,"%B %Y")),paste0("Perth All Groups, annual inflation · ",format(latest_perth_cpi$date,"%B %Y")),paste0("Trimmed mean, Australia · ",format(latest_trimmed$date,"%B %Y")),paste0("6-month annualised, seasonally adjusted · ",format(latest_momentum$date,"%B %Y")),paste0("Seasonally adjusted · ",format(latest_dwelling$date,"%B %Y"))),
  fuel_row = FALSE
)

# Daily fuel: no trends or percentage-change calculations.
fuel_daily <- tryCatch(bind_rows(get_lowest_fuel_price("ulp","Perth"), get_lowest_fuel_price("diesel","Perth")), error=function(e){ warning("Daily fuel refresh failed; keeping cached fuel rows. ", conditionMessage(e)); NULL })

if (!is.null(fuel_daily) && nrow(fuel_daily)==2) {
  fuel_rows <- fuel_daily |> mutate(
    indicator = ifelse(fuel=="ULP","Lowest listed ULP price — Perth","Lowest listed diesel price — Perth"),
    group = "Fuel",
    latest_display = paste0(format(lowest_price_cpl,nsmall=1)," cpl"),
    month_display="—",year_display="—",base_display="—",
    latest_status="neutral",month_status="empty",year_status="empty",base_status="empty",
    spark_values="",spark_colour=ifelse(fuel=="ULP","#b85c00","#40566f"),
    note = paste0("WA Fuel Finder · ", effective, ifelse(nzchar(station), paste0(" · ",station), "")),
    fuel_row=TRUE
  ) |> select(names(base_summary))
} else {
  old <- if (file.exists("data/dashboard_summary.csv")) read_csv("data/dashboard_summary.csv",show_col_types=FALSE) else tibble()
  fuel_rows <- old |> filter(group=="Fuel") |> select(any_of(names(base_summary)))
  if (nrow(fuel_rows)==0) {
    fuel_rows <- tibble(indicator=c("Lowest listed ULP price — Perth","Lowest listed diesel price — Perth"),group="Fuel",latest_display="Unavailable",month_display="—",year_display="—",base_display="—",latest_status="neutral",month_status="empty",year_status="empty",base_status="empty",spark_values="",spark_colour=c("#b85c00","#40566f"),note="Daily price refresh unavailable",fuel_row=TRUE)
  }
}

summary <- bind_rows(base_summary,fuel_rows)
write_csv(summary,"data/dashboard_summary.csv")

# Optional monthly FuelWatch refresh. Without PARSE_API_KEY, cached values remain available.
update_fuelwatch_monthly("data/fuel_monthly.csv")

# Refresh detail tables from the public ABS release pages; keep the packaged snapshots if page structure changes.
safe_table_update(labour_url, function(x) any(str_detect(names(x),fixed("Western Australia"))) && any(as.character(x[[1]])=="Unemployment rate"), "data/labour_market.csv")
safe_table_update(cpi_url, function(x) any(names(x)=="Perth") && any(as.character(x[[1]])=="All groups"), "data/cpi_capital_cities.csv")
safe_table_update(dwelling_url, function(x) any(str_detect(names(x),fixed("Total dwelling units approved"))) && any(as.character(x[[1]])=="Western Australia"), "data/dwelling_approvals.csv")

metadata <- list(
  updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  labour_reference_period = format(latest_unemp$date,"%B %Y"),
  cpi_reference_period = format(latest_perth_cpi$date,"%B %Y"),
  dwelling_reference_period = format(latest_dwelling$date,"%B %Y"),
  daily_fuel_reference_period = if (!is.null(fuel_daily)) paste(unique(fuel_daily$effective),collapse=" / ") else "cached",
  fuel_monthly_mode = if (nzchar(Sys.getenv("PARSE_API_KEY"))) "automated FuelWatch-backed connector" else "cached monthly values (PARSE_API_KEY not configured)",
  cpi_trend_definition = "Six-month annualised change in the seasonally adjusted Australian All Groups CPI index. Dashboard calculation.",
  sources = list(labour=labour_url,cpi=cpi_url,dwelling=dwelling_url,ulp="https://wafuelfinder.com/ulp/Perth/today",diesel="https://wafuelfinder.com/diesel/Perth/today",fuelwatch_monthly="https://www.fuelwatch.wa.gov.au/retail/monthly")
)
write_json(metadata,"data/metadata.json",pretty=TRUE,auto_unbox=TRUE)
source("R/validate_data.R")
message("Data refresh complete.")
