# ============================================================
# WA ECONOMIC DASHBOARD
# DATA UPDATE SCRIPT
# ============================================================

source("R/helpers.R")


# ============================================================
# 1. SOURCE URLS
# ============================================================

labour_url <- paste0(
  "https://www.abs.gov.au/statistics/labour/",
  "employment-and-unemployment/labour-force-australia/latest-release"
)

cpi_url <- paste0(
  "https://www.abs.gov.au/statistics/economy/",
  "price-indexes-and-inflation/consumer-price-index-australia/latest-release"
)

dwelling_url <- paste0(
  "https://www.abs.gov.au/statistics/industry/",
  "building-and-construction/building-approvals-australia/latest-release"
)

rba_url <- "https://www.rba.gov.au/statistics/cash-rate/"


# ============================================================
# 2. CREATE DATA DIRECTORY
# ============================================================

dir.create(
  "data/raw",
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. DOWNLOAD LATEST ABS WORKBOOKS
# ============================================================

download_abs_xlsx(
  labour_url,
  "62020006\\.xlsx$",
  "data/raw/labour_wa.xlsx"
)

download_abs_xlsx(
  cpi_url,
  "640106\\.xlsx$",
  "data/raw/cpi_analytical.xlsx"
)

download_abs_xlsx(
  cpi_url,
  "6401011\\.xlsx$",
  "data/raw/cpi_capital_annual.xlsx"
)

download_abs_xlsx(
  cpi_url,
  "6401012\\.xlsx$",
  "data/raw/cpi_capital_monthly.xlsx"
)

download_abs_xlsx(
  dwelling_url,
  "8731005\\.xlsx$",
  "data/raw/dwellings_wa.xlsx"
)


# ============================================================
# 4. EXTRACT ABS SERIES
# ============================================================

unemployment <- find_abs_series(
  "data/raw/labour_wa.xlsx",
  "^Unemployment rate ;\\s+Persons ;$",
  "Seasonally Adjusted"
)


perth_cpi_annual <- find_abs_series(
  "data/raw/cpi_capital_annual.xlsx",
  paste0(
    "Percentage Change from Corresponding Month ",
    "of Previous Year ;\\s+All groups CPI ;\\s+Perth"
  ),
  "Original"
)


perth_cpi_monthly <- find_abs_series(
  "data/raw/cpi_capital_monthly.xlsx",
  paste0(
    "Percentage Change from Previous Period ;",
    "\\s+All groups CPI ;\\s+Perth"
  ),
  "Original"
)


trimmed_mean_annual <- find_abs_series(
  "data/raw/cpi_analytical.xlsx",
  paste0(
    "Percentage Change from Corresponding Month ",
    "of Previous Year ;\\s+Trimmed Mean ;\\s+Australia"
  ),
  "Seasonally Adjusted"
)


trimmed_mean_monthly <- find_abs_series(
  "data/raw/cpi_analytical.xlsx",
  paste0(
    "Percentage Change from Previous Period ;",
    "\\s+Trimmed Mean ;\\s+Australia"
  ),
  "Seasonally Adjusted"
)


australia_cpi_sa_index <- find_abs_series(
  "data/raw/cpi_analytical.xlsx",
  paste0(
    "Index Numbers ;\\s+All groups CPI, ",
    "seasonally adjusted ;\\s+Australia"
  ),
  "Seasonally Adjusted"
)


dwellings <- find_abs_series(
  "data/raw/dwellings_wa.xlsx",
  paste0(
    "Total number of dwelling units ;",
    "\\s+Western Australia ;",
    "\\s+Total \\(Type of Building\\) ;",
    "\\s+Total Sectors"
  ),
  "Seasonally Adjusted"
)


# ============================================================
# 5. CPI SHORT-TERM TREND
# ============================================================

cpi_momentum <- annualised_rate(
  australia_cpi_sa_index,
  periods = 6,
  periods_per_year = 12
)


# ============================================================
# 6. LATEST ABS VALUES
# ============================================================

latest_unemp <- tail(unemployment, 1)

latest_perth_cpi <- tail(
  perth_cpi_annual,
  1
)

latest_perth_month <- tail(
  perth_cpi_monthly,
  1
)

latest_trimmed <- tail(
  trimmed_mean_annual,
  1
)

latest_trimmed_month <- tail(
  trimmed_mean_monthly,
  1
)

latest_momentum <- tail(
  cpi_momentum,
  1
)

latest_dwelling <- tail(
  dwellings,
  1
)


# ============================================================
# 7. ABS COMPARISONS
# ============================================================

unemp_month <-
  latest_unemp$value -
  unemployment$value[nrow(unemployment) - 1]


unemp_year <-
  latest_unemp$value -
  lookup_value(
    unemployment,
    latest_unemp$date %m-% years(1)
  )


unemp_covid <-
  latest_unemp$value -
  lookup_value(
    unemployment,
    "2020-03-01"
  )


momentum_month <-
  latest_momentum$value -
  cpi_momentum$value[nrow(cpi_momentum) - 1]


momentum_year_date <-
  latest_momentum$date %m-% years(1)


momentum_year <- if (
  momentum_year_date %in% cpi_momentum$date
) {

  latest_momentum$value -
    lookup_value(
      cpi_momentum,
      momentum_year_date
    )

} else {

  NA_real_

}


dwelling_month <-
  100 * (
    latest_dwelling$value /
      dwellings$value[nrow(dwellings) - 1] -
      1
  )


dwelling_year <-
  100 * (
    latest_dwelling$value /
      lookup_value(
        dwellings,
        latest_dwelling$date %m-% years(1)
      ) -
      1
  )


dwelling_covid <-
  100 * (
    latest_dwelling$value /
      lookup_value(
        dwellings,
        "2020-03-01"
      ) -
      1
  )


# ============================================================
# 8. RBA CASH RATE
# ============================================================

message("Downloading RBA cash-rate decisions...")


get_rba_cash_rate_decisions <- function(page_url) {

  page <- read_html(page_url)

  tables <- page |>
    html_elements("table") |>
    html_table(fill = TRUE)


  # Find the table containing the RBA cash-rate decisions
  cash_tables <- keep(
    tables,
    function(tbl) {

      headings <- names(tbl) |>
        str_squish()

      has_date <- any(
        str_detect(
          headings,
          regex(
            "Effective Date",
            ignore_case = TRUE
          )
        )
      )

      has_rate <- any(
        str_detect(
          headings,
          regex(
            "Cash rate target",
            ignore_case = TRUE
          )
        )
      )

      has_date && has_rate

    }
  )


  if (length(cash_tables) == 0) {

    stop(
      "Could not locate the RBA cash-rate decision table."
    )

  }


  raw <- cash_tables[[1]]


  # Clean the column names
  names(raw) <- names(raw) |>
    str_squish() |>
    str_to_lower() |>
    str_replace_all("%", " percent ") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")


  date_col <- names(raw)[
    str_detect(
      names(raw),
      "effective.*date"
    )
  ][1]


  change_col <- names(raw)[
    str_detect(
      names(raw),
      "change"
    )
  ][1]


  rate_col <- names(raw)[
    str_detect(
      names(raw),
      "cash.*rate.*target"
    )
  ][1]


  if (
    is.na(date_col) ||
    is.na(change_col) ||
    is.na(rate_col)
  ) {

    stop(
      "RBA cash-rate columns were not recognised."
    )

  }


  out <- tibble(

    effective_date =
      as.Date(
        parse_date_time(
          as.character(
            raw[[date_col]]
          ),
          orders = c(
            "d b Y",
            "d B Y"
          ),
          quiet = TRUE
        )
      ),

    change_ppt =
      parse_number(
        as.character(
          raw[[change_col]]
        )
      ),

    cash_rate_target =
      parse_number(
        as.character(
          raw[[rate_col]]
        )
      )

  ) |>

    filter(
      !is.na(effective_date),
      !is.na(cash_rate_target)
    ) |>

    mutate(

      decision = case_when(

        change_ppt > 0 ~ "Increase",

        change_ppt < 0 ~ "Decrease",

        TRUE ~ "No change"

      )

    ) |>

    select(
      effective_date,
      decision,
      change_ppt,
      cash_rate_target
    ) |>

    distinct(
      effective_date,
      .keep_all = TRUE
    ) |>

    arrange(
      effective_date
    )


  if (nrow(out) == 0) {

    stop(
      "RBA cash-rate table contained no usable observations."
    )

  }


  out
}


# ============================================================
# 9. DOWNLOAD RBA DATA WITH CACHE FALLBACK
# ============================================================

rba_cache_file <-
  "data/cash_rate_decisions.csv"


rba_decisions <- tryCatch(

  {

    rba_data <-
      get_rba_cash_rate_decisions(
        rba_url
      )

    write_csv(
      rba_data,
      rba_cache_file
    )

    message(
      "RBA cash-rate data refreshed successfully."
    )

    rba_data

  },

  error = function(e) {

    warning(
      "RBA refresh failed: ",
      conditionMessage(e),
      ". Attempting to use cached RBA data."
    )


    if (
      file.exists(rba_cache_file)
    ) {

      cached <- read_csv(
        rba_cache_file,
        show_col_types = FALSE
      ) |>

        mutate(

          effective_date =
            as.Date(effective_date),

          change_ppt =
            as.numeric(change_ppt),

          cash_rate_target =
            as.numeric(cash_rate_target)

        ) |>

        arrange(
          effective_date
        )


      message(
        "Using cached RBA cash-rate data."
      )

      cached

    } else {

      NULL

    }

  }

)


if (
  is.null(rba_decisions) ||
  nrow(rba_decisions) == 0
) {

  stop(
    paste0(
      "RBA cash-rate data could not be downloaded ",
      "and no cached file is available."
    )
  )

}


# ============================================================
# 10. CURRENT PERTH DATE
# ============================================================

perth_now <- with_tz(
  Sys.time(),
  "Australia/Perth"
)

today_perth <- as.Date(
  perth_now
)


# ============================================================
# 11. FUNCTION: RBA RATE IN EFFECT ON A GIVEN DATE
# ============================================================

rba_rate_on <- function(
  decisions,
  target_date
) {

  target_date <- as.Date(
    target_date
  )


  available <- decisions |>

    filter(
      effective_date <= target_date
    ) |>

    arrange(
      desc(effective_date)
    )


  if (nrow(available) == 0) {

    return(
      NA_real_
    )

  }


  available$cash_rate_target[[1]]

}


# ============================================================
# 12. CURRENT CASH RATE
# ============================================================

latest_rba_decision <- rba_decisions |>

  filter(
    effective_date <= today_perth
  ) |>

  arrange(
    desc(effective_date)
  ) |>

  slice(1)


if (nrow(latest_rba_decision) == 0) {

  stop(
    "No current RBA cash-rate decision was found."
  )

}


latest_cash_rate <-
  latest_rba_decision$cash_rate_target[[1]]


latest_cash_date <-
  latest_rba_decision$effective_date[[1]]


# ============================================================
# 13. CASH-RATE COMPARISONS
# ============================================================

cash_rate_month_ago <- rba_rate_on(
  rba_decisions,
  today_perth %m-% months(1)
)


cash_rate_year_ago <- rba_rate_on(
  rba_decisions,
  today_perth %m-% years(1)
)


cash_rate_covid <- rba_rate_on(
  rba_decisions,
  as.Date("2020-03-01")
)


cash_month_change <-
  latest_cash_rate -
  cash_rate_month_ago


cash_year_change <-
  latest_cash_rate -
  cash_rate_year_ago


cash_covid_change <-
  latest_cash_rate -
  cash_rate_covid


# ============================================================
# 14. CASH-RATE SPARKLINE
#     MONTH-END RATE FOR LAST 36 MONTHS
# ============================================================

cash_month_starts <- seq.Date(

  from =
    floor_date(
      today_perth,
      "month"
    ) %m-% months(35),

  to =
    floor_date(
      today_perth,
      "month"
    ),

  by = "month"

)


cash_spark_values <- vapply(

  seq_along(
    cash_month_starts
  ),

  function(i) {

    month_start <-
      cash_month_starts[i]


    month_end <-
      as.Date(
        ceiling_date(
          month_start,
          "month"
        ) -
          days(1)
      )


    comparison_date <- min(
      month_end,
      today_perth
    )


    rba_rate_on(
      rba_decisions,
      comparison_date
    )

  },

  numeric(1)

)


# ============================================================
# 15. BUILD MAIN ECONOMIC SUMMARY
# ============================================================

# Perth monthly CPI begins in 2024, so the
# March 2020 cumulative price change is not
# calculated for that series.

base_summary <- tibble(

  indicator = c(

    "WA unemployment rate",

    "Perth headline CPI",

    "Australia underlying CPI",

    "Australia CPI short-term trend",

    "RBA cash rate target",

    "WA dwelling approvals"

  ),


  group = c(

    "Labour market",

    "Prices",

    "Prices",

    "Prices",

    "Monetary policy",

    "Housing"

  ),


  latest_display = c(

    paste0(
      round(
        latest_unemp$value,
        1
      ),
      "%"
    ),

    paste0(
      round(
        latest_perth_cpi$value,
        1
      ),
      "%"
    ),

    paste0(
      round(
        latest_trimmed$value,
        1
      ),
      "%"
    ),

    paste0(
      round(
        latest_momentum$value,
        1
      ),
      "%"
    ),

    paste0(
      format(
        round(
          latest_cash_rate,
          2
        ),
        nsmall = 2,
        trim = TRUE
      ),
      "%"
    ),

    format(
      round(
        latest_dwelling$value
      ),
      big.mark = ",",
      scientific = FALSE
    )

  ),


  month_display = c(

    signed_number(
      unemp_month,
      1,
      " ppt"
    ),

    signed_number(
      latest_perth_month$value,
      1,
      "%"
    ),

    signed_number(
      latest_trimmed_month$value,
      1,
      "%"
    ),

    signed_number(
      momentum_month,
      1,
      " ppt"
    ),

    signed_number(
      cash_month_change,
      2,
      " ppt"
    ),

    signed_number(
      dwelling_month,
      1,
      "%"
    )

  ),


  year_display = c(

    signed_number(
      unemp_year,
      1,
      " ppt"
    ),

    signed_number(
      latest_perth_cpi$value,
      1,
      "%"
    ),

    signed_number(
      latest_trimmed$value,
      1,
      "%"
    ),

    signed_number(
      momentum_year,
      1,
      " ppt"
    ),

    signed_number(
      cash_year_change,
      2,
      " ppt"
    ),

    signed_number(
      dwelling_year,
      1,
      "%"
    )

  ),


  base_display = c(

    signed_number(
      unemp_covid,
      1,
      " ppt"
    ),

    "n/a",

    "n/a",

    "n/a",

    signed_number(
      cash_covid_change,
      2,
      " ppt"
    ),

    signed_number(
      dwelling_covid,
      1,
      "%"
    )

  ),


  latest_status = c(

    "bad",

    inflation_level_status(
      latest_perth_cpi$value
    ),

    inflation_level_status(
      latest_trimmed$value
    ),

    inflation_level_status(
      latest_momentum$value
    ),

    "neutral",

    "good"

  ),


  month_status = c(

    change_status(
      unemp_month,
      TRUE
    ),

    change_status(
      latest_perth_month$value,
      TRUE
    ),

    change_status(
      latest_trimmed_month$value,
      TRUE
    ),

    change_status(
      momentum_month,
      TRUE
    ),

    "neutral",

    change_status(
      dwelling_month
    )

  ),


  year_status = c(

    change_status(
      unemp_year,
      TRUE
    ),

    inflation_level_status(
      latest_perth_cpi$value
    ),

    inflation_level_status(
      latest_trimmed$value
    ),

    change_status(
      momentum_year,
      TRUE
    ),

    "neutral",

    change_status(
      dwelling_year
    )

  ),


  base_status = c(

    change_status(
      unemp_covid,
      TRUE
    ),

    "neutral",

    "neutral",

    "neutral",

    "neutral",

    change_status(
      dwelling_covid
    )

  ),


  spark_values = c(

    paste(
      tail(
        unemployment$value,
        36
      ),
      collapse = "|"
    ),

    paste(
      tail(
        perth_cpi_annual$value,
        36
      ),
      collapse = "|"
    ),

    paste(
      tail(
        trimmed_mean_annual$value,
        36
      ),
      collapse = "|"
    ),

    paste(
      tail(
        cpi_momentum$value,
        36
      ),
      collapse = "|"
    ),

    paste(
      cash_spark_values,
      collapse = "|"
    ),

    paste(
      tail(
        dwellings$value,
        36
      ),
      collapse = "|"
    )

  ),


  spark_colour = c(

    "#0759a6",

    "#7dbb19",

    "#5c43a5",

    "#bf4b7a",

    "#28666e",

    "#d88600"

  ),


  note = c(

    paste0(
      "Seasonally adjusted · ",
      format(
        latest_unemp$date,
        "%B %Y"
      )
    ),

    paste0(
      "Perth All Groups, annual inflation · ",
      format(
        latest_perth_cpi$date,
        "%B %Y"
      )
    ),

    paste0(
      "Trimmed mean, Australia · ",
      format(
        latest_trimmed$date,
        "%B %Y"
      )
    ),

    paste0(
      "6-month annualised, seasonally adjusted · ",
      format(
        latest_momentum$date,
        "%B %Y"
      )
    ),

    paste0(
      "RBA cash rate target · effective ",
      format(
        latest_cash_date,
        "%d %B %Y"
      )
    ),

    paste0(
      "Seasonally adjusted · ",
      format(
        latest_dwelling$date,
        "%B %Y"
      )
    )

  ),


  fuel_row = FALSE

)


# ============================================================
# 16. DAILY PERTH FUEL PRICES
# ============================================================

fuel_daily <- tryCatch(

  bind_rows(

    get_lowest_fuel_price(
      "ulp",
      "Perth"
    ),

    get_lowest_fuel_price(
      "diesel",
      "Perth"
    )

  ),

  error = function(e) {

    warning(
      paste0(
        "Daily fuel refresh failed; ",
        "keeping cached fuel rows. ",
        conditionMessage(e)
      )
    )

    NULL

  }

)


if (
  !is.null(fuel_daily) &&
  nrow(fuel_daily) == 2
) {

  fuel_rows <- fuel_daily |>

    mutate(

      indicator = ifelse(

        fuel == "ULP",

        "Lowest listed ULP price — Perth",

        "Lowest listed diesel price — Perth"

      ),

      group = "Fuel",

      latest_display = paste0(
        format(
          lowest_price_cpl,
          nsmall = 1
        ),
        " cpl"
      ),

      month_display = "—",

      year_display = "—",

      base_display = "—",

      latest_status = "neutral",

      month_status = "empty",

      year_status = "empty",

      base_status = "empty",

      spark_values = "",

      spark_colour = ifelse(
        fuel == "ULP",
        "#b85c00",
        "#40566f"
      ),

      note = paste0(

        "WA Fuel Finder · ",

        effective,

        ifelse(
          nzchar(station),
          paste0(
            " · ",
            station
          ),
          ""
        )

      ),

      fuel_row = TRUE

    ) |>

    select(
      names(base_summary)
    )


} else {


  old <- if (
    file.exists(
      "data/dashboard_summary.csv"
    )
  ) {

    read_csv(
      "data/dashboard_summary.csv",
      show_col_types = FALSE
    )

  } else {

    tibble()

  }


  fuel_rows <- old |>

    filter(
      group == "Fuel"
    ) |>

    select(
      any_of(
        names(base_summary)
      )
    )


  if (nrow(fuel_rows) == 0) {

    fuel_rows <- tibble(

      indicator = c(

        "Lowest listed ULP price — Perth",

        "Lowest listed diesel price — Perth"

      ),

      group = "Fuel",

      latest_display = "Unavailable",

      month_display = "—",

      year_display = "—",

      base_display = "—",

      latest_status = "neutral",

      month_status = "empty",

      year_status = "empty",

      base_status = "empty",

      spark_values = "",

      spark_colour = c(
        "#b85c00",
        "#40566f"
      ),

      note =
        "Daily price refresh unavailable",

      fuel_row = TRUE

    )

  }

}


# ============================================================
# 17. COMBINE SUMMARY DATA
# ============================================================

summary <- bind_rows(
  base_summary,
  fuel_rows
)


write_csv(
  summary,
  "data/dashboard_summary.csv"
)


message(
  "Dashboard summary written."
)


# ============================================================
# 18. MONTHLY FUELWATCH DATA
# ============================================================

# Optional monthly FuelWatch refresh.
# If PARSE_API_KEY is not configured,
# the cached values are retained.

update_fuelwatch_monthly(
  "data/fuel_monthly.csv"
)


# ============================================================
# 19. REFRESH DETAIL TABLES
# ============================================================

safe_table_update(

  labour_url,

  function(x) {

    any(
      str_detect(
        names(x),
        fixed(
          "Western Australia"
        )
      )
    ) &&

      any(
        as.character(
          x[[1]]
        ) ==
          "Unemployment rate"
      )

  },

  "data/labour_market.csv"

)


safe_table_update(

  cpi_url,

  function(x) {

    any(
      names(x) ==
        "Perth"
    ) &&

      any(
        as.character(
          x[[1]]
        ) ==
          "All groups"
      )

  },

  "data/cpi_capital_cities.csv"

)


safe_table_update(

  dwelling_url,

  function(x) {

    any(
      str_detect(
        names(x),
        fixed(
          "Total dwelling units approved"
        )
      )
    ) &&

      any(
        as.character(
          x[[1]]
        ) ==
          "Western Australia"
      )

  },

  "data/dwelling_approvals.csv"

)


# ============================================================
# 20. METADATA
# ============================================================

metadata <- list(

  # Always display dashboard refresh time in Perth
  updated_at = format(
    with_tz(
      Sys.time(),
      "Australia/Perth"
    ),
    "%Y-%m-%d %H:%M:%S %Z"
  ),


  labour_reference_period =
    format(
      latest_unemp$date,
      "%B %Y"
    ),


  cpi_reference_period =
    format(
      latest_perth_cpi$date,
      "%B %Y"
    ),


  dwelling_reference_period =
    format(
      latest_dwelling$date,
      "%B %Y"
    ),


  rba_reference_period =
    format(
      latest_cash_date,
      "%d %B %Y"
    ),


  daily_fuel_reference_period =
    if (
      !is.null(fuel_daily)
    ) {

      paste(
        unique(
          fuel_daily$effective
        ),
        collapse = " / "
      )

    } else {

      "cached"

    },


  fuel_monthly_mode =
    if (
      nzchar(
        Sys.getenv(
          "PARSE_API_KEY"
        )
      )
    ) {

      "automated FuelWatch-backed connector"

    } else {

      paste0(
        "cached monthly values ",
        "(PARSE_API_KEY not configured)"
      )

    },


  cpi_trend_definition =
    paste0(
      "Six-month annualised change in the ",
      "seasonally adjusted Australian All Groups ",
      "CPI index. Dashboard calculation."
    ),


  sources = list(

    labour =
      labour_url,

    cpi =
      cpi_url,

    dwelling =
      dwelling_url,

    cash_rate =
      rba_url,

    ulp =
      "https://wafuelfinder.com/ulp/Perth/today",

    diesel =
      "https://wafuelfinder.com/diesel/Perth/today",

    fuelwatch_monthly =
      paste0(
        "https://www.fuelwatch.wa.gov.au/",
        "retail/monthly"
      )

  )

)


write_json(

  metadata,

  "data/metadata.json",

  pretty = TRUE,

  auto_unbox = TRUE

)


# ============================================================
# 21. VALIDATE UPDATED DASHBOARD
# ============================================================

# Validation is included here because the dashboard now
# contains eight rows rather than the previous seven.

summary_check <- read_csv(
  "data/dashboard_summary.csv",
  show_col_types = FALSE
)


if (nrow(summary_check) != 8) {

  stop(
    paste0(
      "Expected 8 summary rows; found ",
      nrow(summary_check)
    )
  )

}


required <- c(

  "WA unemployment rate",

  "Perth headline CPI",

  "Australia underlying CPI",

  "Australia CPI short-term trend",

  "RBA cash rate target",

  "WA dwelling approvals",

  "Lowest listed ULP price — Perth",

  "Lowest listed diesel price — Perth"

)


if (
  !all(
    required %in%
      summary_check$indicator
  )
) {

  stop(
    "One or more required dashboard rows are missing."
  )

}


parse_latest <- function(label) {

  readr::parse_number(

    summary_check$latest_display[
      summary_check$indicator ==
        label
    ]

  )

}


if (
  !dplyr::between(
    parse_latest(
      "WA unemployment rate"
    ),
    0,
    20
  )
) {

  stop(
    "Unemployment rate failed range check."
  )

}


if (
  !dplyr::between(
    parse_latest(
      "Perth headline CPI"
    ),
    -10,
    20
  )
) {

  stop(
    "Perth CPI failed range check."
  )

}


if (
  !dplyr::between(
    parse_latest(
      "Australia underlying CPI"
    ),
    -10,
    20
  )
) {

  stop(
    "Underlying CPI failed range check."
  )

}


if (
  !dplyr::between(
    parse_latest(
      "RBA cash rate target"
    ),
    0,
    20
  )
) {

  stop(
    "RBA cash rate failed range check."
  )

}


if (
  !dplyr::between(
    parse_latest(
      "WA dwelling approvals"
    ),
    0,
    20000
  )
) {

  stop(
    "Dwelling approvals failed range check."
  )

}


for (
  label in c(
    "Lowest listed ULP price — Perth",
    "Lowest listed diesel price — Perth"
  )
) {

  x <- parse_latest(
    label
  )

  if (
    !is.na(x) &&
    !dplyr::between(
      x,
      50,
      600
    )
  ) {

    stop(
      label,
      " failed range check."
    )

  }

}


message(
  "Validation passed."
)


# ============================================================
# 22. FINISHED
# ============================================================

message(
  paste0(
    "RBA cash rate: ",
    format(
      latest_cash_rate,
      nsmall = 2
    ),
    "%"
  )
)


message(
  paste0(
    "Latest RBA effective date: ",
    format(
      latest_cash_date,
      "%d %B %Y"
    )
  )
)


message(
  paste0(
    "RBA decisions saved: ",
    nrow(rba_decisions)
  )
)


message(
  "Data refresh complete."
)
