# WA Economic Indicators — live v1

This package contains:

- `index.html`: immediately viewable June 2026 live-data snapshot
- `index.qmd`: Quarto source
- `styles.css`: dashboard styling
- `R/update_data.R`: downloads and processes the latest ABS spreadsheets/tables
- `R/helpers.R`: reusable data-import functions
- `data/`: current dashboard CSV files
- `.github/workflows/update-dashboard.yml`: weekday data refresh and GitHub Pages deployment

## Local setup

Install R, RStudio and Quarto. Then install the packages:

```r
install.packages(c(
  "readxl", "rvest", "xml2", "dplyr", "purrr", "stringr",
  "tidyr", "readr", "lubridate", "glue", "jsonlite", "htmltools"
))
```

Refresh the ABS data:

```r
source("R/update_data.R")
```

Preview the Quarto site:

```bash
quarto preview
```

## Indicator definitions

- WA unemployment rate: persons, seasonally adjusted
- Perth CPI: All Groups annual percentage change
- WA dwelling approvals: total dwelling units, total sectors, seasonally adjusted

The March 2020 comparison is:

- percentage-point change for unemployment
- cumulative index change for CPI
- percentage change in approval numbers for dwellings

## Important

ABS data can be revised. The refresh script redownloads the complete relevant
series rather than appending only the newest observation.