# WA Economic Indicators — master project

This is the single project folder to use going forward. It combines the latest dashboard design and the update code.

## What updates automatically

- WA unemployment rate — ABS
- Perth headline CPI — ABS
- Australia trimmed-mean CPI — ABS
- Australia CPI short-term trend — calculated from ABS seasonally adjusted CPI
- WA dwelling approvals — ABS
- Lowest listed Perth ULP — WA Fuel Finder
- Lowest listed Perth diesel — WA Fuel Finder

## FuelWatch monthly averages

FuelWatch's official monthly page is JavaScript-only and does not expose a public bulk monthly API in the WA data catalogue. The project therefore has two modes:

1. **Cached mode (default):** the dashboard renders with the last saved monthly averages.
2. **Automatic mode (optional):** set `PARSE_API_KEY` to use a FuelWatch-backed third-party connector for Metro ULP and diesel monthly averages. The free tier is more than sufficient for two monthly-series calls per dashboard refresh, but you should review its terms before using it in a work product.

If you do not want a third-party connector, leave the key blank. The ABS and daily WA Fuel Finder figures still update automatically.

## First run on your home PC

1. Install R, RStudio Desktop and Quarto.
2. Extract this ZIP.
3. Double-click `WA-Economic-Dashboard.Rproj`.
4. In the RStudio Console run:

```r
source("setup.R")
```

5. Then run:

```r
source("run_dashboard.R")
```

6. Open `_site/index.html`.

If `source("R/update_data.R")` reports an error, copy the complete error message into ChatGPT and we can fix that specific step.

## Preview without updating

The root `index.html` is a standalone snapshot. You can double-click it immediately without R.

## GitHub Pages

The included GitHub Actions workflow runs every day at approximately 8:15am Perth time and publishes the refreshed site. You can also run it manually from the Actions tab.


## v3.1 date-import fix

The ABS importer now reads workbook metadata (rows 1-10) separately from observation rows. This prevents mixed text/date columns from triggering `charToDate()` errors and suppresses the large `New names: ...` message from `readxl`.
