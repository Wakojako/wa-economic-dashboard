summary_check <- readr::read_csv("data/dashboard_summary.csv", show_col_types = FALSE)
if (nrow(summary_check) != 7) stop("Expected 7 summary rows; found ", nrow(summary_check))
required <- c("WA unemployment rate","Perth headline CPI","Australia underlying CPI","Australia CPI short-term trend","WA dwelling approvals","Lowest listed ULP price — Perth","Lowest listed diesel price — Perth")
if (!all(required %in% summary_check$indicator)) stop("One or more required dashboard rows are missing")
parse_latest <- function(label) readr::parse_number(summary_check$latest_display[summary_check$indicator==label])
if (!dplyr::between(parse_latest("WA unemployment rate"), 0, 20)) stop("Unemployment rate failed range check")
if (!dplyr::between(parse_latest("Perth headline CPI"), -10, 20)) stop("Perth CPI failed range check")
if (!dplyr::between(parse_latest("Australia underlying CPI"), -10, 20)) stop("Underlying CPI failed range check")
if (!dplyr::between(parse_latest("WA dwelling approvals"), 0, 20000)) stop("Dwelling approvals failed range check")
for (label in c("Lowest listed ULP price — Perth","Lowest listed diesel price — Perth")) {
  x <- parse_latest(label)
  if (!is.na(x) && !dplyr::between(x, 50, 600)) stop(label," failed range check")
}
message("Validation passed.")
