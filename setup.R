packages <- c("readxl","rvest","xml2","dplyr","purrr","stringr","tidyr","readr","lubridate","glue","jsonlite","htmltools","httr2")
missing <- packages[!vapply(packages, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if (length(missing)) install.packages(missing, repos="https://cloud.r-project.org")
message("R packages ready. Next run: source('run_dashboard.R')")
