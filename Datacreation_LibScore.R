# ============================================================================
# Datacreation_LibScore.R
# Load liberal social science country-year data, map to V-Dem codes,
# merge with V-Dem variables, operationalize, create lags, save
# ============================================================================
rm(list = ls())

if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(countrycode)
library(dplyr)

# --- Load liberal social science data ---
cy_path <- "Data/libsci_countryyear.csv"
if (!file.exists(cy_path)) stop(paste0("File not found: ", cy_path))

cy <- read.csv(cy_path, stringsAsFactors = FALSE)
cat("Data loaded from", cy_path, "\n")
cat("Dimensions:", nrow(cy), "rows,", ncol(cy), "columns\n")
cat("Columns:", paste(names(cy), collapse = ", "), "\n\n")

# --- Map iso3c to V-DEM codes and country names ---
cy$vdem_code <- countrycode(cy$iso3c, origin = "iso3c", destination = "vdem", warn = FALSE)
cy$country <- countrycode(cy$iso3c, origin = "iso3c", destination = "country.name", warn = FALSE)

cat("V-DEM code mapping summary:\n")
cat("  Total rows:", nrow(cy), "\n")
cat("  Rows successfully mapped:", sum(!is.na(cy$vdem_code)), "\n")
cat("  Rows with missing V-DEM codes:", sum(is.na(cy$vdem_code)), "\n\n")

# List unmapped countries
unmapped <- unique(cy$iso3c[is.na(cy$vdem_code)])
if (length(unmapped) > 0) {
  cat("UNMAPPED ISO3C CODES (", length(unmapped), "):\n", sep = "")
  for (i in seq_along(unmapped)) cat("  ", i, ". ", unmapped[i], "\n", sep = "")
  write.csv(data.frame(iso3c = unmapped), "Data/unmapped_countries_libsci.csv", row.names = FALSE)
  cat("Saved to: Data/unmapped_countries_libsci.csv\n\n")
} else {
  cat("All ISO3C codes successfully mapped to V-DEM codes.\n\n")
}

# Drop rows without V-DEM code
cy <- cy |> filter(!is.na(vdem_code))
cat("After dropping unmapped:", nrow(cy), "rows\n")

# --- Load and merge V-DEM data ---
vdem_path <- "Data/V-Dem-CY-Full+Others-v15.rds"
if (!file.exists(vdem_path)) stop(paste0("V-DEM file not found: ", vdem_path))

vdem <- readRDS(vdem_path)
cat("V-DEM dataset loaded:", nrow(vdem), "rows,", ncol(vdem), "columns\n")

required_vdem_cols <- c("country_id", "year", "v2x_polyarchy", "e_gdppc", "v2x_regime", "e_pop")
if (!all(required_vdem_cols %in% names(vdem))) {
  stop(paste("Missing V-DEM columns:", paste(setdiff(required_vdem_cols, names(vdem)), collapse = ", ")))
}

vdem_subset <- vdem[, required_vdem_cols]

cy_merged <- merge(cy, vdem_subset,
                   by.x = c("vdem_code", "year"),
                   by.y = c("country_id", "year"),
                   all.x = TRUE)

cat("\nMerge results:\n")
cat("  Original rows:", nrow(cy), "\n")
cat("  Merged rows:", nrow(cy_merged), "\n")
cat("  Rows with v2x_polyarchy:", sum(!is.na(cy_merged$v2x_polyarchy)), "\n")
cat("  Rows missing v2x_polyarchy:", sum(is.na(cy_merged$v2x_polyarchy)), "\n\n")

# --- Operationalize ---
cy_merged$log_gdp <- log(cy_merged$e_gdppc + 1)
cy_merged$log_pop <- log(cy_merged$e_pop + 1)
cy_merged$autocracy <- ifelse(!is.na(cy_merged$v2x_regime) & cy_merged$v2x_regime < 2, 1,
                              ifelse(!is.na(cy_merged$v2x_regime) & cy_merged$v2x_regime >= 2, 0, NA))

cat("=== Autocracy indicator ===\n")
cat("  Autocracy (1):", sum(cy_merged$autocracy == 1, na.rm = TRUE), "\n")
cat("  Democracy (0):", sum(cy_merged$autocracy == 0, na.rm = TRUE), "\n")
cat("  NA:", sum(is.na(cy_merged$autocracy)), "\n\n")

# --- Create lagged variables (2, 3, and 4 years) ---
data <- cy_merged |>
  arrange(vdem_code, year) |>
  group_by(vdem_code) |>
  mutate(
    v2x_polyarchy_lag2 = dplyr::lag(v2x_polyarchy, 2, order_by = year),
    v2x_polyarchy_lag3 = dplyr::lag(v2x_polyarchy, 3, order_by = year),
    v2x_polyarchy_lag4 = dplyr::lag(v2x_polyarchy, 4, order_by = year),
    autocracy_lag2 = dplyr::lag(autocracy, 2, order_by = year),
    autocracy_lag3 = dplyr::lag(autocracy, 3, order_by = year),
    autocracy_lag4 = dplyr::lag(autocracy, 4, order_by = year),
    log_gdp_lag2 = dplyr::lag(log_gdp, 2, order_by = year),
    log_gdp_lag3 = dplyr::lag(log_gdp, 3, order_by = year),
    log_gdp_lag4 = dplyr::lag(log_gdp, 4, order_by = year),
    log_pop_lag2 = dplyr::lag(log_pop, 2, order_by = year),
    log_pop_lag3 = dplyr::lag(log_pop, 3, order_by = year),
    log_pop_lag4 = dplyr::lag(log_pop, 4, order_by = year)
  ) |>
  ungroup()

cat("=== Lag variable coverage ===\n")
lag_vars <- c("autocracy_lag2", "autocracy_lag3", "autocracy_lag4",
              "log_gdp_lag2", "log_gdp_lag3", "log_gdp_lag4",
              "log_pop_lag2", "log_pop_lag3", "log_pop_lag4")
for (v in lag_vars) {
  cat(sprintf("  %-25s non-NA: %d / %d\n", v, sum(!is.na(data[[v]])), nrow(data)))
}

cat("\n=== Summary of key variables ===\n")
print(summary(data[, c("lib_score", "autocracy", "log_gdp", "log_pop")]))

# --- Save ---
out_path <- "Data/cy_libscore_operationalized.rds"
saveRDS(data, out_path)
cat("\nSaved to:", out_path, "\n")
cat("Final dimensions:", nrow(data), "rows,", ncol(data), "columns\n")
cat("Done.\n")
