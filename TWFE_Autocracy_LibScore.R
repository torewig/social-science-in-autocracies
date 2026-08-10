# ============================================================================
# TWFE_Autocracy_LibScore.R
# Two-way fixed effects: lib_score ~ lagged autocracy + lagged controls
# ============================================================================

if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest")
library(fixest)
library(dplyr)

# Load operationalized data
input_path <- "Data/cy_libscore_operationalized.rds"
data <- readRDS(input_path)

# Sort for lagging
data <- data |> arrange(vdem_code, year)

# Lag independent variables by 2 years (by country)
data <- data |>
  group_by(vdem_code) |>
  mutate(
    autocracy_l2 = lag(autocracy, 2),
    log_gdp_l2 = lag(log_gdp, 2),
    log_pop_l2 = lag(log_pop, 2)
  ) |>
  ungroup()

# Drop rows with missing lags or outcome
model_data <- data |>
  filter(!is.na(lib_score) & !is.na(autocracy_l2) & !is.na(log_gdp_l2) & !is.na(log_pop_l2))

# Estimate two-way fixed effects model
fe_model <- feols(lib_score ~ autocracy_l2 + log_gdp_l2 + log_pop_l2 | vdem_code + year, data = model_data)

# Print summary
print(summary(fe_model))

# Output LaTeX table
if (!requireNamespace("stargazer", quietly = TRUE)) install.packages("stargazer")
library(stargazer)
cat("\nLaTeX table for Overleaf:\n")
stargazer(fe_model, type = "latex",
          title = "TWFE: Liberal Science Score on Lagged Autocracy, GDP, Population",
          label = "tab:twfe_libscore_2lag", single.row = TRUE)

# --- 3-year lagged model ---
data <- data |>
  group_by(vdem_code) |>
  mutate(
    autocracy_l3 = lag(autocracy, 3),
    log_gdp_l3 = lag(log_gdp, 3),
    log_pop_l3 = lag(log_pop, 3)
  ) |>
  ungroup()

model_data3 <- data |>
  filter(!is.na(lib_score) & !is.na(autocracy_l3) & !is.na(log_gdp_l3) & !is.na(log_pop_l3))

fe_model3 <- feols(lib_score ~ autocracy_l3 + log_gdp_l3 + log_pop_l3 | vdem_code + year, data = model_data3)

cat("\n--- 3-year lagged model ---\n")
print(summary(fe_model3))

cat("\nLaTeX table for Overleaf (3-year lag):\n")
stargazer(fe_model3, type = "latex",
          title = "TWFE: Liberal Science Score on 3-year Lagged Autocracy, GDP, Population",
          label = "tab:twfe_libscore_3lag", single.row = TRUE)
