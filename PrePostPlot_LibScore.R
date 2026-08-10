# ============================================================================
# PrePostPlot_LibScore.R
# Pre-post plot: average lib_score 5 years before and after transition to autocracy
# ============================================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)
library(dplyr)

dir.create("Figures", showWarnings = FALSE)

# Load operationalized data
input_path <- "Data/cy_libscore_operationalized.rds"
data <- readRDS(input_path)

if (!all(c("country", "year", "autocracy", "lib_score") %in% names(data))) {
  stop("Data must contain 'country', 'year', 'autocracy', and 'lib_score' columns.")
}

data <- data |> arrange(country, year)

# Identify transitions to autocracy (autocracy changes from 0 to 1)
data <- data |>
  group_by(country) |>
  mutate(autocracy_lag = lag(autocracy),
         transition = ifelse(!is.na(autocracy_lag) & autocracy_lag == 0 & autocracy == 1, 1, 0)) |>
  ungroup()

# For each transition, collect lib_score for -5 to +5 years around transition year
prepost_list <- list()
for (i in which(data$transition == 1)) {
  ctry <- data$country[i]
  t_year <- data$year[i]
  window <- data |> filter(country == ctry & year >= (t_year - 5) & year <= (t_year + 5)) |>
    mutate(rel_year = year - t_year)
  window$transition_country <- ctry
  window$transition_year <- t_year
  prepost_list[[length(prepost_list) + 1]] <- window
}

prepost_data <- bind_rows(prepost_list)

# Compute average lib_score by relative year
avg_libscore <- prepost_data |>
  group_by(rel_year) |>
  summarise(mean_libscore = mean(lib_score, na.rm = TRUE), n = n())

# Plot
p <- ggplot(avg_libscore, aes(x = rel_year, y = mean_libscore)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 2, color = "darkblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Average Liberal Science Score: 5 Years Before and After Transition to Autocracy",
       x = "Years from Transition (0 = year of transition)",
       y = "Average Liberal Science Score") +
  theme_minimal(base_size = 14)

print(p)
ggsave("Figures/prepost_libscore_autocracy.png", plot = p, width = 8, height = 5, dpi = 300)
cat("Pre-post plot saved as Figures/prepost_libscore_autocracy.png\n")
