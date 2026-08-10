# ============================================================================
# Descriptive_Plots.R
# Descriptive plots: democracy/autocracy vs CSS Mean and Liberal Science Score
# ============================================================================
rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

dir.create("Figures", showWarnings = FALSE)

# --- Load and merge both datasets ---
css <- readRDS("Data/cy_cssmean_operationalized.rds")
lib <- readRDS("Data/cy_libscore_operationalized.rds")

d <- merge(
  css[, c("vdem_code", "year", "country", "cssmean", "autocracy",
          "v2x_polyarchy", "v2x_regime", "log_gdp", "log_pop")],
  lib[, c("vdem_code", "year", "lib_score")],
  by = c("vdem_code", "year")
)
cat("Merged data:", nrow(d), "obs,", length(unique(d$vdem_code)), "countries\n")

# Regime type labels
regime_labels <- c("0" = "Closed autocracy", "1" = "Electoral autocracy",
                   "2" = "Electoral democracy", "3" = "Liberal democracy")
d$regime_type <- factor(d$v2x_regime, levels = 0:3, labels = regime_labels)

# Consistent color palette for 4 regime types
regime_colors <- c("Closed autocracy" = "#B2182B",
                   "Electoral autocracy" = "#EF8A62",
                   "Electoral democracy" = "#67A9CF",
                   "Liberal democracy" = "#2166AC")

# ============================================================================
# PLOT 1: Scatter — Polyarchy vs CSS Mean and Liberal Science Score
# ============================================================================
cat("Plot 1: Polyarchy scatter...\n")

d_long <- d |>
  filter(!is.na(regime_type)) |>
  pivot_longer(cols = c(cssmean, lib_score),
               names_to = "measure", values_to = "score") |>
  mutate(measure = ifelse(measure == "cssmean",
                          "Critical Social Science", "Liberal Social Science"))

p1 <- ggplot(d_long, aes(x = v2x_polyarchy, y = score, color = regime_type)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 0.8) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_color_manual(values = regime_colors) +
  labs(x = "V-Dem Polyarchy Index",
       y = "Score",
       color = "Regime type") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave("Figures/desc_polyarchy_scatter.png", p1, width = 10, height = 5.5, dpi = 300)
cat("Saved: Figures/desc_polyarchy_scatter.png\n")

# ============================================================================
# PLOT 2: Box plots by regime type
# ============================================================================
cat("Plot 2: Regime boxplot...\n")

p2 <- ggplot(d_long, aes(x = regime_type, y = score, fill = regime_type)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_fill_manual(values = regime_colors) +
  labs(x = NULL, y = "Score") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, hjust = 1),
        strip.text = element_text(face = "bold"))

ggsave("Figures/desc_regime_boxplot.png", p2, width = 10, height = 5.5, dpi = 300)
cat("Saved: Figures/desc_regime_boxplot.png\n")

# ============================================================================
# PLOT 3: Time trends by regime type
# ============================================================================
cat("Plot 3: Regime trends...\n")

trends <- d_long |>
  filter(!is.na(regime_type) & !is.na(score) & year >= 1991) |>
  group_by(year, regime_type, measure) |>
  summarise(mean_score = mean(score, na.rm = TRUE),
            n = n(), .groups = "drop")

p3 <- ggplot(trends, aes(x = year, y = mean_score, color = regime_type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_color_manual(values = regime_colors) +
  labs(x = "Year", y = "Mean score", color = "Regime type") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave("Figures/desc_regime_trends.png", p3, width = 10, height = 5.5, dpi = 300)
cat("Saved: Figures/desc_regime_trends.png\n")

# ============================================================================
# PLOT 4: Scatter — CSS Mean vs Liberal Science Score
# ============================================================================
cat("Plot 4: CSS vs LibScore scatter...\n")

d_plot4 <- d |> filter(!is.na(regime_type))

p4 <- ggplot(d_plot4, aes(x = cssmean, y = lib_score, color = regime_type)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.7, linetype = "dashed") +
  scale_color_manual(values = regime_colors) +
  labs(x = "Critical Social Science Score",
       y = "Liberal Social Science Score",
       color = "Regime type") +
  annotate("text", x = 70, y = 20,
           label = paste0("r = ", round(cor(d$cssmean, d$lib_score, use = "complete.obs"), 2)),
           size = 4.5, fontface = "italic") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("Figures/desc_css_vs_libscore.png", p4, width = 7, height = 6, dpi = 300)
cat("Saved: Figures/desc_css_vs_libscore.png\n")

# ============================================================================
# PLOT 5: Country-level averages — democracy gap
# ============================================================================
cat("Plot 5: Country averages...\n")

country_avgs <- d |>
  group_by(vdem_code, country) |>
  summarise(mean_css = mean(cssmean, na.rm = TRUE),
            mean_lib = mean(lib_score, na.rm = TRUE),
            mean_polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
            n_years = n(),
            .groups = "drop")

# Identify outliers to label (high score + low democracy, or vice versa)
# Filter out countries with missing polyarchy before computing residuals
country_avgs <- country_avgs |> filter(!is.na(mean_polyarchy))
country_avgs <- country_avgs |>
  mutate(resid_css = resid(lm(mean_css ~ mean_polyarchy, data = country_avgs, na.action = na.exclude)),
         resid_lib = resid(lm(mean_lib ~ mean_polyarchy, data = country_avgs, na.action = na.exclude)))

# Label countries with large residuals (top/bottom 5 each)
label_css <- country_avgs |>
  filter(rank(desc(abs(resid_css))) <= 8) |>
  pull(country)
label_lib <- country_avgs |>
  filter(rank(desc(abs(resid_lib))) <= 8) |>
  pull(country)
labels_all <- unique(c(label_css, label_lib))

ca_long <- country_avgs |>
  pivot_longer(cols = c(mean_css, mean_lib),
               names_to = "measure", values_to = "score") |>
  mutate(measure = ifelse(measure == "mean_css",
                          "Critical Social Science", "Liberal Social Science"),
         label = ifelse(country %in% labels_all, country, ""))

if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(ggrepel)

p5 <- ggplot(ca_long, aes(x = mean_polyarchy, y = score)) +
  geom_point(alpha = 0.5, size = 1.5, color = "gray40") +
  geom_smooth(method = "lm", se = TRUE, color = "#2166AC", linewidth = 0.8) +
  geom_text_repel(aes(label = label), size = 2.8, max.overlaps = 15,
                  color = "gray20", segment.color = "gray70") +
  facet_wrap(~ measure, scales = "free_y") +
  labs(x = "Mean V-Dem Polyarchy Index",
       y = "Mean score (country average)") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))

ggsave("Figures/desc_country_averages.png", p5, width = 10, height = 5.5, dpi = 300)
cat("Saved: Figures/desc_country_averages.png\n")

# ============================================================================
# PLOT 6: Pre-post combined — both measures around autocratic transitions
# ============================================================================
cat("Plot 6: Pre-post combined...\n")

# Identify transitions
d_trans <- d |>
  arrange(vdem_code, year) |>
  group_by(vdem_code) |>
  mutate(autocracy_lag = lag(autocracy),
         transition = ifelse(!is.na(autocracy_lag) & autocracy_lag == 0 & autocracy == 1, 1, 0)) |>
  ungroup()

# Collect windows around each transition
prepost_list <- list()
for (i in which(d_trans$transition == 1)) {
  vc <- d_trans$vdem_code[i]
  t_year <- d_trans$year[i]
  window <- d_trans |>
    filter(vdem_code == vc & year >= (t_year - 5) & year <= (t_year + 5)) |>
    mutate(rel_year = year - t_year)
  prepost_list[[length(prepost_list) + 1]] <- window
}

prepost <- bind_rows(prepost_list)

# Standardize both measures for comparability on same y-axis
prepost <- prepost |>
  mutate(css_z = (cssmean - mean(cssmean, na.rm = TRUE)) / sd(cssmean, na.rm = TRUE),
         lib_z = (lib_score - mean(lib_score, na.rm = TRUE)) / sd(lib_score, na.rm = TRUE))

prepost_avg <- prepost |>
  group_by(rel_year) |>
  summarise(CSS = mean(css_z, na.rm = TRUE),
            LSS = mean(lib_z, na.rm = TRUE),
            n = n(), .groups = "drop") |>
  pivot_longer(cols = c(CSS, LSS), names_to = "measure", values_to = "z_score") |>
  mutate(measure = ifelse(measure == "CSS",
                          "Critical Social Science", "Liberal Social Science"))

p6 <- ggplot(prepost_avg, aes(x = rel_year, y = z_score, color = measure)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Critical Social Science" = "#D6604D",
                                "Liberal Social Science" = "#2166AC")) +
  labs(x = "Years from autocratic transition (0 = transition year)",
       y = "Standardized score (z)",
       color = NULL) +
  scale_x_continuous(breaks = -5:5) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("Figures/desc_prepost_combined.png", p6, width = 8, height = 5.5, dpi = 300)
cat("Saved: Figures/desc_prepost_combined.png\n")

cat("\n=== ALL DESCRIPTIVE PLOTS DONE ===\n")
