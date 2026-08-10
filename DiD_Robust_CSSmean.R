# ============================================================================
# DiD_Robust_CSSmean.R
# Heterogeneity-robust DiD estimators for cssmean ~ autocracy
# Callaway & Sant'Anna (2021), Sun & Abraham (2021), Borusyak et al. (2024)
# Plus TWFE benchmark
# ============================================================================
rm(list = ls())

# --- Packages ---
library(fixest)
library(did)
library(dplyr)
library(ggplot2)
if (!requireNamespace("didimputation", quietly = TRUE)) {
  install.packages("didimputation", repos = "https://cloud.r-project.org")
}
library(didimputation)

dir.create("Tables", showWarnings = FALSE)
dir.create("Figures", showWarnings = FALSE)

# ============================================================================
# PART 1: DATA PREPARATION
# ============================================================================

data <- readRDS("Data/cy_cssmean_operationalized.rds")
cat("Loaded data:", nrow(data), "rows,", ncol(data), "cols\n")

# Sort by country-year
data <- data |> arrange(vdem_code, year)

# Identify transitions: autocracy goes from 0 to 1
data <- data |>
  group_by(vdem_code) |>
  mutate(autocracy_lag1 = dplyr::lag(autocracy, 1, order_by = year),
         transition = ifelse(!is.na(autocracy_lag1) & autocracy_lag1 == 0 & autocracy == 1, 1, 0)) |>
  ungroup()

# First treatment year per country
first_treat <- data |>
  filter(transition == 1) |>
  group_by(vdem_code) |>
  summarise(first_treat_year = min(year), .groups = "drop")

data <- data |> left_join(first_treat, by = "vdem_code")

# Never-treated: code as 0 for `did`, will recode for other packages
data$first_treat_year[is.na(data$first_treat_year)] <- 0

# Analysis sample: non-missing cssmean and autocracy
adata <- data |> filter(!is.na(cssmean) & !is.na(autocracy))
cat("Analysis sample:", nrow(adata), "obs\n")

# Diagnostics
n_treated <- sum(adata$first_treat_year > 0)
n_control <- sum(adata$first_treat_year == 0)
cohorts <- adata |> filter(first_treat_year > 0) |> distinct(vdem_code, first_treat_year)
cat("Treated country-years:", n_treated, "\n")
cat("Control country-years:", n_control, "\n")
cat("Number of treated countries:", nrow(cohorts), "\n")
cat("Number of treatment cohorts:", length(unique(cohorts$first_treat_year)), "\n")
cat("\nCohort distribution:\n")
print(table(cohorts$first_treat_year))

# ============================================================================
# PART 2: TWFE BENCHMARK
# ============================================================================
cat("\n=== TWFE BENCHMARK ===\n")

setFixest_dict(c(
  cssmean = "CSS Mean",
  autocracy = "Autocracy",
  autocracy_lag2 = "Autocracy (t-2)",
  autocracy_lag3 = "Autocracy (t-3)",
  autocracy_lag4 = "Autocracy (t-4)",
  log_gdp = "log(GDP p.c.)",
  log_gdp_lag2 = "log(GDP p.c.) (t-2)",
  log_gdp_lag3 = "log(GDP p.c.) (t-3)",
  log_gdp_lag4 = "log(GDP p.c.) (t-4)",
  log_pop = "log(Population)",
  log_pop_lag2 = "log(Population) (t-2)",
  log_pop_lag3 = "log(Population) (t-3)",
  log_pop_lag4 = "log(Population) (t-4)",
  vdem_code = "Country",
  year = "Year"
))

# Contemporaneous
m_twfe0 <- feols(cssmean ~ autocracy + log_gdp + log_pop | vdem_code + year,
                 data = adata, vcov = "hetero")

# Lagged specifications
m_twfe2 <- feols(cssmean ~ autocracy_lag2 + log_gdp_lag2 + log_pop_lag2 | vdem_code + year,
                 data = adata |> filter(!is.na(autocracy_lag2) & !is.na(log_gdp_lag2) & !is.na(log_pop_lag2)),
                 vcov = "hetero")

m_twfe3 <- feols(cssmean ~ autocracy_lag3 + log_gdp_lag3 + log_pop_lag3 | vdem_code + year,
                 data = adata |> filter(!is.na(autocracy_lag3) & !is.na(log_gdp_lag3) & !is.na(log_pop_lag3)),
                 vcov = "hetero")

m_twfe4 <- feols(cssmean ~ autocracy_lag4 + log_gdp_lag4 + log_pop_lag4 | vdem_code + year,
                 data = adata |> filter(!is.na(autocracy_lag4) & !is.na(log_gdp_lag4) & !is.na(log_pop_lag4)),
                 vcov = "hetero")

cat("\nContemporaneous TWFE:\n")
print(summary(m_twfe0))

etable(m_twfe0, m_twfe2, m_twfe3, m_twfe4,
       title = "TWFE: CSS Mean and Autocracy",
       headers = c("Contemp.", "Lag 2", "Lag 3", "Lag 4"),
       depvar = TRUE,
       fixef.group = list("Country FE" = "vdem_code", "Year FE" = "year"),
       se.below = TRUE,
       fitstat = c("n", "r2", "ar2"),
       file = "Tables/TWFE_DiD.tex",
       replace = TRUE,
       style.tex = style.tex("aer"),
       label = "tab:twfe",
       notes = "Heteroskedasticity-robust standard errors in parentheses. Dependent variable: CSS Mean. Autocracy = 1 if V-Dem regime type < 2.")
cat("Saved: Tables/TWFE_DiD.tex\n")

# ============================================================================
# PART 3: TWFE EVENT STUDY (for comparison)
# ============================================================================
cat("\n=== TWFE EVENT STUDY ===\n")

# Create relative time variable
adata$rel_year <- ifelse(adata$first_treat_year > 0,
                         adata$year - adata$first_treat_year,
                         NA)

# Bin endpoints at -5 and +5
adata$rel_year_binned <- adata$rel_year
adata$rel_year_binned[!is.na(adata$rel_year_binned) & adata$rel_year_binned < -5] <- -5
adata$rel_year_binned[!is.na(adata$rel_year_binned) & adata$rel_year_binned > 5] <- 5

# TWFE event study with fixest
m_twfe_es <- feols(cssmean ~ i(rel_year_binned, ref = -1) + log_gdp + log_pop | vdem_code + year,
                   data = adata |> filter(!is.na(rel_year_binned)),
                   vcov = "hetero")

cat("TWFE event study estimated\n")

# ============================================================================
# PART 4: CALLAWAY & SANT'ANNA (2021)
# ============================================================================
cat("\n=== CALLAWAY & SANT'ANNA ===\n")

# att_gt requires numeric id and group variable
# first_treat_year = 0 means never-treated (did package convention)
# Note: covariates dropped from C-S due to singularity in small cohort-time cells
cs_out <- att_gt(
  yname = "cssmean",
  tname = "year",
  idname = "vdem_code",
  gname = "first_treat_year",
  xformla = ~ 1,
  control_group = "notyettreated",
  est_method = "reg",
  base_period = "varying",
  anticipation = 0,
  allow_unbalanced_panel = TRUE,
  data = adata
)

cat("att_gt completed\n")

# Aggregate to event-study (dynamic)
cs_dynamic <- aggte(cs_out, type = "dynamic", min_e = -5, max_e = 5, na.rm = TRUE)
cat("\nCallaway-Sant'Anna dynamic ATT:\n")
print(summary(cs_dynamic))

# Overall ATT
cs_overall <- aggte(cs_out, type = "simple", na.rm = TRUE)
cat("\nCallaway-Sant'Anna overall ATT:\n")
print(summary(cs_overall))

# Extract event-study data for plotting
cs_es_data <- data.frame(
  rel_year = cs_dynamic$egt,
  estimate = cs_dynamic$att.egt,
  se = cs_dynamic$se.egt,
  ci_lower = cs_dynamic$att.egt - 1.96 * cs_dynamic$se.egt,
  ci_upper = cs_dynamic$att.egt + 1.96 * cs_dynamic$se.egt,
  method = "Callaway & Sant'Anna"
)

# ============================================================================
# PART 5: SUN & ABRAHAM (2021)
# ============================================================================
cat("\n=== SUN & ABRAHAM ===\n")

# sunab needs never-treated coded as large number
adata$first_treat_sa <- ifelse(adata$first_treat_year == 0, 10000, adata$first_treat_year)

m_sa <- feols(cssmean ~ sunab(first_treat_sa, year) + log_gdp + log_pop | vdem_code + year,
              data = adata,
              vcov = "hetero")

cat("Sun-Abraham estimated\n")
cat("\nOverall ATT:\n")
print(summary(m_sa, agg = "ATT"))

# Extract event-study coefficients
sa_coefs <- coeftable(m_sa)
# sunab coefficients are named like "year::rel_year"
sa_idx <- grepl("^year::", rownames(sa_coefs))
sa_es_data <- data.frame(
  rel_year = as.numeric(gsub("year::", "", rownames(sa_coefs)[sa_idx])),
  estimate = sa_coefs[sa_idx, "Estimate"],
  se = sa_coefs[sa_idx, "Std. Error"],
  stringsAsFactors = FALSE
)
sa_es_data$ci_lower <- sa_es_data$estimate - 1.96 * sa_es_data$se
sa_es_data$ci_upper <- sa_es_data$estimate + 1.96 * sa_es_data$se
sa_es_data$method <- "Sun & Abraham"

# Filter to -5 to +5 window
sa_es_data <- sa_es_data |> filter(rel_year >= -5 & rel_year <= 5)

# ============================================================================
# PART 6: BORUSYAK, JARAVEL & SPIESS (2024)
# ============================================================================
cat("\n=== BORUSYAK, JARAVEL & SPIESS ===\n")

# didimputation needs never-treated as Inf
adata$first_treat_bjs <- ifelse(adata$first_treat_year == 0, Inf, adata$first_treat_year)

# Run imputation estimator
bjs_out <- did_imputation(
  data = adata,
  yname = "cssmean",
  gname = "first_treat_bjs",
  tname = "year",
  idname = "vdem_code",
  first_stage = ~ 0 | vdem_code + year,
  horizon = 0:5,
  pretrends = -5:-1
)

cat("BJS estimated\n")
print(bjs_out)

# Extract event-study data
bjs_es_data <- data.frame(
  rel_year = bjs_out$term,
  estimate = bjs_out$estimate,
  se = bjs_out$std.error,
  ci_lower = bjs_out$estimate - 1.96 * bjs_out$std.error,
  ci_upper = bjs_out$estimate + 1.96 * bjs_out$std.error,
  method = "Borusyak et al."
)
# Convert term to numeric (terms like "0", "1", "-1" etc.)
bjs_es_data$rel_year <- as.numeric(as.character(bjs_es_data$rel_year))

# ============================================================================
# PART 7: EVENT-STUDY PLOTS
# ============================================================================
cat("\n=== EVENT-STUDY PLOTS ===\n")

# --- Individual plots ---

# Helper function for consistent styling
plot_eventstudy <- function(es_data, title, color = "#2166AC") {
  ggplot(es_data, aes(x = rel_year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray70") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = color, alpha = 0.15) +
    geom_line(color = color, linewidth = 0.8) +
    geom_point(color = color, size = 2) +
    labs(title = title,
         x = "Years relative to autocratization",
         y = "Estimate (CSS Mean)") +
    scale_x_continuous(breaks = seq(-5, 5, 1)) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

# Callaway-Sant'Anna
p_cs <- plot_eventstudy(cs_es_data, "Callaway & Sant'Anna (2021)", "#D6604D")
ggsave("Figures/fig_eventstudy_CS.png", p_cs, width = 8, height = 5, dpi = 300)
cat("Saved: Figures/fig_eventstudy_CS.png\n")

# Sun-Abraham
p_sa <- plot_eventstudy(sa_es_data, "Sun & Abraham (2021)", "#2166AC")
ggsave("Figures/fig_eventstudy_SA.png", p_sa, width = 8, height = 5, dpi = 300)
cat("Saved: Figures/fig_eventstudy_SA.png\n")

# Borusyak et al.
p_bjs <- plot_eventstudy(bjs_es_data, "Borusyak, Jaravel & Spiess (2024)", "#4DAF4A")
ggsave("Figures/fig_eventstudy_BJS.png", p_bjs, width = 8, height = 5, dpi = 300)
cat("Saved: Figures/fig_eventstudy_BJS.png\n")

# --- Combined comparison plot ---
all_es <- bind_rows(cs_es_data, sa_es_data, bjs_es_data)

# Also add TWFE event study
twfe_coefs <- coeftable(m_twfe_es)
twfe_idx <- grepl("^rel_year_binned::", rownames(twfe_coefs))
twfe_es_data <- data.frame(
  rel_year = as.numeric(gsub("rel_year_binned::", "", rownames(twfe_coefs)[twfe_idx])),
  estimate = twfe_coefs[twfe_idx, "Estimate"],
  se = twfe_coefs[twfe_idx, "Std. Error"],
  stringsAsFactors = FALSE
)
twfe_es_data$ci_lower <- twfe_es_data$estimate - 1.96 * twfe_es_data$se
twfe_es_data$ci_upper <- twfe_es_data$estimate + 1.96 * twfe_es_data$se
twfe_es_data$method <- "TWFE"

all_es <- bind_rows(all_es, twfe_es_data)

method_colors <- c("TWFE" = "gray40",
                   "Callaway & Sant'Anna" = "#D6604D",
                   "Sun & Abraham" = "#2166AC",
                   "Borusyak et al." = "#4DAF4A")

p_combined <- ggplot(all_es, aes(x = rel_year, y = estimate, color = method, fill = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray70") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.1, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  scale_color_manual(values = method_colors) +
  scale_fill_manual(values = method_colors) +
  labs(title = "Event-Study Estimates: Effect of Autocratization on CSS Mean",
       subtitle = "Comparison of TWFE and heterogeneity-robust estimators",
       x = "Years relative to autocratization",
       y = "Estimate (CSS Mean)",
       color = "Method", fill = "Method") +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("Figures/fig_eventstudy_comparison.png", p_combined, width = 9, height = 6, dpi = 300)
cat("Saved: Figures/fig_eventstudy_comparison.png\n")

# ============================================================================
# PART 8: COMPARISON TABLE
# ============================================================================
cat("\n=== COMPARISON TABLE ===\n")

# Collect overall ATTs
twfe_att <- coef(m_twfe0)["autocracy"]
twfe_se <- sqrt(vcov(m_twfe0)["autocracy", "autocracy"])

cs_att <- cs_overall$overall.att
cs_se <- cs_overall$overall.se

sa_agg <- summary(m_sa, agg = "ATT")
sa_att <- sa_agg$coeftable[1, "Estimate"]
sa_se <- sa_agg$coeftable[1, "Std. Error"]

# BJS overall: average of post-treatment horizons
bjs_post <- bjs_es_data |> filter(rel_year >= 0)
bjs_att <- mean(bjs_post$estimate)
bjs_se_avg <- sqrt(mean(bjs_post$se^2))

comparison <- data.frame(
  Method = c("TWFE (contemporaneous)", "TWFE (lag 2)", "TWFE (lag 3)",
             "Callaway \\& Sant'Anna", "Sun \\& Abraham", "Borusyak et al."),
  ATT = c(twfe_att, coef(m_twfe2)["autocracy_lag2"], coef(m_twfe3)["autocracy_lag3"],
          cs_att, sa_att, bjs_att),
  SE = c(twfe_se,
         sqrt(vcov(m_twfe2)["autocracy_lag2", "autocracy_lag2"]),
         sqrt(vcov(m_twfe3)["autocracy_lag3", "autocracy_lag3"]),
         cs_se, sa_se, bjs_se_avg)
)
comparison$CI_lower <- comparison$ATT - 1.96 * comparison$SE
comparison$CI_upper <- comparison$ATT + 1.96 * comparison$SE

cat("\nComparison of ATT estimates:\n")
print(comparison)

# Write LaTeX table
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Comparison of ATT Estimates: Effect of Autocratization on CSS Mean}",
  "\\label{tab:did_comparison}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "Method & ATT & SE & 95\\% CI lower & 95\\% CI upper \\\\",
  "\\midrule"
)
for (i in 1:nrow(comparison)) {
  tex_lines <- c(tex_lines,
    sprintf("%s & %.3f & %.3f & %.3f & %.3f \\\\",
            comparison$Method[i], comparison$ATT[i], comparison$SE[i],
            comparison$CI_lower[i], comparison$CI_upper[i]))
}
tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\small",
  "\\item \\textit{Notes:} TWFE estimated with country and year fixed effects, heteroskedasticity-robust SEs.",
  "Callaway \\& Sant'Anna uses doubly-robust estimation with never-treated controls.",
  "Sun \\& Abraham uses interaction-weighted estimation.",
  "Borusyak et al.\\ uses imputation-based estimation (average of post-treatment horizons).",
  "\\end{tablenotes}",
  "\\end{table}"
)

writeLines(tex_lines, "Tables/DiD_Comparison.tex")
cat("Saved: Tables/DiD_Comparison.tex\n")

cat("\n=== ALL DONE ===\n")
