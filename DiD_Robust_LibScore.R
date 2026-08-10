# ============================================================================
# DiD_Robust_LibScore.R
# Heterogeneity-robust DiD estimators for lib_score ~ autocracy
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

data <- readRDS("Data/cy_libscore_operationalized.rds")
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

# Analysis sample: non-missing lib_score and autocracy
adata <- data |> filter(!is.na(lib_score) & !is.na(autocracy))
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
  lib_score = "Liberal Science Score",
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
m_twfe0 <- feols(lib_score ~ autocracy + log_gdp + log_pop | vdem_code + year,
                 data = adata, vcov = "hetero")

# Lagged specifications
m_twfe2 <- feols(lib_score ~ autocracy_lag2 + log_gdp_lag2 + log_pop_lag2 | vdem_code + year,
                 data = adata |> filter(!is.na(autocracy_lag2) & !is.na(log_gdp_lag2) & !is.na(log_pop_lag2)),
                 vcov = "hetero")

m_twfe3 <- feols(lib_score ~ autocracy_lag3 + log_gdp_lag3 + log_pop_lag3 | vdem_code + year,
                 data = adata |> filter(!is.na(autocracy_lag3) & !is.na(log_gdp_lag3) & !is.na(log_pop_lag3)),
                 vcov = "hetero")

m_twfe4 <- feols(lib_score ~ autocracy_lag4 + log_gdp_lag4 + log_pop_lag4 | vdem_code + year,
                 data = adata |> filter(!is.na(autocracy_lag4) & !is.na(log_gdp_lag4) & !is.na(log_pop_lag4)),
                 vcov = "hetero")

cat("\nContemporaneous TWFE:\n")
print(summary(m_twfe0))

etable(m_twfe0, m_twfe2, m_twfe3, m_twfe4,
       title = "TWFE: Liberal Science Score and Autocracy",
       headers = c("Contemp.", "Lag 2", "Lag 3", "Lag 4"),
       depvar = TRUE,
       fixef.group = list("Country FE" = "vdem_code", "Year FE" = "year"),
       se.below = TRUE,
       fitstat = c("n", "r2", "ar2"),
       file = "Tables/TWFE_DiD_LibScore.tex",
       replace = TRUE,
       style.tex = style.tex("aer"),
       label = "tab:twfe_libscore",
       notes = "Heteroskedasticity-robust standard errors in parentheses. Dependent variable: Liberal Science Score. Autocracy = 1 if V-Dem regime type < 2.")
cat("Saved: Tables/TWFE_DiD_LibScore.tex\n")

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
m_twfe_es <- feols(lib_score ~ i(rel_year_binned, ref = -1) + log_gdp + log_pop | vdem_code + year,
                   data = adata |> filter(!is.na(rel_year_binned)),
                   vcov = "hetero")

cat("TWFE event study estimated\n")

# ============================================================================
# PART 4: CALLAWAY & SANT'ANNA (2021)
# ============================================================================
cat("\n=== CALLAWAY & SANT'ANNA ===\n")

cs_ok <- FALSE
cs_out <- tryCatch({
  att_gt(
    yname = "lib_score",
    tname = "year",
    idname = "vdem_code",
    gname = "first_treat_year",
    xformla = ~ 1,
    control_group = "notyettreated",
    est_method = "reg",
    base_period = "varying",
    anticipation = 0,
    allow_unbalanced_panel = TRUE,
    faster_mode = FALSE,
    data = adata
  )
}, error = function(e) {
  cat("Callaway-Sant'Anna failed:", conditionMessage(e), "\n")
  NULL
})

cs_es_data <- NULL
cs_overall <- NULL

if (!is.null(cs_out)) {
  cs_ok <- TRUE
  cat("att_gt completed\n")

  cs_dynamic <- aggte(cs_out, type = "dynamic", min_e = -5, max_e = 5, na.rm = TRUE)
  cat("\nCallaway-Sant'Anna dynamic ATT:\n")
  print(summary(cs_dynamic))

  cs_overall <- aggte(cs_out, type = "simple", na.rm = TRUE)
  cat("\nCallaway-Sant'Anna overall ATT:\n")
  print(summary(cs_overall))

  cs_es_data <- data.frame(
    rel_year = cs_dynamic$egt,
    estimate = cs_dynamic$att.egt,
    se = cs_dynamic$se.egt,
    ci_lower = cs_dynamic$att.egt - 1.96 * cs_dynamic$se.egt,
    ci_upper = cs_dynamic$att.egt + 1.96 * cs_dynamic$se.egt,
    method = "Callaway & Sant'Anna"
  )
} else {
  cat("Skipping Callaway-Sant'Anna event study and overall ATT.\n")
}

# ============================================================================
# PART 5: SUN & ABRAHAM (2021)
# ============================================================================
cat("\n=== SUN & ABRAHAM ===\n")

# sunab needs never-treated coded as large number
adata$first_treat_sa <- ifelse(adata$first_treat_year == 0, 10000, adata$first_treat_year)

m_sa <- feols(lib_score ~ sunab(first_treat_sa, year) + log_gdp + log_pop | vdem_code + year,
              data = adata,
              vcov = "hetero")

cat("Sun-Abraham estimated\n")
cat("\nOverall ATT:\n")
print(summary(m_sa, agg = "ATT"))

# Extract event-study coefficients
sa_coefs <- coeftable(m_sa)
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
bjs_ok <- FALSE
bjs_es_data <- NULL
bjs_out <- tryCatch({
  did_imputation(
    data = adata,
    yname = "lib_score",
    gname = "first_treat_bjs",
    tname = "year",
    idname = "vdem_code",
    first_stage = ~ 0 | vdem_code + year,
    horizon = 0:5,
    pretrends = -5:-1
  )
}, error = function(e) {
  cat("BJS failed:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(bjs_out)) {
  bjs_ok <- TRUE
  cat("BJS estimated\n")
  print(bjs_out)

  bjs_es_data <- data.frame(
    rel_year = bjs_out$term,
    estimate = bjs_out$estimate,
    se = bjs_out$std.error,
    ci_lower = bjs_out$estimate - 1.96 * bjs_out$std.error,
    ci_upper = bjs_out$estimate + 1.96 * bjs_out$std.error,
    method = "Borusyak et al."
  )
  bjs_es_data$rel_year <- as.numeric(as.character(bjs_es_data$rel_year))
} else {
  cat("Skipping BJS event study.\n")
}

# ============================================================================
# PART 7: EVENT-STUDY PLOTS
# ============================================================================
cat("\n=== EVENT-STUDY PLOTS ===\n")

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
         y = "Estimate (Liberal Science Score)") +
    scale_x_continuous(breaks = seq(-5, 5, 1)) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
}

# Callaway-Sant'Anna
if (!is.null(cs_es_data)) {
  p_cs <- plot_eventstudy(cs_es_data, "Callaway & Sant'Anna (2021)", "#D6604D")
  ggsave("Figures/fig_eventstudy_CS_libscore.png", p_cs, width = 8, height = 5, dpi = 300)
  cat("Saved: Figures/fig_eventstudy_CS_libscore.png\n")
}

# Sun-Abraham
p_sa <- plot_eventstudy(sa_es_data, "Sun & Abraham (2021)", "#2166AC")
ggsave("Figures/fig_eventstudy_SA_libscore.png", p_sa, width = 8, height = 5, dpi = 300)
cat("Saved: Figures/fig_eventstudy_SA_libscore.png\n")

# Borusyak et al.
if (!is.null(bjs_es_data)) {
  p_bjs <- plot_eventstudy(bjs_es_data, "Borusyak, Jaravel & Spiess (2024)", "#4DAF4A")
  ggsave("Figures/fig_eventstudy_BJS_libscore.png", p_bjs, width = 8, height = 5, dpi = 300)
  cat("Saved: Figures/fig_eventstudy_BJS_libscore.png\n")
}

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
  labs(title = "Event-Study Estimates: Effect of Autocratization on Liberal Science Score",
       subtitle = "Comparison of TWFE and heterogeneity-robust estimators",
       x = "Years relative to autocratization",
       y = "Estimate (Liberal Science Score)",
       color = "Method", fill = "Method") +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("Figures/fig_eventstudy_comparison_libscore.png", p_combined, width = 9, height = 6, dpi = 300)
cat("Saved: Figures/fig_eventstudy_comparison_libscore.png\n")

# ============================================================================
# PART 8: COMPARISON TABLE
# ============================================================================
cat("\n=== COMPARISON TABLE ===\n")

# Collect overall ATTs
twfe_att <- coef(m_twfe0)["autocracy"]
twfe_se <- sqrt(vcov(m_twfe0)["autocracy", "autocracy"])

sa_agg <- summary(m_sa, agg = "ATT")
sa_att <- sa_agg$coeftable[1, "Estimate"]
sa_se <- sa_agg$coeftable[1, "Std. Error"]

# Build comparison table dynamically based on which estimators succeeded
methods <- c("TWFE (contemporaneous)", "TWFE (lag 2)", "TWFE (lag 3)", "Sun \\& Abraham")
atts <- c(twfe_att, coef(m_twfe2)["autocracy_lag2"], coef(m_twfe3)["autocracy_lag3"], sa_att)
ses <- c(twfe_se,
         sqrt(vcov(m_twfe2)["autocracy_lag2", "autocracy_lag2"]),
         sqrt(vcov(m_twfe3)["autocracy_lag3", "autocracy_lag3"]),
         sa_se)

if (cs_ok && !is.null(cs_overall)) {
  methods <- c(methods, "Callaway \\& Sant'Anna")
  atts <- c(atts, cs_overall$overall.att)
  ses <- c(ses, cs_overall$overall.se)
}

if (bjs_ok && !is.null(bjs_es_data)) {
  bjs_post <- bjs_es_data |> filter(rel_year >= 0)
  bjs_att <- mean(bjs_post$estimate)
  bjs_se_avg <- sqrt(mean(bjs_post$se^2))
  methods <- c(methods, "Borusyak et al.")
  atts <- c(atts, bjs_att)
  ses <- c(ses, bjs_se_avg)
}

comparison <- data.frame(Method = methods, ATT = atts, SE = ses)
comparison$CI_lower <- comparison$ATT - 1.96 * comparison$SE
comparison$CI_upper <- comparison$ATT + 1.96 * comparison$SE

cat("\nComparison of ATT estimates:\n")
print(comparison)

# Write LaTeX table
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Comparison of ATT Estimates: Effect of Autocratization on Liberal Science Score}",
  "\\label{tab:did_comparison_libscore}",
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

writeLines(tex_lines, "Tables/DiD_Comparison_LibScore.tex")
cat("Saved: Tables/DiD_Comparison_LibScore.tex\n")

cat("\n=== ALL DONE ===\n")
