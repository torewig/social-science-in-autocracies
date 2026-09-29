# ---------------------------------------------------------------------------
# Interpretability step 1 and 2.
#   1. What does the certainty score track? Regress it on named features.
#   2. Do those features mediate the academic-freedom effect? If the effect
#      survives controlling for every feature we can name, the model is
#      reading something our parse-based instruments cannot see, which is
#      exactly why UnScientify and the Hyland list miss it.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"

con <- dbConnect(duckdb())
features <- as.data.table(dbGetQuery(con, sprintf(
  "SELECT * FROM read_parquet('%s/sentence_features.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)
scores <- fread(file.path(probe_dir, "certainty_scores_full.csv"))
sentences <- merge(scores, features, by = c("ut", "sent_id"))
sentences <- sentences[n_tokens >= 3]

feature_names <- c("n_hedge","n_approximator","n_booster","n_modal","n_negation","n_passive",
                   "n_agent_phrase","n_nominalisation","n_number","n_propn","n_adjective",
                   "n_adverb","n_verb","n_subordinator","n_first_person","n_past_tense",
                   "n_present_tense","n_comparative","n_subclause")
# rates per token, so length is not doing all the work
for (f in feature_names) sentences[, (paste0(f, "_rate")) := get(f) / n_tokens]
rate_names <- paste0(feature_names, "_rate")

cat("############ 1. WHAT DOES THE SCORE TRACK? ############\n")
cat(sprintf("sentences: %d\n\n", nrow(sentences)))
simple <- lm(certainty ~ log(n_tokens), sentences)
cat(sprintf("length alone:                R2 = %.3f\n", summary(simple)$r.squared))
full <- lm(as.formula(paste("certainty ~ log(n_tokens) +", paste(rate_names, collapse = " + "))),
           sentences)
cat(sprintf("length + 19 named features:  R2 = %.3f\n\n", summary(full)$r.squared))

coefs <- as.data.table(summary(full)$coefficients, keep.rownames = "term")
setnames(coefs, c("term", "estimate", "se", "t", "p"))
sds <- sapply(c("log(n_tokens)", rate_names), function(v)
  if (v == "log(n_tokens)") sd(log(sentences$n_tokens)) else sd(sentences[[v]]))
coefs <- coefs[term != "(Intercept)"]
coefs[, standardised := estimate * sds[term] / sd(sentences$certainty)]
cat("standardised contribution to the certainty score (top 12 by magnitude):\n")
print(head(coefs[order(-abs(standardised)), .(term, standardised = round(standardised, 3),
                                              p = signif(p, 2))], 12))

cat("\n############ 2. DO NAMED FEATURES MEDIATE THE EFFECT? ############\n")
by_side <- sentences[, c(.(certainty = mean(certainty)),
                         lapply(.SD, mean)), by = .(ut, is_power_sentence), .SDcols = rate_names]
wide <- dcast(by_side, ut ~ is_power_sentence, value.var = c("certainty", rate_names))
contrast_cols <- c("certainty", rate_names)
for (v in contrast_cols) {
  set(wide, j = paste0(v, "_contrast"),
      value = wide[[paste0(v, "_1")]] - wide[[paste0(v, "_0")]])
}
wide <- wide[!is.na(certainty_contrast)]

papers <- readRDS(file.path(probe_dir, "mover_stayer_papers.rds"))
panel <- merge(wide, papers[, .(ut, daisng_id, iso3, year, v2xca_academ)], by = "ut")
author_range <- panel[, .(n = .N, rng = max(v2xca_academ) - min(v2xca_academ)), by = daisng_id]
real <- author_range[n >= 2 & rng >= 0.5, daisng_id]

contrast_controls <- paste0(rate_names, "_contrast")
run <- function(label, data, controls) {
  form <- sprintf("certainty_contrast ~ v2xca_academ %s | daisng_id + year",
                  if (length(controls)) paste("+", paste(controls, collapse = " + ")) else "")
  fit <- feols(as.formula(form), data, cluster = ~iso3)
  star <- if (pvalue(fit)["v2xca_academ"] < .01) "**" else
          if (pvalue(fit)["v2xca_academ"] < .05) "*" else
          if (pvalue(fit)["v2xca_academ"] < .1) "." else " "
  cat(sprintf("%-46s %+8.5f (se %.5f, p %.3f)%s n=%d\n", label,
              coef(fit)["v2xca_academ"], se(fit)["v2xca_academ"],
              pvalue(fit)["v2xca_academ"], star, nobs(fit)))
}
cat("all authors\n")
run("  no controls", panel, character(0))
run("  + all 19 feature contrasts", panel, contrast_controls)
cat("authors with >= 0.5 SD change\n")
run("  no controls", panel[daisng_id %in% real], character(0))
run("  + all 19 feature contrasts", panel[daisng_id %in% real], contrast_controls)
saveRDS(panel, file.path(probe_dir, "certainty_feature_panel.rds"))
