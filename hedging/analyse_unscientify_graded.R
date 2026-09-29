# ---------------------------------------------------------------------------
# Resolving the instrument disagreement, step 1.
#
# The binary UnScientify flag (any of 12 pattern groups matched, minus
# cancellation) shows nothing where the certainty model shows +0.016. The
# cheaper explanation is that a binary share is too coarse to carry a 3%-of-SD
# effect. The graded version uses the COUNT of pattern groups matched, which is
# the same instrument at higher resolution.
# ---------------------------------------------------------------------------

library(duckdb)
library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"

con <- dbConnect(duckdb())
sentences <- as.data.table(dbGetQuery(con, sprintf("
  SELECT ut, CAST(is_power_sentence AS INTEGER) AS is_power_sentence,
         CAST(is_su_expression AS INTEGER) AS is_su_expression,
         CAST(n_groups_matched AS INTEGER) AS n_groups_matched,
         CAST(n_tokens AS INTEGER) AS n_tokens
  FROM read_parquet('%s/unscientify_sentences.parquet')", probe_dir)))
dbDisconnect(con, shutdown = TRUE)
sentences <- sentences[n_tokens >= 3]

# graded: count of groups, length-residualised like every other measure
sentences[, graded_adjusted := residuals(lm(n_groups_matched ~ log(n_tokens), sentences))]
sentences[, binary_adjusted := residuals(lm(is_su_expression ~ log(n_tokens), sentences))]

cat(sprintf("mean groups matched: power %.3f | other %.3f\n",
            sentences[is_power_sentence == 1, mean(n_groups_matched)],
            sentences[is_power_sentence == 0, mean(n_groups_matched)]))
cat(sprintf("length-adjusted graded gap: %+.4f\n",
            sentences[is_power_sentence == 1, mean(graded_adjusted)] -
            sentences[is_power_sentence == 0, mean(graded_adjusted)]))

by_side <- sentences[, .(graded = mean(graded_adjusted), binary = mean(binary_adjusted)),
                     by = .(ut, is_power_sentence)]
wide <- dcast(by_side, ut ~ is_power_sentence, value.var = c("graded", "binary"))
setnames(wide, c("graded_0","graded_1","binary_0","binary_1"),
         c("graded_other","graded_power","binary_other","binary_power"))
wide <- wide[!is.na(graded_power) & !is.na(graded_other)]
wide[, graded_contrast := graded_power - graded_other]
wide[, binary_contrast := binary_power - binary_other]

papers <- readRDS(file.path(probe_dir, "mover_stayer_papers.rds"))
panel <- merge(wide, papers[, .(ut, daisng_id, iso3, year, v2xca_academ, v2cafres)], by = "ut")
certainty <- readRDS(file.path(probe_dir, "certainty_panel_full.rds"))
panel <- merge(panel, certainty[, .(ut, certainty_contrast_adjusted)], by = "ut", all.x = TRUE)

author_range <- panel[, .(n = .N, rng = max(v2xca_academ) - min(v2xca_academ)), by = daisng_id]
real <- author_range[n >= 2 & rng >= 0.5, daisng_id]
cat(sprintf("\npanel %d docs | authors with >=0.5 SD change: %d\n", nrow(panel), length(real)))
cat(sprintf("sd graded contrast %.4f | sd binary contrast %.4f | sd certainty contrast %.4f\n\n",
            sd(panel$graded_contrast), sd(panel$binary_contrast),
            sd(panel$certainty_contrast_adjusted, na.rm = TRUE)))

run <- function(lbl, d, y, x) {
  f <- feols(as.formula(sprintf("%s ~ %s | daisng_id + year", y, x)), d[!is.na(get(y))], cluster = ~iso3)
  st <- if (pvalue(f)[x] < .01) "**" else if (pvalue(f)[x] < .05) "*" else
        if (pvalue(f)[x] < .1) "." else " "
  # effect in units of the outcome's own SD, so the three are comparable
  sd_y <- sd(d[[y]], na.rm = TRUE)
  cat(sprintf("%-40s %+8.5f (p %.3f)%s  %+6.3f SD  n=%d\n", lbl,
              coef(f)[x], pvalue(f)[x], st, coef(f)[x] / sd_y, nobs(f)))
}

for (x in c("v2xca_academ", "v2cafres")) {
  cat(sprintf("=== %s ===\n", x))
  cat("UnScientify predicts NEGATIVE, certainty predicts POSITIVE\n")
  for (subset_label in c("all authors", "change >= 0.5 SD")) {
    d <- if (subset_label == "all authors") panel else panel[daisng_id %in% real]
    cat(sprintf("-- %s\n", subset_label))
    run("   UnScientify binary", d, "binary_contrast", x)
    run("   UnScientify graded", d, "graded_contrast", x)
    run("   certainty model", d, "certainty_contrast_adjusted", x)
  }
  cat("\n")
}
