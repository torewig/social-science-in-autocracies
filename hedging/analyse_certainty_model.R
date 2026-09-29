# ---------------------------------------------------------------------------
# Does a learned certainty model show what the Hyland hedge list showed?
#
# pedropei/sentence-level-certainty (Pei & Jurgens, EMNLP 2021) is SciBERT
# fine-tuned on 2,167 annotated scientific findings, predicting certainty on a
# 1-6 scale. Their paper's headline is that hedge words explain only part of
# perceived certainty, so if the lexicon was missing the construct, this is
# where it shows.
#
# Same three questions as the lexicon version:
#   1. descriptive: are power sentences more or less certain than the same
#      author's other sentences?
#   2. does the within-document contrast move with academic freedom, with
#      author fixed effects?
#   3. how well do the two measures agree at all?
# ---------------------------------------------------------------------------

library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
hedging_dir <- "/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging"
source(file.path(hedging_dir, "probe_panel_helpers.R"))

scores <- fread(file.path(probe_dir, "certainty_scores.csv"))
cat(sprintf("scored sentences: %d | documents: %d\n", nrow(scores), uniqueN(scores$ut)))

# ── 1. Descriptive, sentence level ─────────────────────────────────────────
cat("\n############ 1. SENTENCE LEVEL ############\n")
cat(sprintf("power sentences  mean certainty %.3f (sd %.3f, n %d)\n",
            scores[is_power_sentence == 1, mean(certainty)],
            scores[is_power_sentence == 1, sd(certainty)],
            scores[is_power_sentence == 1, .N]))
cat(sprintf("other sentences  mean certainty %.3f (sd %.3f, n %d)\n",
            scores[is_power_sentence == 0, mean(certainty)],
            scores[is_power_sentence == 0, sd(certainty)],
            scores[is_power_sentence == 0, .N]))

# ── 2. Within-document contrast ────────────────────────────────────────────
by_side <- scores[, .(certainty = mean(certainty), n_sentences = .N),
                  by = .(ut, is_power_sentence)]
wide <- dcast(by_side, ut ~ is_power_sentence, value.var = c("certainty", "n_sentences"))
setnames(wide, c("certainty_0", "certainty_1", "n_sentences_0", "n_sentences_1"),
         c("certainty_other", "certainty_power", "n_other", "n_power"))
wide <- wide[!is.na(certainty_power) & !is.na(certainty_other)]
wide[, certainty_contrast := certainty_power - certainty_other]
cat(sprintf("\nwithin-document contrast (power minus other): %+.4f (sd %.4f) over %d documents\n",
            mean(wide$certainty_contrast), sd(wide$certainty_contrast), nrow(wide)))
cat(sprintf("share of documents where power sentences are LESS certain: %.1f%%\n",
            100 * mean(wide$certainty_contrast < 0)))

sample_docs <- readRDS(file.path(probe_dir, "certainty_sample_docs.rds"))
papers <- readRDS(file.path(probe_dir, "mover_stayer_papers.rds"))
panel <- merge(wide, papers[, .(ut, daisng_id, iso3, year, v2xca_academ, v2cafexch,
                                hedge_contrast, away, origin_freedom)], by = "ut")
cat(sprintf("estimation sample: %d documents, %d authors, %d countries\n",
            nrow(panel), uniqueN(panel$daisng_id), uniqueN(panel$iso3)))

cat("\n############ 2. AUTHOR FIXED EFFECTS ############\n")
for (treatment in c("v2xca_academ", "v2cafexch")) {
  fit <- feols(as.formula(sprintf("certainty_contrast ~ %s | daisng_id + year", treatment)),
               panel, cluster = ~iso3)
  cat(sprintf("%-16s %+8.5f (se %.5f, p %.3f) n=%d\n", treatment,
              coef(fit)[treatment], se(fit)[treatment], pvalue(fit)[treatment], nobs(fit)))
}
cat("\nthe same documents, scored with the Hyland lexicon instead:\n")
for (treatment in c("v2xca_academ", "v2cafexch")) {
  fit <- feols(as.formula(sprintf("hedge_contrast ~ %s | daisng_id + year", treatment)),
               panel[!is.na(hedge_contrast)], cluster = ~iso3)
  cat(sprintf("%-16s %+8.5f (se %.5f, p %.3f) n=%d\n", treatment,
              coef(fit)[treatment], se(fit)[treatment], pvalue(fit)[treatment], nobs(fit)))
}

cat("\n############ 3. DO THE TWO MEASURES AGREE? ############\n")
both <- panel[!is.na(hedge_contrast)]
cat(sprintf("documents with both measures: %d\n", nrow(both)))
cat(sprintf("correlation of the two contrasts: %.3f\n",
            cor(both$certainty_contrast, both$hedge_contrast)))
cat(sprintf("correlation at sentence level, certainty vs hedge rate: computed below\n"))
saveRDS(panel, file.path(probe_dir, "certainty_panel.rds"))
