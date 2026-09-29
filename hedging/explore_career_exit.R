# ---------------------------------------------------------------------------
# EXPLORATORY. The silence, measured in careers rather than in text.
#
# The memo says silence cannot be observed because it is not in the text. It
# is not in any one text, but it is in an author's sequence of texts. If
# writing about domestic power becomes costly, the people who do it should
# leave the corpus, or stop doing it, at a higher rate than colleagues in the
# same country and year who never touched the topic.
#
# Discrete-time hazard. Each author is at risk from their first publication
# year. Exit is the last year in which they ever appear, counted only when
# that year is 2016 or earlier so there are three further years of corpus in
# which their absence is real rather than censoring.
#
# Country-by-year fixed effects absorb everything common to a national system
# in a year, including the academic freedom level itself. The estimand is the
# interaction: does falling academic freedom raise the exit hazard MORE for
# authors who write about power than for their compatriots who do not.
# ---------------------------------------------------------------------------

library(data.table)
library(fixest)

probe_dir <- "/home/martigso/wos_parsed/hedging_probe"
LAST_RISK_YEAR <- 2016L
CORPUS_END <- 2019L

papers <- readRDS(file.path(probe_dir, "topic_avoidance_papers.rds"))
papers <- papers[!is.na(daisng_id) & !is.na(iso3) & year %between% c(1991, CORPUS_END)]
setorder(papers, daisng_id, year)

author <- papers[, .(first_year = min(year), last_year = max(year), n_papers = .N,
                     iso3 = iso3[1]), by = daisng_id]
author <- author[n_papers >= 2]
cat(sprintf("authors with 2+ papers: %d\n", nrow(author)))

# year of first power paper and first domestic-power paper
first_power <- papers[has_power_clause == 1, .(first_power_year = min(year)), by = daisng_id]
first_domestic <- papers[has_domestic_power == 1, .(first_domestic_year = min(year)), by = daisng_id]
author <- merge(author, first_power, by = "daisng_id", all.x = TRUE)
author <- merge(author, first_domestic, by = "daisng_id", all.x = TRUE)

# author-year panel over the years each author is at risk
panel <- author[, .(year = seq(first_year, min(last_year, LAST_RISK_YEAR))), by = daisng_id]
panel <- merge(panel, author, by = "daisng_id")
panel <- panel[year <= LAST_RISK_YEAR]
panel[, exit := as.integer(year == last_year & last_year <= LAST_RISK_YEAR)]
panel[, career_age := year - first_year]
panel[, power_writer := as.integer(!is.na(first_power_year) & year >= first_power_year)]
panel[, domestic_power_writer := as.integer(!is.na(first_domestic_year) & year >= first_domestic_year)]

vdem_path <- paste0("/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/",
                    "Data and scripts/AutoKnow_socsci2/Data/V-Dem-CY-Full+Others-v15.rds")
vdem <- as.data.table(readRDS(vdem_path))[, .(iso3 = country_text_id, year,
                                              af = scale(v2xca_academ)[, 1])]
panel <- merge(panel, vdem, by = c("iso3", "year"))
panel <- panel[!is.na(af)]

cat(sprintf("author-years at risk: %d | exits: %d (%.1f%%)\n",
            nrow(panel), sum(panel$exit), 100 * mean(panel$exit)))
cat(sprintf("author-years by a power writer: %.1f%% | domestic power writer: %.1f%%\n",
            100 * mean(panel$power_writer), 100 * mean(panel$domestic_power_writer)))

report <- function(label, fit, term) {
  if (!(term %in% names(coef(fit)))) { cat(sprintf("%-46s not estimable\n", label)); return(invisible()) }
  star <- if (pvalue(fit)[term] < .01) "**" else if (pvalue(fit)[term] < .05) "*" else
          if (pvalue(fit)[term] < .1) "." else " "
  cat(sprintf("%-46s %+8.5f (se %.5f, p %.3f)%s n=%d\n", label,
              coef(fit)[term], se(fit)[term], pvalue(fit)[term], star, nobs(fit)))
}

cat("\n############ EXIT HAZARD ############\n")
cat("baseline difference: do power writers exit more, overall?\n")
report("  power writer", feols(exit ~ power_writer | iso3^year + career_age, panel, cluster = ~iso3),
       "power_writer")
report("  domestic power writer",
       feols(exit ~ domestic_power_writer | iso3^year + career_age, panel, cluster = ~iso3),
       "domestic_power_writer")

cat("\nthe estimand: power writer x academic freedom, within country-year\n")
cat("mechanism predicts NEGATIVE (less freedom, more exit for power writers)\n")
report("  power writer x academic freedom",
       feols(exit ~ power_writer * af | iso3^year + career_age, panel, cluster = ~iso3),
       "power_writer:af")
report("  domestic power writer x academic freedom",
       feols(exit ~ domestic_power_writer * af | iso3^year + career_age, panel, cluster = ~iso3),
       "domestic_power_writer:af")

cat("\nrestricted to authors in countries where academic freedom actually moves\n")
country_range <- panel[, .(rng = max(af) - min(af)), by = iso3][rng >= 0.5, iso3]
report("  power writer x academic freedom",
       feols(exit ~ power_writer * af | iso3^year + career_age, panel[iso3 %in% country_range],
             cluster = ~iso3), "power_writer:af")
report("  domestic power writer x academic freedom",
       feols(exit ~ domestic_power_writer * af | iso3^year + career_age,
             panel[iso3 %in% country_range], cluster = ~iso3), "domestic_power_writer:af")

cat("\nPLACEBO: the same test on a topic with no political exposure\n")
papers[, is_education := as.integer(field == "Education & Educational Research")]
first_edu <- papers[is_education == 1, .(first_edu_year = min(year)), by = daisng_id]
panel <- merge(panel, first_edu, by = "daisng_id", all.x = TRUE)
panel[, education_writer := as.integer(!is.na(first_edu_year) & year >= first_edu_year)]
report("  education writer x academic freedom",
       feols(exit ~ education_writer * af | iso3^year + career_age, panel, cluster = ~iso3),
       "education_writer:af")
saveRDS(panel, file.path(probe_dir, "career_exit_panel.rds"))
