-- ---------------------------------------------------------------------------
-- Interpretability step 1: what linguistic features does the certainty model
-- track? Per-sentence features from our own parse, for every sentence the
-- model scored, so the black box can be regressed on things we can name.
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='10GB';
PRAGMA threads=8;
PRAGMA temp_directory='/home/martigso/wos_parsed/duckdb_temp';

CREATE OR REPLACE TABLE eligible_ut AS
SELECT ut FROM read_csv('/home/martigso/wos_parsed/hedging_probe/certainty_full_uts.csv', header = true);

CREATE OR REPLACE TABLE lex AS
SELECT lower(term) AS term, type, subtype
FROM read_csv('/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging/hyland_hedges_boosters.csv', header = true);

CREATE OR REPLACE TABLE sentence_feature AS
SELECT
    t.ut, t.sent_id,
    count(*) FILTER (t.upos <> 'PUNCT')                                        AS n_tokens,
    count(*) FILTER (l.type = 'hedge' AND l.subtype <> 'approximator')         AS n_hedge,
    count(*) FILTER (l.type = 'hedge' AND l.subtype = 'approximator')          AS n_approximator,
    count(*) FILTER (l.type = 'booster')                                       AS n_booster,
    count(*) FILTER (t.upos = 'AUX' AND lower(t.lemma) IN
        ('may','might','could','would','should','can'))                        AS n_modal,
    count(*) FILTER (t.deprel = 'advmod' AND lower(t.lemma) IN ('not','never')) AS n_negation,
    count(*) FILTER (t.deprel = 'nsubj:pass')                                  AS n_passive,
    count(*) FILTER (t.deprel = 'obl:agent')                                   AS n_agent_phrase,
    count(*) FILTER (t.upos = 'NOUN' AND (lower(t.lemma) LIKE '%tion'
        OR lower(t.lemma) LIKE '%ment' OR lower(t.lemma) LIKE '%ance'))        AS n_nominalisation,
    count(*) FILTER (t.upos = 'NUM')                                           AS n_number,
    count(*) FILTER (t.upos = 'PROPN')                                         AS n_propn,
    count(*) FILTER (t.upos = 'ADJ')                                           AS n_adjective,
    count(*) FILTER (t.upos = 'ADV')                                           AS n_adverb,
    count(*) FILTER (t.upos = 'VERB')                                          AS n_verb,
    count(*) FILTER (t.upos = 'SCONJ')                                         AS n_subordinator,
    count(*) FILTER (lower(t.text) IN ('we','our','us','i','my'))              AS n_first_person,
    count(*) FILTER (t.feats LIKE '%Tense=Past%')                              AS n_past_tense,
    count(*) FILTER (t.feats LIKE '%Tense=Pres%')                              AS n_present_tense,
    count(*) FILTER (t.feats LIKE '%Degree=Cmp%' OR t.feats LIKE '%Degree=Sup%') AS n_comparative,
    count(*) FILTER (t.deprel IN ('advcl','ccomp','xcomp','acl','acl:relcl'))  AS n_subclause
FROM read_parquet('/home/martigso/wos_parsed/tokens/*.parquet') AS t
SEMI JOIN eligible_ut AS e ON t.ut = e.ut
LEFT JOIN lex AS l ON lower(t.lemma) = l.term
     AND ((l.subtype = 'modal' AND t.upos IN ('AUX','VERB'))
       OR (l.subtype IN ('epistemic_verb','reporting') AND t.upos = 'VERB')
       OR (l.subtype IN ('epistemic_adjadv','approximator','emphatic') AND t.upos IN ('ADJ','ADV')))
WHERE t.field = 'abstract'
GROUP BY t.ut, t.sent_id;

COPY sentence_feature TO '/home/martigso/wos_parsed/hedging_probe/sentence_features.parquet' (FORMAT PARQUET);
SELECT count(*) AS sentences FROM sentence_feature;
