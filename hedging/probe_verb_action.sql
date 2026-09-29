-- ---------------------------------------------------------------------------
-- Verbs as actions, mirroring the noun topic work, plus one thing nouns
-- cannot do.
--
-- A. verb proximity: the same regime-blind log-odds score, over verb lemmas.
--    Nouns say what the paper is about; verbs say what is being done in it.
--
-- B. agency attribution: for every power mention that is an argument of a
--    verb, is the power-holder the active subject ("the ministry closed the
--    papers"), the passive subject ("the ministry was criticised"), an object
--    ("they sued the ministry") or an oblique? The mechanism predicts fewer
--    power-holders placed in the active subject slot where writing about
--    power is risky, since that is the slot that attributes the action.
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='14GB';
PRAGMA threads=12;

SET VARIABLE token_glob = '/home/martigso/wos_parsed/tokens/*.parquet';
SET VARIABLE hedging_dir = '/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging';

CREATE OR REPLACE TABLE power_lemma_v AS
SELECT lower(term) AS term, in_strict_list
FROM read_csv(getvariable('hedging_dir') || '/power_holder_lemmas.csv', header = true);

CREATE OR REPLACE TABLE document_power_flag AS
SELECT * FROM read_parquet('/home/martigso/wos_parsed/hedging_probe/document_power_flag.parquet');

-- ── A. verb profiles ───────────────────────────────────────────────────────
CREATE OR REPLACE TABLE verb_token AS
SELECT t.ut, lower(t.lemma) AS lemma
FROM read_parquet(getvariable('token_glob')) AS t
WHERE t.field = 'abstract' AND t.upos = 'VERB' AND length(t.lemma) >= 3;

CREATE OR REPLACE TABLE verb_document_frequency AS
SELECT lemma, count(DISTINCT ut) AS document_frequency
FROM verb_token GROUP BY lemma HAVING count(DISTINCT ut) >= 100;

CREATE OR REPLACE TABLE document_verb AS
SELECT v.ut, v.lemma, count(*) AS n
FROM verb_token AS v
SEMI JOIN verb_document_frequency AS f ON v.lemma = f.lemma
GROUP BY v.ut, v.lemma;

CREATE OR REPLACE TABLE verb_group_size AS
SELECT sum(has_power_clause) AS n_power_documents,
       count(*) - sum(has_power_clause) AS n_other_documents
FROM document_power_flag;

CREATE OR REPLACE TABLE verb_score AS
SELECT dv.lemma,
       count(*) FILTER (f.has_power_clause = 1) AS in_power_documents,
       log((count(*) FILTER (f.has_power_clause = 1) + 0.5) /
           ((SELECT n_power_documents FROM verb_group_size) - count(*) FILTER (f.has_power_clause = 1) + 0.5))
     - log((count(*) FILTER (f.has_power_clause = 0) + 0.5) /
           ((SELECT n_other_documents FROM verb_group_size) - count(*) FILTER (f.has_power_clause = 0) + 0.5))
       AS action_proximity
FROM document_verb AS dv
JOIN document_power_flag AS f ON dv.ut = f.ut
GROUP BY dv.lemma;

CREATE OR REPLACE TABLE document_verb_score AS
SELECT dv.ut,
       sum(dv.n * vs.action_proximity) / sum(dv.n) AS action_proximity_score,
       sum(dv.n) AS n_verb_tokens
FROM document_verb AS dv
JOIN verb_score AS vs ON dv.lemma = vs.lemma
GROUP BY dv.ut;

-- ── B. what slot the power-holder occupies ─────────────────────────────────
CREATE OR REPLACE TABLE power_slot AS
SELECT t.ut, t.sent_id, t.tok_id, t.head AS predicate_id, t.deprel,
       lower(t.lemma) AS power_lemma
FROM read_parquet(getvariable('token_glob')) AS t
JOIN power_lemma_v AS p ON lower(t.lemma) = p.term
WHERE t.field = 'abstract' AND t.upos IN ('NOUN', 'PROPN')
  AND t.deprel IN ('nsubj', 'nsubj:pass', 'obj', 'iobj', 'obl', 'obl:agent');

CREATE OR REPLACE TABLE document_power_slot AS
SELECT ut,
       count(*)                                        AS n_power_arguments,
       count(*) FILTER (deprel = 'nsubj')              AS n_active_subject,
       count(*) FILTER (deprel = 'nsubj:pass')         AS n_passive_subject,
       count(*) FILTER (deprel IN ('obj', 'iobj'))     AS n_object,
       count(*) FILTER (deprel = 'obl')                AS n_oblique,
       count(*) FILTER (deprel = 'obl:agent')          AS n_expressed_agent
FROM power_slot GROUP BY ut;

-- what power-holders are shown doing, when they are the active subject
CREATE OR REPLACE TABLE power_action_verb AS
SELECT lower(v.lemma) AS verb, count(*) AS n
FROM power_slot AS ps
JOIN read_parquet(getvariable('token_glob')) AS v
  ON ps.ut = v.ut AND ps.sent_id = v.sent_id AND v.tok_id = ps.predicate_id
WHERE v.field = 'abstract' AND v.upos = 'VERB' AND ps.deprel = 'nsubj'
GROUP BY 1 ORDER BY n DESC;

COPY document_verb_score  TO '/home/martigso/wos_parsed/hedging_probe/document_verb_score.parquet' (FORMAT PARQUET);
COPY document_power_slot  TO '/home/martigso/wos_parsed/hedging_probe/document_power_slot.parquet' (FORMAT PARQUET);
COPY (SELECT * FROM verb_score ORDER BY action_proximity DESC)
  TO '/home/martigso/wos_parsed/hedging_probe/verb_action_proximity.csv' (HEADER, DELIMITER ',');
COPY power_action_verb TO '/home/martigso/wos_parsed/hedging_probe/power_action_verb.csv' (HEADER, DELIMITER ',');

SELECT lemma, round(action_proximity, 2) AS score, in_power_documents
FROM verb_score WHERE in_power_documents >= 400 ORDER BY action_proximity DESC LIMIT 15;
SELECT verb, n FROM power_action_verb LIMIT 20;
SELECT sum(n_power_arguments) AS power_args, sum(n_active_subject) AS active_subject,
       sum(n_passive_subject) AS passive_subject, sum(n_object) AS object_slot,
       sum(n_oblique) AS oblique FROM document_power_slot;
