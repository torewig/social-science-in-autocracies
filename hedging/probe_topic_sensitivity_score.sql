-- ---------------------------------------------------------------------------
-- A regime-blind "closeness to power" score for every document.
--
-- Each noun gets a log-odds score for how much more often it appears in
-- documents that contain a power clause than in documents that do not. No
-- regime label enters. The 35 power lemmas themselves are excluded from both
-- the scoring and the document score, so the measure is about the vocabulary
-- around power (election, protest, corruption, reform) rather than the
-- trigger words, which would be circular.
--
-- The document score is the count-weighted mean noun score: how close to
-- political subject matter this abstract sits, whether or not it contains a
-- power clause.
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='14GB';
PRAGMA threads=12;

SET VARIABLE hedging_dir = '/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging';

CREATE OR REPLACE TABLE power_lemma_excluded AS
SELECT lower(term) AS term
FROM read_csv(getvariable('hedging_dir') || '/power_holder_lemmas.csv', header = true);

CREATE OR REPLACE TABLE document_noun AS
SELECT * FROM read_parquet('/home/martigso/wos_parsed/hedging_probe/document_noun.parquet')
WHERE lemma NOT IN (SELECT term FROM power_lemma_excluded);

CREATE OR REPLACE TABLE document_power_flag AS
SELECT * FROM read_parquet('/home/martigso/wos_parsed/hedging_probe/document_power_flag.parquet');

CREATE OR REPLACE TABLE group_size AS
SELECT sum(has_power_clause) AS n_power_documents,
       count(*) - sum(has_power_clause) AS n_other_documents
FROM document_power_flag;

CREATE OR REPLACE TABLE noun_score AS
SELECT
    dn.lemma,
    count(*) FILTER (f.has_power_clause = 1) AS in_power_documents,
    count(*) FILTER (f.has_power_clause = 0) AS in_other_documents,
    log((count(*) FILTER (f.has_power_clause = 1) + 0.5) /
        ((SELECT n_power_documents FROM group_size) - count(*) FILTER (f.has_power_clause = 1) + 0.5))
  - log((count(*) FILTER (f.has_power_clause = 0) + 0.5) /
        ((SELECT n_other_documents FROM group_size) - count(*) FILTER (f.has_power_clause = 0) + 0.5))
        AS power_proximity
FROM document_noun AS dn
JOIN document_power_flag AS f ON dn.ut = f.ut
GROUP BY dn.lemma;

CREATE OR REPLACE TABLE document_topic_score AS
SELECT dn.ut,
       sum(dn.n * ns.power_proximity) / sum(dn.n) AS power_proximity_score,
       sum(dn.n) AS n_noun_tokens
FROM document_noun AS dn
JOIN noun_score AS ns ON dn.lemma = ns.lemma
GROUP BY dn.ut;

COPY document_topic_score TO '/home/martigso/wos_parsed/hedging_probe/document_topic_score.parquet' (FORMAT PARQUET);
COPY (SELECT * FROM noun_score ORDER BY power_proximity DESC)
  TO '/home/martigso/wos_parsed/hedging_probe/noun_power_proximity.csv' (HEADER, DELIMITER ',');

SELECT lemma, round(power_proximity, 2) AS score, in_power_documents
FROM noun_score WHERE in_power_documents >= 500 ORDER BY power_proximity DESC LIMIT 15;
SELECT lemma, round(power_proximity, 2) AS score, in_other_documents
FROM noun_score WHERE in_other_documents >= 500 ORDER BY power_proximity ASC LIMIT 10;
