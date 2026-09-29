-- ---------------------------------------------------------------------------
-- Nouns as topics, for the topic-avoidance test.
--
-- Everything so far conditions on the author already writing about power.
-- This asks the prior question: does the same author take the topic on at
-- all? Three document-level outcomes, all measured over the whole corpus
-- rather than the slice with power clauses on both sides:
--
--   has_power_clause          any power clause anywhere in the abstract
--   has_domestic_power_clause a power clause in a sentence naming the
--                             author's own country
--   noun profile              counts per document, for a sensitivity score
--                             built regime-blind in R
--
-- Nouns are kept when they occur in at least 100 documents, which drops the
-- long tail without touching anything a topic measure would use.
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='14GB';
PRAGMA threads=12;

SET VARIABLE token_glob = '/home/martigso/wos_parsed/tokens/*.parquet';

ATTACH IF NOT EXISTS '/home/martigso/wos_parsed/hedging_probe/widened.duckdb' AS widened (READ_ONLY);

CREATE OR REPLACE TABLE noun_token AS
SELECT t.ut, lower(t.lemma) AS lemma
FROM read_parquet(getvariable('token_glob')) AS t
WHERE t.field = 'abstract' AND t.upos IN ('NOUN', 'PROPN')
  AND length(t.lemma) >= 3;

CREATE OR REPLACE TABLE noun_document_frequency AS
SELECT lemma, count(DISTINCT ut) AS document_frequency
FROM noun_token GROUP BY lemma HAVING count(DISTINCT ut) >= 100;

CREATE OR REPLACE TABLE document_noun AS
SELECT n.ut, n.lemma, count(*) AS n
FROM noun_token AS n
SEMI JOIN noun_document_frequency AS f ON n.lemma = f.lemma
GROUP BY n.ut, n.lemma;

-- Document-level power flags
CREATE OR REPLACE TABLE document_power_flag AS
SELECT d.ut,
       CASE WHEN p.ut IS NOT NULL THEN 1 ELSE 0 END AS has_power_clause,
       coalesce(p.n_power_clauses, 0) AS n_power_clauses
FROM (SELECT DISTINCT ut FROM noun_token) AS d
LEFT JOIN (SELECT ut, count(*) AS n_power_clauses FROM widened.power_argument GROUP BY ut) AS p
       ON d.ut = p.ut;

COPY document_noun       TO '/home/martigso/wos_parsed/hedging_probe/document_noun.parquet' (FORMAT PARQUET);
COPY document_power_flag TO '/home/martigso/wos_parsed/hedging_probe/document_power_flag.parquet' (FORMAT PARQUET);

SELECT 'documents' AS what, count(*) AS n FROM document_power_flag
UNION ALL SELECT 'nouns kept', count(*) FROM noun_document_frequency
UNION ALL SELECT 'document-noun pairs', count(*) FROM document_noun
UNION ALL SELECT 'documents with a power clause', sum(has_power_clause) FROM document_power_flag;
