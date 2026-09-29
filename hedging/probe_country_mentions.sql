-- ---------------------------------------------------------------------------
-- Which countries does each abstract talk about?
--
-- Needed for the domestic/foreign split: naming the European Commission from
-- Turkey is not the same act as naming a Turkish ministry. Also gives the
-- spatial displacement measure directly, as the share of a country's output
-- that studies itself.
--
-- Matching is exact on lowercased one to three token spans of adjacent proper
-- nouns and adjectives, so "United States", "South African" and "Turkey" all
-- match while substring false positives cannot occur. Candidate spans are
-- restricted up front to those whose first token starts some gazetteer form.
-- ---------------------------------------------------------------------------

PRAGMA memory_limit='14GB';
PRAGMA threads=12;

SET VARIABLE token_glob = '/home/martigso/wos_parsed/tokens/*.parquet';
SET VARIABLE hedging_dir = '/home/martigso/Dropbox/postdoc/liberal_socialscience/Social_science_in_autocracies/Data and scripts/AutoKnow_socsci2/hedging';

CREATE OR REPLACE TABLE country_form AS
SELECT iso3, match_form, form_type, n_tokens
FROM read_csv(getvariable('hedging_dir') || '/country_gazetteer.csv', header = true);

CREATE OR REPLACE TABLE form_first_token AS
SELECT DISTINCT split_part(match_form, ' ', 1) AS first_token FROM country_form;

-- Proper nouns and adjectives only; these are where country names live.
CREATE OR REPLACE TABLE name_token AS
SELECT t.ut, t.sent_id, t.tok_id, lower(t.text) AS word
FROM read_parquet(getvariable('token_glob')) AS t
WHERE t.field = 'abstract' AND t.upos IN ('PROPN', 'ADJ');

CREATE OR REPLACE TABLE span_start AS
SELECT n.ut, n.sent_id, n.tok_id, n.word
FROM name_token AS n
SEMI JOIN form_first_token AS f ON n.word = f.first_token;

CREATE OR REPLACE TABLE candidate_span AS
SELECT s.ut, s.sent_id, s.tok_id, s.word AS span_1,
       s.word || ' ' || t2.word                  AS span_2,
       s.word || ' ' || t2.word || ' ' || t3.word AS span_3
FROM span_start AS s
LEFT JOIN name_token AS t2
       ON s.ut = t2.ut AND s.sent_id = t2.sent_id AND t2.tok_id = s.tok_id + 1
LEFT JOIN name_token AS t3
       ON s.ut = t3.ut AND s.sent_id = t3.sent_id AND t3.tok_id = s.tok_id + 2;

-- Longest match wins, so "United States" is not also counted as a one-token
-- form, and a three-token form is not double counted as its two-token prefix.
CREATE OR REPLACE TABLE country_mention AS
WITH matched AS (
    SELECT c.ut, c.sent_id, c.tok_id, f.iso3, f.n_tokens, f.form_type
    FROM candidate_span AS c
    JOIN country_form AS f
      ON f.match_form = CASE f.n_tokens WHEN 1 THEN c.span_1
                                        WHEN 2 THEN c.span_2
                                        ELSE c.span_3 END
),
ranked AS (
    SELECT *, row_number() OVER (PARTITION BY ut, sent_id, tok_id
                                 ORDER BY n_tokens DESC) AS rank_in_position
    FROM matched
)
SELECT ut, sent_id, tok_id, iso3, form_type FROM ranked WHERE rank_in_position = 1;

CREATE OR REPLACE TABLE document_country AS
SELECT ut, iso3,
       count(*) AS n_mentions,
       max(CASE WHEN form_type = 'demonym' THEN 1 ELSE 0 END) AS by_demonym
FROM country_mention GROUP BY ut, iso3;

COPY document_country TO '/home/martigso/wos_parsed/hedging_probe/document_country.parquet' (FORMAT PARQUET);

SELECT 'documents naming a country' AS what, count(DISTINCT ut) AS n FROM document_country
UNION ALL SELECT 'country mentions', count(*) FROM country_mention
UNION ALL SELECT 'distinct countries named', count(DISTINCT iso3) FROM document_country;

SELECT iso3, sum(n_mentions) AS mentions
FROM document_country GROUP BY iso3 ORDER BY mentions DESC LIMIT 12;
