# Regime and academic language — theory memo

**Working off of the claim:** 

> Regime changes how academics write about one thing: the people who hold power.
The rest of the same text is untouched. Selectivity is the signature — anything that moves
a whole document uniformly is wealth, field or training, not regime.

**Mechanism.** Modern autocracies prosecute academics rather than jail them: insult,
defamation, terrorism statutes. A charge needs an assertion of fact, about an identifiable
party, in that jurisdiction, about the present. Protective writing breaks those elements
one at a time.

| protective move | example | requirement it defeats |
|---|---|---|
| hedge the claim | "the closures may have been politically motivated" | that a fact is asserted at all |
| delete the agent | "the newspapers were closed" versus "the newspapers were closed by the ministry"| that a party is identified |
| displace in time | study the 1990s rather than the present | that it concerns current officeholders |
| displace in space | study Hungary rather than Turkey | that it falls in the jurisdiction |

The last two are Aesopian language (Loseff 1984). The mechanism predicts **hedging rather
than silence** — hedging is in the text; silence is not.

**What we measure.** Per clause: whether the agent is expressed (`nsubj:pass` with no
`obl:agent`), nominalisation, epistemic modality, and whether a power-holding entity is
ever the grammatical subject of a negative predicate. Entities are linked to Wikidata and
typed by its hierarchy, so classification is country-appropriate without a keyword list.
The unit is the clause; the comparison is against the same author's non-political clauses
in the same document, which holds field, journal, training and national wealth fixed by
construction.

The instrument is stated entirely in Universal Dependencies labels, so every feature is a
fact about a parse rather than a reading of a text.

**What we estimate.** For each country-year, a mitigation profile across object classes —
head of state, security services, courts, church, minorities, economy, infrastructure. The
shape of that profile is the result. Sensitivity is local, so we do not assume which
objects carry it; the profile is what locates it. In Thailand we expect the spike on the
monarchy, in Turkey on the Kurdish conflict.

The instrument is fixed in advance and the profile is estimated, so discovery and
confirmation run on different data.

**Predictions.**

1. Within a document, only clauses about power-holding entities move; the author's
   other clauses (method, results, etc.) do not.
2. Asymmetry by target: the domestic executive, not markets, society or foreign states.
3. Gradient by the entity's proximity to power: water utility < line ministry <
   presidency.
4. Substitution rather than reduction — criticism changes form instead of disappearing.

**What would falsify it.** Caution spread evenly through the text. The same pattern in
biomedical abstracts from the same countries. Movement only after formal legal change
rather than before it. Critical content dropping without any change in form.

**Our variant.** Strauss (1952) and Loseff (1984) describe *deliberate* concealment for a
knowing reader. We claim habituation rather than choice, which is separable: deliberate
concealment concentrates among the exposed and is consistent within author, while
habituation is diffuse and appears in low-stakes writing by people never threatened.

**Feasibility.** SSH corpus: 2.27M abstracts, 25.8% carry a political-actor sentence,
~562,000 usable for a within-document contrast. Densest fields are International Relations,
Public Administration, Law, Political Science, Area Studies and History.

**Cited.** Strauss 1952 *Persecution and the Art of Writing* · Loseff 1984 *On the
Beneficence of Censorship* · Brown & Levinson (power term in politeness weight) · Kuran
1995 *Private Truths, Public Lies* · Roberts 2018 *Censored* · Hyland 2005 (hedge
inventory) · Fausey & Boroditsky 2010 (agency and blame).

---

## Preliminary numbers (2026-08-28)

From the stanza dependency re-parse of the WoS corpus, at 2,287,365 abstracts
(95% of the English corpus; the run finishes today). 15,768,454 sentences,
421M annotated words. These are pooled baselines, not yet a regime comparison
— there is no country dimension in them.

**Agent deletion is the common case.** Of 4,537,675 sentences carrying a
passive subject, 86.2% name no agent anywhere in the sentence. The measure is
therefore not detecting a rare event, and it has room to move in both
directions.

**Power-holders are named *more*, not less.** Using a 20-lemma keyword stand-in
for the entity instrument (`government, ministry, police, court, parliament,
president, party, military, prosecutor, …` as nouns), sentences that mention a
power-holder delete the agent ten points *less* often than the rest of the
corpus, and hedge slightly less:

| sentences | n | passives | agent deleted | hedges / 1k words |
|---|---|---|---|---|
| mention a power-holder | 737,333 | 189,478 | **76.6%** | 3.26 |
| all others | 15,031,121 | 4,348,197 | **86.6%** | 3.78 |

**The within-document contrast runs the same way.** Restricting to documents
with passive clauses on both sides and differencing within the document, so
field, journal, training and wealth cancel by construction:

| documents | contrast | mean paired difference | t |
|---|---|---|---|
| 105,600 (≥1 passive per side) | power − other | −0.075 | −50.2 |
| 9,537 (≥2 passives per side) | power − other | −0.067 | −18.7 |

**Reading this.** The pooled corpus is overwhelmingly written from wealthy
democracies, so a baseline in which academics name governments *more* readily
than they name other actors is what a working instrument should show. The
prediction in this memo is about variation across regimes, and none of the
above tests it. What these numbers establish is that the three quantities the
design needs — passives, expressed agents, and a within-document power/other
split — exist in the corpus at workable volume, and that the within-document
difference is measurable with a large t at n = 105,600 before any of the real
instrument is built.

**What is provisional.** (1) The power proxy is a keyword list, not the
Wikidata typing this memo specifies; `state`, `party` and `security` are
ambiguous and will be capturing non-actor senses. (2) It fires on a power noun
anywhere in the sentence, not on the entity being an argument of the predicate,
which is looser than the clause-level instrument. (3) `obl:agent` in the same
sentence is a floor for "agent expressed": reduced and nominalised agents are
not counted. Each of these should move the numbers, and none of them changes
the fact that the raw material is there.

**Feasibility, restated on parsed data.** 17.8% of abstracts (407,869) contain
at least one power-proxy sentence, against the memo's earlier 25.8% estimate
from a broader political-actor definition. 105,600 documents already support a
within-document passive contrast on the keyword proxy alone; the entity
instrument should raise both figures.
