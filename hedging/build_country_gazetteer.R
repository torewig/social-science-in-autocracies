# ---------------------------------------------------------------------------
# Country surface forms for the domestic/foreign split.
#
# Names, aliases and demonyms for every sovereign state with an ISO3 code,
# from Wikidata. Written as one row per surface form so the parse can be
# matched by exact string equality rather than by substring search.
#
# Output: country_gazetteer.csv (iso3, surface_form, form_type, n_tokens)
# ---------------------------------------------------------------------------

library(data.table)

endpoint <- "https://query.wikidata.org/sparql"
user_agent <- "clause-measure-probe/0.1 (research; martin.g.soyland@gmail.com)"

run_sparql <- function(query) {
  output <- system2("curl",
    c("-s", "-G", shQuote(endpoint),
      "--data-urlencode", shQuote(paste0("query=", query)),
      "-H", shQuote("Accept: text/csv"),
      "-A", shQuote(user_agent)),
    stdout = TRUE)
  fread(text = paste(output, collapse = "\n"))
}

names_and_demonyms <- run_sparql('SELECT ?iso3 ?name ?demonym WHERE {
  ?c wdt:P31 wd:Q3624078 ; wdt:P298 ?iso3 .
  ?c rdfs:label ?name FILTER(lang(?name)="en")
  OPTIONAL { ?c wdt:P1549 ?demonym FILTER(lang(?demonym)="en") }
}')

aliases <- run_sparql('SELECT ?iso3 ?alias WHERE {
  ?c wdt:P31 wd:Q3624078 ; wdt:P298 ?iso3 ; skos:altLabel ?alias .
  FILTER(lang(?alias)="en")
}')

gazetteer <- rbindlist(list(
  unique(names_and_demonyms[, .(iso3, surface_form = name,    form_type = "name")]),
  unique(names_and_demonyms[!is.na(demonym) & demonym != "",
                            .(iso3, surface_form = demonym,   form_type = "demonym")]),
  unique(aliases[,           .(iso3, surface_form = alias,    form_type = "alias")])
))

# Historical states the corpus covers but Wikidata's sovereign-state class
# does not, mapped to the successor code the address data uses.
historical <- data.table(
  iso3         = c("RUS", "RUS", "SRB", "SRB", "CZE", "DEU", "DEU"),
  surface_form = c("Soviet Union", "USSR", "Yugoslavia", "Yugoslavian",
                   "Czechoslovakia", "West Germany", "East Germany"),
  form_type    = "historical")
gazetteer <- rbind(gazetteer, historical)

gazetteer[, surface_form := trimws(sub("^[Tt]he ", "", surface_form))]
gazetteer <- gazetteer[nchar(surface_form) >= 4]

# Surface forms that are ordinary English words or that collide across
# countries carry no country information and are dropped.
ambiguous_forms <- c("states", "state", "union", "republic", "kingdom", "island",
                     "islands", "federation", "emirates", "north", "south",
                     "east", "west", "central", "new", "guinea", "america",
                     "american", "european", "african", "asian", "western",
                     "eastern", "northern", "southern", "national", "people",
                     "peoples", "democratic", "socialist", "arab", "commonwealth")
gazetteer <- gazetteer[!tolower(surface_form) %in% ambiguous_forms]

colliding <- gazetteer[, .(n_countries = uniqueN(iso3)), by = .(form = tolower(surface_form))]
gazetteer <- gazetteer[!tolower(surface_form) %in% colliding[n_countries > 1, form]]

gazetteer[, match_form := tolower(surface_form)]
gazetteer[, n_tokens := lengths(strsplit(match_form, "[ -]+"))]
gazetteer <- unique(gazetteer[n_tokens <= 3], by = c("iso3", "match_form"))

fwrite(gazetteer[order(iso3, form_type, match_form)], "country_gazetteer.csv")
cat(sprintf("%d surface forms, %d countries, %d with a demonym\n",
            nrow(gazetteer), uniqueN(gazetteer$iso3),
            uniqueN(gazetteer[form_type == "demonym", iso3])))
print(gazetteer[iso3 %in% c("TUR", "RUS", "USA"), .(iso3, surface_form, form_type)])
