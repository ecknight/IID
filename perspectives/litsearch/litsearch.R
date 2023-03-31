library(tidyverse)
library(litsearchr)

#https://elizagrames.github.io/litsearchr/litsearchr_vignette.html

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/LitSearch"

#1. Conduct naive search----
#Web of Science
#acoustic AND "individual identification"  (Topic) AND wildlife OR bird OR mammal OR amphibian OR insect OR fish  (Topic) Timespan:1864-2023. 

#2. Import results----
naiveimport <- litsearchr::import_results(file=file.path(root, "savedrecs.txt"))

#3. Remove duplicates----
#Shouldn't be any because I only used one search engine...
naiveresults <-
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")

#4. Identify potential keywords----
rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 3,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )

#5. Build keyword cooccurrence network----
all_keywords <- rakedkeywords

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 2,
    min_occ = 2
  )

#6. Cutoff terms----
cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = .8,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

searchterms <- litsearchr::get_keywords(reducedgraph)

#7. Manual review----
searchterms
use <- c(0,0,1,1,0,0,1,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0)
terms <- data.frame(terms = searchterms,
                    use = use)
View(terms)

#8. Write out the ones to use----