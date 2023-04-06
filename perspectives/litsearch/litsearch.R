library(tidyverse)
library(litsearchr)

#https://elizagrames.github.io/litsearchr/litsearchr_vignette.html

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/LitSearch"

#1. Conduct naive search----
#Web of Science
#acoustic AND "individual identification"  (Topic) AND wildlife OR bird OR mammal OR amphibian OR insect OR fish  (Topic) Timespan:1864-2023. 

#2. Import results----
naiveimport <- litsearchr::import_results(file=file.path(root, "iid_naivesearch.txt"))

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
group <- c(0,0,1,1,0,0,1,NA,0,0,0,0,0,1,1,1,1,1,0,0,NA,NA,0)
terms <- data.frame(terms = searchterms,
                    group = group)
terms %>% 
       dplyr::filter(!is.na(group))

mysearchterms <- list(dplyr::filter(terms, group==1)$terms,
                      dplyr::filter(terms, group==0)$terms)

#8. Write out the ones to use----
my_search <-
  litsearchr::write_search(
    groupdata = mysearchterms,
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = FALSE,
    verbose = TRUE
  )

#9. Choose some gold standard references----
gold_standard <- read.csv(file.path(root, "goldstandard.csv"))$Title

#10. Go do the search----
#https://www-webofscience-com.login.ezproxy.library.ualberta.ca/wos/alldb/summary/3037af31-ebc2-472f-a182-76600350a9bd-7f323585/relevance/1

#11. Read in the results----
retrieved_articles1 <- litsearchr::import_results(file=file.path(root, "iid_fullsearch", "iid_fullsearch1.txt"))
retrieved_articles2 <- litsearchr::import_results(file=file.path(root, "iid_fullsearch", "iid_fullsearch2.txt"))
retrieved_articles <- data.table::rbindlist(list(retrieved_articles1, retrieved_articles2), fill=TRUE)
retrieved_articles <- remove_duplicates(retrieved_articles, field="title", method="string_osa")

#12. Check recall----
articles_found <- check_recall(true_hits=gold_standard,
                               retrieved = retrieved_articles$title) %>% 
  data.frame() %>% 
  arrange(desc(Similarity))

write.csv(articles_found, file.path(root, "goldstandard_recall.csv"), row.names = FALSE)




















