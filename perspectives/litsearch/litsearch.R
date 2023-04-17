library(tidyverse)
library(litsearchr)

#https://elizagrames.github.io/litsearchr/litsearchr_vignette.html

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/LitSearch"

#1. Conduct naive search----
#https://www-webofscience-com.login.ezproxy.library.ualberta.ca/wos/woscc/summary/7b7f8c32-f97d-4ba3-ac3f-3629e60b4f16-820fd886/relevance/1
#same search on SCOPUS

#2. Import results----
# naiveimport_wos <- litsearchr::import_results(file=file.path(root, "iid_naivesearch3_wos.bib"))
# naiveimport_scopus <- litsearchr::import_results(file=file.path(root, "iid_naivesearch3_scopus.bib"))
naiveimport <- litsearchr::import_results(directory = file.path(root, "naivesearch"), verbose=TRUE)

#3. Remove duplicates----
naiveresults <-
  litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")

#4. Identify potential keywords----
rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 5,
    ngrams = TRUE,
    min_n = 2,
    max_n = 2,
    language = "English"
  )

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = naiveresults$keywords,
    method = "tagged",
    min_freq = 3,
    ngrams = TRUE,
    min_n = 1,
    max_n = 3,
    language = "English"
  )

#5. Build keyword cooccurrence network----
all_keywords <- unique(append(taggedkeywords, rakedkeywords))

naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 3,
    min_occ = 5
  )

#6. Cutoff terms----
cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = 0.7,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

searchterms <- data.frame(terms = litsearchr::get_keywords(reducedgraph),
                          group = NA)

#write.csv(searchterms, file.path(root, "searchterms.csv"), row.names = FALSE)
searchterms <- read.csv(file.path(root, "searchterms.csv"))

#7. Manual review----
searchterms

mysearchterms <- list(dplyr::filter(searchterms, group%in%c("acoustic"))$terms,
                       dplyr::filter(searchterms, group=="individual")$terms,
                      dplyr::filter(searchterms, group=="taxa")$terms)

#8. Write out the ones to use----
my_search <-
  litsearchr::write_search(
    groupdata = mysearchterms,
    languages = "English",
    stemming = TRUE,
    closure = "none",
    exactphrase = TRUE,
    writesearch = TRUE,
    verbose = TRUE,
    directory = file.path(root, "LitSearch")
  )
my_search

#removed: individu*, "ident* inform*"
#added: anim*, "kin recognit*"

#9. Go do the search----
#https://www-webofscience-com.login.ezproxy.library.ualberta.ca/wos/alldb/summary/e511fd6d-307e-4fd0-ab94-f2cf01622fad-8210a246/relevance/1
#also in SCOPUS

#10. Read in the results----
retrieved_articles <- import_results(directory = file.path(root, "fullsearch"))

#11. Remove duplicates----
retrieved_articles <- remove_duplicates(retrieved_articles, field="title", method="string_osa")

#12. Choose some gold standard references----
gold_standard <- read.csv(file.path(root, "goldstandard.csv"))$Title

#13. Check recall----
articles_found <- check_recall(true_hits=gold_standard,
                               retrieved = retrieved_articles$title) %>% 
  data.frame() %>% 
  arrange(desc(Similarity))
#30/36

articles_found_naive <- check_recall(true_hits=gold_standard,
                                     retrieved = naiveresults$title) %>% 
  data.frame() %>% 
  arrange(desc(Similarity))
#21/36

write.csv(articles_found, file.path(root, "fullsearch_recall.csv"), row.names = FALSE)
write.csv(articles_found_naive, file.path(root, "naivesearch_recall.csv"), row.names = FALSE)

#14. Assign the search results to each of 4 observers----
observer <- data.frame(observerid = seq(1, 4, 1),
                       observer = c("Matt", "Elly", "Tessa", "Devin"))
set.seed(1234)
out <- retrieved_articles %>% 
  dplyr::select(author, year, title, journal, volume, number, pages, doi, abstract) %>% 
  mutate(observerid = sample.int(4, size=max(row_number()), replace=TRUE)) %>% 
  left_join(observer)

table(out$observer)

#15. Save!----
write.csv(out, file.path(root, "Literature Review.csv"), row.names = FALSE)
















