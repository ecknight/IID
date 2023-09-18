#1. Preamble----

library(tidyverse)
library(googlesheets4)
library(googledrive)

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/"

#2. Read in raw table----
raw <- read_sheet(ss="1J9SN-VR6WF3GVycpxd5RY6gXN3gIvmo7AJutYRDOJcU") %>% 
  data.frame()

# dat <- raw %>% 
#   unnest(cols=colnames(raw))

#TO DO: FIX DF ISSUE####

#3. Check for incompleted rows----
todo <- raw %>% 
  mutate(todo = case_when(is.na(Relevant) ~ "Eval
dat <- raw %>% uate",
                          (is.na(Classification.Method) & Relevant=="Yes") ~ "AllDetails",
                          (!is.na(Classification.Method) & is.na(Model.Type) & !Flag %in% c("Remove - insufficient methods detail ", "Needs checked (no access)") & Relevant!="Review") ~ "ModelDetails")) %>% 
  dplyr::filter(!is.na(todo))
nrow(todo)
table(todo$todo, todo$observer)

#3. Look at relevant proportion----
table(raw$Relevant)
# Duplicate        No    Review       Yes 
# 6      3833        50       660

#4. Filter to relevant and reviews, wrangle----
#change Tess & Elly to just Elly
#Tidy application
review <- raw %>% dplyr::filter(Relevant=="Review")
use <- raw %>% dplyr::filter(Relevant=="Yes",
                             !Flag %in% c("Remove - insufficient methods detail ", "Needs checked (no access)")) %>% 
  mutate(observer = ifelse(observer=="Tessa & Elly", "Elly", observer),
         Application = case_when(Application=="Communication behaviour" ~ "Behaviour",
                                 Application=="Demographic parameters (recruitment, immigration, emigration, survival)" ~ "Demography",
                                 Application=="Density estimation" ~ "Density",
                                 Application=="IID per se" ~ "AIID",
                                 Application=="Individual state" ~ "State", 
                                 Application=="Migration Tracking" ~ "Movement",
                                 Application=="Other (add 'notes')" ~ "Other",
                                 Application=="Population census" ~ "Census",
                                 Application=="Seasonal movement" ~ "Movement"))

write.csv(use, file.path(root, "Results", "PapersToAnalyze.csv"), row.names=FALSE)

#5. Export model type column values for tidying----
mods.raw <- data.frame('Model Type' = sort(unique(use$Model.Type)))

write.csv(mods.raw, file.path(root, "LitSearch", "datacleaninglookups", "ModelType_raw.csv"), row.names = FALSE)

#6. Read back in and join to data----
mods <- read.csv(file.path(root, "LitSearch", "datacleaninglookups", "ModelType.csv"))

use.mods <- use %>% 
  left_join(mods) %>% 
  separate(Model, into=c("Model1", "Model2", "Model3", "Model4"), sep=", ", remove=TRUE) %>% 
  pivot_longer(Model1:Model4, names_to="ModelNo", values_to="Model") %>% 
  dplyr::filter(!is.na(Model))

#7. Check----
#Check for unique model names
# table(use.mods$Model) %>% 
#   data.frame() %>% 
#   dplyr::filter(Freq > 0) %>%
#   rename(Model=Var1) %>% 
#   arrange(Model) %>% 
#   View()

#check for classification type
# table(use.mods$Model, use.mods$Classification.Method) %>% 
#   data.frame() %>% 
#   dplyr::filter(Freq > 0) %>%
#   rename(Model=Var1, Method=Var2) %>% 
#   arrange(Model) %>% 
#   View()

#TO DO: COME BACK TO CLASSIFICATION TYPE VS MODEL####

#8. Look at difference found----
table(use.mods$Difference.found)

#9. Tidy sample size----
use.n <- use %>% 
  separate(Sample.Size.., into=c("n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9", "n10"), sep=", ", remove=FALSE) %>% 
  pivot_longer(n1:n10, names_to="SampleNo", values_to="Individuals") %>% 
  mutate(Individuals = as.numeric(Individuals)) %>% 
  dplyr::filter(!is.na(Individuals)) %>% 
  arrange(-Individuals)

#10. Visualize sample size----
ggplot(use.n) +
  geom_histogram(aes(x=Individuals, fill=Taxa)) +
  facet_wrap(~Classification.Method, scales="free")

#11. Explore recording method----
table(use$Recording.Method)

#12. Tidy application----
table(use$Application)

#13. Explore taxa----
table(use$Taxa)

#14. Test for differences in observer----
