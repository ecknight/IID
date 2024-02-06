#A. PREAMBLE####

#1. Packages----

library(tidyverse)
library(googlesheets4)
library(googledrive)

#2. Files location----
root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/"

#B. WRANGLE####

#1. Read in raw table----
raw <- read_sheet(ss="1J9SN-VR6WF3GVycpxd5RY6gXN3gIvmo7AJutYRDOJcU",
                  col_types="cncccccncccccccccccccccccccc") %>% 
  data.frame()
2
# dat <- raw %>% 
#   unnest(cols=colnames(raw))

#TO DO: FIX DF ISSUE####

#2. Check for incompleted rows----
todo <- raw %>% 
  mutate(todo = case_when(is.na(Relevant) ~ "Evaluate",
                          (is.na(Classification.Method) & Relevant=="Yes") ~ "AllDetails",
                          (!is.na(Classification.Method) & is.na(Model.Type) & !Flag %in% c("Remove - insufficient methods detail ", "Needs checked (no access)", "Not from original research") & Relevant!="Review") ~ "ModelDetails")) %>% 
  dplyr::filter(!is.na(todo),
                Flag!="Needs checked (no access)",
                Flag!="Remove - insufficient methods detail",
                Relevant!="No")
nrow(todo)
table(todo$todo, todo$observer)

#3. Look at relevant proportion----
table(raw$Relevant)

#4. Filter to relevant and reviews, wrangle----
#change Tess & Elly to just Elly
#Tidy application
#Remove unwanted fields
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
                                 Application=="Seasonal movement" ~ "Movement")) %>% 
  mutate(Citation = paste0(author, ". ", year, ". ", title, ". ", journal, " ", volume, ":", pages, ". DOI:", doi)) %>% 
  rename(Author=author, Year=year, Title=title, Journal=journal, Volume=volume, Number=number, Pages=pages, DOI=doi, Observer=observer, ClassificationMethod=Classification.Method, ModelName=Model.Type, DifferenceFound=Difference.found, SampleSize=Sample.Size.., RecordingMethod=Recording.Method) %>% 
  dplyr::select(Citation, Author, Year, Title, Journal, Volume, Number, Pages, DOI, Observer, ClassificationMethod, ModelName, DifferenceFound, SampleSize, RecordingMethod, Application, Taxa)

#C. MODEL TYPE####

#1. Export model type column values for tidying----
mods.raw <- data.frame(ModelName = sort(unique(use$Model)))

write.csv(mods.raw, file.path(root, "LitSearch", "datacleaninglookups", "ModelType_raw.csv"), row.names = FALSE)

#Read back in add to the existing annotated file
mods <- read.csv(file.path(root, "LitSearch", "datacleaninglookups", "ModelType.csv")) %>% 
  right_join(mods.raw) %>% 
  arrange(Model)

write.csv(mods, file.path(root, "LitSearch", "datacleaninglookups", "ModelType.csv"), row.names = FALSE)

#2. Write out tidied model types for grouping----
modgroup.raw <- mods %>% 
  dplyr::select(Model) %>% 
  separate(Model, into=c("Model1", "Model2", "Model3", "Model4"), sep=", ", remove=TRUE) %>% 
  pivot_longer(Model1:Model4, names_to="ModelNo", values_to="Model") %>% 
  dplyr::filter(!is.na(Model)) %>% 
  dplyr::select(Model) %>% 
  unique() %>% 
  arrange(Model)

write.csv(modgroup.raw, file.path(root, "LitSearch", "datacleaninglookups", "ModelGroup_raw.csv"), row.names = FALSE)

#Read back in add to the existing annotated file
modgroup <- read.csv(file.path(root, "LitSearch", "datacleaninglookups", "ModelGroup.csv")) %>%
  right_join(modgroup.raw) %>% 
  arrange(Model)

write.csv(modgroup, file.path(root, "LitSearch", "datacleaninglookups", "ModelGroup.csv"), row.names = FALSE)

#3. Remove model types that don't fit classification type----
mods <- read.csv(file.path(root, "LitSearch", "datacleaninglookups", "ModelType.csv"))
modgroup <- read.csv(file.path(root, "LitSearch", "datacleaninglookups", "ModelGroup.csv")) %>% 
  rename(ModelObjective = Objective,
         ModelClassifier = Classifier,
         ModelGroup = Group)

use.mods.long <- use %>% 
  left_join(mods) %>% 
  separate(Model, into=c("Model1", "Model2", "Model3", "Model4"), sep=", ", remove=TRUE) %>% 
  pivot_longer(Model1:Model4, names_to="ModelNo", values_to="Model") %>% 
  dplyr::filter(!is.na(Model)) %>% 
  left_join(modgroup) %>% 
  dplyr::filter(!(ClassificationMethod %in% c("Closed set", "Open set") &
                    ModelObjective=="Difference"))

#4. Save model details file----
out.mods <- use.mods.long %>% 
  dplyr::select(Citation, ClassificationMethod, Model, ModelGroup) %>% 
  arrange(Citation, Model, ModelGroup)

write.csv(out.mods, file.path(root, "Results", "ModelTypeResults.csv"), row.names = FALSE)
  
#5. Reformat wide----
use.mods <- use.mods.long %>% 
  dplyr::select(-ModelGroup, -ModelObjective, -ModelClassifier, -Notes) %>% 
  group_by(Citation) %>% 
  mutate(ModelNo = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(values_from=Model, names_from=ModelNo, names_prefix="Model") %>% 
  mutate(Model = case_when(is.na(Model2) ~ Model1,
                           is.na(Model3) ~ paste(Model1, Model2, sep=", "),
                           is.na(Model4) ~ paste(Model1, Model2, Model3, sep=", "),
                           !is.na(Model4) ~ paste(Model1, Model2, Model3, Model4))) %>% 
  dplyr::select(-Model1, -Model2, -Model3, -Model4) %>% 
  arrange(Year, Author)

#D. SAMPLE SIZE####

#1. Tidy sample size----
use.n.long <- use.mods %>% 
  separate(SampleSize, into=c("n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9", "n10"), sep=", ", remove=FALSE) %>% 
  pivot_longer(n1:n10, names_to="SampleNo", values_to="Individuals") %>% 
  mutate(SampleSize = as.numeric(Individuals)) %>% 
  dplyr::filter(!is.na(SampleSize))

#2. Save out sample size file----
out.n <- use.n.long %>% 
  dplyr::select(Citation, SampleSize) %>% 
  arrange(Citation)

write.csv(out.n, file.path(root, "Results", "SampleSizeResults.csv"), row.names = FALSE)

#3. Visualize sample size----
ggplot(use.n.long) +
  geom_histogram(aes(x=SampleSize, fill=Taxa)) +
  facet_wrap(~ClassificationMethod, scales="free")

#E. FINAL TABLE####

#1. Write it out----
write.csv(use.mods, file.path(root, "Results", "AIIDPapersCleaned.csv"), row.names = FALSE)
