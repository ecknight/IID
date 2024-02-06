#A. PREAMBLE####

#1. Packages----

library(tidyverse)
library(googlesheets4)
library(googledrive)

#2. Files location----
root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/"

#3. Files----
dat <- read.csv(file.path(root, "Results", "AIIDPapersCleaned.csv"))
dat.n <- read.csv(file.path(root, "Results", "SampleSizeResults.csv"))
dat.mods <- read.csv(file.path(root, "Results", "ModelTypeResults.csv"))

#B. EXISTING APPROACHES####

#1. Studies reviewed----
nrow(dat)

#2. Number per year----
year.dat <- dat %>% 
  group_by(Year, ClassificationMethod) %>% 
  summarize(n=n()) %>% 
  group_by(Year) %>% 
  mutate(yeartotal = sum(n),
         ratio = n/yeartotal) %>% 
  ungroup

ggplot(year.dat%>% dplyr::filter(Year!=2023)) +
  geom_smooth(aes(x=Year, y=yeartotal)) +
  geom_point(aes(x=Year, y=yeartotal))

ggplot(dat %>% dplyr::filter(Year!=2023)) +
  geom_histogram(aes(x=Year, fill=ClassificationMethod))

year.dat %>% 
  mutate(decade = round(Year, -1)) %>% 
  group_by(decade) %>% 
  summarize(n=mean(yeartotal)) %>% 
  ungroup()

#2. Proportion of evidence----
table(dat$DifferenceFound)
dat %>% 
  mutate(total=n()) %>% 
  group_by(DifferenceFound, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

dat %>% 
  dplyr::filter(DifferenceFound=="No") %>% 
  group_by(Taxa) %>% 
  summarize(n=n()) %>% 
  ungroup()

#3. Number of classificaiton studies----
dat %>% 
  dplyr::filter(ClassificationMethod!="Difference test") %>% 
  nrow()

#4. Closed vs open set----
dat %>% 
  dplyr::filter(ClassificationMethod!="Difference test") %>% 
  mutate(total=n()) %>% 
  group_by(ClassificationMethod, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

#5. Model groups in closed set----
dat.mods %>% 
  dplyr::filter(ClassificationMethod=="Closed set") %>% 
  mutate(total=n()) %>% 
  group_by(ModelGroup, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

#6. Number of studies with DFA----
dat.mods %>% 
  dplyr::filter(ClassificationMethod=="Closed set") %>% 
  mutate(total=n()) %>% 
  group_by(Model, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

#7. Effect of year on method----
dat.dfa <- dat.mods %>% 
  dplyr::filter(ClassificationMethod=="Closed set") %>% 
  mutate(dfa = ifelse(Model=="DFA", 1, 0)) %>% 
  group_by(Citation) %>% 
  summarize(dfa = ifelse(sum(dfa) > 0, 1, 0)) %>% 
  left_join(dat)

ggplot(dat.dfa) +
  geom_smooth(aes(x=Year, y=dfa))

lm1 <- glm(dfa ~ Year, data=dat.dfa, family="binomial")
lm2 <- glm(dfa ~ poly(Year,2), data=dat.dfa, family="binomial")
AIC(lm1, lm2)
summary(lm1)
summary(lm2)

pred2 <- data.frame(predict(lm2, type="response", se.fit=TRUE)) %>% 
  cbind(dat.dfa) %>% 
  mutate(fit.low = fit-1.96*se.fit,
         fit.high = fit+1.96*se.fit) %>% 
  dplyr::select(fit, fit.low, fit.high, Year) %>% 
  unique()

ggplot(pred2) +
  geom_line(aes(x=Year, y=fit)) +
  geom_ribbon(aes(x=Year, ymin=fit.low, ymax=fit.high), alpha = 0.5) +
  ylim(c(0,1.1))

ggplot(dat.dfa) +
  geom_smooth(aes(x=Year, y=dfa), method="lm") +
  ylim(c(0,1.1))

year.dfa <- dat.dfa %>% 
  group_by(Year) %>% 
  summarize(dfa = sum(dfa),
            n = n()) %>% 
  ungroup() %>% 
  mutate(ratio = dfa/n*100)

ggplot(year.dfa) +
  geom_point(aes(x=Year, y=ratio))

dat.deep <- dat.mods %>% 
  dplyr::filter(ClassificationMethod=="Closed set") %>% 
  mutate(deep = ifelse(ModelGroup=="Deep learning", 1, 0)) %>% 
  group_by(Citation) %>% 
  summarize(deep = ifelse(sum(deep) > 0, 1, 0)) %>% 
  left_join(dat)

ggplot(dat.deep) +
  geom_smooth(aes(x=Year, y=deep))

lm1 <- glm(deep ~ Year, data=dat.deep, family="binomial")
lm2 <- glm(deep ~ poly(Year, 2), data=dat.deep, family="binomial")
lm3 <- glm(deep ~ poly(Year, 3), data=dat.deep, family="binomial")
AIC(lm1, lm2, lm3)
summary(lm3)

year.deep <- dat.deep %>% 
  group_by(Year) %>% 
  summarize(deep = sum(deep),
            n = n()) %>% 
  ungroup() %>% 
  mutate(ratio = deep/n*100)

ggplot(year.deep) +
  geom_point(aes(x=Year, y=ratio))

pred3 <- data.frame(predict(lm3, type="response", se.fit=TRUE)) %>% 
  cbind(dat.deep) %>% 
  mutate(fit.low = fit-1.96*se.fit,
         fit.high = fit+1.96*se.fit) %>% 
  dplyr::select(fit, fit.low, fit.high, Year) %>% 
  unique()

ggplot(pred3) +
  geom_line(aes(x=Year, y=fit)) +
  geom_ribbon(aes(x=Year, ymin=fit.low, ymax=fit.high), alpha = 0.5)

#8. Model groups in open set----
dat.mods %>% 
  dplyr::filter(ClassificationMethod=="Open set") %>% 
  mutate(total=n()) %>% 
  group_by(ModelGroup, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

#9. Recording method----
dat %>% 
  dplyr::filter(ClassificationMethod!="Difference test") %>% 
  mutate(total=n()) %>% 
  group_by(RecordingMethod, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

#10. Applications----
dat %>% 
  dplyr::filter(ClassificationMethod!="Difference test") %>% 
  mutate(total=n()) %>% 
  group_by(Application, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

#11. Non AIID per se applications----
dat %>% 
  dplyr::filter(ClassificationMethod!="Difference test", Application!="AIID") %>% 
  mutate(total=n()) %>%  
  group_by(Application, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

#12. Non Behavioural applications----
dat %>% 
  dplyr::filter(ClassificationMethod!="Difference test", !Application %in% c("AIID", "Behaviour")) %>% 
  mutate(total=n(),
         Application=ifelse(Application %in% c("Census", "Density"), "PopSize", Application)) %>% 
  group_by(Application, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

#C. CHALLENGES####

#1. Sample size----
dat.n %>% 
  left_join(dat %>% 
              dplyr::select(-SampleSize)) %>% 
  dplyr::filter(ClassificationMethod!="Difference test") %>% 
  summarize(n=n(),
            mean=mean(SampleSize),
            sd=sd(SampleSize),
            min=min(SampleSize),
            max=max(SampleSize))

#2. Taxa----
dat %>% 
  dplyr::filter(ClassificationMethod!="Difference test") %>% 
  mutate(total=n()) %>% 
  group_by(Taxa, total) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(percent = n/total)

#3. Sample size by taxa----
dat.n.taxa <- dat.n %>% 
  left_join(dat %>% 
              dplyr::select(-SampleSize)) %>% 
  dplyr::filter(ClassificationMethod!="Difference test") %>% 
  mutate(Taxa = factor(Taxa),
         Taxa = relevel(Taxa, ref="Birds")) %>% 
  dplyr::filter(!is.na(SampleSize),
                !is.na(Taxa))

ggplot(dat.n.taxa) +
  geom_histogram(aes(x=SampleSize, fill=Taxa)) +
  facet_wrap(~Taxa)

lmt <- glm(SampleSize ~ Taxa, data=dat.n.taxa, family="poisson")
summary(lmt)
