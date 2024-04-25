library(tidyverse)
library(lubridate)
library(sf)

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/DatasetExample"

#1. Get annotation data----
dat <- read.csv(file.path(root, "OvenbirdIndividualAnnotations.csv"))

#2. Geographic coverage----
dat.sf <- st_as_sf(dplyr::filter(dat, !is.na(latitude)), coords=c("longitude", "latitude"), crs=4326)

ggplot(dat.sf) +
  geom_sf()

st_bbox(dat.sf)

#3. Temporal coverage----
min(ymd_hms(dat$recording_date_time))
