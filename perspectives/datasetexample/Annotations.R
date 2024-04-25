library(wildRtrax)
library(tidyverse)

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/DatasetExample"

#1. login----
config <- "perspectives/login.R"
source(config)
wt_auth()

#2. Download data----
raw <- wt_download_report(project_id=623, sensor_id="ARU", weather_cols = FALSE, reports=c("main", "definitions"))

#3. Separate out main report and filter----
dat <- raw$`BU_Bayne-OVEN-BU-AnyYr_2021_main_report.csv` |> 
  dplyr::filter(aru_task_status!="New",
                species_code=="OVEN",
                recording_date_time!=ymd_hms("2000-01-04 02:00:00"),
                latitude < 60) |> 
  dplyr::select(location, longitude, latitude, equipment_make, equipment_model, recording_id, recording_date_time, task_duration, species_scientific_name, individual_order, individual_count, vocalization, detection_time, tag_duration, observer_id) |>
  rename(recording_duration = task_duration)
  
#4. Get relevant column definitions----
defs <- raw$english_column_definitions.csv |> 
  dplyr::filter(column_name_english %in% colnames(dat))

#This is not useful, nevermind

#5. Save----
write.csv(dat, file.path(root, "OvenbirdIndividualAnnotations.csv"), row.names = FALSE)
