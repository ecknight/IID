library(tidyverse)
library(tuneR)
library(seewave)

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/DatasetExample"

#1. Get annotation data----
dat <- read.csv(file.path(root, "OvenbirdIndividualAnnotations.csv"))

#2. Get recording inventory----
load("G:/My Drive/ABMI/Projects/Recognizers/Data/clip_view_2024-03-25.Rdata")

#3. Get the location & date list---
reclist <- dat |> 
  dplyr::select(location, recording_date_time, recording_id, recording_duration) |> 
  rename(date = recording_date_time) |> 
  unique()

#4. Get the recording URLs----
recurls <- raw |> 
  dplyr::select(location, date, recording_url) |> 
  unique() |> 
  right_join(reclist) |> 
  mutate(filetype = str_sub(recording_url, -4, -1),
         filetype = ifelse(filetype==".mp3", "mp3", filetype))

#5. Check for NAs----
na <- dplyr::filter(recurls, is.na(recording_url))
nrow(na)
  
#6. Download the recordings----
for(i in 1:nrow(recurls)){
  
  #download compressed recordings
  try(download.file(recurls$recording_url[i],
                    destfile = file.path(tempdir(), paste0(recurls$recording_id[i], ".", recurls$filetype[i])),
                    quiet=TRUE,
                    method="libcurl",
                    mode="wb"))
  
  #convert to wav if mp3
  if(recurls$filetype[i]=="mp3"){
    try(mp3.i <- readMP3(file.path(tempdir(), paste0(recurls$recording_id[i], ".", recurls$filetype[i]))))
    if(class(mp3.i)=="Wave"){
      wav.i <- cutw(mp3.i, from=0, to=recurls$recording_duration[i], output="Wave")
      writeWave(wav.i, file.path(root, "Recordings", paste0(recurls$recording_id[i], ".wav")))
    }
  }
  
  #convert to wav if flac
  if(recurls$filetype[i]=="flac"){
    
    setwd(tempdir())
    
    #convert
    wav2flac(file=paste0(recurls$recording_id[i], ".flac"),
             reverse=TRUE)
    
    #readin duration needed
    wav.i <- readWave(file.path(tempdir(), paste0(recurls$recording_id[i], ".wav")), from=0, to=recurls$recording_duration[i], units="seconds")
    
    #Write wave
    writeWave(wav.i, file.path(root, "Recordings", paste0(recurls$recording_id[i], ".wav")))
    
  }
  
  print(paste0("Finished recording ", i, " of ", nrow(recurls)))
  
}
