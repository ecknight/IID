library(tidyverse)

root <- "G:/.shortcut-targets-by-id/14H5BXdBP8k2jv4jgUjO5QTqHvOmxMSBn/IndividualID/PerspectivesPaper/"

raw <- readxl::read_excel(file.path(root, "LitSearch", "Literature Review - Individual ID.xlsx"))
