
## Libraries 
library(readxl)
library(tidyverse)
library(sf)
#library(terra)


## Create folders
dir.create("data", showWarnings = FALSE)
dir.create("report", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

## paths
path_datafile <- "data-source/NFI_Pilot_2024_20240510.xlsx"
