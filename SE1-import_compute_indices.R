# Script for computing acoustic indices using the soundecology package.
# Tristan Louth-Robins. 2021-22
###################################

# REQUIRED LIBRARIES
library(tidyverse)
library(soundecology)
library(tuneR)
library(warbleR) # for entropy, dominant frequency contours, etc. See: https://cran.r-project.org/web/packages/warbleR/vignettes/warbleR_workflow_03.html

getwd() # get working directory and paste as string below 
dir <- setwd("/Users/tristanlouth-robins/data_science/acoustic_ecology_tests") # set working directory
files <- "raw_data/" # File path for import of raw acoustic data:
file.import <- paste(dir, files, sep = "/")

bi <-  "bioacoustic_index"
aci <-  "acoustic_complexity"
adi <-  "acoustic_diversity"
aei <- "acoustic_evenness"
ndsi <-  "ndsi"

?multiple_sounds

compute_indices <- function(index, site, batch_no, note){
  # define lookup table for indices
  
  # create the destination and output
  dest1 <- paste(site, "batch", batch_no, index, note, sep="_")
  dest2 <- paste(dest1, "csv", sep=".")
  dest3 <- "results" # destination folder
  dest4 <- paste(dest3, dest2, sep = "/")
  dest <- paste(dir, dest4, sep ="/")
  # call multiple_sounds() function
  multiple_sounds(directory = file.import, 
                  resultfile = dest,
                  soundindex = index,
                  no_cores = "max")
}

compute_indices(aci,"parkside",4,"")

# Note: Explore more with tweaking the parameters.

### FOR NDSI ### PREVIOUS CODE - CURRENTLY NOT FOR USE (TO BE UPDATED AT A LATER STAGE.)

import_ndsi_data <- function(path, dest, index, cores, a_min, a_max, b_min, b_max) {
  multiple_sounds(directory = path, 
                  resultfile = dest,
                  soundindex = index,
                  no_cores = cores,
                  anthro_min = a_min,
                  anthro_max = a_max,
                  bio_min = b_min,
                  bio_max = b_max)
}

###############
