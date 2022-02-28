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
  # create the destination and output
  dest1 <- paste(site, "batch", batch_no, index, note, sep="_")
  dest2 <- paste(dest1, "csv", sep=".")
  dest3 <- "results" # destination folder
  dest4 <- paste(dest3, dest2, sep = "/")
  dest <- paste(dir, dest4, sep ="/")
  # call multiple_sounds() and pass relevant parameters
  multiple_sounds(directory = file.import, 
                  resultfile = dest,
                  soundindex = index,
                  no_cores = "max")
}

compute_indices(aci,          # <-- index
                "parkside",   # <-- string for site of recording
                1,            # <-- batch number
                "")           # <-- misc. note


### FOR CUSTOM NDSI (i.e. setting anthro/bio ranges) ### 

compute_custom_ndsi <- function(site, batch_no, note, a_min, a_max, b_min, b_max) {
  # create the destination and output
  dest1 <- paste(site, "batch", batch_no, "ndsi", note, sep="_")
  dest2 <- paste(dest1, "csv", sep=".")
  dest3 <- "results" # destination folder
  dest4 <- paste(dest3, dest2, sep = "/")
  dest <- paste(dir, dest4, sep ="/")
  # call multiple_sounds() and pass relevant parameters
  multiple_sounds(directory = file.import, 
                  resultfile = dest,
                  soundindex = "ndsi",
                  no_cores = "max",
                  anthro_min = a_min,
                  anthro_max = a_max,
                  bio_min = b_min,
                  bio_max = b_max)
}

compute_custom_ndsi("parkside",   # <-- string for site of recording
                    4,             # <-- batch number
                    "50",         # <-- note: lower range of anthropophony in Hz
                    50,           # <-- anthro min in Hz
                    1999,          # <-- anthro max in Hz
                    2000,          # <-- biophony min in Hz
                    8000)          # <-- biophony max in Hz

###############
