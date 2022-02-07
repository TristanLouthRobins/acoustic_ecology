# Soundecology script for importing and processing acoustic data from AudioMoth.
# Tristan Louth-Robins. 2021

###################################

# REQUIRED LIBRARIES
library(chron)
library(forcats)
library(stringr)
library(tidyverse)

library(gghighlight)
library(lubridate)
library(soundecology)
library(tuneR)
library(warbleR) # for entropy, dominant frequency contours, etc. See: https://cran.r-project.org/web/packages/warbleR/vignettes/warbleR_workflow_03.html

# 1. SCRIPT SET UP: 

# getwd() # locate current wd
setwd("/Users/tristanlouth-robins/data_science/acoustic_ecology_tests") # set working directory

# Directory path for import of raw acoustic data:
raw.path <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/raw_data2/"

# Name of output file:
file <- "parkside_batch4_2-ndsi-1000.csv"
# Directory path for the above file:
path <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/data_analysis/"
file.path <- paste(path,file, sep = "")

# Acoustic indices:
bi <- "bioacoustic_index"
aci <- "acoustic_complexity"
adi <- "acoustic_diversity"
aei <- "acoustic_evenness"
ndsi <- "ndsi"


# FUNCTION: import_data() 
## This function imports the raw acoustic data into R and outputs it as a
## .csv file. 
## 'path' = dir. path of raw data.
## 'dest' = dir. path for .csv output (same path for analysis import)
## 'cores' = number of cores to use for processing. 'max' recommended for large imports.

import_data <- function(path, dest, index, cores) {
  multiple_sounds(directory = path, 
                  resultfile = dest,
                  soundindex = index,
                  no_cores = cores)
}

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
# IMPORT DATA #
###############

# call import_data()
import_data(raw.path, file.path, ndsi, "max")

import_ndsi_data(raw.path, file.path, ndsi, "max", 500, 1999, 2000, 8000)

################
################