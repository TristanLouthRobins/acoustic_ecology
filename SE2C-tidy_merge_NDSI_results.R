# Soundecology script for tidying, joining acoustic data from AudioMoth.
# Tristan Louth-Robins. 2021-2022

# Latest version 2.1: 4/1/22, cleaned up function and added new categorical data variable inclusions:
# 1) period of day; 2) season

library(chron)
library(forcats)
library(stringr)
library(lubridate)
library(tidyverse)

## ACOUSTIC ECOLOGY TIDY ##

# IMPORT THE CSV FOR ANALYSIS AND TIDY

path.name <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/results/"
project.name <- "parkside_batch_"
file.ndsi <- "ndsi_50.csv"
file.path.ndsi <- paste(path.name,project.name,file.ndsi, sep = "")

ndsi <- read_csv(file.path.ndsi) %>% as_tibble()

# Tidy dataset:

tidy_data <- function(input) {
  output <- input %>% 
    separate(FILENAME, into = c("date", "time", sep = "_")) %>% 
    mutate(date = gsub("^(.{4})(.*)$", "\\1-\\2", date), date = gsub("^(.{7})(.*)$", "\\1-\\2", date)) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(time = gsub("^(.{2})(.*)$", "\\1:\\2", time), time = gsub("^(.{5})(.*)$", "\\1:\\2", time)) %>% 
    mutate(time = chron(times = time)) %>% # coerce chr time string into time data format.
    mutate(mins = format(strptime(time,"%H:%M:%S"), '%M')) %>% 
    mutate(hour = format(strptime(time,"%H:%M:%S"), '%H')) %>% 
    mutate(mins = as.numeric(mins), hour = as.numeric(hour)) %>% 
    select(date, time, mins, hour, LEFT_CHANNEL) %>% 
    rename(index = LEFT_CHANNEL) %>% 
    group_by(date) 
  
  output <- output %>% 
    mutate(date = as.character.Date(date)) %>% 
    mutate(date.data = date) %>% 
    separate(date.data, into = c("year", "month", "day", sep = "-")) %>% 
    select(- `-`) 
  
  output <- output[,c("date", "time", "year", "month", "day", "mins", "hour", "index")]  
  
  return(output)
}

ndsi.tidy <- tidy_data(ndsi) %>% rename(NDSI = index) 

# project name pasted to new dataframe with all data joined

project.name <- paste(project.name,"all_data", sep = "")

all.data <- ndsi.tidy

#######################
# DATA TRANSFORMATION #
#######################

# Categorical 1: Period of day, defined by range of 'hour' variable. 

all.data <- all.data %>%   
  mutate(period = ifelse(hour %in% 0:4, "pre-dawn", 
                         ifelse(hour %in% 5:7, "dawn", 
                                ifelse(hour %in% 8:11, "morning",
                                       ifelse(hour %in% 12:13, "midday",
                                              ifelse(hour %in% 14:17, "afternoon",
                                                     ifelse(hour %in% 18:19, "dusk", 
                                                            ifelse(hour %in% 20: 23, "night","X"))))))) 
  ) %>% 
  mutate(period = as.factor(period)) %>% 
  # Categorical 2: Season, defined by range of 'month' variable.
  mutate(season = ifelse(month == 12 | 1 | 2, "Summer",
                         ifelse(month == 3 | 4 | 5, "Autumn",
                                ifelse(month == 6 | 7 | 8, "Winter",
                                       ifelse(month == 9 | 10 | 11, "Spring","X"))))
  ) %>% 
  mutate(season = as.factor(season))

# re-order factors 
all.data$period <- ordered(all.data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
all.data$season <- ordered(all.data$season, levels = c("Summer", "Autumn", "Winter", "Spring"))

# re-coerce date variable as date data type:
all.data <- all.data %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  # coerce split (individual) date variable as factors
  mutate(year = as.factor(year),
         month = as.factor(month),
         day = as.factor(day)) 

############################
# WRITE MERGED DATA TO CSV #
############################

file <-  paste(path.name, project.name, ".csv", sep = "")

write_csv(all.data, file)

###################################

#####################################
# SUPPLEMENTAL: MERGE PREVIOUS DATA #
#####################################

# merge data sets
one <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/results/tidy-parkside--ndsi_500.csv" # 
two <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/results/tidy_parkside_batch_4_ndsi_500.csv" # 

one <- read_csv(one) 
two <- read_csv(two)

# ensure that split date variables are factorised:
one <- one %>% 
  mutate(year = as.factor(year),
         month = as.factor(month),
         day = as.factor(day)) 

two <- two %>% 
  mutate(year = as.factor(year),
         month = as.factor(month),
         day = as.factor(day)) 

both <- bind_rows(one, two)

# new project name goes here
project.name <- "tidy-parkside-"

file <-  paste(path.name, project.name, "-ndsi_500",".csv", sep = "")

write_csv(both, file)

# join sets

one <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/results/tidy-parkside--ndsi_50.csv" # 
two <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/results/tidy-parkside--ndsi_500.csv" # 

one <- read_csv(one) %>% 
  rename(NDSI.50 = NDSI)
two <- read_csv(two) %>% 
  rename(NDSI.500 = NDSI)

view(one)
two

full <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/results/parkside--merged.csv" # 
full <- read_csv(full) %>% 
  rename(NDSI.1000 = NDSI)

full

all <- full_join(full,two)
all

project.name <- "parkside-"

file <-  paste(path.name, project.name, "-complete",".csv", sep = "")

write_csv(all, file)
