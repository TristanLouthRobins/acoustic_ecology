# Soundecology script for tidying, joining acoustic data from AudioMoth.
# Tristan Louth-Robins. 2021-2022

# Latest version 2.1: 4/1/22, cleaned up function and added new categorical data variable inclusions:
# 1) period of day; 2) season

library(chron)
library(forcats)
library(stringr)
library(tidyverse)

## ACOUSTIC ECOLOGY TIDY ##

# IMPORT THE CSV FOR ANALYSIS AND TIDY

path.name <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/data_analysis/"
project.name <- "parkside_batch3-"

file.bi <- "bi.csv"
file.aci <- "aci.csv"
file.adi <- "adi.csv"
file.aei <- "aei.csv"
file.ndsi <- "ndsi_mid_anthro.csv"

file.path.bi <- paste(path.name,project.name,file.bi, sep = "")
file.path.aci <- paste(path.name,project.name,file.aci, sep = "")
file.path.adi <- paste(path.name,project.name,file.adi, sep = "")
file.path.aei <- paste(path.name,project.name,file.aei, sep = "")
file.path.ndsi <- paste(path.name,project.name,file.ndsi, sep = "")

bi <- read_csv(file.path.bi) %>% as_tibble()
aci <- read_csv(file.path.aci) %>% as_tibble()
adi <- read_csv(file.path.adi) %>% as_tibble()
aei <- read_csv(file.path.aei) %>% as_tibble()
ndsi <- read_csv(file.path.ndsi) %>% as_tibble()

# NOTE 4/1/2022: ADI and AEI processes are producing 'double-runs' due to the DB_threshold variable being reported 
# in the second run. Therefore, the DB_threshold variable is omitted, then duplicates are removed from the respective
# datasets.

adi <- adi %>% select(-DB_THRESHOLD)
aei <- aei %>% select(-DB_THRESHOLD)

adi <- unique(adi)
aei <- unique(aei)

# Tidy dataset:
## Takes the 'FILENAME' string and separates it into new variables for date and time. 
## They are then coerced into their respective formats (date, time)
## e.g '20210810_060000.WAV' becomes '2021-08-10', '06:00:00'

# FUNCTION: tidy data
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

bi.tidy <- tidy_data(bi) %>% rename(BI = index)
aci.tidy <- tidy_data(aci) %>% rename(ACI = index) 
adi.tidy <- tidy_data(adi) %>% rename(ADI = index) 
aei.tidy <- tidy_data(aei) %>% rename(AEI = index) 
ndsi.tidy <- tidy_data(ndsi) %>% rename(NDSI = index) 

join.1 <- full_join(bi.tidy, aci.tidy)
join.2 <- full_join(adi.tidy, join.1)
join.3 <- full_join(aei.tidy, join.2)
join.4 <- full_join(ndsi.tidy, join.3)

# project name pasted to new dataframe with all data joined

project.name <- paste(project.name,"all_data", sep = "")

all.data <- join.4
  
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
one <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/data_analysis/parkside--merged.csv"
two <- "/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/data_analysis/parkside_batch3-all_data.csv"

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
project.name <- "parkside-"

file <-  paste(path.name, project.name, "-merged2",".csv", sep = "")

write_csv(both, file)
