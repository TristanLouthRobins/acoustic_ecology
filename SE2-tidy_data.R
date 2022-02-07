library(chron)
library(forcats)
library(stringr)
library(tidyverse)

data <- read_csv("/Users/tristanlouth-robins/Documents/Documents - MacBook Pro/R data and projects/acoustic_ecology_tests/Middle Farm research/analysis data/mfavif-2021-0810_to_1017.csv")

# First, convert time into as.POSIXct format (times)

data <- data %>% 
  mutate(time.posix = as.POSIXct(time))  # Convert time to POSIXct

# Separate dataframe into periods before and after daylight savings (< 3/10/2021, >= 3/10/2021)
  
data.1 <- data %>% filter(date < "2021-10-03")
data.2 <- data %>% filter(date > "2021-10-02")   

data.2 <- data.2 %>% 
  mutate(time.posix = time.posix + 3600) # Add one hour (3600 seconds)
  
# Join dataframes back together

data.3 <- full_join(data.1, data.2) %>% 
  mutate(time = format(time.posix,"%H:%M:%S")) %>% 
  mutate(time = chron(time = time)) %>% 
  select(-time.posix)

data <- data.3

data
