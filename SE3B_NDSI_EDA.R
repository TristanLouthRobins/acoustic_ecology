library(tidyverse)
library(rlang)
library(gghighlight)
library(ggplot2)
library(ggbeeswarm)

data <- read_csv("/Users/tristanlouth-robins/Documents/Documents - MacBook Pro/R data and projects/acoustic_ecology_tests/acoustic_ecology/data/parkside--complete.csv")

data$period <- ordered(data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
data$season <- ordered(data$season, levels = c("Summer", "Autumn", "Winter", "Spring"))

data_pivot <- data %>% 
  pivot_longer(cols = c("NDSI.1000","AEI","ADI","BI","ACI","NDSI.500"), names_to = "index")

ndsi <- data_pivot %>% 
  filter(index %in% c("NDSI.1000", "NDSI.500"))

# overall
overall <- ggplot(ndsi, aes(x=date, y=value)) 

overall + geom_point() +
  facet_wrap(~index)

proportions <- ggplot(ndsi, aes(value)) 

proportions + 
  geom_histogram(aes(fill= index), alpha = 0.6) +
  geom_vline(aes(xintercept=0.0), alpha = 0.5) +
  facet_wrap(~period)

proportions +
  geom_density(aes(fill= index), alpha = 0.6) +
  facet_wrap(~period)


##################

# given day
ndsi_nyd <- ndsi %>% 
  filter(date == "2022-01-01")

ggplot(ndsi_nyd, aes(x=time, y=value, colour = index)) +
  geom_point() 

ggplot(ndsi_nyd, aes(x=time, y=value, colour = index)) +
  geom_point() 

##################

# Correlation with BI

new_pivot <- data %>% 
  pivot_longer(cols = c("NDSI.1000","NDSI.500"), names_to = "index") %>% 
  filter(BI < 10,
         value != "NA")


new_pivot <- new_pivot %>% mutate(soundscape = ifelse(value < -0.75, "Anthropophony: strong", 
                     ifelse(value > -0.74 & value < -0.3, "Anthropophony: moderate", 
                            ifelse(value > -0.3 & value < 0.3, "Neutral", 
                                   ifelse(value > 0.3 & value < 0.75, "Biophony: moderate", 
                                          ifelse(value > 0.75, "Biophony: strong", "x")))))) 

new_pivot$soundscape <- ordered(new_pivot$soundscape, levels = c("Anthropophony: strong", "Anthropophony: moderate", "Neutral", "Biophony: moderate", "Biophony: strong"))

new_pivot <- new_pivot %>% na.omit()

NDSI_range = list("NDSI.1000" = "1kHZ to 2kHz", 
                  "NDSI.500" = "500Hz to 2kHz")

NDSI_labels <- function(variable,value){
  return(NDSI_range[value])
}

compare <- ggplot(new_pivot, aes(x=value, y= BI, colour = soundscape)) +
  geom_point() +
  theme_dark() +
  labs(x="NDSI", y="Bioacoustic Index") +
  geom_vline(aes(xintercept=0.0), alpha = 0.5, colour = "black") +
  facet_wrap(index ~ ., labeller=NDSI_labels)

compare + scale_colour_brewer(palette = "RdYlGn")

compare2 <- ggplot(new_pivot, aes(x=BI)) +
  geom_density(aes(fill = index), alpha = 0.6) +
  theme_dark() +
  labs(x="Bioacoustic Index", y="Density") +
  facet_wrap(~soundscape, ncol=1)

compare2 + scale_colour_brewer(palette = "Set2") 
