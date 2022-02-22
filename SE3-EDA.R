# Sketches for EDA. To rework into coherent script at a later stage. Everything from Line 75 onwards deals with an older project and is only retained for reference to the current project and how EDA might be best realised.
# Tristan Louth-Robins. 2021-22

library(tidyverse)
library(rlang)
library(gghighlight)
library(ggplot2)
library(ggbeeswarm)

data <- read_csv("/Users/tristanlouth-robins/Documents/Documents - MacBook Pro/R data and projects/acoustic_ecology_tests/acoustic_ecology/data/parkside_summer.csv")

data$period <- ordered(data$period, levels = c("pre-dawn", "dawn", "morning", "midday", "afternoon", "dusk", "night"))
data$season <- ordered(data$season, levels = c("Summer", "Autumn", "Winter", "Spring"))

###

data %>% 
  filter(max_temp > 40)

data <- data %>% 
  filter(BI < 10)

data_BI.summary <- data %>% 
  group_by(date) %>% 
  summarise(mean.BI = mean(BI), max_temp = mean(max_temp)) 

data_ACI.summary <- data %>% 
  group_by(date) %>% 
  summarise(mean.ACI = mean(ACI), max_temp = mean(max_temp)) 

data_ADI.summary <- data %>% 
  group_by(date) %>% 
  summarise(mean.ADI = mean(ADI), max_temp = mean(max_temp)) 

data_AEI.summary <- data %>% 
  group_by(date) %>% 
  summarise(mean.AEI = mean(AEI), max_temp = mean(max_temp)) 

data_NDSI.summary <- data %>% 
  group_by(date) %>% 
  summarise(mean.NDSI = mean(NDSI), max_temp = mean(max_temp)) 

#

ggplot(data_ADI.summary, aes(x = mean.ADI, y = max_temp)) +
  geom_point(alpha = 0.7) +
  geom_smooth()

cor(data_ADI.summary$mean.ADI, data_ADI.summary$max_temp)

ggplot(data_AEI.summary, aes(x = mean.AEI, y = max_temp)) +
  geom_point(alpha = 0.7) +
  geom_smooth()

cor(data_AEI.summary$mean.AEI, data_AEI.summary$max_temp)

ggplot(data_ADI.summary, aes(x = mean.ADI, y = max_temp)) +
  geom_point(alpha = 0.7) +
  geom_smooth()

cor(data_ADI.summary$mean.ADI, data_ADI.summary$max_temp)

ggplot(data_NDSI.summary, aes(x = mean.NDSI, y = max_temp)) +
  geom_point(alpha = 0.7) +
  geom_smooth()

cor(data_NDSI.summary$mean.NDSI, data_NDSI.summary$max_temp) 

###
data.pivot <- data %>% 
  pivot_longer(NDSI:ACI, names_to = "index_type")

data.pivot

###################################

# VISUALISATION

# CUSTOM THEME:

theme_avif <- function(){
  theme(
    text = element_text(family = "Arial"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "khaki2"),
    panel.background = element_rect(fill = "lightyellow2"),
    legend.title = element_text(family = "Courier", size = 9),
    legend.text = element_text(family = "Courier", size = 8),
    legend.background = element_rect(fill = "mistyrose2")
  )
}

mf_avif.dataF <- mf_avif.data %>% 
  filter(ACI > 1600)


Sys.setenv(TZ = "CST") # Sets R system timezone to Central Standard Time

ggplot(mf_avif.dataF, aes(time, BI)) +
  geom_point() +
  ylim(0, 10) +
  scale_x_chron(format = "%H:%M") +
  theme_avif() +
  labs(
    title = "BIOACOUSTIC INDEX",
    subtitle = "Sep 22-28 2021",
    x = "",
    y = "BI",
    caption = "Tristan Louth-Robins, AudioMoth 1.2.0."
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggplot(mf_avif.dataF, aes(time, ACI)) +
  geom_point() +
  ylim(1630, 3000) +
  scale_x_chron(format = "%H:%M") +
  theme_avif() +
  labs(
    title = "ACOUSTIC COMPLEXITY INDEX",
    subtitle = "Sep 22-28 2021",
    x = "",
    y = "ACI",
    caption = "Tristan Louth-Robins, AudioMoth 1.2.0."
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

###################################

# Facet by week

ggplot(mf_avif.dataF, aes(time, BI)) +
  geom_point() +
  ylim(0, 10) +
  scale_x_chron(format = "%H:%M") +
  theme_avif() +
  labs(
    title = "BIOACOUSTIC INDEX",
    subtitle = "August 2021",
    x = "",
    y = "BI",
    caption = "Tristan Louth-Robins, AudioMoth 1.2.0."
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~ week, ncol = 1)

ggplot(mf_avif.dataF, aes(time, ACI)) +
  geom_point() +
  ylim(1630, 1800) +
  scale_x_chron(format = "%H:%M") +
  theme_avif() +
  labs(
    title = "ACOUSTIC COMPLEXITY INDEX",
    subtitle = "August 2021",
    x = "",
    y = "ACI",
    caption = "Tristan Louth-Robins, AudioMoth 1.2.0."
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~ week, ncol = 1)


ggplot(mf_avif.dataF, aes(time, BI)) +
  geom_point() +
  ylim(0, 10) +
  scale_x_chron(format = "%H:%M") +
  theme_avif() +
  labs(
    title = "BIOACOUSTIC INDEX",
    subtitle = "August 2021",
    x = "",
    y = "BI",
    caption = "Tristan Louth-Robins, AudioMoth 1.2.0."
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~ date)

###################################

# View facet with BI and ACI combined:

bi.aci.plot <-ggplot(mf_avif.dataF, aes(time, BI)) +
        geom_point(aes(colour = ACI)) +
        ylim(0, 10) +
        scale_x_chron(format = "%H:%M") +
        theme_avif() +
        labs(
        title = "BIOACOUSTIC + ACOUSTIC COMPLEXITY INDEX",
        subtitle = "August 2021",
        x = "",
        y = "BI",
        caption = "Tristan Louth-Robins, AudioMoth 1.2.0."
      ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_wrap(~ week, ncol = 1)

bi.aci.plot + scale_color_gradient(low="blue", high="red")

#############################################

# Boxplot analysis

bi.boxplot <-ggplot(mf_avif.dataF, aes(week, BI)) +
  geom_boxplot() +
  theme_avif() +
  labs(
    title = "BIOACOUSTIC INDEX",
    subtitle = "August 2021",
    x = "",
    y = "BI",
    caption = "Tristan Louth-Robins, AudioMoth 1.2.0."
  ) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) 

bi.boxplot

##

aci.boxplot <-ggplot(mf_avif.dataF, aes(week, ACI)) +
  geom_boxplot() +
  theme_avif() +
  labs(
    title = "ACOUSTIC COMPLEXITY INDEX",
    subtitle = "August 2021",
    x = "",
    y = "ACI",
    caption = "Tristan Louth-Robins, AudioMoth 1.2.0."
  ) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) 

aci.boxplot
  
#############################################
####EXPERIMENTAL SECTION - NOT FOR BLOG######
#############################################

plot <- function(df, index, stats, title, dates) {
  ggplot2::ggplot(df, aes(x = time, y = {{ index }})) +
    geom_point()
  
  p + scale_x_chron(format = "%H:%M") +
    geom_hline(data = {{ stats }}, aes(yintercept = mean), colour = "blue", size = 1, alpha = 0.5) +
    theme_avif() +
    labs(
      title = title,
      subtitle = dates,
      x = "Time",
      y = index,
      caption = "Tristan Louth-Robins, AudioMoth 1.2.0."
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_wrap(~ date)
}

bi.plot <- plot(mf_avif.data, "BI", daily.stats.bi, "Bioacoustic Index", "August 2021")
bi.plot

aci.plot <- plot(mf_avif.data, "ACI", daily.stats.aci, "Bioacoustic Index", "August 2021")
aci.plot
