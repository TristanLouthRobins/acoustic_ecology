library(rlang)
library(gghighlight)
library(ggplot2)
library(ggbeeswarm)

# EDA script
# Tristan Louth-Robins. 2021-2022

data <- read_csv("/Users/tristanlouth-robins/data_science/acoustic_ecology_tests/results/parkside--merged.csv")

# SUMMARY STATISTICS

# Summary of daily stats

ndsi <- data %>% 
  group_by(date) %>% 
  summarise(ndsi.mean = round(mean(NDSI, rm.na = TRUE), 4))

ggplot(ndsi, aes(x = date, y = ndsi.mean)) +
  geom_line() +
  ylim(-1, 1)

###
data.pivot <- data %>% 
  pivot_longer(NDSI:ACI, names_to = "index_type")



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