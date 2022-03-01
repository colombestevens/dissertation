# Data analysis
# Colombe Stevens
# 01.03.2022

# Workflow ----

# Library ----
library(tidyverse)
library(ggplot2)

# CHANGE DATA SET IN FIRST SECTIONS 

# Importing data ----
Mean_temps <- read.csv("Mean_temperatures.csv")
Vegetation <- read.csv("Vegetation_cover.csv")
data <- read.csv("Tidy_data.csv")

# TEMPERATURE CHANGE OVER TIME ----

# Checking T data distribution
#(temp_hist <- ggplot(Mean_temps, aes(x = Mean_temp)) +
# geom_histogram() +
#  facet_wrap(~ Site, scales = "free"))

# Plotting temperature over time per site
(temp_scatter <- ggplot(Mean_temps, aes(x = Year, y = Mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Site, scales = "free_y") +
  theme_bw() +
  theme(panel.grid = element_blank()))

# Plotting temperature over time per region
(temp_scatter <- ggplot(Mean_temps, aes(x = Year, y = Mean_temp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~Region, scales = "free_y") +
    theme_bw() +
    theme(panel.grid = element_blank())) # maybe should calculate T mean beforehand?

# VEGETATION COVER CHANGE OVER TIME ----
(veg_scatter <- ggplot(Vegetation, aes(x = Year, y = Percentage_cover)) +
   geom_point() +
   geom_smooth(method = "lm") +
   facet_wrap(~Site, scales = "free_y") +
   theme_bw() +
   theme(panel.grid = (element_blank())))

# VEGETATION COVER CHANGE IN RESPONSE TO TEMPERATURE ----
(veg_temp <- ggplot(data, aes(x = Mean_temp, y = Percentage_cover)) +
   geom_point() +
   geom_smooth(method = "lm") +
   facet_wrap(~Site, scales = "free") +
   theme_bw() +
   theme(panel.grid = element_blank()))




