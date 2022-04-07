# Final stats analysis
# Colombe Stevens
# 02.04.2022

# Libraries ----
library(tidyverse)
library(ggplot2)
library(lme4)

# Importing data ----
data <- read.csv("Tidy_summer_data.csv")

# Transforming data ----
data <- na.omit(data) %>% 
  filter(Percentage_cover != 0.00)

# RQ1: VEGETATION COVER CHANGE OVER TIME ----

# Model comparison
veg_reduced_simple <- lm(Percentage_cover ~ Year + Region, data = reduced_data)
veg_reduced_s <- lmer(Percentage_cover ~ Year + Region + (1|Site), data = reduced_data)
veg_reduced_s_interaction <- lmer(Percentage_cover ~ Year * Region + (1|Site), data = reduced_data)
veg_reduced_s_nested <- lmer(Percentage_cover ~ Year + Region + (1|Region/Site), data = reduced_data)
veg_reduced_t <- lmer(Percentage_cover ~ Year + Region + (1|Year), data = reduced_data)
veg_reduced_t_interaction <- lmer(Percentage_cover ~ Year * Region + (1|Year), data = reduced_data)
veg_reduced_s_t <- lmer(Percentage_cover ~ Year + Region + (1|Year) + (1|Site), data = reduced_data)
veg_reduced_s_t_interaction <- lmer(Percentage_cover ~ Year * Region + (1|Year) + (1|Site), data = reduced_data)
veg_reduced_interaction <- lm(Percentage_cover ~ Year * Region, data = reduced_data)
veg_reduced_null <- lm(Percentage_cover ~ 1, data = reduced_data)

AICc(veg_reduced_simple, veg_reduced_s, veg_reduced_s_interaction, veg_reduced_s_nested, veg_reduced_t, veg_reduced_t_interaction, veg_reduced_s_t, veg_reduced_s_t_interaction, veg_reduced_interaction, veg_reduced_null)






