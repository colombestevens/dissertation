# Data analysis
# Colombe Stevens
# 01.03.2022

# Workflow ----

# Library ----
library(tidyverse)
library(ggplot2)
library(lme4)

# Importing data ----
data <- read.csv("Tidy_data.csv")

# Transforming data ----
# logging data
data <- mutate(data, Cover_log = log(Percentage_cover))
hist(data$Cover_log)
(log_hist <- ggplot(data = data, aes(x = Cover_log)) +
      geom_histogram(binwidth = 1))

data <- mutate(data, Cover_log2 = log(Percentage_cover + 1))
hist(data$Cover_log2)
(log_hist2 <- ggplot(data = data, aes(x = Cover_log2)) +
      geom_histogram(binwidth = 1))

# square root
data <- mutate(data, Cover_sqrt = sqrt(Percentage_cover))
hist(data$Cover_sqrt)

# TEMPERATURE CHANGE OVER TIME ----

# Checking T data distribution
#(temp_hist <- ggplot(Mean_temps, aes(x = Mean_temp)) +
# geom_histogram() +
#  facet_wrap(~ Site, scales = "free"))

# Plotting temperature over time per site
(temp_scatter <- ggplot(data, aes(x = Year, y = Mean_temp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Site, scales = "free_y") +
  theme_bw() +
  theme(panel.grid = element_blank()))

# Plotting temperature over time per region
(temp_scatter <- ggplot(data, aes(x = Year, y = Mean_temp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~Region, scales = "free_y") +
    theme_bw() +
    theme(panel.grid = element_blank())) # maybe should calculate T mean beforehand?

# model
temp_time <- lm(Mean_temp ~ Year + Region, data = data)
summary(temp_time) # significant (overall p + West), Adj-R2 = 0.547

# RQ1: HOW DOES VEGETATION COVER DIFFER BETWEEN THE 2 REGIONS? ----




# RQ2: VEGETATION COVER CHANGE OVER TIME ----
# Data distribution ----
(veg_hist <- ggplot(data, aes(x = Percentage_cover)) +
   geom_histogram(binwidth = 10) )
   #facet_wrap(~ Site, scales = "free"))

# Modelling brainstorm ----
# linear model
veg_time <- lm(Percentage_cover ~ Year, data = data)
summary(veg_time)

veg_resids <- resid(veg_time)
hist(veg_resids)
shapiro.test(veg_resids) # not normally distributed

# linear mixed model
veg_mixed <- lmer(Percentage_cover ~ Year + Region + (1|Site), data = data)
summary(veg_mixed)

plot(veg_mixed)
qqnorm(resid(veg_mixed))
qqline(resid(veg_mixed))

# log linear model
veg_log <- lm(Cover_log ~ Year, data = data)

# ancova
# without interaction
veg_ancova <- lm(Percentage_cover ~ Year + Region, data = data)
summary(veg_ancova) # not significant, Adj-R2 = 0.037

# with interaction
veg_ancova_interaction <- lm(Percentage_cover ~ Year*Region, data = data)
summary(veg_ancova_interaction) # significant, Adj-R2 = 0.172
plot(veg_ancova_interaction)
interaction_resids <- resid(veg_ancova_interaction)
shapiro.test(interaction_resids) # not normally distributed

# log-ancova: time and region
# without interaction
veg_ancova_log <- lm(Cover_log2 ~ Year + Region, data = data)
summary(veg_ancova_log) # significant, adj-R2 = 0.119

# with interaction
veg_ancova_log_interaction <- lm(Cover_log2 ~ Year*Region, data = data)
summary(veg_ancova_log_interaction) # highly significant, adj-R2 = 0.453
veg_interaction_resids <- resid(veg_ancova_log_interaction)
shapiro.test(veg_interaction_resids) # NORMALLY DISTRIBUTED BABY
bartlett.test(Cover_log2 ~ Year*Region, data = data)
plot(veg_ancova_log_interaction)

# log-ancova: time and site
# without interaction
veg_site_log <- lm(Cover_log2 ~ Year + Site, data = data)
summary(veg_site_log) # not significant overall (but signi for Casey and KGI), adj-R2 = 0.189

# with interaction
veg_site_log_interaction <- lm(Cover_log2 ~ Year*Site, data = data)
summary(veg_site_log_interaction) # highly significant overall, adj-R2 = 0.624
veg_site_log_resids <- resid(veg_site_log_interaction)
shapiro.test(veg_site_log_resids) # normally distributed

# Plotting data ----
# plotting data per site
(veg_scatter <- ggplot(data, aes(x = Year, y = Percentage_cover)) +
   geom_point() +
   geom_smooth(method = "lm") +
   facet_wrap(~Site, scales = "free_y") +
   theme_bw() +
   theme(panel.grid = (element_blank())))

# plotting data per region
(veg_region_scatter <- ggplot(data, aes(x = Year, y = Percentage_cover)) +
      geom_point() +
      geom_smooth(method = "lm") +
      facet_wrap(~Region, scales = "free_y") +
      theme_bw() +
      theme(panel.grid = (element_blank())))

# plotting log data per region
(veg_region_scatter_log <- ggplot(data, aes(x = Year, y = Cover_log2)) +
      geom_point() +
      geom_smooth(method = "lm") +
      facet_wrap(~Region, scales = "free_y") +
      theme_bw() +
      theme(panel.grid = (element_blank())))

# RQ3: VEGETATION COVER CHANGE IN RESPONSE TO TEMPERATURE ----

# Model brainstorm ---- 

# ancova
# without interaction
veg_temp <- lm(Percentage_cover ~ Mean_temp + Region, data = data)
summary(veg_temp) # not significant, adj-R2 = -0.015

# with intercation
veg_temp_interaction <- lm(Percentage_cover ~ Mean_temp*Region, data = data)
summary(veg_temp_interaction) # not significant overall (p = 0.051), interaction term significant, adj-R2 = 0.107

# log-ancova: temp and region
# without interaction
veg_temp_log <- lm(Cover_log2 ~ Mean_temp + Region, data = data)
summary(veg_temp_log) # significant, adj-R2 = 0.1
shapiro.test(veg_temp_log)

# with interaction
veg_temp_log_interaction <- lm(Cover_log2 ~ Mean_temp*Region, data = data)
summary(veg_temp_log_interaction) # significant, adj-R2 = 0.138
veg_temp_interaction_resids <- resid(veg_temp_log_interaction)
shapiro.test(veg_temp_interaction_resids) # NORMALLY DISTRIBUTED
bartlett.test(Cover_log2 ~ Mean_temp * Region, data = data)

# log-ancova: temp and site
# without interaction
veg_temp_site_log <- lm(Cover_log2 ~ Mean_temp + Site, data = data)
summary(veg_temp_site_log) # significant, adj-R2 = 0.199
veg_temp_site_resids <- resid(veg_temp_site_log)
shapiro.test(veg_temp_site_resids) # not normally distributed

# with interaction
veg_temp_site_interaction_log <- lm(Cover_log2 ~ Mean_temp*Site, data = data)
summary(veg_temp_site_interaction_log) # not significant, adj-R2 = 0.083

# sqrt ancova
# without interaction
veg_temp_sqrt <- lm(Cover_sqrt ~ Mean_temp + Region, data = data)
summary(veg_temp_sqrt) # not significant overall (but signi for E), adj-R2 = 0.055

# with interaction
veg_temp_sqrt_interaction <- lm(Cover_sqrt ~ Mean_temp*Region, data = data)
summary(veg_temp_sqrt_interaction) # significant, adj-R2 = 0.119

# Plotting data ----
(veg_temp <- ggplot(data, aes(x = Mean_temp, y = Percentage_cover)) +
   geom_point() +
   geom_smooth(method = "lm") +
   facet_wrap(~Region, scales = "free") +
   theme_bw() +
   theme(panel.grid = element_blank()))




