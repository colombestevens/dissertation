# Data analysis
# Colombe Stevens
# 01.03.2022

# Workflow ----

# Library ----
library(tidyverse)
library(ggplot2)
library(lme4)
library(stargazer)
library(MuMIn)

# Importing data ----
data <- read.csv("Tidy_data.csv")

# Transforming data ----
# logging data
data <- na.omit(data)
data$Region <- as.factor(as.character(data$Region))
data_west <- filter(data, Region == "West")
data_east <- filter(data, Region == "East")

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

# averaging T data
t_mean <- data %>% 
   na.omit() %>% 
   group_by(Region, Year) %>% 
   summarise(Mean_temp = mean(Mean_temp))

# TEMPERATURE CHANGE OVER TIME ----

# All T data ----
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

# per region
# west
hist(data_west$Mean_temp)
(temp_W_scatter <- ggplot(data_west, aes(x = Year, y = Mean_temp)) +
   geom_point() +
   geom_smooth(method = "lm"))
west_temp <- lm(Mean_temp ~ Year, data = data_west)
summary(west_temp)

# east
hist(data_east$Mean_temp)
(temp_E_scatter <- ggplot(data_east, aes(x = Year, y = Mean_temp)) +
      geom_point() +
      geom_smooth(method = "lm"))
east_temp <- lm(Mean_temp ~ Year, data = data_east)
summary(east_temp)

# Mean T data ----
# Plotting data
(t_mean_scatter <- ggplot(t_mean, aes(x = Year, y = Mean_temp)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~Region, scales = "free_y") +
    theme_bw() +
    theme(panel.grid = element_blank()))

# model
Mean_t_ancova <- lm(Mean_temp ~ Year + Region, data = t_mean)
summary(Mean_t_ancova) # significant (overall + West), adj-R2 = 0.7707


# RQ1: HOW DOES VEGETATION COVER DIFFER BETWEEN THE 2 REGIONS? ----
# could average out present vegetaion cover in each region do box plot??



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

# LINEAR MIXED MODEL
veg_mixed_interaction <- lmer(Cover_log2 ~ Year*Region + (1|Site), data = data, REML = FALSE)
summary(veg_mixed_interaction) # Site explains ~54% of leftover variance after the variance explained by fixed effect

veg_mixed_null <- lmer(Cover_log2 ~ 1 + (1|Site), data = data, REML = FALSE)
veg_mixed <- lmer(Cover_log2 ~ Year + Region + (1|Site), data = data, REML = FALSE)
veg_mixed_simple <- lm(Cover_log2 ~ Year*Region, data = data, REML = FALSE)
veg_mixed_crossed <- lmer(Cover_log2 ~ Year*Region + (1|Region/Site), data = data, REML = FALSE)
veg_mixed_random <- lmer(Cover_log2 ~ Year*Region + (Region|Site), data = data, REML = FALSE)

AIC(veg_mixed, veg_mixed_interaction, veg_mixed_null, veg_mixed_simple, veg_mixed_crossed, veg_mixed_random)
AICc(veg_mixed, veg_mixed_interaction, veg_mixed_null, veg_mixed_simple, veg_mixed_crossed, veg_mixed_random)
# should use intercation as there's barely any difference between null and no interaction
# AIC of random slope model is way higher than all the others when using interaction
# HOWEVER, AIC of random slope is the lowest when only using Region
# AIC and AICc of intercation & random effect is lower than of interaction w/o random effect and of interaction w/ nested

r.squaredGLMM(veg_mixed_interaction) # R2m = 0.306; R2c = 0.685, i.e. including random effect increases explanatory power
r.squaredGLMM(veg_mixed_null)

par(mfrow=c(1,2))
plot(data$Cover_log2,fitted(veg_mixed_simple))
abline(0,1)
plot(data$Cover_log2,fitted(veg_mixed_interaction))
abline(0,1)

stargazer(veg_mixed_interaction, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# checking assumptions
plot(veg_mixed_interaction) # resid vs fitted

qqnorm(resid(veg_mixed_interaction))
qqline(resid(veg_mixed_interaction)) # really good normal Q-Q plot

plot(veg_mixed_interaction,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) # scale-location not so good

plot(veg_mixed_interaction, rstudent(.) ~ hatvalues(.)) # resids vs leverage

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
shapiro.test(veg_interaction_resids) # not.... NORMALLY DISTRIBUTED BABY
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
bartlett.test(Cover_log2 ~ Mean_temp*Region, data = data)
par(mfrow=c(1,1))
plot(veg_temp_log_interaction)

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

# LINEAR MIXED MODEL
veg_temp_mixed_interaction <- lmer(Cover_log2 ~ Mean_temp*Region + (1|Site), data = data, REML = FALSE)
summary(veg_temp_mixed_interaction) # Site explains ~54% of leftover variance after the variance explained by fixed effect

veg_temp_mixed <- lmer(Cover_log2 ~ Mean_temp + Region + (1|Site), data = data, REML = FALSE)
summary(veg_temp_mixed)


# Plotting data ----
(veg_temp <- ggplot(data, aes(x = Mean_temp, y = Percentage_cover)) +
   geom_point() +
   geom_smooth(method = "lm") +
   facet_wrap(~Region, scales = "free") +
   theme_bw() +
   theme(panel.grid = element_blank()))




