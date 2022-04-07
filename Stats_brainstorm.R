# Stats brainstorm
# Colombe Stevens
# 01.03.2022

# Workflow ----

# Library ----
library(tidyverse)
library(ggplot2)
library(lme4)
library(stargazer)
library(MuMIn)
library(corrplot)

# Importing data ----
data <- read.csv("Tidy_summer_data.csv")

# Transforming data ----
# logging data
data <- na.omit(data)
data <- mutate(data, Cover_log2 = log(Percentage_cover + 1))

data_west <- filter(data, Region == "West")
data_east <- filter(data, Region == "East")

scaled_data <- data %>% 
   mutate(scaled_cover = scale(data$Percentage_cover, center = TRUE, scale = TRUE),
          scaled_log = scale(data$Cover_log2, center = TRUE, scale = TRUE),
          proportion_cover = Percentage_cover/100, 
          site_nb = case_when(
             grepl("Cape_Hallet", Site) ~ "1",
             grepl("Casey", Site) ~ "2",
             grepl("MZB", Site) ~ "3",
             grepl("KGI", Site) ~ "4",
             grepl("Anch_Isl", Site) ~ "5",
             grepl("Lag_Isl", Site) ~ "6"
          )) 

hist(data$Cover_log2)
(log_hist2 <- ggplot(data = data, aes(x = Cover_log2)) +
      geom_histogram(binwidth = 1))

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
summary(temp_time) # significant (overall p + West), Adj-R2 = 0.655

temp_time_resids <- resid(temp_time)
shapiro.test(temp_time_resids) # NORMALLY DISTRIBUTED
bartlett.test(Mean_temp ~ Year + Region, data = data)
bartlett.test(temp_time)
par(mfrow=c(1,1))
plot(temp_time)

# per region
# west
hist(data_west$Mean_temp)
(temp_W_scatter <- ggplot(data_west, aes(x = Year, y = Mean_temp)) +
   geom_point() +
   geom_smooth(method = "lm"))
west_temp <- lm(Mean_temp ~ Year, data = data_west)
summary(west_temp) # not significant, adj-R2 = 0.004

# east
hist(data_east$Mean_temp)
(temp_E_scatter <- ggplot(data_east, aes(x = Year, y = Mean_temp)) +
      geom_point() +
      geom_smooth(method = "lm"))
east_temp <- lm(Mean_temp ~ Year, data = data_east)
summary(east_temp) # not significant, adj-R2 = -0.052

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
# cancelling this RQ


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
# Model selection
veg_mixed_interaction <- lmer(Cover_log2 ~ Year*Region + (1|Site), data = data, REML = FALSE)
summary(veg_mixed_interaction) # Site explains ~54% of leftover variance after the variance explained by fixed effect (without KGI99)
# Site expains 0% residual variation with KGI99

data <- filter(data, Year != "1999") # substracting KGI99

veg_scaled_interaction <- lmer(scaled_cover ~ Year*Region + (1|Site), data = scaled_data, REML = FALSE)
summary(veg_scaled_interaction) # Site explains 21.3% residual variation with scaled_cover
veg_scaled_log_interaction <- lmer(scaled_log ~ Year*Region + (1|Site), data = scaled_data, REML = FALSE)
summary(veg_scaled_log_interaction) # Site explains 0% variation with scaled_log

# Model comparison: non-scaled data
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

# Model comparison: scaled-data
veg_scaled_null_s <- lmer(scaled_cover ~ 1 + (1|Site), data = scaled_data, REML = FALSE)
veg_scaled_null_t <- lmer(scaled_cover ~ 1 + (1|Year), data = scaled_data, REML = FALSE)
veg_scaled_null_t_s <- lmer(scaled_cover ~ 1 + (1|Year) + (1|Site), data = scaled_data, REML = FALSE)
veg_scaled_simple_interaction <- lm(scaled_cover ~ Year*Region, data = scaled_data, REML = FALSE)
veg_scaled_simple <- lm(scaled_cover ~ Year +Region, data = scaled_data, REML = FALSE)
summary(veg_scaled_simple_interaction)
AIC(veg_scaled_interaction, veg_scaled_null_s, veg_scaled_null_t, veg_scaled_null_t_s, veg_scaled_simple_interaction, veg_scaled_simple)
AICc(veg_scaled_interaction, veg_scaled_null_s, veg_scaled_null_t, veg_scaled_null_t_s, veg_scaled_simple_interaction, veg_scaled_simple)


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
qqline(resid(veg_mixed_interaction)) # good normal Q-Q plot

plot(veg_mixed_interaction,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1) # scale-location not so good

plot(veg_mixed_interaction, rstudent(.) ~ hatvalues(.)) # resids vs leverage

bartlett.test(veg_mixed_interaction)

# correlation analysis
data_correlation_w <- data_west %>% 
   select(-Region) %>% 
   select(-Site) %>% 
   select(-Year)

corrplot(cor(data_correlation_w))

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
summary(veg_ancova_log) # significant, adj-R2 = 0.113

# with interaction
veg_ancova_log_interaction <- lm(Cover_log2 ~ Year*Region, data = data)
summary(veg_ancova_log_interaction) # highly significant, adj-R2 = 0.357
veg_interaction_resids <- resid(veg_ancova_log_interaction)
shapiro.test(veg_interaction_resids) # NORMALLY DISTRIBUTED
bartlett.test(veg_mixed_interaction)
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

# GENERALISED LINEAR MODEL
# Model comparison
veg_glm_simple <- glm(Percentage_cover ~ Year + Region, data = scaled_data, family = binomial)
veg_glm_scaled_simple <- glm(scaled_cover ~ Year + Region, data = scaled_data)
veg_glm_proport_simple <- glm(proportion_cover ~ Year + Region, data = scaled_data, family = binomial)
summary(veg_glm_proport_simple)
veg_glm_scaled_interaction <- glm(scaled_cover ~ Year*Region, data = scaled_data, family = binomial)
summary(veg_glm_scaled_interaction)
veg_glm_scaled_null <- glm(scaled_cover ~ 1, data = scaled_data, family = binomial)
veg_glm_scaled_time <- glm(scaled_cover ~ Year, data = scaled_data)
veg_glm_scaled_region <- glm(scaled_cover ~ Region, data = scaled_data)
veg_glm_scaled_interaction_s <- glmer(scaled_cover ~ Year*Region + (1|Site), data = scaled_data)
veg_glm_scaled_interaction_t <- glmer(scaled_cover ~ Year*Region + (1|Year), data = scaled_data)
veg_glm_scaled_interaction_t_s <- glmer(scaled_cover ~ Year*Region + (1|Year) + (1|Site), data = scaled_data)

AIC(veg_glm_simple, veg_glm_scaled_simple, veg_glm_scaled_interaction, veg_glm_scaled_null, veg_glm_scaled_time, veg_glm_scaled_region, veg_glm_scaled_interaction_t, veg_glm_scaled_interaction_s, veg_glm_scaled_interaction_t_s)
AICc(veg_glm_simple, veg_glm_scaled_simple, veg_glm_scaled_interaction, veg_glm_scaled_null, veg_glm_scaled_time, veg_glm_scaled_region, veg_glm_scaled_interaction_t, veg_glm_scaled_interaction_s, veg_glm_scaled_interaction_t_s)

# AICc for simple model with normal data is 415 and for scaled data is 139, i.e. should use scaled data
# AICc for scaled model without intercation is 139 and for scaled with interaction is 136, i.e. should use interaction
# AICc of null model equivalent to interaction model, i.e. no influence of Region and Year...

# Model comparison: proportion data
veg_glm_proport_simple <- glm(proportion_cover ~ Year + Region, data = scaled_data, family = binomial)
veg_glm_proport_interaction <- glm(proportion_cover ~ Year*Region, data = scaled_data, family = binomial)
veg_glm_proport_null <- glm(proportion_cover ~ 1, data = scaled_data, family = binomial)
veg_glm_proport_t <- glmer(proportion_cover ~ Year + Region + (1|Year), data = scaled_data, family = binomial, nAGQ = 0)
veg_glm_proport_s <- glmer(proportion_cover ~ Year + Region + (1|Site), data = scaled_data, family = binomial)
veg_glm_proport_s_interaction <- glmer(proportion_cover ~ Year*Region + (1|Site), data = scaled_data, family = binomial)
veg_glm_proport_s_t <- glmer(proportion_cover ~ Year + Region + (1|Site) + (1|Year), data = scaled_data, family = binomial)
veg_glm_proport_s_t_interaction <- glmer(proportion_cover ~ Year*Region + (1|Site) + (1|Year), data = scaled_data, family = binomial, nAGQ = 0)
veg_glm_proport_s_random <- glmer(proportion_cover ~ Year + Region + (Region|Site), data = scaled_data, family = binomial)

AIC(veg_glm_proport_simple, veg_glm_proport_interaction, veg_glm_proport_null, veg_glm_proport_s, veg_glm_proport_s_t, veg_glm_proport_t, veg_glm_proport_s_t_interaction)
AICc(veg_glm_proport_simple, veg_glm_proport_interaction, veg_glm_proport_null, veg_glm_proport_s, veg_glm_proport_s_t, veg_glm_proport_t, veg_glm_proport_s_t_interaction, veg_glm_proport_s_random)

summary(veg_glm_proport_s)
stargazer(veg_glm_proport_s, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# AICc for interaction is higher (32) than without interaction (28), i.e. shouldn't use interaction
# try nAGQ = 0 to fix non-convergence in Year model

# checking validation 
plot(veg_glm_proport_s)
plot(proportion_cover ~ Year + Region + (1|Site), data = scaled_data, family = binomial)
points(scaled_data$Year, fitted(veg_glm_proport_s))

# Plotting data ----
# LOOK INTO ggeffects
# plotting data per site
(veg_scatter <- ggplot(data, aes(x = Year, y = Cover_log2)) +
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
summary(veg_temp) # not significant, adj-R2 = 0.011

# with intercation
veg_temp_interaction <- lm(Percentage_cover ~ Mean_temp*Region, data = data)
summary(veg_temp_interaction) # not significant overall, interaction term significant, adj-R2 = -0.003

# log-ancova: temp and region
# without interaction
veg_temp_log <- lm(Cover_log2 ~ Mean_temp + Region, data = data)
summary(veg_temp_log) # significant (overall p + East), adj-R2 = 0.1028
shapiro.test(veg_temp_log) # doesn't work
bartlett.test(Cover_log2 ~ Mean_temp + Region, data = data)
plot(veg_temp_log)

# with interaction
veg_temp_log_interaction <- lm(Cover_log2 ~ Mean_temp*Region, data = data)
summary(veg_temp_log_interaction) # not significant, adj-R2 = 0.086
veg_temp_interaction_resids <- resid(veg_temp_log_interaction)
shapiro.test(veg_temp_interaction_resids) # NORMALLY DISTRIBUTED (not if include KGI99)
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
summary(veg_temp_mixed_interaction) # LMM doesn't explain more if include KGI99

veg_temp_mixed <- lmer(Cover_log2 ~ Mean_temp + Region + (1|Site), data = data, REML = FALSE)
summary(veg_temp_mixed)

# GENERALISED LINEAR MODEL
# Model comparison: scaled

veg_temp_glm_simple <- glm(Percentage_cover ~ Mean_temp + Region, data = scaled_data)
veg_temp_glm_scaled_simple <- glm(scaled_cover ~ Mean_temp + Region, data = scaled_data)
veg_temp_glm_scaled_interaction <- glm(scaled_cover ~ Mean_temp*Region, data = scaled_data)
veg_temp_glm_scaled_null <- glm(scaled_cover ~ 1, data = scaled_data)

AIC(veg_temp_glm_simple, veg_temp_glm_scaled_simple, veg_temp_glm_scaled_interaction)
AICc(veg_temp_glm_simple, veg_temp_glm_scaled_simple, veg_temp_glm_scaled_interaction)

# AICc of scaled much lower (141) than non scaled (415), i.e. should use scaled data

# Model comparison: proportion data
veg_temp_glm_proport_simple <- glm(proportion_cover ~ Mean_temp + Region, data = scaled_data, family = binomial)
veg_temp_glm_proport_interaction <- glm(proportion_cover ~ Mean_temp * Region, data = scaled_data, family = binomial)
veg_temp_glm_proport_null <- glm(proportion_cover ~ 1, data = scaled_data, family = binomial)
veg_temp_glm_proport_s <- glmer(proportion_cover ~ Mean_temp + Region + (1|Site), data = scaled_data, family = binomial)
veg_temp_glm_proport_s_random <- glmer(proportion_cover ~ Mean_temp + Region + (Region|Site), data = scaled_data, family = binomial)
veg_temp_glm_proport_s_interaction <- glmer(proportion_cover ~ Mean_temp * Region + (1|Site), data = scaled_data, family = binomial)
veg_temp_glm_proport_t <- glmer(proportion_cover ~ Mean_temp + Region + (1|Year), data = scaled_data, family = binomial)
veg_temp_glm_proport_t_s <- glmer(proportion_cover ~ Mean_temp + Region + (1|Year) + (1|Site), data = scaled_data, family = binomial)
veg_temp_glm_proport_t_s_interaction <- glmer(proportion_cover ~ Mean_temp * Region + (1|Year) + (1|Site), data = scaled_data, family = binomial)


AIC(veg_temp_glm_proport_simple, veg_temp_glm_proport_interaction, veg_temp_glm_proport_null, veg_temp_glm_proport_s, veg_temp_glm_proport_t, veg_temp_glm_proport_t_s, veg_temp_glm_proport_t_s_interaction)
AICc(veg_temp_glm_proport_simple, veg_temp_glm_proport_interaction, veg_temp_glm_proport_null, veg_temp_glm_proport_s, veg_temp_glm_proport_t, veg_temp_glm_proport_t_s, veg_temp_glm_proport_t_s_interaction, veg_temp_glm_proport_s_random)

# Plotting data ----
(veg_temp <- ggplot(data, aes(x = Mean_temp, y = Percentage_cover)) +
   geom_point() +
   geom_smooth(method = "lm") +
   facet_wrap(~Region, scales = "free") +
   theme_bw() +
   theme(panel.grid = element_blank()))

### ***ANALYSIS WITH REDUCED DATA*** ----

# Reducing dataframe

reduced_data <- filter(data, Percentage_cover != 0.00)

## TEMPERATURE OVER TIME - ALL DATA

all_data <- read.csv("Tidy_summer_data.csv")
all_data <- all_data %>% 
   select(-Percentage_cover) %>% 
   na.omit()

temp_time_all <- lm(Mean_temp ~ Year + Region, data = all_data)
summary(temp_time_all)

(temp_scatter2 <- ggplot(all_data, aes(x = Year, y = Mean_temp, colour = Region)) +
      geom_point() +
      geom_smooth(method = "lm") +
      #facet_wrap(~Region, scales = "free_y") +
      theme_bw() +
      theme(panel.grid = element_blank()))

## RQ1: VEGETATION COVER CHANGE OVER TIME ----

# Checking distribution of data
hist(reduced_data$Percentage_cover)
hist(reduced_data$Cover_log2)
(veg_hist2 <- ggplot(reduced_data, aes(x = Cover_log2)) +
      geom_histogram(binwidth = 0.5) )

# Linear mixed model:
veg_reduced_simple <- lm(Percentage_cover ~ Year + Region, data = reduced_data)
summary(veg_reduced_simple)
veg_reduced_s <- lmer(Percentage_cover ~ Year + Region + (1|Site), data = reduced_data)
summary(veg_reduced_s) # Site explains tiny % (0.00000...1%) of residual variance
veg_reduced_s_interaction <- lmer(Percentage_cover ~ Year * Region + (1|Site), data = reduced_data)
summary(veg_reduced_s_interaction)
veg_reduced_s_nested <- lmer(Percentage_cover ~ Year + Region + (1|Region/Site), data = reduced_data)
veg_reduced_t <- lmer(Percentage_cover ~ Year + Region + (1|Year), data = reduced_data)
veg_reduced_t_interaction <- lmer(Percentage_cover ~ Year * Region + (1|Year), data = reduced_data)
summary(veg_reduced_t_interaction) # Year accounts for 14.66% of residual variance
veg_reduced_s_t <- lmer(Percentage_cover ~ Year + Region + (1|Year) + (1|Site), data = reduced_data)
summary(veg_reduced_s_t) # Site accounts for 0% of residual variance, Year for 12.60%
veg_reduced_s_t_interaction <- lmer(Percentage_cover ~ Year * Region + (1|Year) + (1|Site), data = reduced_data)
summary(veg_reduced_s_t_interaction) # Site accounts for 0% of residuel variance, Year for 14.66%
veg_reduced_interaction <- lm(Percentage_cover ~ Year * Region, data = reduced_data)
veg_reduced_null <- lm(Percentage_cover ~ 1, data = reduced_data)
veg_reduced_null_s <- lmer(Percentage_cover ~ 1 + (1|Site), data = reduced_data)
veg_reduced_null_t <- lmer(Percentage_cover ~ 1 + (1|Year), data = reduced_data)
veg_reduced_null_s_t <- lmer(Percentage_cover ~ 1 + (1|Year) + (1|Site), data = reduced_data)

AICc(veg_reduced_simple, veg_reduced_s, veg_reduced_s_interaction, veg_reduced_s_nested, veg_reduced_t, veg_reduced_t_interaction, veg_reduced_s_t, veg_reduced_s_t_interaction, veg_reduced_interaction, veg_reduced_null, veg_reduced_null_s, veg_reduced_null_t, veg_reduced_null_s_t)

r.squaredGLMM(veg_reduced_s_interaction)
r.squaredGLMM(veg_reduced_t_interaction)
r.squaredGLMM(veg_reduced_s_t_interaction) # R2m = 0.367; R2c = 0.460
r.squaredGLMM(veg_reduced_s_t) # R2m = 0.345; R2c = 0.427

# checking assumptions: Site as random effect
veg_reduced_s_interaction_resid <- resid(veg_reduced_s_interaction)
shapiro.test(veg_reduced_s_interaction_resid) # NORMALLY DISTRIBUTED
qqnorm(veg_reduced_s_interaction_resid)
qqline(veg_reduced_s_interaction_resid)
plot(veg_reduced_s_interaction)

# checking assumptions: Year as random effect
veg_reduced_t_interaction_resid <- resid(veg_reduced_t_interaction)
shapiro.test(veg_reduced_t_interaction_resid) # NORMALLY DISTRIBUTED
qqnorm(veg_reduced_t_interaction_resid)
qqline(veg_reduced_t_interaction_resid)
plot(veg_reduced_t_interaction)

# checking assumptions: Site + Year as random effects w/ interaction
veg_reduced_s_t_interaction_resid <- resid(veg_reduced_s_t_interaction)
shapiro.test(veg_reduced_s_t_interaction_resid) # NORMALLY DISTRIBUTED
qqnorm(veg_reduced_s_t_interaction_resid)
qqline(veg_reduced_s_t_interaction_resid)
plot(veg_reduced_s_t_interaction)

# checking assumptions: Site + Year as random effects w/o interaction
veg_reduced_s_t_resid <- resid(veg_reduced_s_t)
shapiro.test(veg_reduced_s_t_resid) # NORMALLY DISTRIBUTED
qqnorm(veg_reduced_s_t_resid)
qqline(veg_reduced_s_t_resid)
plot(veg_reduced_s_t)

# Linear mixed model: log data
veg_reduced_simple_log <- lm(Cover_log2 ~ Year + Region, data = reduced_data)
summary(veg_reduced_simple_log)
veg_reduced_s_log <- lmer(Cover_log2 ~ Year + Region + (1|Site), data = reduced_data)
summary(veg_reduced_s_log)
veg_reduced_t_log <- lmer(Cover_log2 ~ Year + Region + (1|Year), data = reduced_data)
veg_reduced_s_t_log <- lmer(Cover_log2 ~ Year + Region + (1|Year) + (1|Site), data = reduced_data)
veg_reduced_interaction_log <- lm(Cover_log2 ~ Year * Region, data = reduced_data)
veg_reduced_null_log <- lm(Cover_log2 ~ 1, data = reduced_data)

AICc(veg_reduced_simple_log, veg_reduced_s_log, veg_reduced_t_log, veg_reduced_s_t_log, veg_reduced_interaction_log, veg_reduced_null_log)

# checking assumptions
summary(veg_reduced_simple_log)
plot(veg_reduced_simple_log)
veg_reduced_simple_resid <- resid(veg_reduced_simple_log)
shapiro.test(veg_reduced_simple_resid) # normally distributed
bartlett.test(Cover_log2 ~ Year + Region, data = reduced_data) # doesn't work...
# do I just use AICcs to compare including a random effect? and if not then i just do an ancova and that's all?

## RQ2: VEGETATION COVER CHANGE IN RESPONSE TO TEMPERATURE ----

# LINEAR MIXED MODEL: non-transformed data
veg_temp_reduced_simple <- lm(Percentage_cover ~ Mean_temp + Region, data = reduced_data)
summary(veg_temp_reduced_simple)
veg_temp_reduced_s <- lmer(Percentage_cover ~ Mean_temp + Region + (1|Site), data = reduced_data)
summary(veg_temp_reduced_s)
veg_temp_reduced_s_nested <- lmer(Percentage_cover ~ Mean_temp + Region + (1|Region/Site), data = reduced_data)
veg_temp_reduced_s_interaction <- lmer(Percentage_cover ~ Mean_temp * Region + (1|Site), data = reduced_data)
summary(veg_temp_reduced_s_interaction)
veg_temp_reduced_t <- lmer(Percentage_cover ~ Mean_temp + Region + (1|Year), data = reduced_data)
summary(veg_temp_reduced_t)
veg_temp_reduced_t_interaction <- lmer(Percentage_cover ~ Mean_temp * Region + (1|Year), data = reduced_data)
summary(veg_temp_reduced_t_interaction)
veg_temp_reduced_s_t <- lmer(Percentage_cover ~ Mean_temp + Region + (1|Site) + (1|Year), data = reduced_data)
veg_temp_reduced_s_t_interaction <- lmer(Percentage_cover ~ Mean_temp * Region + (1|Site) + (1|Year), data = reduced_data)
summary(veg_temp_reduced_s_t_interaction)
veg_temp_reduced_interaction <- lm(Percentage_cover ~ Mean_temp * Region, data = reduced_data)
veg_temp_reduced_null_lm <- lm(Percentage_cover ~ 1, data = reduced_data)
veg_temp_reduced_null_s <- lmer(Percentage_cover ~ 1 + (1|Site), data = reduced_data)
veg_temp_reduced_null_t <- lmer(Percentage_cover ~ 1 + (1|Year), data = reduced_data)
veg_temp_reduced_null_s_t <- lmer(Percentage_cover ~ 1 + (1|Site) + (1|Year), data = reduced_data)

AIC(veg_temp_reduced_simple, veg_temp_reduced_s, veg_temp_reduced_t, veg_temp_reduced_s_t, veg_temp_reduced_interaction, veg_temp_reduced_null_lm, veg_temp_reduced_null_s, veg_temp_reduced_null_t, veg_temp_reduced_null_s_t, veg_temp_reduced_s_nested, veg_temp_reduced_s_interaction, veg_temp_reduced_t_interaction, veg_temp_reduced_s_t_interaction)
AICc(veg_temp_reduced_simple, veg_temp_reduced_s, veg_temp_reduced_t, veg_temp_reduced_s_t, veg_temp_reduced_interaction, veg_temp_reduced_null_lm, veg_temp_reduced_null_s, veg_temp_reduced_null_t, veg_temp_reduced_null_s_t, veg_temp_reduced_s_nested, veg_temp_reduced_s_interaction, veg_temp_reduced_t_interaction, veg_temp_reduced_s_t_interaction)

# interaction allows for Regions to differ in how percentage cover changes in response to temperature

# R2m and R2c
r.squaredGLMM(veg_temp_reduced_s_interaction) # R2m = 0.196; R2C = 0.466
r.squaredGLMM(veg_temp_reduced_t_interaction) # R2m = 0.249; R2c = 0.327
r.squaredGLMM(veg_temp_reduced_s_t_interaction) # R2m = 0.196; R2c = 0.466

# Residual variance explained by random effect
106/(106+209) # Site explains 33.65% of residual variance
27.46/(27.46+235.91) # Year explains 10.43% of residual variance
2.369e-07/(2.369e-07+1.060e+02+2.095e+02) # with T + S, Year explains a tiny % of residual variance
1.060e+02/(2.369e-07+1.060e+02+2.095e+02) # with T + S, Site explains 33.60% of residual variance

# checking assumptions: Site as random effect
veg_temp_reduced_s_interaction_resid <- resid(veg_temp_reduced_s_interaction)
shapiro.test(veg_temp_reduced_s_interaction_resid) # NORMALLY DISTRIBUTED
qqnorm(veg_temp_reduced_s_interaction_resid)
qqline(veg_temp_reduced_s_interaction_resid)
plot(veg_temp_reduced_s_interaction)

# checking assumptions: Year as random effect
veg_temp_reduced_t_interaction_resid <- resid(veg_temp_reduced_t_interaction)
shapiro.test(veg_temp_reduced_t_interaction_resid) # NORMALLY DISTRIBUTED
qqnorm(veg_temp_reduced_t_interaction_resid)
qqline(veg_temp_reduced_t_interaction_resid)
plot(veg_temp_reduced_t_interaction)

# checking assumptions: Site + Year as random effects
veg_temp_reduced_s_t_interaction_resid <- resid(veg_temp_reduced_s_t_interaction)
shapiro.test(veg_temp_reduced_s_t_interaction_resid) # NORMALLY DISTRIBUTED
qqnorm(veg_temp_reduced_s_t_interaction_resid)
qqline(veg_temp_reduced_s_t_interaction_resid)
plot(veg_temp_reduced_s_t_interaction)

# LINEAR MIXED MODEL: log data
veg_temp_reduced_simple_log <- lm(Cover_log2 ~ Mean_temp + Region, data = reduced_data)
summary(veg_temp_reduced_simple_log)
veg_temp_reduced_s_log <- lmer(Cover_log2 ~ Mean_temp + Region + (1|Site), data = reduced_data)
summary(veg_temp_reduced_s_log)
veg_temp_reduced_s_log_nested <- lmer(Cover_log2 ~ Mean_temp + Region + (1|Region/Site), data = reduced_data)
veg_temp_reduced_t_log <- lmer(Cover_log2 ~ Mean_temp + Region + (1|Year), data = reduced_data)
veg_temp_reduced_s_t_log <- lmer(Cover_log2 ~ Mean_temp + Region + (1|Site) + (1|Year), data = reduced_data)
veg_temp_reduced_interaction_log <- lm(Cover_log2 ~ Mean_temp * Region, data = reduced_data)
veg_temp_reduced_null_log <- lm(Cover_log2 ~ 1, data = reduced_data)

AIC(veg_temp_reduced_simple_log, veg_temp_reduced_s_log, veg_temp_reduced_t_log, veg_temp_reduced_s_t_log, veg_temp_reduced_interaction_log, veg_temp_reduced_null_log, veg_temp_reduced_s_log_nested)
AICc(veg_temp_reduced_simple_log, veg_temp_reduced_s_log, veg_temp_reduced_t_log, veg_temp_reduced_s_t_log, veg_temp_reduced_interaction_log, veg_temp_reduced_null_log, veg_temp_reduced_s_log_nested)

r.squaredGLMM(veg_temp_reduced_s_log)

# checking assumptions
summary(veg_temp_reduced_simple_log) # not significant overall (but signi for E), adj-R2 = 0.161
plot(veg_temp_reduced_simple_log)
veg_temp_reduced_resid <- resid(veg_temp_reduced_simple_log)
shapiro.test(veg_temp_reduced_resid) # normally distributed
bartlett.test(Cover_log2 ~ Mean_temp + Region, data = reduced_data)




