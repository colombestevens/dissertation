# Final stats analysis + plots
# Colombe Stevens
# 02.04.2022

# Libraries ----
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggeffects)
library(wesanderson)
library(viridis)

# Importing data ----
data <- read.csv("Tidy_summer_data.csv")

# Transforming data ----
reduced_data <- na.omit(data) %>% 
  filter(Percentage_cover != 0.00)

reduced_west <- filter(reduced_data, Region == "West")
reduced_east <- filter(reduced_data, Region == "East")

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
veg_reduced_null_s <- lmer(Percentage_cover ~ 1 + (1|Site), data = reduced_data)
veg_reduced_null_t <- lmer(Percentage_cover ~ 1 + (1|Year), data = reduced_data)
veg_reduced_null_s_t <- lmer(Percentage_cover ~ 1 + (1|Year) + (1|Site), data = reduced_data)

AICc(veg_reduced_simple, veg_reduced_s, veg_reduced_s_interaction, veg_reduced_s_nested, veg_reduced_t, veg_reduced_t_interaction, veg_reduced_s_t, veg_reduced_s_t_interaction, veg_reduced_interaction, veg_reduced_null, veg_reduced_null_s, veg_reduced_null_t, veg_reduced_null_s_t)

summary(veg_reduced_s_t)

# R2m and R2c
r.squaredGLMM(veg_reduced_s_t) # R2m = 0.345; R2c = 0.427

# Residual variance explained by random effects
27.48/(27.48+190.61) # Year accounts for 12.60% of residual variance, and Site for 0%

# checking assumptions: Site + Year as random effects w/o interaction
veg_reduced_s_t_resid <- resid(veg_reduced_s_t)
shapiro.test(veg_reduced_s_t_resid) # NORMALLY DISTRIBUTED
qqnorm(veg_reduced_s_t_resid)
qqline(veg_reduced_s_t_resid)
plot(veg_reduced_s_t)

# Regional trends
# West
veg_time_w <- lm(Percentage_cover ~ Year, data = reduced_west)
summary(veg_time_w)
plot(veg_time_w)

# East
veg_time_e <- lm(Percentage_cover ~ Year, data = reduced_east)
summary(veg_time_e)
plot(veg_time_e)

# Extracting predictions
veg_time_preds <- ggpredict(veg_reduced_s_t, terms = c("Year", "Region"))

# Plotting data
(veg_time_preds_scatter <- ggplot(veg_time_preds) +
    geom_line(aes(x = x, y = predicted)) +
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +
    geom_point(reduced_data, mapping = aes(x = Year, y = Percentage_cover, colour = Region)) +
    theme_bw() +
    theme(panel.grid = element_blank()))

(veg_time_scatter <- ggplot(reduced_data, aes(x = Year, y = Percentage_cover, colour = Region)) +
    geom_point() +
    geom_smooth(method = "lm", aes(fill = Region)) +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette("Cavalcanti1")) +
    labs(x = "\nTime (years)",
         y = "Vegetation cover (% cover)\n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)))

ggsave(veg_time_scatter, filename = "veg_time_scatter.png") # saving plot

# RQ2: VEGETATION COVER CHANGE IN RESPONSE TO TEMPERATURE ----

# Model comparison
veg_temp_reduced_simple <- lm(Percentage_cover ~ Mean_temp + Region, data = reduced_data)
veg_temp_reduced_s <- lmer(Percentage_cover ~ Mean_temp + Region + (1|Site), data = reduced_data)
veg_temp_reduced_s_nested <- lmer(Percentage_cover ~ Mean_temp + Region + (1|Region/Site), data = reduced_data)
veg_temp_reduced_s_interaction <- lmer(Percentage_cover ~ Mean_temp * Region + (1|Site), data = reduced_data)
veg_temp_reduced_t <- lmer(Percentage_cover ~ Mean_temp + Region + (1|Year), data = reduced_data)
veg_temp_reduced_t_interaction <- lmer(Percentage_cover ~ Mean_temp * Region + (1|Year), data = reduced_data)
veg_temp_reduced_s_t <- lmer(Percentage_cover ~ Mean_temp + Region + (1|Site) + (1|Year), data = reduced_data)
veg_temp_reduced_s_t_interaction <- lmer(Percentage_cover ~ Mean_temp * Region + (1|Site) + (1|Year), data = reduced_data)
veg_temp_reduced_interaction <- lm(Percentage_cover ~ Mean_temp * Region, data = reduced_data)
veg_temp_reduced_null_lm <- lm(Percentage_cover ~ 1, data = reduced_data)
veg_temp_reduced_null_s <- lmer(Percentage_cover ~ 1 + (1|Site), data = reduced_data)
veg_temp_reduced_null_t <- lmer(Percentage_cover ~ 1 + (1|Year), data = reduced_data)
veg_temp_reduced_null_s_t <- lmer(Percentage_cover ~ 1 + (1|Site) + (1|Year), data = reduced_data)

AICc(veg_temp_reduced_simple, veg_temp_reduced_s, veg_temp_reduced_t, veg_temp_reduced_s_t, veg_temp_reduced_interaction, veg_temp_reduced_null_lm, veg_temp_reduced_null_s, veg_temp_reduced_null_t, veg_temp_reduced_null_s_t, veg_temp_reduced_s_nested, veg_temp_reduced_s_interaction, veg_temp_reduced_t_interaction, veg_temp_reduced_s_t_interaction)

summary(veg_temp_reduced_s_t)

# R2m and R2c
r.squaredGLMM(veg_temp_reduced_s_t) # R2m = 0.202; R2c = 0.489

# Residual variance explained by random effects
# Year explains 0% of residual variance
110.8/(110.8+196.8) # Site explains 36.02% of residual variance

# checking assumptions: Site + Year as random effects
veg_temp_reduced_s_t_interaction_resid <- resid(veg_temp_reduced_s_t_interaction)
shapiro.test(veg_temp_reduced_s_t_interaction_resid) # NORMALLY DISTRIBUTED
qqnorm(veg_temp_reduced_s_t_interaction_resid)
qqline(veg_temp_reduced_s_t_interaction_resid)
plot(veg_temp_reduced_s_t_interaction)

# Regional trends
# West
veg_temp_w <- lm(Percentage_cover ~ Mean_temp, data = reduced_west)
summary(veg_temp_w)
plot(veg_temp_w)

# East
veg_temp_e <- lm(Percentage_cover ~ Mean_temp, data = reduced_east)
summary(veg_temp_e)
plot(veg_temp_e)

# Extracting predictions
veg_temp_preds <- ggpredict(veg_temp_reduced_s_t, terms = c("Mean_temp", "Region"))

# Plotting data

# Per region
(veg_temp_preds_scatter <- ggplot(veg_temp_preds) +
    geom_line(aes(x = x, y = predicted)) +
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +
    geom_point(reduced_data, mapping = aes(x = Mean_temp, y = Percentage_cover, colour = Region)) +
    theme_bw() +
    theme(panel.grid = element_blank()))

(veg_temp_scatter <-ggplot(data, aes(x = Mean_temp, y = Percentage_cover, colour = Region)) +
    geom_point() +
    geom_smooth(method = "lm", aes(fill = Region)) +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette("Cavalcanti1")) +
    labs(x = "\nMean temperature (°C)",
         y = "Vegetation cover (% cover)\n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)))

ggsave(veg_temp_scatter, filename = "veg_temp_scatter.png") # saving plot

# Per site ## CHECK OUT HELP FOR 6 WES COLOURS
(veg_temp_scatter_sites <- ggplot(data, aes(x = Mean_temp, y = Percentage_cover, colour = Site)) +
  geom_point() +
  geom_smooth(method = "lm", aes(fill = Site)) +
  facet_wrap(~Site, scales = "free_y") +
  #scale_colour_manual(values = wes_palette("Cavalcanti1")) +
  #scale_fill_manual(values = wes_palette("Cavalcanti1")) +
  #scale_colour_viridis(discrete = TRUE) +
  #scale_fill_viridis(discrete = TRUE) +
  scale_colour_manual(values = c("#556B2F", "#EEC900", "#9BCD9B", "#8B2323", "#CDC673", "#9AC0CD")) +
  scale_fill_manual(values = c("#556B2F", "#EEC900", "#9BCD9B", "#8B2323", "#CDC673", "#9AC0CD")) +
  labs(x = "\nMean temperature (°C)",
       y = "Vegetation cover (% cover)\n") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.background = element_rect(colour = "#00688B", fill = "white"),
        strip.text = element_text(size = 11, face = "italic", colour = "#00688B"),
        legend.position = "none"))

ggsave(veg_temp_scatter_sites, filename = "veg_temp_scatter_sites.png") # saving plot

c("#556B2F", "#EEC900", "#9BCD9B", "#8B2323", "#CDC673", "#9AC0CD")
c("#104E8B", "#00688B", "#9AC0CD", "#9FB6CD")
c("#698B69", "#8FBC8F", "#9BCD9B", "#B4EEB4", "#C1FFC1")
c("#8B3626", "#A52A2A", "#CD3333", "#8B3A3A")
c("#CDAD00", "#EEC900", "#FFD700")


# TEMPERATURE CHANGE OVER TIME ----

# Transforming data to have all temperature values
all_data <- read.csv("Tidy_summer_data.csv")
all_data <- all_data %>% 
  select(-Percentage_cover) %>% 
  na.omit()

# Linear model: temperature over time
temp_time_all <- lm(Mean_temp ~ Year + Region, data = all_data)
summary(temp_time_all)

# Checking model assumptions
temp_time_all_resid <- resid(temp_time_all)
shapiro.test(temp_time_all_resid) # NORMALLY DISTRIBUTED
plot(temp_time_all)

# Plotting data
(temp_time_scatter <- ggplot(all_data, aes(x = Year, y = Mean_temp, colour = Region)) +
    geom_point() +
    geom_smooth(method = "lm", aes(fill = Region)) +
    scale_colour_manual(values = wes_palette("Cavalcanti1")) +
    scale_fill_manual(values = wes_palette(("Cavalcanti1"))) +
    labs(x = "\nTime (years)",
         y = "Mean summer temperature (°C)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)))

ggsave(temp_time_scatter, filename = "temp_time_scatter.png")
