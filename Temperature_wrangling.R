# Temperature wrangling
# Colombe Stevens
# 17/02/2022

# Aim of script:
# Wrangling temperature data to get monthly averages of each year

# Library
library(tidyverse)

# Importing data
data <- read.csv("MZB_raw_T_data.csv")

###### CHECK AMOUNT OF NA DATA BEFORE NA.OMIT -- esp MZB 2009, 2010

# Wrangle
MZB <- data %>% 
  separate(Date.Time_UTC, c("Date", "Time"), sep = " ", remove = TRUE) %>%
  select(-Time) %>% 
  mutate(Month = case_when(
    grepl("01/", Date) ~ "January",
    grepl("02/", Date) ~ "February",
    grepl("03/", Date) ~ "March",
    grepl("04/", Date) ~ "April",
    grepl("05/", Date) ~ "May",
    grepl("06/", Date) ~ "June",
    grepl("07/", Date) ~ "July",
    grepl("08/", Date) ~ "August",
    grepl("09/", Date) ~ "September",
    grepl("10/", Date) ~ "October",
    grepl("11/", Date) ~ "November",
    grepl("12/", Date) ~ "December")) 
group_by(Month) %>% 
  mutate(Mean_temp = mean(Temp))

MZB$Temp <- as.numeric(MZB$Temp)
MZB2 <- na.omit(MZB)

MZB_mean <- MZB2 %>%
  mutate(Date = case_when(
    grepl("2000", Date) ~ "2000",
    grepl("2001", Date) ~ "2001")) %>% 
  group_by(Site, Month, Date) %>% 
  summarise(Mean_temp = mean(Temp))

