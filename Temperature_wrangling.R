# Temperature wrangling
# Colombe Stevens
# 17/02/2022

# Aim of script:
# Wrangling temperature data to get monthly averages of each year

# Library
library(tidyverse)

# Importing data
MZB_raw <- read.csv("MZB_raw_T_data.csv")

###### CHECK AMOUNT OF NA DATA BEFORE NA.OMIT -- esp MZB 2009, 2010

# Separating date/time, creating month
MZB <- MZB_raw %>% 
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

MZB$Temp <- as.numeric(MZB$Temp)

# Calculating monthly averages 2000-2020
MZB_mean <- MZB %>%
  na.omit(MZB) %>% 
  mutate(Year = case_when(
    grepl("2000", Date) ~ "2000",
    grepl("2001", Date) ~ "2001",
    grepl("2002", Date) ~ "2002",
    grepl("2003", Date) ~ "2003",
    grepl("2004", Date) ~ "2004",
    grepl("2005", Date) ~ "2005",
    grepl("2006", Date) ~ "2006",
    grepl("2007", Date) ~ "2007",
    grepl("2008", Date) ~ "2008",
    grepl("2009", Date) ~ "2009",
    grepl("2010", Date) ~ "2010",
    grepl("2011", Date) ~ "2011",
    grepl("2012", Date) ~ "2012",
    grepl("2013", Date) ~ "2013",
    grepl("2014", Date) ~ "2014",
    grepl("2015", Date) ~ "2015",
    grepl("2016", Date) ~ "2016",
    grepl("2017", Date) ~ "2017",
    grepl("2018", Date) ~ "2018",
    grepl("2019", Date) ~ "2019",
    grepl("2020", Date) ~ "2020")) %>%
  select(-Date) %>% 
  group_by(Site, Month, Year) %>% 
  summarise(Mean_temp = mean(Temp)) %>% 
  ungroup()
