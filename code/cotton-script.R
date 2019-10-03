########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 25, 2019

# Module 6 Homework: Tidying cotton data
## dataset: annual observations of yield (lbs/acre) and area harvested(acres) of cotton for each county and ag. distict across the U.S. 

# Questions: 

#### How have yield and area harvested of cotton changed across all of the NC agricultural disticts over time?

#### What were the top 3 cotton producing countries in NC in terms of total lbs of cotton for 2018?

########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----

cotton <- read_csv("data/cotton-usda-nass.csv")
str(cotton)
head(cotton) #value and percent probably shouldm't be chr
tail(cotton) #1925 - 12017
dim(cotton)
summary(cotton)

# 3.1. Create a NC data subset ----

cotton %>%
  filter(state == "NORTH CAROLINA") %>%
  select(year, state, ag_district, county, data_item, value) -> cotton_nc
cotton_nc
# 3.2. Divide the data_item column ----

cotton_nc %>%
  separate(data_item, 
           into = c("cotton_type", "measurement"),
           sep = " - ") -> cotton_nc

cotton_nc

# 3.3. Convert the value column to numeric type ----

cotton_nc %>%
  filter(value != "(D)") -> cotton_nc

cotton_nc$value <- as.numeric(cotton_nc$value)
summary(cotton_nc)

# 4. Visualizing trends ----

cotton_nc %>%
  ggplot(mapping = aes(x = year, y = value)) +
  geom_point(size = 2) +
  theme_minimal() +
  facet_grid(rows = vars(measurement), 
             cols = vars(ag_district), 
             scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90))

# 5. Summarize data from 2018 ----

cotton %>%
  filter(state == "NORTH CAROLINA", year == "2018") %>%
  separate(data_item, 
           into = c("cotton_type", "measurement"),
           sep = " - ") -> cotton_nc
cotton_nc %>%
  spread(key = measurement, value = value) %>%
  drop_na(`ACRES HARVESTED`) %>%
  drop_na(`YIELD, MEASURED IN LB / ACRE`) %>%
  select(year, county, `ACRES HARVESTED`, `YIELD, MEASURED IN LB / ACRE`) -> cotton_nc_tidy

cotton_nc_tidy$`ACRES HARVESTED` <- as.numeric(cotton_nc_tidy$`ACRES HARVESTED`) 
cotton_nc_tidy$`YIELD, MEASURED IN LB / ACRE` <- as.numeric(cotton_nc_tidy$`YIELD, MEASURED IN LB / ACRE`)

cotton_nc_tidy %>%
  mutate(total_lbs = `ACRES HARVESTED` * `YIELD, MEASURED IN LB / ACRE`) %>%
  arrange(desc(total_lbs)) %>%
  select(county, total_lbs) -> cotton_nc_tidy_2

cotton_nc_tidy_2 %>%
  top_n(3)

# proud of myself for being able to do this both for all time and also only for 2018! 
