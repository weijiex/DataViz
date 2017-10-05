# Weijie Xin
# This file is an exploratory analysis of the DoT FARS data.

### Open RStudio and Install Pertinent Packages ###
library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

### Load the FARS Data into R ###

# set working directory
setwd('~/desktop/DataViz')

acc2014 <- read_sas('accident.sas7bdat')
acc2015 <- read_csv('accident.csv')
ls() #check
class(acc2014)
class(acc2015)

### Combining the two years of FARS data ###

# convert empty string to NA
acc2014 <- mutate(acc2014, TWAY_ID2 = na_if(TWAY_ID2, ""))
table(is.na(acc2014$TWAY_ID2)) #check

# check number of rows and columns
dim(acc2014)
dim(acc2015)

# check colnames
colnames(acc2014)[!colnames(acc2014) %in% colnames(acc2015)]
# "ROAD_FNC" is in acc2014 but not in acc2015
colnames(acc2015)[!colnames(acc2015) %in% colnames(acc2014)]
# "RUR_URB"  "FUNC_SYS" "RD_OWNER" are in acc2015 but not in acc2014

# combine two tibbles
acc <- bind_rows(acc2014, acc2015)

# create a frequency table of the RUR_URB variable
count(acc, RUR_URB) 
# There are over 30,000 NA values for "RUR_URB" because acc2014 does not have this variable.

### Merging on another data source ###
fips <- read_csv("fips.csv")
glimpse(fips)

# convert integers to characters
acc$STATE <- as.character(acc$STATE)
acc$COUNTY <- as.character(acc$COUNTY)

# add leading zeros
acc$STATE <- str_pad(acc$STATE, 2, "left", "0" )
acc$COUNTY <- str_pad(acc$COUNTY, 3, "left", "0" )

# rename the State and County variables
acc <- rename(acc , 'StateFIPSCode' = 'STATE', 'CountyFIPSCode' = 'COUNTY')

# merge the fips data onto the acc tibble
acc <- left_join(acc, fips, by = c("StateFIPSCode", "CountyFIPSCode"))

### Exploratory Data Analysis in R’s dplyr and tidyr package ###
agg <- acc %>% group_by(StateName, YEAR)  %>% summarise(TOTAL = sum(FATALS))

agg_wide <- spread(agg, YEAR, TOTAL)
agg_wide <- rename(agg_wide, "YEAR2014" = '2014', "YEAR2015" = '2015')

agg_wide <- mutate(agg_wide, percent_diff = (YEAR2015 - YEAR2014) / YEAR2014)

agg_wide <- arrange(agg_wide, desc(agg_wide$percent_diff))

agg_wide <- filter(agg_wide, percent_diff > 0.15 & !is.na(StateName))

# rewrite the prior six steps with chain operator
agg <- acc %>%
  group_by(StateName, YEAR) %>%
  summarise(TOTAL = sum(FATALS)) %>%
  spread(YEAR, TOTAL) %>%
  rename("YEAR2014" = "2014", "YEAR2015" = "2015") %>%
  mutate(percent_diff = (YEAR2015 - YEAR2014) / YEAR2014) %>%
  arrange(desc(percent_diff)) %>%
  filter(percent_diff > 0.15 & !is.na(StateName))

glimpse(agg)

