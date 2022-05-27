library(tidyverse)
library(purrr)
library(fixest)
library(readxl)
library(dplyr)
library(jtools)
library(lubridate)

# Uploading trends data
list.files(path= "~/Desktop/Lab3_Rawdata")
flist <-list.files(path= "~/Desktop/Lab3_Rawdata", full.names = TRUE, pattern = "trends")
trendData <- flist %>%
  map(read_csv) %>%
  bind_rows()

#Uploading Scorecard data and id-name
scorecardData <-read_csv("~/Desktop/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv")
scorecardData <-rename(scorecardData, opeid = OPEID)
scorecardData <-rename(scorecardData, unitid = UNITID)

id_name_link <- read.csv("~/Desktop/Lab3_Rawdata/id_name_link.csv")
id_name_link <- id_name_link %>%
  group_by(schname) %>%
  mutate(N=n()) %>%
  filter(N==1)

#Joining data tables
d <- trendData %>%
  left_join(id_name_link, by = "schname")
fullData <- d %>%
  left_join(scorecardData, by = "opeid")

##Data Cleaning
#standardize index: (index-mean(index))/sd(index)
fullDatafinal <- fullData %>%
  group_by(schname) %>%
  mutate(index.s = (index - mean(index, na.rm = TRUE))/sd(index, na.rm = TRUE))

#get bachelor info and map out date
bachelorData <- filter(fullDatafinal, PREDDEG == 3)
bachelorData <- bachelorData %>%
  mutate (date = str_sub(monthorweek, 1, 10)) %>%
  mutate(date = ymd(date)) %>%
  mutate(after_release = date > ymd ('2015-08-31'))
#move unitid.y
bachelorData <- subset(bachelorData, select = -unitid.y)

#map out earnings using ifelse
bachelorData<-rename(bachelorData, 
                     median_earnings = "md_earn_wne_p10-REPORTED-EARNINGS")
bachelorData$high_income <-  ifelse(bachelorData$median_earnings > 67860, "High", "Low")

#save data into clean data
write.csv(bachelorData, "~/Desktop/Lab3_Rawdata/Clean_data.csv")
