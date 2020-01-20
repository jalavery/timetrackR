###########################################################################
#Program: work_clean.R

# Date      Update
# 26NOV2018 Update project path
# 20JAN2020 Update to use here package
#           Clean up code whitespacing/flow
###########################################################################


#read in my project tracker
library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
library(DT)
library(rsconnect)
library(stringr)
library(here)

# setwd("G:/Biostats/R/Project Management Shiny App")

#read in long hours sheet
JL1 <- read_xlsx(path = "G:/Project Tracker.xlsx", sheet = 1)
# MC1 <- read_xlsx(path="G:/Biostats/R/Project Management Shiny App/Project Tracker MC_oldv1.xlsx",sheet=1)
MC1b <- read_xlsx(path = "G:/Biostats/R/proj_mgmnt/Project Tracker MC.xlsx", sheet = 1)

#modify to merge long sheets
# MC1b <- MC1 %>% 
#   mutate(date_orig = Date,
#          Date_date = as.Date(as.numeric(date_orig),format = "%Y-%m-%d",origin =  '1899-12-30'),
#          Date_char = format(Date_date,'%Y-%m-%d'),
#          Date = as.POSIXct(Date_char,origin = "1970-01-01") #works best off of a character date instead of a date variable for some reason
#   ) %>% 
#   select(-date_orig,-Date_date,-Date_char) #have to use excel's origin date

tracker1_pre <- bind_rows(JL1, MC1b)

tracker1 <- tracker1_pre %>% 
  mutate(Project = ifelse(Project == "Thyroid Active survallence","Thyroid: AS vs Surgery", Project))

#read in summary sheet
JL2 <- read_xlsx(path = "G:/Project Tracker.xlsx", sheet = 3)
MC2 <- read_xlsx(path = "G:/Biostats/R/Project Management Shiny App/Project Tracker MC.xlsx", sheet = 2)

#define variables in summary sheet
#note: subtracting POSIXt dates gives difference in seconds, have to convert
JL2b <- JL2 %>% 
  mutate(ttl = `Study title`, statistician = "Jessica Lavery",
         init = ifelse(statistician == "Jessica Lavery", "JL","UNK"),
         proj_start = as.Date(`Project initiated`),
         proj_end = as.Date(`Project completed`),
         start_dt = format(`Project initiated`,'%Y-%m-%d'),
         end_dt = format(`Project completed`,'%Y-%m-%d'),
         completed = ifelse(word(`Current status`,1) == "Completed",1,0),
         upcoming = ifelse(word(`Current status`,1) == "Upcoming:",1,0),
         dropped = ifelse(word(`Current status`,1) == "Dropped:",1,0),
         daystocompletion = as.numeric((`Project completed` - `Project initiated`)/(60*60*24)),
         weekstocompletion = format(as.numeric((`Project completed` - `Project initiated`)/(7*60*60*24)),digits = 1),
         monthstocompletion = as.numeric((`Project completed` - `Project initiated`)/(4*7*60*60*24))) %>%
  filter(is.na(`Project initiated`) == FALSE)

#hard coding start date for Mike's projects
#subtracting as.Date() variables gives days difference, modified date code
MC2b <- MC2 %>% 
  # select(`Study title``Project initiated`) %>% 
  filter(!`Study title` %in% c("Hospital Size Estimation","John Hopkins Meeting"), PI != "Curry, Mike") %>% 
  mutate(ttl = `Study title`, statistician = "Mike Curry",
         init = "MC",
         PI = ifelse(PI %in% c("Li, Diane","Krell, Robert"),"Snyderman, Allison",
                     ifelse(PI == "Nobel, Tamar","Molena, Daniela", PI)),
         proj_start = as.Date("2017-11-15"),
         proj_end = as.Date(`Project completed`),
         start_dt = format(proj_start,'%Y-%m-%d') ,
         end_dt = format(`Project completed`,'%Y-%m-%d'),
         completed = ifelse(word(`Current status`,1) == "Completed",1,0),
         upcoming = ifelse(word(`Current status`,1) == "Upcoming:",1,0),
         dropped = ifelse(word(`Current status`,1) == "Dropped:",1,0),
         daystocompletion = as.numeric((proj_end - proj_start)),
         weekstocompletion = format(as.numeric((proj_end - proj_start)/(7)),digits = 1),
         monthstocompletion = as.numeric((proj_end - proj_start)/(4*7)))

#set project trackers together
tracker2_pre <- bind_rows(JL2b, MC2b)
tracker2 <- tracker2_pre %>% mutate(`Study title` = ifelse(`Study title` == "Thyroid Active survallence", "Thyroid: AS vs Surgery", `Study title`),
                                    ttl = ifelse(ttl == "Thyroid Active survallence", "Thyroid: AS vs Surgery", ttl))

#active projects
active <- tracker2 %>% 
  filter(completed != 1 & upcoming != 1 & dropped != 1)

#upcoming projects
upcoming <- tracker2 %>% 
  filter(upcoming == 1)

#inactive (completed + dropped) projects
inactive <- tracker2 %>% 
  filter(completed == 1 | dropped == 1) %>%  
  select('statistician','Study title','PI','start_dt',`Current status`,'end_dt','Hours','weekstocompletion')

#merge two spreadsheets together
tracker <- merge(tracker1, tracker2, by.x = "Project", by.y =  "ttl") 


save(tracker,file = paste0(here::here(), "/tracker.rdata"))
save(active,file = paste0(here::here(), "/active.rdata"))
save(upcoming,file = paste0(here::here(), "/upcoming.rdata"))
save(inactive,file = paste0(here::here(), "/inactive.rdata"))
