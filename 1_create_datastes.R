# Program: 1_create_datasets.R

# Purpose: Read in time tracker information and prepare for use in shiny app

library(tidyverse)
library(readxl)
library(lubridate)


# step 1 -------
# read in spreadsheet with 1 record per task tracked for each statistician of interest for app
# necessary variables are: date, hours, task, project, project phase

#read in long hours sheet
time_tracker_JL_long <- read_xlsx(path = "G:/Project Tracker.xlsx", sheet = 1)

time_tracker_MC_long <- read_xlsx(path = "G:/Biostats/R/proj_mgmnt/Project Tracker MC.xlsx", sheet = 1)

# set data for each statistician together
time_tracker_long <- bind_rows(time_tracker_JL_long, time_tracker_MC_long) %>% 
  # hardcode work done on the same project across JL/MC to be the same
  mutate(Project = ifelse(Project == "Thyroid Active survallence","Thyroid: AS vs Surgery", Project))

# step 2 -------
# read in summary spreadsheet with 1 record per project and its current status
# necessary variables include: 
project_summary_JL <- read_xlsx(path = "G:/Project Tracker.xlsx", sheet = 3)
project_summary_MC <- read_xlsx(path = "G:/Biostats/R/Project Management Shiny App/Project Tracker MC.xlsx", sheet = 2)

#define variables in summary sheet
#note: subtracting POSIXt dates gives difference in seconds, have to convert
project_summary_JL_mod <- project_summary_JL %>% 
  mutate(statistician = "Jessica Lavery",
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
  rename(ttl = `Study title`)

#hard coding start date for Mike's projects
#subtracting as.Date() variables gives days difference, modified date code
project_summary_MC_mod <- project_summary_MC %>% 
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

# set project summaries for each statistician together
proj_summary <- bind_rows(project_summary_JL_mod, project_summary_MC_mod) %>% 
  # collapse the same project that we called two different names
  mutate(`Study title` = ifelse(`Study title` == "Thyroid Active survallence", "Thyroid: AS vs Surgery", `Study title`),
         ttl = ifelse(ttl == "Thyroid Active survallence", "Thyroid: AS vs Surgery", ttl))

# step 3 ----
# separate projects into active, upcoming and inactive
#active projects
active <- proj_summary %>% 
  filter(completed != 1 & upcoming != 1 & dropped != 1)

#upcoming projects
upcoming <- proj_summary %>% 
  filter(upcoming == 1)

#inactive (completed + dropped) projects
inactive <- proj_summary %>% 
  filter(completed == 1 | dropped == 1) %>%  
  select(statistician, `Study title`, PI, start_dt, `Current status`, end_dt, Hours, weekstocompletion)

# merge two spreadsheets together
tracker <- merge(time_tracker_long, proj_summary, by.x = "Project", by.y =  "ttl") 

# step 4 -----
# save data to access in shiny app
save(tracker, active, upcoming, inactive,file = paste0(here::here(), "/tracker.rdata"))
