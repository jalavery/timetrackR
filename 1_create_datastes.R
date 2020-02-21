# Program: 1_create_datasets.R

# Purpose: Read in time tracker information and prepare for use in shiny app

library(tidyverse)
library(readxl)
library(lubridate)


# step 1 -------
# read in spreadsheet with 1 record per task tracked for each statistician of interest for app
# necessary variables are: date, hours, task, project, project phase

#read in long hours sheet
time_tracker_JL_long <- read_xlsx(path = "G:/Project Tracker.xlsx", sheet = 1) %>% 
  mutate(statistician = "Jessica Lavery")

time_tracker_MC_long <- read_xlsx(path = here::here("Project Tracker MC.xlsx"), sheet = 1) %>% 
  mutate(statistician = "Mike Curry")

# set data for each statistician together
time_tracker_long <- bind_rows(time_tracker_JL_long, time_tracker_MC_long) %>% 
  janitor::clean_names() %>% 
  # hardcode work done on the same project across JL/MC to be the same
  mutate(project = ifelse(project == "Thyroid Active survallence", "Thyroid: AS vs Surgery", project)) %>% 
  select(-detailed) %>% 
  rename(study_title = project)

# step 2 -------
# read in summary spreadsheet with 1 record per project and its current status
# necessary variables include: 
project_summary_JL_a <- read_xlsx(path = "G:/Project Tracker.xlsx", sheet = 2)
# moved completed projects to a separate tab
project_summary_JL_b <- read_xlsx(path = "G:/Project Tracker.xlsx", sheet = 3)

project_summary_MC <- read_xlsx(path = here::here("Project Tracker MC.xlsx"), sheet = 2)

#define variables in summary sheet
#note: subtracting POSIXt dates gives difference in seconds, have to convert
project_summary_JL_mod <- bind_rows(project_summary_JL_a, project_summary_JL_b) %>% 
  janitor::clean_names() %>% 
  select(-priority, -group, -lead_stats, -second_stats, -in_my_court, -protocol_no, -dataset, -path_to_files, -notes, -protocol_status) %>% 
  mutate(statistician = "Jessica Lavery",
         init = ifelse(statistician == "Jessica Lavery", "JL", "UNK"),
         proj_start = as.Date(project_initiated),
         proj_end = as.Date(project_completed))

#hard coding start date for Mike's projects
#subtracting as.Date() variables gives days difference, modified date code
project_summary_MC_mod <- project_summary_MC %>% 
  janitor::clean_names() %>% 
  # exclude specific projects from tracker
  filter(!study_title %in% c("Hospital Size Estimation","John Hopkins Meeting"), pi != "Curry, Mike") %>% 
  mutate(statistician = "Mike Curry",
         init = "MC",
         # combine sub-projects under a single PI
         pi = ifelse(pi %in% c("Li, Diane", "Krell, Robert"), "Snyderman, Allison",
                     ifelse(pi == "Nobel, Tamar", "Molena, Daniela", pi)),
         # hard code the date Mike started tracking his projects
         proj_start = as.Date("2017-11-15"),
         proj_end = as.Date(project_completed)) %>% 
  select(-dataset, -notes)

# set project summaries for each statistician together
proj_summary <- bind_rows(project_summary_JL_mod, project_summary_MC_mod) %>% 
  # collapse the same project that we called two different names
  mutate(study_title = ifelse(study_title == "Thyroid Active survallence", "Thyroid: AS vs Surgery", study_title)) %>% 
  # drop rows without project start date (miscallaneous asks, IT, etc.)
  drop_na(proj_start) %>% 
  # create variables for summaries
  mutate(status = case_when(word(current_status, 1) == "Upcoming:" ~ "Upcoming",
                            word(current_status, 1) %in% c("Complete", "Completed", "Completed:", "Complete:") ~ "Completed",
                            word(current_status, 1) %in% c("Dropped", "Dropped:") ~ "Dropped",
                            TRUE ~ "Active"),
         daystocompletion = (proj_end - proj_start),
         weekstocompletion = round(daystocompletion/7),
         monthstocompletion = round(daystocompletion/365.25)) %>% 
  rename(total_hours = hours)

# merge two spreadsheets together
# 1 record per time entry with summary information merged on
tracker <- left_join(time_tracker_long, 
                     proj_summary,
                     by = c("study_title", "statistician")) %>% 
  mutate(date = as.Date(date))

# step 3 -----
# save data to access in shiny app
save(tracker, proj_summary, file = here::here("/tracker.rdata"))
