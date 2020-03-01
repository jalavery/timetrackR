# Program: 1_create_datasets.R

# Purpose: Read in time tracker information and prepare for use in shiny app

library(tidyverse)
library(readxl)
library(lubridate)

# import toggl csv file
# in toggl: Reports -> Detailed -> Change date range -> Export as CSV
tracker_toggl <- read_csv(here::here("data/Toggl_time_entries_2019-03-02_to_2020-03-01.csv")) %>% 
  mutate(current_status = "Active",
         Duration = period_to_seconds(hms(Duration))/(60^2))

# save file to load in shiny app
save(tracker_toggl, file = here::here("tracker_toggl.rdata"))
