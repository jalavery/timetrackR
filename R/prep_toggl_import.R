# this file takes my previously logged time and prepares it for import directly into toggl

library(tidyverse)
library(readxl)
library(lubridate)

### import existing time tracking data
tracker_JL_long <- read_xlsx(path = here::here("data/Project Tracker JL.xlsx"), sheet = 1)

### set up for toggl import
tracker_JL_long_toggl <- tracker_JL_long %>% 
  rename(Tags = `Project phase`,
         Description = Task) %>% 
  mutate(`Start Date` = as.Date(word(Date, 1, sep = "T")),
         email = "laveryj@mskcc.org",
         # duration must be formatted as hours minutes seconds
         Duration = case_when(nchar(Hours) > 1 ~ paste0(floor(Hours), ":", as.numeric(substr(as.character(Hours), 2, 4))*60, ":00"),
                              TRUE ~ paste0(floor(Hours), ":00:00")),
         # start time is required, arbitrarily set to 9am
         `Start Time` = "9:00:00") %>% 
  # sample_n(1) %>% 
  select(-Detailed, -Date, -Hours)

### merge on client (PI) from project summary tab
# active projects
project_summary_JL_a <- read_xlsx(path = "data/Project Tracker JL.xlsx", sheet = 2)
# completed projects
project_summary_JL_b <- read_xlsx(path = "data/Project Tracker JL.xlsx", sheet = 3)
# set together
project_summary_JL_mod <- bind_rows(project_summary_JL_a, project_summary_JL_b) %>% 
  rename(Project  = `Study title`,
         Client = PI) %>% 
  select(Project, Client)

# merge PI onto hours log
export_toggl_JL <- left_join(tracker_JL_long_toggl, 
                     project_summary_JL_mod,
                     by = c("Project")) %>% 
  mutate(Client = case_when(!is.na(Client) ~ Client,
                            Project %in% c("Admin", "MSK") ~ "Admin"))


### export to csv to import into toggl
write_csv(export_toggl_JL, path = here::here("data/project_tracker_toggl_JL_test.csv"))
