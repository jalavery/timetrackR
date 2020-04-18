# barchart of changes over time

toggl <- read_csv(here::here("data/Toggl_time_entries_2019-04-07_to_2020-04-06.csv"))

toggl2 <- toggl %>% 
  # change duration to hours
  mutate(Duration = period_to_seconds(hms(Duration))/(60^2)) %>% 
  janitor::clean_names() %>%
  drop_na(project, client) %>% 
  mutate(year = year(start_date),
         month = month(start_date),
         year_month_fct = factor(paste0(year, "-", month)),
         year_month = fct_reorder(year_month_fct, start_date)) %>% 
  # get denominator to calculate percent of hours
  group_by(year_month) %>% 
  mutate(denominator = sum(duration)) %>% 
  # get total number of hours per project by year and month
  group_by(year_month, project, client, denominator) %>% 
  summarize(tot_hours = sum(duration)) %>% 
  mutate(pct_hours = (100*tot_hours)/denominator) %>% 
  # if total number of hours is less than 5% of total time, lump into other category
  mutate(project_collapsed = case_when(pct_hours < 10 ~ "Other",
                                       TRUE ~ project),
         client_collapsed = case_when(pct_hours < 10 ~ "Other",
                                      TRUE ~ client)) %>% 
  # group other projects together
  group_by(year_month, client_collapsed, project_collapsed) %>% 
  summarize(pct_hours_collapsed = sum(pct_hours)) %>% 
  mutate(hover_text = paste0("Project: ", project_collapsed, "<br>",
                       "Client: ", client_collapsed, "<br>",
                       "Effort: ", round(pct_hours_collapsed), "%"))

g1 <- ggplot(toggl2, aes(x = year_month, y = pct_hours_collapsed, fill = project_collapsed, text = hover_text)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month, Year",
       y = "% effort")
  

ggplotly(g1, tooltip = "text")
