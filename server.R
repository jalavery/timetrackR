#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# 23MAY2018 Add pie chart
# 14AUG2018 Add completed projects table
# 15AUG2018 Add bar chart
# 16AUG2018 Add Mike's project tracker data
# 31JAN2020 Update to run on cleaned up code
# 03FEB2020 Update to filter on date rather than using year that project started


library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
library(DT)
library(rsconnect)
library(shiny)
library(ggplot2)

load("tracker.rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # statistician names
  output$statistician <- renderUI({
    selectizeInput("statistician", label = "Statistician(s)", 
                   choices = unique(tracker$statistician),
                   selected = unique(tracker$statistician)[c(1,2)], multiple = TRUE)
  })
    
  # list of PIs based on statistician choice
  # output$PI_choice <- renderUI({
  #   # filter on the statisticians selected and get unique list of PIs
  #   pi_list <- tracker %>% 
  #     filter(statistician %in% input$statistician) %>%
  #     distinct(pi) %>% 
  #     select(pi) %>% 
  #     arrange(pi)
    
  #   selectizeInput("PI_choice", 
  #                  label = "Investigator(s)", 
  #                  choices = pi_list,
  #                  selected = pull(pi_list, pi),
  #                  multiple = TRUE)
  # })
  
  # list of available years for menu on side
  # start by selecting the previous year and curent year
  output$years <- renderUI({
    dateRangeInput(inputId = "years", 
                   label = "As of date: ",
                   # default time from start of current quarter through current date
                   # floor_date(Sys.Date(), unit = "quarter")
                   start = Sys.Date() - years(1), end = Sys.Date(), min = NULL,
                   max = Sys.Date(), format = "yyyy-mm-dd", startview = "month",
                   weekstart = 0, language = "en", separator = " to ", width = NULL,
                   autoclose = TRUE)
  })
  
  ######################
  #active project table#
  ######################
  active_projs = reactive({
    proj_summary %>% filter(statistician %in% input$statistician, 
                      # pi %in% input$PI_choice,
                      status == "Active") %>% 
      rename(`Statistician` = statistician, 
             `Start date` = proj_start,
             `PI` = pi,
             `Study title` = study_title,
             `Current status` = current_status,
             `Hours` = total_hours) %>% 
      select("PI", "Study title", "Current status", "Start date", "Hours", "Statistician") 
  })
  
  
  output$table <- DT::renderDataTable({
    #rownames=FALSE removes obs #
    DT::datatable(active_projs(), rownames = FALSE) 
  })

  #########################
  #upcoming project table#
  #########################
  upcoming_projs = reactive({
    proj_summary %>% filter(statistician %in% input$statistician, 
                        # pi %in% input$PI_choice,
                        status == "Upcoming") %>% 
      rename(`Statistician` = statistician,
             `PI` = pi,
             `Start date` = proj_start,
             `Study title` = study_title,
             `Current status` = current_status) %>% 
      select("Statistician", "PI", "Study title", "Current status") 
  })
  
  output$table_upcoming <- DT::renderDataTable({
    DT::datatable(upcoming_projs(),rownames = FALSE) #rownames=FALSE removes obs #
  })
  
  #########################
  #completed project table#
  #########################
  inactive_projs = reactive({ 
    proj_summary %>% 
      filter(statistician %in% input$statistician, 
             # pi %in% input$PI_choice, 
             status %in% c("Dropped", "Completed")) %>%
      rename(`Statistician` = statistician,
             PI = pi,
             `Study title` = study_title,
             `Start date` = proj_start,
             `End date` = proj_end,
             `Current status` = current_status,
             `Total hours` = total_hours,
             `Weeks to completion` = weekstocompletion) %>% 
      select("PI", "Study title", "Current status", "Start date", "Total hours", Statistician)
  })
  
  # active_projs2 <- active_projs[,c("PI","Study title","Project initiated","Hours")]
  output$table2 <- DT::renderDataTable({
    DT::datatable(inactive_projs(),rownames = FALSE)
  })
  # output$table2 <- renderTable(completed_projs(),colnames = TRUE)
  
  ######################
  #      pie chart     #
  ######################
  
  # subset data for pie chart based on statistician and dates
  output$pieChart <- renderPlotly({
  pie <- tracker %>% 
    filter(statistician %in% input$statistician, 
           date >= input$years[1],
           date <= input$years[2])
  
  # change grouping to sum depending on which summary level is selected
  ## LEFT OFF HERE: TRYING TO LUMP PROJECTS/PIS  with <5% of time used within the date range
  switch(input$stratify_pct_effort,
         "PI" = pie %>% 
           drop_na(hours, pi) %>% 
           # collapse projects PIs accounting for <3% of time
           mutate(pi_lump = fct_lump(pi, prop = 0.03, w = hours)) %>% 
           group_by(pi_lump, statistician) %>% 
           # re-calculate total number of hours across statistician/proj selected (have to recalc if >1 stat per proj)
           summarize(sum_hrs = sum(hours, na.rm = TRUE)) %>% 
           plot_ly(labels = ~ pi_lump, values = ~sum_hrs, type = 'pie') %>%
           layout(title = '% of total hours by faculty/principal investigator',
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
         "Project" = pie %>% 
           # collapse projects accounting for <3% of time in interval
           drop_na(hours, study_title) %>% 
           mutate(study_title_lump = fct_lump(study_title, prop = 0.03, w = hours)) %>% 
           group_by(study_title_lump, statistician) %>% 
           # re-calculate total number of hours across statistician/proj selected (have to recalc if >1 stat per proj)
           summarize(sum_hrs = sum(hours, na.rm = TRUE)) %>% 
           plot_ly(labels = ~ study_title_lump, values = ~sum_hrs, type = 'pie') %>%
           layout(title = '% of total hours by project',
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
         "Task" = pie %>% 
           mutate(project_phase = case_when(is.na(project_phase) ~ "Other",
                                            TRUE ~ project_phase)) %>% 
           # collapse tasks accounting for <3% of time
           mutate(project_phase_lump = fct_lump(project_phase, prop = 0.03, w = hours)) %>% 
           group_by(project_phase_lump, statistician) %>% 
           # re-calculate total number of hours across statistician/proj selected (have to recalc if >1 stat per proj)
           summarize(sum_hrs = sum(hours, na.rm = TRUE)) %>% 
           plot_ly(labels = ~ project_phase_lump, values = ~sum_hrs, type = 'pie') %>%
           layout(title = '% of total hours by project phase',
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  })
  
  ########################
  # horizontal bar chart #
  ########################
  output$barChart <- renderPlotly({
    bar <- tracker %>%
      # dont want to show randomization and prof dev here, just projects with a current status
      drop_na(current_status) %>% 
      filter(current_status != "Ongoing",
             statistician %in% input$statistician, 
             date >= input$years[1],
             date <= input$years[2],
             # pi %in% input$PI_choice
             ) %>% 
      group_by(pi, current_status, as_of, study_title, statistician, final_product) %>% 
      summarize(sum_hrs = sum(hours, na.rm = TRUE)) %>% 
      mutate(status_desc = paste('<br>PI: ', pi, 
                                 '<br>Hours: ', sum_hrs,
                                 '<br>Status: ', current_status, ' (', as_of, ')', '<br>Final product: ', final_product)) 
      
    
      #bars shrink in width when bar is also grouped by status -- why?? #changed barmode from group to relative
      
      plot_ly(data = bar, y = ~study_title, x = ~sum_hrs, type = 'bar', split = ~pi, hoverinfo = "text", 
              text = ~status_desc, height = 800) %>%
        layout(yaxis = list(title = "", categoryorder = "category descending", categoryarray = order, type = "category", autorange = "reversed"),
               xaxis = list(title = 'Number of hours'), barmode = 'relative', showlegend = FALSE,
               autosize = F, margin = list(l = 235))
  })
  
  ######################
  #    Gantt chart     #
  ######################
  output$GanttChart <- renderPlot({
    phase <- tracker %>% 
      mutate(pi_last =  gsub(",", "",word(pi,1)), 
             status = paste('Statistician:', statistician, 
                            '<br> PI: ', pi, 
                            '<br> Status: ', current_status, 
                            ' (', as_of, ')')) %>%
      filter(is.na(project_phase) == FALSE, 
             statistician %in% input$statistician, 
             date >= input$years[1],
             date <= input$years[2], 
             # pi %in% input$PI_choice
             ) %>% 
      select(study_title, date, project_phase, pi_last, status) %>% 
      group_by(study_title, project_phase, pi_last, status) %>% 
      dplyr::summarize(start_dt = as.Date(min(date)), end_dt = as.Date(max(date))) %>% 
      mutate(end_dt = as.Date(ifelse(start_dt == end_dt, end_dt + days(1), end_dt), origin = "1970-01-01"))
    
    #get start date for each project and order y-axis by that variable by making
    #a factor variable
    phase1b <- phase %>% 
      ungroup() %>%
      group_by(study_title, pi_last) %>%
      dplyr::summarize(overall_start = as.Date(min(start_dt))) %>% 
      ungroup() %>% 
      arrange(desc(overall_start)) %>% 
      mutate(y_new = factor(study_title, levels = study_title[order(desc(overall_start))])) 
    
    #merge onto dataset
    phase1c <- merge(phase, phase1b, by = c("study_title", "pi_last"))
    
    #summarize with min and max of all dates
    phase2 <- phase1c %>% 
      group_by() %>% 
      mutate(mindt = min(start_dt), maxdt = max(end_dt))
    
    # sort by selected variable (project start time, PI)
    switch(input$stratify,
           "Project start time" = phase2 %>% arrange(mindt),
           "PI" = phase2 %>% arrange(pi_last))
    
    ggplot(phase2) +
      geom_segment(aes(x = start_dt, xend = end_dt, 
                       y = y_new, yend = y_new, 
                       colour = project_phase), size = 10) + #, alpha = status), size = 10) +
      theme_bw() +
      theme(axis.title = element_blank(), 
            legend.position = "bottom",
            legend.title = element_blank()) +
      scale_x_date(breaks = "2 months", date_labels = "%b %Y") +
      # facet_grid(switch(input$stratify,
      #                   "PI" = pi_last ~ ., 
      #                   "Status" = status ~.),
      #            space  =  "free", scales = "free_y") +
      scale_colour_discrete(breaks = c("Project planning", "Grant preparation", "Analysis", "Manuscript preparation", "Revisions"))  +  
      scale_alpha_discrete(range = c(1, 0.55), guide = FALSE)
  },
    height = 1050)
  
}) #end of shiny server function, do not delete