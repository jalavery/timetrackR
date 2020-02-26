#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
library(DT)
library(rsconnect)
library(shiny)
library(ggplot2)

# load saved dataset with time tracking information
load("tracker.rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # statistician names
  output$statistician <- renderUI({
    selectizeInput("statistician", label = "Statistician(s)", 
                   choices = unique(tracker$statistician),
                   selected = unique(tracker$statistician)[c(1,2)], multiple = TRUE)
  })
    

  # date range of interest
  # start by selecting the previous year and curent year
  output$years <- renderUI({
    dateRangeInput(inputId = "years", 
                   label = "As of date: ",
                   # default time from 1 year prior through current date
                   start = Sys.Date() - years(1), end = Sys.Date(), 
                   min = Sys.Date() - years(3), max = Sys.Date(), 
                   format = "yyyy-mm-dd", startview = "year",
                   separator = " to ", width = NULL, autoclose = TRUE)
  })

  ######################
  #      % effort      #
  ######################
  
  # print summary of figure
  output$pie_text <- renderText({
    paste("The following donut chart shows the breakdown of percent effort by ", 
          input$stratify_pct_effort, " for ",
          paste0(input$statistician, collapse = " and "), 
          " between ",
          paste0(as.character(input$years), collapse = " and ")
    )
  })
  
  # subset data for pie chart based on statistician, dates, and project status
  output$pieChart <- renderPlotly({
    pie <- tracker %>% 
      drop_na(date) %>% 
      filter(statistician %in% input$statistician,
             # putting req around input removes warning about length
                    date >= req(input$years[1]),
                    date <= req(input$years[2])
      )
    
  # change grouping to sum depending on which summary level is selected
  switch(input$stratify_pct_effort,
         "PI" = pie %>% 
           drop_na(hours, pi) %>% 
           # collapse projects PIs accounting for <3% of time
           mutate(pi_lump = fct_lump(pi, prop = 0.03, w = hours)) %>% 
           group_by(pi_lump, statistician) %>%
           # re-calculate total number of hours across statistician/proj selected (have to recalc if >1 stat per proj)
           summarize(sum_hrs = sum(hours, na.rm = TRUE)) %>%
           plot_ly(labels = ~pi_lump, values = ~sum_hrs) %>%
           add_pie(hole = 0.6) %>%
           layout(#title = "Percent Effort",
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
         
         "Project" = pie %>% 
           # collapse projects accounting for <3% of time in interval
           drop_na(hours, study_title) %>% 
           mutate(study_title_lump = fct_lump(study_title, prop = 0.03, w = hours)) %>% 
           group_by(study_title_lump, statistician) %>% 
           # re-calculate total number of hours across statistician/proj selected (have to recalc if >1 stat per proj)
           summarize(sum_hrs = sum(hours, na.rm = TRUE)) %>% 
           plot_ly(labels = ~ study_title_lump, values = ~ sum_hrs) %>%
           add_pie(hole = 0.6) %>% 
           layout(title = '% of total hours by project',
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
         
         "Phase" = pie %>% 
           drop_na(hours, project_phase) %>% 
           mutate(project_phase = case_when(is.na(project_phase) ~ "Other",
                                            TRUE ~ project_phase)) %>% 
           # collapse tasks accounting for <3% of time
           mutate(project_phase_lump = fct_lump(project_phase, prop = 0.03, w = hours)) %>% 
           group_by(project_phase_lump, statistician) %>% 
           # re-calculate total number of hours across statistician/proj selected (have to recalc if >1 stat per proj)
           summarize(sum_hrs = sum(hours, na.rm = TRUE)) %>% 
           plot_ly(labels = ~ project_phase_lump, values = ~sum_hrs) %>%
           add_pie(hole = 0.6) %>%
           layout(title = '% of total hours by project phase',
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  })
  
  ########################
  # horizontal bar chart #
  ########################
  
  # print summary of figure
  output$bar_text <- renderText({
    paste("The following bar chart shows the total number of hours per project for ", 
          str_to_lower(input$status_filter_bar), " for ",
          paste0(input$statistician, collapse = " and "), 
          " between ",
          paste0(as.character(input$years), collapse = " and ")
    )
  })
  
  # figure
  output$barChart <- renderPlotly({
    switch(input$status_filter_bar,
           "Active projects" = bar_filtered <- tracker %>% 
             filter(status == "Active"),
           "All projects" = bar_filtered <- tracker)
    
    bar <- bar_filtered %>%
      # dont want to show randomization and prof dev here, just projects with a current status
      drop_na(current_status) %>% 
      filter(current_status != "Ongoing",
             statistician %in% input$statistician,
             date >= input$years[1],
             date <= input$years[2],
             ) %>%
      group_by(pi, current_status, as_of, study_title, statistician, final_product) %>% 
      summarize(sum_hrs = sum(hours, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(# order study title by PI to group in order long the axis
        study_title_factor = fct_reorder(study_title, pi),
        status_desc = paste0('<br>PI: ', pi, 
                             '<br>Hours: ', sum_hrs,
                             '<br>Status: ', current_status, 
                             ' (', as_of, ')', '<br>Final product: ', 
                             final_product))
    
      # create barchart for total number of hours
      plot_ly(data = bar, y = ~study_title_factor, x = ~sum_hrs, 
              type = 'bar', split = ~pi, hoverinfo = "text", 
              text = ~status_desc, height = "80%") %>%
        layout(yaxis = list(title = "", categoryorder = "array", 
                            categoryarray = order, type = "category", autorange = "reversed"),
               xaxis = list(title = 'Number of hours'), barmode = 'relative', showlegend = FALSE,
               autosize = T)#, margin = list(l = 235))
  })
  
  ######################
  #    Gantt chart     #
  ######################
  
  # print summary of figure
  output$gantt_text <- renderText({
    paste("The following Gantt chart shows the timeline for ",
          str_to_lower(input$status_filter_gantt), " for ",
          paste0(input$statistician, collapse = " and "), 
          " between ",
          paste0(as.character(input$years), collapse = " and ")
    )
  })
  
  # figure
  output$GanttChart <- renderPlot({
    phase <- tracker %>%
      filter(is.na(project_phase) == FALSE, 
               !(project_phase %in% c("Departmental seminars, service", "Professional development", "Randomization")),
               statistician %in% input$statistician,
               date >= input$years[1],
               date <= input$years[2]) %>% 
      mutate(project_phase = factor(case_when(project_phase %in% c("Abstract", "Conference") ~ "Conference",
                                         project_phase %in% c("Project closeout", "Publication") ~ as.character(NA),
                                         project_phase %in% c("Grant preparation", "Project planning", "Protocol development") ~ "Project planning",
                                         TRUE ~ project_phase), levels = c("Project planning", "Analysis", "Manuscript preparation", "Revisions", "Re-analysis"))) %>% 
      drop_na(project_phase) %>% 
      # select(project_phase) %>% gtsummary::tbl_summary()
      # filter(study_title %in% c("Lung SMARCA4", "GENIE BPC")) %>% 
      arrange(pi, study_title, date) %>% 
      group_by(study_title, pi) %>%
      # create numeric phase in order to get start/stop date when switching back and forth between phases
      mutate(project_phase_number = cumsum(project_phase != lag(project_phase, default = first(project_phase))) + 1) %>% 
      ungroup() %>%
      # get the start/stop date of each phase
      group_by(pi, study_title, project_phase_number, project_phase, status) %>% 
      summarize(start_dt = as.Date(min(date)), 
                end_dt = as.Date(max(date))) %>% 
      # if start/stop date are the same, add 1 day so that it shows up on figure
      mutate(end_dt = as.Date(ifelse(start_dt == end_dt, 
                                     end_dt + days(1), 
                                     end_dt), origin = "1970-01-01")) %>% 
      # get overall project start and stop dates
      ungroup() %>%
      group_by(pi, study_title) %>%
      mutate(overall_start = as.Date(min(start_dt))) %>% 
      # order projects by start time
      ungroup() %>% 
      mutate(study_title = fct_reorder(study_title, desc(start_dt)))
    
    # filter by selected variable (active projects vs all projects)
    switch(input$status_filter_gantt,
           "Active projects" = phase_filtered <- phase %>% 
             filter(status == "Active"),
           "All projects" = phase_filtered <- phase)
    
    # create Gantt chart
    ggplot(phase_filtered) +
      geom_segment(aes(x = start_dt, xend = end_dt, 
                       y = study_title, yend = study_title, 
                       colour = project_phase), size = 4) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            axis.title = element_blank(), 
            axis.ticks = element_blank(),
            axis.text = element_text(size = 11)) +
      scale_x_date(breaks = "3 months", date_labels = "%b %Y") 
    })
}) #end of shiny server function, do not delete