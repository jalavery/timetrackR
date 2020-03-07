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
# load("tracker_toggl.rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  tracker_toggl <- reactive({
    
    req(input$file1)
    
    df <- read_csv(input$file1$datapath,
                              # header = input$header,
                              # sep = input$sep,
                              # quote = input$quote
    ) %>%
      mutate(current_status = "Active",
             # change duration to hours
             Duration = period_to_seconds(hms(Duration))/(60^2))
    
    return(df)
  })
  

  # date range of interest
  # start by selecting the previous year and curent year
  output$years <- renderUI({
    dateRangeInput(inputId = "years", 
                   label = "Date range: ",
                   # default time from 1 year prior through current date
                   start = Sys.Date() - months(6), end = Sys.Date() + 1, 
                   min = Sys.Date() - years(3), max = Sys.Date() + 1, 
                   format = "yyyy-mm-dd", startview = "year",
                   separator = " to ", width = NULL, autoclose = TRUE)
  })
  
  output$dateRangeText2 <- renderText({
    paste("input$years is", 
          as.character(input$years)
    )
  })

  ######################
  #      % effort      #
  ######################
  
  # print summary of figure
  output$pie_text <- renderText({
    paste("The following donut chart shows the breakdown of percent effort by ",
          input$stratify_pct_effort,
          " between ", 
          paste0(format(input$years, "%b %d, %Y"), collapse = " and "),
          ". Note that projects representing 3% or less are collapsed into the 'Other' category."
    )
  })
  
  # subset data for pie chart based on dates, and project status
  output$pieChart <- renderPlotly({
    pie <- tracker_toggl() %>% 
      drop_na(`Start date`) %>% 
      filter(# putting req around input removes warning about length
            `Start date` >= req(input$years[1]),
            `Start date` <= req(input$years[2])
      )
    
  # change grouping to sum depending on which summary level is selected
  switch(input$stratify_pct_effort,
          "PI" = pie %>% 
           drop_na(Duration, Client) %>% 
           # collapse projects PIs accounting for <3% of time
           mutate(pi_lump = fct_lump(Client, prop = 0.03, w = Duration)) %>% 
           group_by(pi_lump) %>%
           # re-calculate total number of hours across proj selected (have to recalc if >1 stat per proj)
           summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>%
           plot_ly(labels = ~pi_lump, values = ~sum_hrs) %>%
           add_pie(hole = 0.6) %>%
           layout(#title = "Percent Effort",
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
         
         "Project" = pie %>% 
           # collapse projects accounting for <3% of time in interval
           drop_na(Duration, Project) %>% 
           mutate(Project_lump = fct_lump(Project, prop = 0.03, w = Duration)) %>% 
           group_by(Project_lump) %>% 
           # re-calculate total number of hours across proj selected (have to recalc if >1 stat per proj)
           summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
           plot_ly(labels = ~ Project_lump, values = ~ sum_hrs) %>%
           add_pie(hole = 0.6) %>% 
           layout(#title = '% of total hours by project',
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
         
         "Phase" = pie %>% 
           drop_na(Duration, Tags) %>% 
           mutate(Tags = case_when(is.na(Tags) ~ "Other",
                                            TRUE ~ Tags)) %>% 
           # collapse tasks accounting for <3% of time
           mutate(Tags_lump = fct_lump(Tags, prop = 0.03, w = Duration)) %>% 
           group_by(Tags_lump) %>% 
           # re-calculate total number of hours across proj selected (have to recalc if >1 stat per proj)
           summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
           plot_ly(labels = ~ Tags_lump, values = ~sum_hrs) %>%
           add_pie(hole = 0.6) %>%
           layout(#title = '% of total hours by project Tags',
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  })
  
  ########################
  # horizontal bar chart #
  ########################
  
  # print summary of figure
  output$bar_text <- renderText({
    paste("The following bar chart shows the total number of hours per project for ", 
          str_to_lower(input$status_filter_bar),
          " between ",
          paste0(format(input$years, "%b %d, %Y"), collapse = " and ")
    )
  })
  
  # figure
  output$barChart <- renderPlotly({
    switch(input$status_filter_bar,
           "Active projects" = bar_filtered <- tracker_toggl() %>% 
             filter(current_status == "Active"),
           "All projects" = bar_filtered <- tracker_toggl())
    
    bar <- bar_filtered %>%
      # dont want to show randomization and prof dev here, just projects with a current status
      drop_na(current_status) %>% 
      filter(current_status != "Ongoing",
             `Start date` >= input$years[1],
             `Start date` <= input$years[2],
             ) %>%
      group_by(Client, current_status, Project) %>% 
      summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(# order study title by PI to group in order long the axis
        Project_factor = fct_reorder(Project, Client),
        status_desc = paste0('<br>PI: ', Client, 
                             '<br>Hours: ', sum_hrs,
                             '<br>Status: ', current_status))
    
      # create barchart for total number of hours
      plot_ly(data = bar, y = ~Project_factor, x = ~sum_hrs, 
              type = 'bar', split = ~Client, hoverinfo = "text", 
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
    paste("The following project timeline shows data for ",
          str_to_lower(input$status_filter_gantt), 
          " between ",
          paste0(format(input$years, "%b %d, %Y"), collapse = " and ")
    )
  })
  
  # figure
  output$GanttChart <- renderPlot({
    for_timeline <- tracker_toggl() %>%
      filter(is.na(Tags) == FALSE, 
               !(Tags %in% c("Departmental seminars, service", "Professional development", "Randomization")),
               `Start date` >= input$years[1],
               `Start date` <= input$years[2]
             ) %>% 
      mutate(Tags = factor(case_when(Tags %in% c("Abstract", "Conference") ~ "Conference",
                                         Tags %in% c("Project closeout", "Publication") ~ as.character(NA),
                                         Tags %in% c("Grant preparation", "Project planning", "Protocol development") ~ "Project planning",
                                         TRUE ~ Tags), levels = c("Project planning", "Analysis", "Manuscript preparation", "Revisions", "Re-analysis"))) %>% 
      drop_na(Tags) %>% 
      # select(Tags) %>% gtsummary::tbl_summary()
      # filter(Project %in% c("Lung SMARCA4", "GENIE BPC")) %>% 
      arrange(Client, Project, `Start date`) %>% 
      group_by(Project, Client) %>%
      # create numeric Tags in order to get start/stop `Start date` when switching back and forth between Tagss
      mutate(Tags_number = cumsum(Tags != lag(Tags, default = first(Tags))) + 1) %>% 
      ungroup() %>%
      # get the start/stop `Start date` of each Tags
      group_by(Client, Project, Tags_number, Tags, current_status) %>% 
      summarize(start_dt = as.Date(min(`Start date`)), 
                end_dt = as.Date(max(`Start date`))) %>% 
      # if start/stop `Start date` are the same, add 1 day so that it shows up on figure
      mutate(end_dt = as.Date(ifelse(start_dt == end_dt, 
                                     end_dt + days(1), 
                                     end_dt), origin = "1970-01-01")) %>% 
      # get overall project start and stop dates
      ungroup() %>%
      group_by(Client, Project) %>%
      mutate(overall_start = as.Date(min(start_dt))) %>% 
      # order projects by start time
      ungroup() %>% 
      mutate(Project = fct_reorder(Project, desc(start_dt)))
    
    # create Gantt chart
    ggplot(for_timeline) +
      geom_segment(aes(x = start_dt, xend = end_dt, 
                       y = Project, yend = Project, 
                       colour = Tags), size = 4) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            axis.title = element_blank(), 
            axis.ticks = element_blank(),
            axis.text = element_text(size = 12)) +
      scale_x_date(breaks = "3 months", date_labels = "%b %Y") 
    })
}) #end of shiny server function, do not delete