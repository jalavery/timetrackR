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
  output$PI_choice <- renderUI({
    # filter on the statisticians selected and get unique list of PIs
    pi_list <- tracker %>% 
      filter(statistician %in% input$statistician) %>%
      distinct(pi) %>% 
      select(pi)
    
    selectizeInput("PI_choice", 
                   label = "Investigator(s)", 
                   choices = pi_list,
                   selected = pull(pi_list, pi),
                   multiple = TRUE)
  })
  
  # list of available years for menu on side
  # start by selecting the previous year and curent year
  output$years <- renderUI({
    selectizeInput("years", 
                   label = "Year project started", 
                   choices = sort(unique(lubridate::year(tracker$proj_start))),
                   selected = c(year(Sys.Date()) - 1, year(Sys.Date())), 
                   multiple = TRUE)
  })
  
  ######################
  #active project table#
  ######################
  active_projs = reactive({
    proj_summary %>% filter(statistician %in% input$statistician, 
                      pi %in% input$PI_choice,
                      lubridate::year(proj_start) %in% input$years,
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
                        pi %in% input$PI_choice,
                        lubridate::year(proj_start) %in% input$years,
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
             pi %in% input$PI_choice, 
             lubridate::year(proj_start) %in% input$years,
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
  output$pieChart <- renderPlotly({
    pie <- tracker %>% 
      filter(statistician %in% input$statistician, 
             year(proj_start) %in% input$years) %>% 
      group_by(pi, statistician) %>% 
      # re-calculate total number of hours across statistician/proj selected (have to recalc if >1 stat per proj)
      summarize(sum_hrs = sum(hours, na.rm = TRUE)) %>% 
      plot_ly(labels = ~ pi, values = ~sum_hrs, type = 'pie') %>%
      layout(title = '% of total hours by PI',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))})
  
  ########################
  # horizontal bar chart #
  ########################
  output$barChart <- renderPlotly({
    bar <- tracker %>%
      mutate(status = paste('Statistician:', statistician, '<br>PI: ', pi, '<br>Status: ', current_status, ' (', as_of, ')', '<br>Final product: ', final_product)) %>% 
      filter(statistician %in% input$statistician, 
             year(proj_start) %in% input$years, 
             pi %in% input$PI_choice) %>% 
      group_by(pi, status, study_title, statistician, final_product) %>% 
      summarize(sum_hrs = sum(hours, na.rm = TRUE))
    
      #bars shrink in width when bar is also grouped by status -- why?? #changed barmode from group to relative
      
      plot_ly(data = bar, y = ~study_title, x = ~sum_hrs, type = 'bar',split = ~pi, hoverinfo = "text", 
              text = ~status, height = 800) %>%
        layout(yaxis = list(title = "", categoryorder = "array", categoryarray = order, type = "category", autorange = "reversed"),
               xaxis = list(title = 'Number of hours'), barmode = 'relative', showlegend = FALSE,
               autosize = F, margin = list(l = 235))
  })
  
  ######################
  #    Gantt chart     #
  ######################
  output$GanttChart <- renderPlot({
    phase <- tracker %>% 
      mutate(pi_last =  gsub(",", "",word(pi,1)), 
             status = paste('Statistician:', statistician, '<br>PI: ', pi, '<br>Status: ', current_status, ' (', as_of, ')')) %>%
      filter(is.na(project_phase) == FALSE, 
             statistician %in% input$statistician, 
             year(proj_start) %in% input$years, 
             pi %in% input$PI_choice) %>% 
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
    phase1c <- merge(phase, phase1b, by = c("study_title","pi_last"))
    
    #summarize with min and max of all dates
    phase2 <- phase1c %>% 
      group_by() %>% 
      mutate(mindt = min(start_dt), maxdt = max(end_dt))
    
    ggplot(phase2) +
      # geom_vline(xintercept = as.Date("2018-06-01"),color = "gray") +
      # geom_vline(xintercept = as.Date("2017-05-01"),color = "gray") +
      geom_segment(aes(x = start_dt, xend = end_dt, 
                       y = y_new, yend = y_new, 
                       colour = project_phase), size = 10) + #, alpha = status), size = 10) +
      theme_bw() +
      theme(axis.title = element_blank(), 
            legend.position = "bottom",
            legend.title = element_blank()) +
      scale_x_date(breaks = "2 months", date_labels = "%b %Y") +
      facet_grid(switch(input$stratify,
                        "PI" = pi_last~., 
                        "Status" = status ~.),
                 space  =  "free", scales = "free_y") +
      scale_colour_discrete(breaks = c("Project planning","Grant preparation", "Analysis", "Manuscript preparation", "Revisions"))  +  
      scale_alpha_discrete(range = c(1, 0.55), guide = FALSE)
  },
    height = 1050)
  
}) #end of shiny server function, do not delete