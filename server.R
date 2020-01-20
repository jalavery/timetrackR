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


library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
library(DT)
library(rsconnect)
library(shiny)
library(ggplot2)

load("tracker.rdata")
load("active.rdata")
load("completed.rdata")
load("upcoming.rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## options for statisticians (remove if this is a drop down instead, have to change UI)
  output$statistician <- renderUI({
    statisticians <- c("Jessica Lavery","Renee Gennarelli","Mike Curry")
    selectizeInput("statistician", label = "Statistician(s)", choices = statisticians,
                   selected = statisticians[c(1,3)], multiple = TRUE)
  })
    
  # get list of PIs based on statistician choice
  output$PI_choice <- renderUI({
    PIs <- tracker %>% filter(statistician %in% input$statistician) %>%
      distinct(PI) %>% pull()
    
    selectizeInput("PI_choice", label = "Investigator(s)", choices = PIs,
                   selected = PIs[], multiple = TRUE)
  })
  
  #get list of available years for menu on side
  output$years <- renderUI({
    years = unique(lubridate::year(tracker$proj_start))
    
    selectizeInput("years", label = "Year project started", choices = years,
                   selected = years[1:2], multiple = TRUE)
  })
  
  ######################
  #active project table#
  ######################
  active_projs = reactive({
    active %>% filter(statistician %in% input$statistician, PI %in% input$PI_choice,
                          !is.na(proj_start),
                          Hours > 0,
                          lubridate::year(proj_start) %in% input$years) %>% 
      mutate(`Statistician` = init,`Start date` = start_dt) %>% 
      select("PI","Study title","Current status","Start date",
             "Hours","Statistician") 
  })
  
  
  output$table <- DT::renderDataTable({
    DT::datatable(active_projs(),rownames = FALSE) #rownames=FALSE removes obs #
  })
  # output$table <- renderTable(active_projs(),colnames=TRUE)
  
  #########################
  #upcoming project table#
  #########################
  upcoming_projs = reactive({
    upcoming %>% filter(statistician %in% input$statistician, PI %in% input$PI_choice,
                      !is.na(proj_start),
                      Hours > 0,
                      lubridate::year(proj_start) %in% input$years) %>% 
      mutate(`Statistician` = init,`Start date` = start_dt) %>% 
      select("Statistician", "PI", "Study title", "Current status") 
  })
  
  output$table_upcoming <- DT::renderDataTable({
    DT::datatable(upcoming_projs(),rownames = FALSE) #rownames=FALSE removes obs #
  })
  
  #########################
  #completed project table#
  #########################
  inactive_projs = reactive({ 
    inactive %>% 
      filter(statistician %in% input$statistician, PI %in% input$PI_choice, lubridate::year(start_dt) %in% input$years) %>%
      mutate(`Statistician` = statistician,`Start date` = start_dt,`End date` = end_dt,`Weeks to completion` = weekstocompletion) %>% 
      # select(-statistician, -start_dt, -end_dt, -weekstocompletion)
      select("PI","Study title","Current status","Start date","Hours",`Statistician`)
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
    pie <- tracker %>% filter(statistician %in% input$statistician, year(proj_start) %in% input$years) %>% 
      group_by(PI,statistician) %>% summarize(sum_hrs = sum(Hours.x,na.rm = TRUE))
      plot_ly(data = pie, labels = ~PI, values = ~sum_hrs, type = 'pie') %>%
      layout(title = '% of total hours by PI',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))})
  
  ######################
  #      bar chart     #
  ######################
  output$barChart <- renderPlotly({
    bar <- tracker %>%
      mutate(status = paste('Statistician:', statistician, '<br>PI: ', tracker$PI, '<br>Status: ', `Current status`, ' (', `As of`, ')', '<br>Final product: ', `Final product`,sep = "")) %>% 
      filter(statistician %in% input$statistician, year(proj_start) %in% input$years, PI %in% input$PI_choice) %>% 
      group_by(PI,status,Project,statistician,`Final product`) %>% summarize(sum_hrs = sum(Hours.x,na.rm = TRUE))
    
      #bars shrink in width when bar is also grouped by status -- why?? #changed barmode from group to relative
      
      plot_ly(data = bar, y = ~Project, x = ~sum_hrs, type = 'bar',split = ~PI, hoverinfo = "text", 
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
      mutate(PI_last =  gsub(",", "",word(PI,1)), status = paste('Statistician:', statistician, '<br>PI: ', tracker$PI, '<br>Status: ', `Current status`, ' (', `As of`, ')',sep = "")) %>%
      filter(is.na(`Project phase`) == FALSE, statistician %in% input$statistician, year(proj_start) %in% input$years, PI %in% input$PI_choice) %>% 
      mutate(ProjPhase = `Project phase`,status = ifelse(completed == 1,"Completed","In progress")) %>% 
      select(Project,Date,ProjPhase,PI_last,status) %>% 
      group_by(Project, ProjPhase,PI_last,status) %>% 
      dplyr::summarize(start_dt = as.Date(min(Date)), end_dt = as.Date(max(Date))) %>% 
      mutate(end_dt = as.Date(ifelse(start_dt == end_dt, end_dt + days(1), end_dt), origin = "1970-01-01"))
    
    #get start date for each project and order y-axis by that variable by making
    #a factor variable
    phase1b <- phase %>% 
      ungroup() %>%
      group_by(Project, PI_last) %>%
      dplyr::summarize(overall_start = as.Date(min(start_dt))) %>% 
      ungroup() %>% 
      arrange(desc(overall_start)) %>% 
      mutate(y_new = factor(Project, levels = Project[order(desc(overall_start))])) 
    
    #merge onto dataset
    phase1c <- merge(phase,phase1b,by = c("Project","PI_last"))
    
    #summarize with min and max of all dates
    phase2 <- phase1c %>% 
      group_by() %>% 
      mutate(mindt = min(start_dt), maxdt = max(end_dt))
    
    ggplot(phase2) +
      geom_vline(xintercept = as.Date("2018-06-01"),color = "gray") +
      geom_vline(xintercept = as.Date("2017-05-01"),color = "gray") +
      geom_segment(aes(x = start_dt, xend = end_dt, y = y_new, yend = y_new, colour = ProjPhase, alpha = status), size = 10) +
      theme_bw() +
      theme(axis.title = element_blank(), legend.position = "bottom",legend.title = element_blank()) +
      scale_x_date(breaks = "2 months", date_labels = "%b %Y") +
      facet_grid(switch(input$stratify,"PI" = PI_last~., "Status" = status~.),space  =  "free", scales = "free_y") +
      scale_colour_discrete(breaks = c("Project planning","Grant preparation","Analysis","Manuscript preparation","Revisions"))  +  
      scale_alpha_discrete(range = c(1, 0.55), guide = FALSE)
  },
    height = 1050)
  
}) #end of shiny server function, do not delete