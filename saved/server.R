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

library(shiny)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## options for statisticians (remove if this is a drop down instead, have to change UI)
  output$statistician <- renderUI({
    statisticians <- c("Jessica Lavery","Renee Gennarelli","Mike Curry")
    selectizeInput("statistician", label = "Statistician(s)", choices = statisticians,
                   selected = statisticians[1], multiple = FALSE)
  })
  
  # get list of PIs based on statistician choice
  output$PI_choice <- renderUI({
    PIs <- JL %>% filter(statistician.x %in% input$statistician) %>%
      distinct(PI) %>% pull()
    
    selectizeInput("PI_choice", label = "Investigator(s)", choices = PIs,
                   selected = PIs[], multiple = TRUE)
  })
  
  ## get list of projects based on statistician and PI choice
  # output$proj_choice <- renderUI({
  #   projs = JL %>% filter(statistician %in% input$statistician & PI %in% input$PI_choice) %>% 
  #     distinct(ttl) %>% pull()
  #   
  #   selectizeInput("proj_choice", label = "Select Project", choices = projs,
  #                  selected = projs[1], multiple = TRUE)
  # })
  
  #get list of available years for menu on side
  output$years <- renderUI({
    years=unique(lubridate::year(JL2$`Project initiated`))
    
    selectizeInput("years", label = "Year project started", choices = years,
                   selected = years[1:2], multiple = TRUE)
  })
  
  ######################
  #active project table#
  ######################
  active_projs = reactive({
    JL2_active %>% filter(statistician %in% input$statistician, PI %in% input$PI_choice,
                          !is.na(`Project initiated`),
                          Hours>0,
                          lubridate::year(`Project initiated`) %in% input$years) %>% 
      mutate(`Statistician`=statistician,`Start date`=start_dt) %>% 
      select("Statistician","PI","Study title","Start date","Current status",
             "Hours") 
  })
  
  
  output$table <-DT::renderDataTable({
    DT::datatable(active_projs(),rownames = FALSE) #rownames=FALSE removes obs #
  })
  # output$table <- renderTable(active_projs(),colnames=TRUE)
  
  
  ## first plot: histogram of hours spent
  # output$hrsPlot <-
  #   renderPlotly({
  #     project_data  <-  JL %>%
  #         filter(statistician %in% input$statistician & PI %in% input$PI_choice
  #                & ttl %in% input$proj_choice)
  # 
  #   plot_ly(data = project_data , y = ~Hours, color = ~PI, type = "box") #%>%
  #   layout(legend = list(x = 0.2, y = 1.0))
  # })
  
  #########################
  #completed project table#
  #########################
  completed_projs = reactive({ 
    JL2_completed %>% 
      filter(statistician %in% input$statistician, PI %in% input$PI_choice, lubridate::year(start_dt) %in% input$years) %>%
      mutate(`Statistician`=statistician,`Start date`=start_dt,`End date`=end_dt,`Weeks to completion`=weekstocompletion) %>% 
      # select(-statistician, -start_dt, -end_dt, -weekstocompletion)
      select(`Statistician`,"PI","Study title","Hours","Start date","End date","Weeks to completion")
  })
  
  # active_projs2 <- active_projs[,c("PI","Study title","Project initiated","Hours")]
  output$table2 <-DT::renderDataTable({
    DT::datatable(completed_projs(),rownames = FALSE)
  })
  # output$table2 <- renderTable(completed_projs(),colnames = TRUE)
  
  ######################
  #      pie chart     #
  ######################
  output$pieChart <- renderPlotly({
    JL %>% filter(statistician.x %in% input$statistician, year(`Project initiated`) %in% input$years) %>% 
      group_by(PI,statistician.x) %>% summarize(sum_hrs=sum(Hours.x,na.rm = TRUE)) %>% 
      plot_ly(labels = ~PI, values = ~sum_hrs, type = 'pie') %>%
      layout(title = '% of total hours by PI',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))})
  
  ######################
  #      bar chart     #
  ######################
  output$barChart <- renderPlotly({
    JL %>% filter(statistician.x %in% input$statistician, year(`Project initiated`) %in% input$years, PI %in% input$PI_choice) %>% 
      group_by(PI,Project, statistician.x) %>% summarize(sum_hrs=sum(Hours.x,na.rm = TRUE)) %>% 
      plot_ly(y = ~Project, x = ~sum_hrs, type = 'bar',split=~PI,hoverinfo=text, 
              text=~paste('PI: ', PI, '</br> Active?')) %>%
      layout(yaxis=list(title="",categoryorder="array",categoryarray=order,type="category",autorange="reversed"),
             xaxis = list(title = 'Number of hours'), barmode = 'group', showlegend=FALSE,
             margin=list(l=235))
  })
  
  
}) #end of shiny server function, do not delete