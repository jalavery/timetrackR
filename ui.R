#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(rsconnect)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Biostatistics Team Time Tracking Summary"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      # select multiple statisticians
      uiOutput("statistician"),
      
      # date range of interest
      uiOutput("years")
    ),
    
    #
    mainPanel(
      tabsetPanel(
        # tabPanel("Active", 
        #          DT::dataTableOutput('table')),
        
        # tabPanel("Inactive",
        #          DT::dataTableOutput('table2')),
        
        tabPanel("Percent effort",tags$br(),
                 radioButtons("stratify_pct_effort", label = h4("Stratify by: "),
                              choices = c("PI", "Project", "Task"), inline = TRUE),
                 plotlyOutput("pieChart")),
        
        tabPanel("Hours by Project",tags$br(),
                 plotlyOutput("barChart")),
        
        tabPanel("Timeline",tags$br(),
                 radioButtons("stratify", label = h4("Arrange plot by:"),
                              choices = c("Project start time","PI"), inline = TRUE),
                 plotOutput("GanttChart"))
        )
      )
    )))

