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
  titlePanel("timetrackR"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      # file input
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # date range of interest
      uiOutput("years"),
      
      # width of sidebarPanel
      width = 3
    ),
    
    #
    mainPanel(
      tabsetPanel(
        tabPanel("Percent effort", tags$br(),
                 radioButtons("stratify_pct_effort", label = h4("Stratify by: "),
                              choices = c("Client", "Project", "Phase", "Task"), inline = TRUE),
                 textOutput("pie_text"),
                 plotlyOutput("pieChart")),
        
        tabPanel("Hours by Project", tags$br(),
                 radioButtons("status_filter_bar", label = h4("Filter: "),
                              choices = c("Client", "Project"), inline = TRUE),
                 textOutput("bar_text"),
                 plotlyOutput("barChart")),
        
        tabPanel("Timeline", tags$br(),
                 textOutput("gantt_text"),
                 plotOutput("GanttChart"))
        )
      )
    )))

