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
      uiOutput("years"),
      
      # width of sidebarPanel
      width = 3
    ),
    
    #
    mainPanel(
      tabsetPanel(
        tabPanel("Percent effort",tags$br(),
                 radioButtons("stratify_pct_effort", label = h4("Stratify by: "),
                              choices = c("PI", "Project", "Phase"), inline = TRUE),
                 textOutput("pie_text"),
                 plotlyOutput("pieChart")),
        
        tabPanel("Hours by Project",tags$br(),
                 radioButtons("status_filter_bar", label = h4("Filter: "),
                              choices = c("Active projects", "All projects"), inline = TRUE),
                 textOutput("bar_text"),
                 plotlyOutput("barChart")),
        
        tabPanel("Timeline",tags$br(),
                 radioButtons("status_filter_gantt", label = h4("Filter: "),
                              choices = c("Active projects", "All projects"), inline = TRUE),
                 textOutput("gantt_text"),
                 plotOutput("GanttChart"))
        )
      )
    )))

