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

#data management
biostats <- c("Jessica Lavery", "Renee Gennarelli", "Mike Curry")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("HORG Biostatistics Project Overview"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      #select multiple statisticians
      uiOutput("statistician"),
      
      #drop down menu for statistician
      # selectInput("statistician", label = "Biostatistician",
      #             choices = biostats, selected = "Jessica Lavery"),
      
      #drop down menu for PIs based on statistician
      uiOutput("PI_choice"),
      
      #drop down menu for projects based on statistician
      # uiOutput("proj_choice")
      
      #drop down menu for years available
      uiOutput("years")
    ),
    
    #   # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Active", 
                 # fluidRow(DT::dataTableOutput('table'))),
                 DT::dataTableOutput('table')),
        #plotlyOutput("activePlot")),
        tabPanel("Upcoming",tags$br(),
                 DT::dataTableOutput('table_upcoming')),
        tabPanel("Inactive",
                 DT::dataTableOutput('table2')),
        tabPanel("Hours by PI",tags$br(),
                 #plotlyOutput("hrsPlot"),
                 #potOutput("hrsPlot"),
                 plotlyOutput("pieChart")),#by PI
        tabPanel("Hours by Project",tags$br(),
                 plotlyOutput("barChart")), #by Project
        tabPanel("Timeline",tags$br(),
                 radioButtons("stratify", label = h4("Stratify plot by:"), 
                              choices = c("PI","Status"),inline = TRUE),
                 plotOutput("GanttChart"))
                 

      )
    )  
    
    #selectInput(inputID="stat_box",label="Biostatistician",choices=list("Jessica","Renee","Mike"))
  )))

