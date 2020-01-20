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
biostats <- c("Jessica Lavery","Renee Gennarelli","Mike Curry")


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
        tabPanel("Active projects", 
                 # fluidRow(DT::dataTableOutput('table'))),
                 dataTableOutput('table')),
        #plotlyOutput("activePlot")),
        tabPanel("Completed projects",
                 dataTableOutput('table2')),
        tabPanel("Hours by PI",
                 #plotlyOutput("hrsPlot"),
                 #potOutput("hrsPlot"),
                 plotlyOutput("pieChart")),#by PI
        tabPanel("Hours by Project",
                 plotlyOutput("barChart")) #by Project
      )
    )  
    
    #selectInput(inputID="stat_box",label="Biostatistician",choices=list("Jessica","Renee","Mike"))
  )))

