# load libraries
library(shinydashboard)
library(shiny)
library(plotly)
library(tidyverse)
library(rsconnect)
library(DT)
library(lubridate)
library(ggplot2)

dashboardPage(
    dashboardHeader(title = "timetrackR"),
    ## Sidebar content
    dashboardSidebar(
        fileInput("file1", "Upload CSV File from Toggl",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # uiOutput("years"),
        dateRangeInput(inputId = "years",
                       label = "Date range: ",
                       # default time from 1 year prior through current date
                       start = Sys.Date() - years(1), end = Sys.Date(),
                       min = Sys.Date() - years(3), max = Sys.Date() + 1,
                       format = "yyyy-mm-dd", startview = "year",
                       separator = " to ", width = NULL, autoclose = TRUE),
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("About", tabName = "about", icon = icon("info"))
        )
    ),
    dashboardBody(
        tabItems(
            
            # Dashboard tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        # infoboxes
                        infoBoxOutput("top_client"),
                        infoBoxOutput("top_project"),
                        infoBoxOutput("top_task")
                        ),
                    fluidRow(
                        tabBox(title = "Cumulative Percent Effort",
                            side = "right", height = "250px",
                            selected = "Client",
                            tabPanel("Task", plotlyOutput("pieChart_task")),
                            tabPanel("Phase", plotlyOutput("pieChart_phase")),
                            tabPanel("Project", plotlyOutput("pieChart_proj")),
                            tabPanel("Client", plotlyOutput("pieChart_client"))
                        ),
                        
                        box(
                            title = "Percent Effort Over Time", status = "primary", solidHeader = TRUE,
                            collapsible = FALSE,
                            plotlyOutput("stacked_bar")
                        ),
                        
                        tabBox(title = "Total Hours",
                               side = "right", #height = "250px",
                               selected = "Client",
                               tabPanel("Project", plotlyOutput("barChart_proj")),
                               tabPanel("Client", plotlyOutput("barChart_client"))
                        ),
                        
                        box(
                            title = "Project Timeline", status = "warning", solidHeader = TRUE,
                            textOutput("output$timeline_text"),
                            plotOutput("timeline")
                        ),
                        
                        box("Note that projects representing 3% or less of the total number of hours are collapsed into the 'Other' category for all figures.",
                            width = 12)
                    )
            ),
            
            # About tab content
            tabItem(tabName = "about",
                    fluidRow(
                        box(title = "About timetrackR", status = "warning", solidHeader = TRUE,
                            img(src = 'timetrackR.png', align = "right", width = 100),
                            "timetrackR is a tool to analyze how you are spending your time. The idea is that you track how you are spending your time in toggl, then export the data from toggl into timetrackR for insights about where your time is going.",
                            br(), br(),
                            "This dashboard was built in RStudio primarily with shiny, shinydashboard, and the tidyverse.",
                            br(), br(),
                            "This app was originally presented at",
                            a(href = "https://www.meetup.com/rladies-newyork/events/268481404/", "RLadies New York"),
                            "in February 2020. That presentation and version of the app can be found ",
                            a(href = "https://github.com/jalavery/timetrackR/tree/archive-r-ladies", "here. "),
                            br(), br(),
                            "Following the RLadies presentation, I transitioned timetrackR from a traditional shiny app to a ",
                            a(href = "https://rstudio.github.io/shinydashboard/", "shinydashboard"),
                            " and integrated Toggl exports. An updated version of the presentation can be found ",
                            a(href = "https://github.com/jalavery/timetrackR/blob/master/Strategy%20and%20Innovation%20timetrackR%20Presentation.pptx", "here."),
                            br(), br(),
                            "All code is available on ",
                            a(href = "https://github.com/jalavery/timetrackR", "GitHub.")
                        ),
                        box(title = "How to use timetrackR", status = "primary", solidHeader = TRUE,
                            "This app is based on tracking your time in Toggl and exporting that data for custom visualizations.",
                            "To export your data from Toggl for use in timetrackR:", br(), 
                            "1. From toggl.com: On the lefthand side, select Reports", br(), br(),
                            "2. At the top of the page, select Detailed reports", br(), br(),
                            "3. Change the timeframe from 'This week' to 'This year' or other time period of interest.", br(), br(),
                            "4. On the top right, hit the download button to download the CSV file of your logged hours.", br(), br(),
                            "5. Upload that CSV file in timetrackR and select the summary level on the side bar panel to the left.", br(), br(),
                            "Note: The project timeline distinguishes project phases based on the tag feature of toggl, and assumes 1 tag per time entry.")
                    ))
        )
    )
)