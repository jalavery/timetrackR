#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)

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
                       start = Sys.Date() - months(6), end = Sys.Date() + 1,
                       min = Sys.Date() - years(3), max = Sys.Date() + 1,
                       format = "yyyy-mm-dd", startview = "year",
                       separator = " to ", width = NULL, autoclose = TRUE),
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Client", tabName = "client", icon = icon("th")),
            menuItem("Project", tabName = "project", icon = icon("th")),
            menuItem("Phase", tabName = "phase", icon = icon("th")),
            menuItem("Task", tabName = "task", icon = icon("th")),
            menuItem("About", tabName = "about", icon = icon("info"))
        )
    ),
    dashboardBody(
        tabItems(
            # Dashboard tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(title = "Instructions",
                            "This app is based on tracking your time in Toggl and exporting that data for custom visualizations.",
                            br(),
                            "To prepare your data for use in the app:", br(), br(),
                            "1. Navigate to toggl.com", br(), br(),
                            "2. Navigate to the Reports menu item", br(), br(),
                            "3. At the top of the page, go to Detailed reports", br(), br(),
                            "4. On the top right, hit the download button to download the CSV file of your logged hours.", br(), br(),
                            "5. Upload that CSV file in timetrackR and select the summary level on the side bar panel to the left.")
                    )
            ),
            
            # Client tab content
            tabItem(tabName = "client",
                    fluidRow(
                        box(
                            title = "Percent Effort", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            textOutput("pie_text"),
                            plotlyOutput("pieChart_client"),
                            width = 6, height = 500
                        ),
                        
                        box(
                            title = "Total Hours", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput("barChart_client"),
                            width = 6, height = 500
                        ), 
                        
                        box(
                            title = "Project Timeline", status = "warning", solidHeader = TRUE,
                            textOutput("output$gantt_text"),
                            plotOutput("timeline_client"),
                            width = 12
                        )
                    )
            ),
            
            # Project tab content
            tabItem(tabName = "project",
                    h2("Widgets tab"),
                    plotOutput("plot2")
            # fluidRow(
                # box(
                    # title = "Percent Effort", status = "primary", solidHeader = TRUE,
                    # collapsible = TRUE,
                    # textOutput("pie_text"),
                    # renderPlot("plot1"),
                    # plotlyOutput("pieChart_client"),
                    # width = 6, height = 500
                # )
            # )
            ),
            
            # Phase tab content
            tabItem(tabName = "phase",
                    h2("Phase tab content")
            ),
            
            # Task tab content
            tabItem(tabName = "task",
                    h2("Task tab content")
            ),
            
            # About tab content
            tabItem(tabName = "about",
                    fluidRow(
                        box(title = "About timetrackR", 
                            "This dashboard was built in RStudio primarily with shiny, shinydashboard, and the tidyverse."
                            ),
                        box(title = "About me"
                            )
            ))
        )
    )
)