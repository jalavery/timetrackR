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
                        box(
                            title = "Percent Effort", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            radioButtons("stratify_pct_effort", label = h4("Stratify by: "),
                                         choices = c("Client", "Project", "Phase", "Task"), inline = TRUE),
                            textOutput("pie_text"),
                            plotlyOutput("pieChart")#,
                            # width = 6, height = 500
                        ),
                        
                        box(
                            title = "Total Hours", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            radioButtons("status_filter_bar", label = h4("Filter: "),
                                         choices = c("Client", "Project"), inline = TRUE),
                            plotlyOutput("barChart")#,
                            # width = 6, height = 500
                        ), 
                        
                        box(
                            title = "Project Timeline", status = "warning", solidHeader = TRUE,
                            textOutput("output$timeline_text"),
                            plotOutput("timeline")#,
                            # width = 12
                        )
                    )
            ),
            
            # About tab content
            tabItem(tabName = "about",
                    fluidRow(
                        box(title = "About timetrackR", 
                            "This dashboard was built in RStudio primarily with shiny, shinydashboard, and the tidyverse.",
                            br(),
                            <img src = "timetrackR.png">,
                            "This app was originally presented at RLadies New York in February 2020. The presentation can be found [here](www.github.com/jalavery/timetrackr)"
                            ),
                        box(title = "About me"
                            ),
                        box(title = "Instructions",
                            "This app is based on tracking your time in Toggl and exporting that data for custom visualizations.",
                            br(),
                            "To prepare your data for use in the app:", br(), br(),
                            "1. Navigate to toggl.com", br(), br(),
                            "2. Navigate to the Reports menu item", br(), br(),
                            "3. At the top of the page, go to Detailed reports", br(), br(),
                            "4. On the top right, hit the download button to download the CSV file of your logged hours.", br(), br(),
                            "5. Upload that CSV file in timetrackR and select the summary level on the side bar panel to the left.")
            ))
        )
    )
)