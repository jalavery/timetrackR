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
                        box(
                            title = "Percent Effort", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            radioButtons("stratify_pct_effort", label = h4("Stratify by: "),
                                         choices = c("Client", "Project", "Phase", "Task"), inline = TRUE),
                            plotlyOutput("pieChart"),
                            textOutput("pie_text")#,
                            #width = 6, height = 600
                        ),
                        
                        box(
                            title = "Percent Effort Over Time", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput("stacked_bar")#,
                            #textOutput("pie_text")#,
                            #width = 6, height = 600
                        ),
                        
                        box(
                            title = "Total Hours", status = "warning", solidHeader = TRUE,
                            collapsible = TRUE,
                            radioButtons("status_filter_bar", label = h4("Filter: "),
                                         choices = c("Client", "Project"), inline = TRUE),
                            plotlyOutput("barChart"),
                            textOutput("bar_text")#,
                            #width = 6, height = 600
                        ), 
                        
                        box(
                            title = "Project Timeline", status = "warning", solidHeader = TRUE,
                            textOutput("output$timeline_text"),
                            plotOutput("timeline")#, height = "440px")#,
                            # height = 500, width = 12
                        )
                    )
            ),
            
            # About tab content
            tabItem(tabName = "about",
                    fluidRow(
                        box(title = "About timetrackR", 
                            img(src = 'timetrackR.png', align = "right", width = 100),
                            "timetrackR is a tool to analyze how you are spending your time. The idea is that you track how you are spending your time in toggl, then export the data from toggl into timetrackR for insights about where your time is going.",
                            br(), br(),
                            "This dashboard was built in RStudio primarily with shiny, shinydashboard, and the tidyverse.",
                            br(), br(),
                            "This app was originally presented at",
                            a(href = "https://www.meetup.com/rladies-newyork/events/268481404/", "RLadies New York"),
                            "in February 2020. The presentation can be found ",
                            a(href = "www.github.com/jalavery/timetrackr", "here. "),
                            br(), br(),
                            "Following the RLadies presentation, I transitioned timetrackR from a traditional shiny app to a ",
                            a(href = "https://rstudio.github.io/shinydashboard/", "shinydashboard"),
                            " and integrated Toggl exports.",
                            br(), br(),
                            "All code is available on ",
                            a(href = "https://github.com/jalavery/timetrackR", "GitHub.")
                        ),
                        box(title = "How to use timetrackR",
                            "This app is based on tracking your time in Toggl and exporting that data for custom visualizations.",
                            "To prepare your data for use in the app:", br(), 
                            "1. Navigate to toggl.com", br(), br(),
                            "2. On the lefthand side, select Reports", br(), br(),
                            "3. At the top of the page, select Detailed reports", br(), br(),
                            "4. On the top right, hit the download button to download the CSV file of your logged hours.", br(), br(),
                            "5. Change the timeframe from 'This week' to 'This year'", br(), br(),
                            "6. Upload that CSV file in timetrackR and select the summary level on the side bar panel to the left.")
                    ))
        )
    )
)