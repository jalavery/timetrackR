library(shiny)
library(shinydashboard)

server <- function(input, output) {
    # read in tracking data from Toggl
    tracker_toggl <- reactive({
        req(input$file1)
        
        df <- read_csv(input$file1$datapath,
                       # header = input$header,
                       # sep = input$sep,
                       # quote = input$quote
        ) %>%
            # change duration to hours
            mutate(Duration = period_to_seconds(hms(Duration))/(60^2))
        
        return(df)
    })
    
    # date range of interest
    # start by selecting the previous year and curent year
    output$years <- renderUI({
        input$years
    })
    
    # from example
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    # read in data
    # read in tracking data from Toggl
    tracker_toggl <- reactive({
        req(input$file1)
        
        df <- read_csv(input$file1$datapath,
                       # header = input$header,
                       # sep = input$sep,
                       # quote = input$quote
        ) %>%
            # change duration to hours
            mutate(Duration = period_to_seconds(hms(Duration))/(60^2))
        
        return(df)
    })
    
    # date range of interest
    # start by selecting the previous year and curent year
    output$years <- renderUI({
        dateRangeInput(inputId = "years",
                       label = "Date range: ",
                       # default time from 1 year prior through current date
                       start = Sys.Date() - months(6), end = Sys.Date() + 1,
                       min = Sys.Date() - years(3), max = Sys.Date() + 1,
                       format = "yyyy-mm-dd", startview = "year",
                       separator = " to ", width = NULL, autoclose = TRUE)
    })
    
    ######################
    #      % effort      #
    ######################
    
    # print summary of figure
    # change to footnote?
    output$pie_text <- renderText({
        paste("The following donut chart shows the breakdown of percent effort by ",
              input$stratify_pct_effort,
              " between ", 
              paste0(format(input$years, "%b %d, %Y"), collapse = " and "),
              ". Note that projects representing 3% or less are collapsed into the 'Other' category."
        )
    })
    
    # subset data for pie chart based on dates
    output$pieChart_client <- renderPlotly({
        pie <- tracker_toggl() %>% 
            drop_na(`Start date`) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            )
        
        # change grouping to sum depending on which summary level is selected
        pie %>% 
            drop_na(Duration, Client) %>% 
            # collapse projects PIs accounting for <3% of time
            mutate(pi_lump = fct_lump(Client, prop = 0.03, w = Duration)) %>% 
            group_by(pi_lump) %>%
            # re-calculate total number of hours across proj selected (have to recalc if >1 stat per proj)
            summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>%
            plot_ly(labels = ~pi_lump, values = ~sum_hrs) %>%
            add_pie(hole = 0.6) %>%
            layout(#title = "Percent Effort",
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$pieChart_project <- renderPlotly({
        pie <- tracker_toggl() %>% 
            drop_na(`Start date`) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            )
        
        # change grouping to sum depending on which summary level is selected
        pie %>% 
            # collapse projects accounting for <3% of time in interval
            drop_na(Duration, Project) %>% 
            mutate(Project_lump = fct_lump(Project, prop = 0.03, w = Duration)) %>% 
            group_by(Project_lump) %>% 
            # re-calculate total number of hours across proj selected (have to recalc if >1 stat per proj)
            summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
            plot_ly(labels = ~ Project_lump, values = ~ sum_hrs) %>%
            add_pie(hole = 0.6) %>% 
            layout(#title = '% of total hours by project',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    ########################
    # horizontal bar chart #
    ########################
    
    # print summary of figure
    output$bar_text <- renderText({
        paste("The following bar chart shows the total number of hours per project between ",          " between ",
              paste0(format(input$years, "%b %d, %Y"), collapse = " and ")
        )
    })
    
    # figure
    output$barChart_client <- renderPlotly({
        
    bar <- tracker_toggl() %>%
           filter(`Start date` >= input$years[1],
                  `Start date` <= input$years[2]) %>%
           group_by(Client) %>% 
           summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
           ungroup() %>% 
           arrange(Client) %>% 
           # order study title by client to group in order long the axis
           mutate(Project_factor = as.factor(Client),
                  status_desc = paste0('<br>Client: ', Client, 
                                       '<br>Hours: ', sum_hrs))
    
    plot_ly(data = bar, y = ~Project_factor, x = ~sum_hrs, 
            type = 'bar', split = ~Client, hoverinfo = "text", 
            text = ~status_desc, height = "80%") %>%
        layout(yaxis = list(title = "", categoryorder = "array", 
                            categoryarray = order, type = "category", autorange = "reversed"),
               xaxis = list(title = 'Number of hours'), barmode = 'relative', showlegend = FALSE,
               autosize = T)
    
    })
    
    ######################
    #    timeline     #
    ######################
    
    # print summary of figure
    output$gantt_text <- renderText({
        paste("The following project timeline shows data for ",
              str_to_lower(input$status_filter_gantt), 
              " between ",
              paste0(format(input$years, "%b %d, %Y"), collapse = " and ")
        )
    })
    
    # figure
    output$timeline_client <- renderPlot({
        for_timeline <- tracker_toggl() %>%
            filter(is.na(Tags) == FALSE, 
                   !(Tags %in% c("Departmental seminars, service", "Professional development", "Randomization")),
                   `Start date` >= input$years[1],
                   `Start date` <= input$years[2]
            ) %>% 
            mutate(Tags = factor(case_when(Tags %in% c("Abstract", "Conference") ~ "Conference",
                                           Tags %in% c("Project closeout", "Publication") ~ as.character(NA),
                                           Tags %in% c("Grant preparation", "Project planning", "Protocol development") ~ "Project planning",
                                           TRUE ~ Tags), levels = c("Project planning", "Analysis", "Manuscript preparation", "Revisions", "Re-analysis"))) %>% 
            drop_na(Tags) %>% 
            # select(Tags) %>% gtsummary::tbl_summary()
            # filter(Project %in% c("Lung SMARCA4", "GENIE BPC")) %>% 
            arrange(Client, Project, `Start date`) %>% 
            group_by(Project, Client) %>%
            # create numeric Tags in order to get start/stop `Start date` when switching back and forth between Tagss
            mutate(Tags_number = cumsum(Tags != lag(Tags, default = first(Tags))) + 1) %>% 
            ungroup() %>%
            # get the start/stop `Start date` of each Tags
            group_by(Client, Project, Tags_number, Tags) %>% 
            summarize(start_dt = as.Date(min(`Start date`)), 
                      end_dt = as.Date(max(`Start date`))) %>% 
            # if start/stop `Start date` are the same, add 1 day so that it shows up on figure
            mutate(end_dt = as.Date(ifelse(start_dt == end_dt, 
                                           end_dt + days(1), 
                                           end_dt), origin = "1970-01-01")) %>% 
            # get overall project start and stop dates
            ungroup() %>%
            group_by(Client, Project) %>%
            mutate(overall_start = as.Date(min(start_dt))) %>% 
            # order projects by start time
            ungroup() %>% 
            mutate(Project = fct_reorder(Project, desc(start_dt)))
        
        # create timeline
        ggplot(for_timeline) +
            geom_segment(aes(x = start_dt, xend = end_dt, 
                             y = Project, yend = Project, 
                             colour = Tags), size = 4) +
            theme_bw() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14),
                  axis.title = element_blank(), 
                  axis.ticks = element_blank(),
                  axis.text = element_text(size = 12)) +
            scale_x_date(breaks = "3 months", date_labels = "%b %Y") 
    })
    
}

# shinyApp(ui, server)