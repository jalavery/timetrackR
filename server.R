library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(rsconnect)
library(DT)
library(lubridate)
library(ggplot2)

server <- function(input, output) {
    # read in tracking data from Toggl
    tracker_toggl <- reactive({
        req(input$file1)
        
        df <- read_csv(input$file1$datapath) %>%
            # change duration to hours
            mutate(Duration = period_to_seconds(hms(Duration))/(60^2))
        
        return(df)
    })
    
    ######################
    #     info boxes     #
    ######################
    output$top_client <- renderInfoBox({
        # get top client during time period
        top_client <- tracker_toggl() %>% 
            drop_na(`Start date`) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            ) %>% 
            group_by(Client) %>% 
            summarize(total_hrs = sum(Duration)) %>% 
            filter(total_hrs == max(total_hrs)) %>% 
            select(Client)
        
        # display in info box
        infoBox(
            "Top Client", top_client$Client, icon = icon("user-clock"),
            color = "maroon"
        )
    })
    
    output$top_project <- renderInfoBox({
        # get top project during time period
        top_proj <- tracker_toggl() %>% 
            drop_na(`Start date`) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            ) %>% 
            group_by(Project) %>% 
            summarize(total_hrs = sum(Duration)) %>% 
            filter(total_hrs == max(total_hrs)) %>% 
            select(Project)
        
        # display in info box
        infoBox(
            "Top Project", top_proj$Project, icon = icon("briefcase"),
            color = "olive"
        )
    })
    
    output$top_task <- renderInfoBox({
        # get top project during time period
        top_task <- tracker_toggl() %>% 
            drop_na(`Start date`, Description) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            ) %>% 
            group_by(Description) %>% 
            summarize(total_hrs = sum(Duration)) %>% 
            filter(total_hrs == max(total_hrs)) %>% 
            select(Description)
        
        # display in info box
        infoBox(
            "Top Task", top_task$Description, icon = icon("tasks"),
            color = "purple"
        )
    })
    
    ######################
    #      % effort      #
    ######################
    
    # print summary of figure
    # change to footnote?
    output$pie_text <- renderText({
        paste(
            # "The following donut chart shows the breakdown of percent effort by ",
              # input$stratify_pct_effort,
              # " between ", 
              # paste0(format(input$years, "%b %d, %Y"), collapse = " and "),
              # ". ",
              "Note that projects representing 3% or less of the total number of hours are collapsed into the 'Other' category."

        )
    })
    
    # subset data for pie chart based on dates
    # update pie chart depending on input selected
    output$pieChart_client <- renderPlotly({
            # set up df
            tracker_toggl() %>% 
            drop_na(`Start date`) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            ) %>% 
            drop_na(Duration, Client) %>% 
            # collapse projects PIs accounting for <3% of time
            mutate(pi_lump = fct_lump(Client, prop = 0.03, w = Duration)) %>% 
            group_by(pi_lump) %>%
            # re-calculate total number of hours across proj selected (have to recalc if >1 stat per proj)
            summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>%
            plot_ly(labels = ~pi_lump, values = ~round(sum_hrs)
                    #,
                    #width = 400, height = 400
            ) %>%
            add_pie(hole = 0.6) %>%
            layout(autosize = TRUE, showlegend = FALSE,
                   #title = "Percent Effort",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
        
    output$pieChart_proj <- renderPlotly({
        # set up df
        tracker_toggl() %>% 
            drop_na(`Start date`) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            ) %>% 
            # collapse projects accounting for <3% of time in interval
            drop_na(Duration, Project) %>% 
            mutate(Project_lump = fct_lump(Project, prop = 0.03, w = Duration)) %>% 
            group_by(Project_lump) %>% 
            # re-calculate total number of hours across proj selected (have to recalc if >1 stat per proj)
            summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
            plot_ly(labels = ~ Project_lump, values = ~round(sum_hrs)#,
                    #width = 400, height = 400
            ) %>%
            add_pie(hole = 0.6) %>% 
            layout(autosize = TRUE, showlegend = FALSE,
                   #title = '% of total hours by project',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$pieChart_phase <- renderPlotly({
        # set up df
        tracker_toggl() %>% 
            drop_na(`Start date`) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            ) %>% 
            drop_na(Duration, Tags) %>% 
            mutate(Tags = case_when(is.na(Tags) ~ "Other",
                                    TRUE ~ Tags)) %>% 
            # collapse tasks accounting for <3% of time
            mutate(Tags_lump = fct_lump(Tags, prop = 0.03, w = Duration)) %>% 
            group_by(Tags_lump) %>% 
            # re-calculate total number of hours across proj selected (have to recalc if >1 stat per proj)
            summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
            plot_ly(labels = ~ Tags_lump, values = ~round(sum_hrs)) %>%
            add_pie(hole = 0.6) %>%
            layout(autosize = TRUE, showlegend = FALSE,
                   #title = '% of total hours by project Tags',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$pieChart_task <- renderPlotly({
        # set up df
        tracker_toggl() %>% 
            drop_na(`Start date`) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            ) %>%
            drop_na(Duration, Description) %>% 
            # group into other if description is missing
            mutate(description = case_when(is.na(Description) ~ "Other",
                                           TRUE ~ Description)) %>% 
            # collapse tasks accounting for <3% of time
            mutate(desc_lump = fct_lump(description, prop = 0.03, w = Duration)) %>% 
            group_by(desc_lump) %>% 
            # re-calculate total number of hours across proj selected (have to recalc if >1 stat per proj)
            summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
            plot_ly(labels = ~ desc_lump, values = ~round(sum_hrs)) %>%
            add_pie(hole = 0.6) %>%
            layout(autosize = TRUE, showlegend = FALSE,
                   #title = '% of total hours by project Tags',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    ##########################
    #    % effort over time  #
    ##########################
    output$stacked_bar <- renderPlotly({
        toggl2 <- tracker_toggl() %>% 
            drop_na(`Start date`) %>% 
            filter(# putting req around input removes warning about length
                `Start date` >= req(input$years[1]),
                `Start date` <= req(input$years[2])
            ) %>% 
            janitor::clean_names() %>%
            drop_na(project, client) %>% 
            mutate(year = year(start_date),
                   month = month(start_date),
                   year_month_fct = factor(paste0(year, "-", month)),
                   year_month = fct_reorder(year_month_fct, start_date)) %>% 
            # get denominator to calculate percent of hours
            group_by(year_month) %>% 
            mutate(denominator = sum(duration)) %>% 
            # get total number of hours per project by year and month
            group_by(year_month, project, client, denominator) %>% 
            summarize(tot_hours = sum(duration)) %>% 
            mutate(pct_hours = (100*tot_hours)/denominator) %>% 
            # if total number of hours is less than 5% of total time, lump into other category
            mutate(project_collapsed = case_when(pct_hours < 10 ~ "Other",
                                                 TRUE ~ project),
                   client_collapsed = case_when(pct_hours < 10 ~ "Other",
                                                TRUE ~ client)) %>% 
            # group other projects together
            group_by(year_month, project_collapsed, client_collapsed) %>% 
            summarize(pct_hours_collapsed = sum(pct_hours)) %>% 
            mutate(hover_text = paste0("Project: ", project_collapsed, "<br>",
                                       "Client: ", client_collapsed, "<br>",
                                       "Effort: ", round(pct_hours_collapsed), "%"))
        
        
        g1 <- ggplot(toggl2, aes(x = year_month, y = pct_hours_collapsed, fill = project_collapsed, text = hover_text)) +
            geom_bar(position = "stack", stat = "identity") +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "",
                 y = "% effort") 
        
        ggplotly(g1, tooltip = "text")
    })
    
    ########################
    # horizontal bar chart #
    ########################
    
    # print summary of figure
    output$bar_text <- renderText({
        "Note that projects representing 3% or less of the total number of hours are collapsed into the 'Other' category."
        # paste("The following bar chart shows the total number of hours per project between ",          " between ",
        #       paste0(format(input$years, "%b %d, %Y"), collapse = " and ")
        # )
    })
    
    # figure
    output$barChart_client <- renderPlotly({
        g2 <- tracker_toggl() %>%
                   filter(`Start date` >= input$years[1],
                          `Start date` <= input$years[2]) %>%
                   group_by(Client) %>% 
                   summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
                   ungroup() %>% 
                   arrange(Client) %>% 
                   # order study title by client to group in order long the axis
                   mutate(Project_factor_pre = as.factor(str_wrap(Client, width = 15)),
                          Project_factor = fct_lump(Project_factor_pre, p = 0.03, w = sum_hrs),
                          Project_factor2 = fct_reorder(Project_factor, sum_hrs),
                          status_desc = paste0('<br>Client: ', Client, 
                                               '<br>Hours: ', round(sum_hrs))) %>% 
                   drop_na() %>% 
            ggplot(aes(x = Project_factor2, y = sum_hrs, fill = Client, text = status_desc)) + 
            geom_bar(stat = "identity") + 
            coord_flip() + 
            theme_bw() + 
            theme(legend.position = "none") + 
            labs(y = "Hours",
                 x = ""
                 )
        
        ggplotly(g2, tooltip = "text")
    })
    
    # bar chart by project
    output$barChart_proj <- renderPlotly({
        g2 <- tracker_toggl() %>%
                   filter(`Start date` >= input$years[1],
                          `Start date` <= input$years[2]) %>%
                   group_by(Client, Project) %>% 
                   summarize(sum_hrs = sum(Duration, na.rm = TRUE)) %>% 
                   ungroup() %>% 
                   # order study title by client to group in order long the axis
                   mutate(project_wrap = str_wrap(Project, width = 45),
                          Project_factor = fct_lump(project_wrap, p = 0.03, w = sum_hrs),
                          Project_factor2 = fct_reorder(Project_factor, sum_hrs),
                          status_desc = paste0('<br>Client: ', Client, 
                                               '<br>Project: ', Project,
                                               '<br>Hours: ', round(sum_hrs))) %>% 
                   drop_na() %>% 
        ggplot(aes(x = Project_factor2, y = sum_hrs, fill = Client, text = status_desc)) + 
            geom_bar(stat = "identity") + 
            coord_flip() + 
            theme_bw() + 
            theme(legend.position = "none") + 
            labs(y = "Hours",
                 x = ""
            )
        
        ggplotly(g2, tooltip = "text")
    })
    
    ######################
    #    timeline     #
    ######################
    
    # figure
    output$timeline <- renderPlot({
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
            mutate(Project = fct_reorder(Project, desc(start_dt))) %>% 
            drop_na() %>% 
            mutate(status_desc = paste0('<br>Client: ', Client, 
                                        '<br>Project: ', Project))
        
        # create timeline
        ggplot(for_timeline) +
            geom_segment(aes(x = start_dt, xend = end_dt, 
                             y = Project, yend = Project, 
                             color = Tags), size = 4) +
            theme_bw() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 11),
                  axis.title = element_blank(), 
                  axis.ticks = element_blank(),
                  axis.text = element_text(size = 11)
                  ) +
            guides(color = guide_legend(nrow = 2)) +
            scale_x_date(breaks = "3 months", date_labels = "%b %Y")
        
        # ggplotly(g3, tooltip = "text") 
    })
    
}

# shinyApp(ui, server)