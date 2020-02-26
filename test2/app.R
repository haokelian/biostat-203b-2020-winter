# install.packages("rgdal")
# install.packages("leaflet")
# install.packages("shinythemes")
# remotes::install_github("GuangchuangYu/nCov2019")
# remotes::install_github("GuangchuangYu/chinamap")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(nCov2019)
library(shiny)
library(lubridate)
library(leaflet) 
library(rgdal)
library(shinythemes)


# Load data updated today ----
# source: JHU #
# load accumulated global cases ny different types
glob_c <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
glob_d <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
glob_r <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
# source: Weixin #
cov_data <- get_nCov2019(lang = 'en')
cov_hist <- load_nCov2019(lang = 'en')


# Tidy data ----
glob_c_l <- glob_c %>%
    pivot_longer(-(`Province/State`:Long), 
                 names_to = "Date", 
                 values_to = "confirmed") %>%
    mutate(Date = (mdy(Date))) # convert string to date-time

glob_r_l <- glob_r %>%
    pivot_longer(-(`Province/State`:Long), 
                 names_to = "Date", 
                 values_to = "recovered") %>%
    mutate(Date = mdy(Date))

glob_d_l <- glob_d %>%
    pivot_longer(-(`Province/State`:Long), 
                 names_to = "Date", 
                 values_to = "death") %>%
    mutate(Date = mdy(Date))

glob_tbl <- glob_c_l %>%
    left_join(glob_r_l) %>%
    left_join(glob_d_l) %>%
    pivot_longer(confirmed:death, 
                 names_to = "Case", 
                 values_to = "Count")

# transform timeseries
glob_timeseries <- glob_tbl %>% group_by(Date, Case) %>%  
    summarise(total_count = sum(Count))

# transform death and heal rate
glob_confirmed_number <- glob_tbl %>%
    group_by(Date, Case) %>%  
    summarise(confirmed_count = sum(Count)) %>%
    filter(Case == "confirmed")

glob_death_number <- glob_tbl %>%
    group_by(Date, Case) %>%  
    summarise(death_count = sum(Count)) %>%
    filter(Case == "death")

glob_recovered_number <- glob_tbl %>%
    group_by(Date, Case) %>%  
    summarise(recovered_count = sum(Count)) %>%
    filter(Case == "recovered")

glob_death_heal <- glob_confirmed_number %>%
    left_join(glob_death_number, by = "Date") %>%
    left_join(glob_recovered_number, by = "Date") %>%
    mutate(healrate = 100*recovered_count/confirmed_count) %>%
    mutate(deathrate = 100*death_count/confirmed_count) %>%
    pivot_longer(ends_with("rate"), 
                 names_to = "CaseRate", 
                 values_to = "Rate") %>%
    select(Date, ends_with("count"), ends_with("rate"))

# store the update time of data
lastdate = glob_timeseries %>% tail(1)
last_date = lastdate[1] %>% pull(Date)



# User interface ----
ui = navbarPage(
        theme = shinytheme("superhero"), "Cronavirus Telescope", 
        
# Tab1: Global Map and Trend 
        # tabPanel("Map", leafletOutput("map_glob", height=1000)),
        tabPanel("Homepage", icon=icon("home"),
                 titlePanel("Global Map and Trend"),
                 sidebarPanel(
                     helpText("World map and data table of coronavirus cases"),
                     dateInput('date_glob',
                               label = 'Date to display',
                               value = Sys.Date()-1),
                     selectInput("type_glob", 
                                label = "Type of cases to display",
                                choices = c("confirmed", "recovered", "death"),
                                selected = "confirmed"),
                 ),
                 mainPanel(helpText("Cases numbers are shown in log scale.
                                     Tab circles to see real numbers"),
                           leafletOutput("map_glob"),
                           tags$style(HTML("
                    .dataTables_wrapper .dataTables_length,
                    .dataTables_wrapper .dataTables_filter,
                    .dataTables_wrapper .dataTables_info,
                    .dataTables_wrapper .dataTables_processing,
                    .dataTables_wrapper .dataTables_paginate {color: #ffffff;}
                    thead {color: #ffffff;}
                    tbody {color: #000000;}"))),
                 h3("Data Table of Global Cases in Map"),
                 h4("Total Counts by Country"),
                 DT::dataTableOutput("table_glob"),
                 h4("Counts by Province/State"),
                 DT::dataTableOutput("test"),
                 h3("Trend"),
                 tabsetPanel(
                     tabPanel("Global Cases", 
                              plotOutput("trend_glob_c")
                              ),
                     tabPanel("Death and Heal Rate", 
                              plotOutput("trend_glob_r")),
                     id = NULL, selected = NULL, type = c("tabs", "pills"),
                     position = NULL),
        ),
        tabPanel("China", "This panel is intentionally left blank"),
        tabPanel("Financial Influences", "This panel is intentionally left blank"),
        tags$head(
            tags$style(".tab-content .tab-content {
                       min-height:400px;}")
        )
    )

server = function(input, output) {
    
# Tab1: Global Map and Trend
    
    # fillter the tibble based on input
    output$table_glob <- DT::renderDataTable(DT::datatable({
        glob_distribution <- glob_tbl %>%
            filter(Date == as.character(input$date_glob)) %>%
            filter(Case == as.character(input$type_glob)) %>%
            group_by(`Country/Region`) %>%
            summarise(total_count = sum(Count)) %>%
            arrange(desc(total_count))
        glob_distribution
    }))
    
    output$test <- DT::renderDataTable(DT::datatable({
        glob_mapdata <- glob_tbl %>%
            filter(Date == as.character(input$date_glob)) %>%
            filter(Case == as.character(input$type_glob)) %>%
            arrange(desc(Count)) 
        select(glob_mapdata, `Province/State`, `Country/Region`, `Count`)
    }))
    
    # plot the worldmap
    output$map_glob <- renderLeaflet({
        Color <- switch(input$type_glob, 
                       "confirmed" = "#FF7F50",
                       "recovered" = "#A2CD5A",
                       "death" = "#8B4513")
        glob_mapdata <- glob_tbl %>%
            filter(Date == as.character(input$date_glob)) %>%
            filter(Case == as.character(input$type_glob)) %>%
            arrange(desc(Count))
        leaflet(glob_mapdata) %>% addTiles() %>%
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, color = Color,
                       radius = ~sqrt(log10(Count)) * 100000,
                       popup = ~`Province/State`, label = ~Count)
    })
    
    # plot the trends
    output$trend_glob_c <- renderPlot({
        glob_tbl %>%
            group_by(Date, Case) %>%  
            summarise(total_count = sum(Count)) %>%
            # print()
            ggplot(mapping = aes(x = Date, y = total_count, color = Case)) +
            geom_line() + geom_point() +
            scale_color_manual(values = c("#FF7F50", "#8B4513", "#A2CD5A")) + 
            # scale_y_log10() + 
            labs(y = "Count", caption = paste("accessed date:", last_date)) 
    })
    output$trend_glob_r <- renderPlot({
        glob_death_heal %>%
            ggplot(mapping = aes(x = Date, y = Rate, color = CaseRate)) +
            geom_line() + geom_point() +
            scale_color_manual(values = c("#666666", "#00CD00")) +
            labs(y = "Percents", caption = paste("accessed date:", last_date))
    })

}

shinyApp(ui,server)
