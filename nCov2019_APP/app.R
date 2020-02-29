# Install and library packages --------------------
# install.packages("leaflet")
# install.packages("shinythemes")
# remotes::install_github("GuangchuangYu/nCov2019")
# remotes::install_github("GuangchuangYu/chinamap")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(nCov2019)
library(chinamap)
library(shiny)
library(lubridate)
library(leaflet) 
library(shinythemes)
library(quantmod)
source("helpers.R")
source("plot.R")
source("utilities.R")


# Load data updated today --------------------
# source: JHU #
# load accumulated global cases ny different types
glob_c <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
glob_d <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
glob_r <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

# source: Weixin #
# load present and historical data
cn_data <- get_nCov2019(lang = 'en')
cn_hist <- load_nCov2019(lang = 'en')

# store name of provinces and cities in China
provinces = cn_data[] %>% select(name)
cities = cn_hist[] %>% select(province, city) %>% distinct() %>% arrange(city)
all_cities = select(cities, city)


# Tidy data --------------------
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

cn_tbl <- cn_hist[] %>% pivot_longer(starts_with("cum_"),
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

# calculate death and heal rate in China similarly
cn_death_heal <- cn_hist[] %>% group_by(time) %>%
    summarize(total_dead = sum(cum_dead),
              total_heal = sum(cum_heal),
              total_confirm = sum(cum_confirm)) %>%
    mutate(deathrate = 100*total_dead/total_confirm,
           healrate = 100*total_heal/total_confirm) %>%
    pivot_longer(ends_with("rate"), 
                 names_to = "CaseRate", 
                 values_to = "Rate") %>%
    select(time, ends_with("count"), ends_with("rate"))

prov_death_heal <- cn_hist[] %>% group_by(time, province) %>%
    summarize(total_confirm = sum(cum_confirm),
              total_dead = sum(cum_dead), 
              total_heal = sum(cum_heal)) %>%
    mutate(deathrate = 100*total_dead/total_confirm,
           healrate = 100*total_heal/total_confirm) %>%
    pivot_longer(ends_with("rate"), 
                 names_to = "CaseRate", 
                 values_to = "Rate")

city_death_heal <- cn_hist[] %>% mutate(deathrate = 100*cum_dead/cum_confirm,
                                        healrate = 100*cum_heal/cum_confirm) %>%
    pivot_longer(ends_with("rate"), 
                 names_to = "CaseRate", 
                 values_to = "Rate")

# store the update time of JHU data
lastdate = glob_timeseries %>% tail(1)
last_date = lastdate[1] %>% pull(Date)



# User interface --------------------
ui = navbarPage(
        theme = shinytheme("superhero"), "Cronavirus Telescope", 
        
# Tab1: Global Map and Trend 
        tabPanel("Homepage", icon = icon("home"),
                 titlePanel("Global Map and Trend"),
                 sidebarPanel(
                     helpText("World map and data table cumulative number 
                              of coronavirus cases"),
                     dateInput('date_glob',
                               label = 'Date to display',
                               value = last_date),
                     selectInput("type_glob", 
                                label = "Type of cases to display",
                                choices = c("confirmed", "recovered", "death"),
                                selected = "confirmed"),
                 ),
                 mainPanel(helpText("Cases numbers are shown in log scale.
                                     Tap circles to see real numbers"),
                           # global map using leaflet
                           leafletOutput("map_glob"),
                           # set table format
                           tags$style(HTML("
                    .dataTables_wrapper .dataTables_length,
                    .dataTables_wrapper .dataTables_filter,
                    .dataTables_wrapper .dataTables_info,
                    .dataTables_wrapper .dataTables_processing,
                    .dataTables_wrapper .dataTables_paginate {color: #ffffff;}
                    thead {color: #ffffff;}
                    tbody {color: #000000;}"))),
                 # table outputs
                 h3("Data Table of Global Cases in Map"),
                 h4("Total Counts by Country"),
                 DT::dataTableOutput("table_glob"),
                 h4("Counts by Province/State"),
                 DT::dataTableOutput("table_prov"),
                 # trend
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

# Tab2: China Map and Trend 

        tabPanel("China",
                 titlePanel("China and Province Map and Trend"),
                 sidebarPanel(
                     dateInput('date_cn',
                               label = 'Date to display',
                               value = time(cn_data))
                     ),
                 mainPanel(helpText("Data of one day before last date may
                                    be missing due to fault of data resource"),
                           plotOutput("map_cn")),
                 h3("Trend"),
                 h4("Trend in China"),
                 tabsetPanel(
                     tabPanel("China Cases", 
                              plotOutput("trend_cn_c")),
                     tabPanel("Death and Heal Rate", 
                              plotOutput("trend_cn_r")),
                     id = NULL, selected = NULL, type = c("tabs", "pills"),
                     position = NULL),
                 h4("Trend in Provinces and Cities"),
                 sidebarPanel(
                     selectInput("province",
                                 label = "Province to display",
                                 choices = c(provinces),
                                 selected = "Hubei"),
                     selectInput("city",
                                 label = "City to display",
                                 choices = c("Total in province",
                                             all_cities),
                                 selected = "Total in province"
                                 ),
                     helpText("List of cities in selected province:"),
                     verbatimTextOutput("cities"),
                 ),
                 mainPanel(tabsetPanel(
                     tabPanel("Cases", 
                              plotOutput("trend_prov_c")),
                     tabPanel("Death and Heal Rate", 
                              plotOutput("trend_prov_r")),
                     id = NULL, selected = NULL, type = c("tabs", "pills"),
                     position = NULL)),
                 ),

# Tab3: Financial Influces 

        tabPanel("Financial Influences",
                 titlePanel("Stock"),
                     sidebarPanel(
                         helpText("Select a stock to examine. 
               Information will be collected from Yahoo finance."),
                         
                         textInput("symb", "Symbol", "GOOG"),
                         
                         dateRangeInput("dates", 
                                        "Date range",
                                        start = "2019-12-21", 
                                        end = as.character(Sys.Date())),
                         
                         br(),
                         br(),
                         
                         checkboxInput("log", "Plot y axis on log scale", 
                                       value = FALSE),
                         
                         checkboxInput("adjust", 
                                       "Adjust prices for inflation",
                                       value = FALSE)
                     ),
                     
                     mainPanel(plotOutput("plot"))
                 ),
        tags$head(
            tags$style(".tab-content .tab-content {
                       min-height:400px;}")
        )
    )


# Server logic --------------------
server = function(input, output) {
    
    # add a loading bar
    withProgress(message = 'Mapping in progress',
                 detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.25)
                     }
                 })
    
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
    
    output$table_prov <- DT::renderDataTable(DT::datatable({
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
    
# Tab2: China Map and Trend
    
    
    # plot map
    output$map_cn <- renderPlot({
        # add a loading bar
        withProgress(message = 'Mapping in progress',
                     detail = 'This may take a while...', value = 0, {
                         for (i in 1:15) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                         }
                     })
        # get chinamap polygons by provinces and cities
        # require(chinamap)
        cn = get_map_china()
        # translate provinces and cities
        cn$province <- trans_province(cn$province)
        if(as.character(input$date_cn) == as.Date(time(cn_data)))
             {plot.nCov2019(cn_data, region = "china",
              chinamap = cn, font.size=2)}
        else{plot.nCov2019History(cn_hist, region = "china",
             chinamap = cn, date = as.character(input$date_cn), font.size=2)}
    })
    

    # plot trends of China
    output$trend_cn_c <- renderPlot({
        glob_tbl %>%
            group_by(Date, Case) %>%  
            filter(`Country/Region` %in% 
                       c("Mainland China", "Macau", "Hong Kong", "Taiwan")) %>%
            summarise(total_count = sum(Count)) %>%
            # print()
            ggplot(mapping = aes(x = Date, y = total_count, color = Case)) +
            geom_line() + geom_point() +
            scale_color_manual(values = c("#FF7F50", "#8B4513", "#A2CD5A")) + 
            # scale_y_log10() + 
            labs(y = "Count", caption = paste("accessed date:", last_date)) 
    })
    output$trend_cn_r <- renderPlot({
        cn_death_heal %>%
            ggplot(aes(time, Rate, color = CaseRate)) +
            geom_line() + geom_point() +
            scale_color_manual(values = c("#666666", "#00CD00")) +
            labs(y = "Percents", caption = paste("accessed date:", time(cn_hist)
                                                 ))
    })
    
    # plot trends of Provinces and Cities
    output$cities <- renderPrint({
        cities = cities %>%
            filter(province == as.character(input$province)) %>%
            pull(city)
        str_c(cities)
        })
    
    output$trend_prov_c <- renderPlot({
        if(as.character(input$city) == "Total in province")
            { # plot trend of all cities in the province
          trend_prov_c = cn_tbl %>% 
            filter(province == as.character(input$province)) %>%
            group_by(time, Case) %>%  
            summarise(total_count = sum(Count)) %>%
            # print()
            ggplot(mapping = aes(x = time, y = total_count, color = Case)) +
            geom_line() + geom_point() +
            scale_color_manual(values = c("#FF7F50", "#8B4513", "#A2CD5A")) + 
            # scale_y_log10() + 
            labs(y = "Count", caption = paste("accessed date:", time(cn_hist)))
        }
        else{ # plot trend of selected city
          trend_prov_c = cn_tbl %>% 
            filter(city == as.character(input$city)) %>%
            group_by(time, Case) %>%  
            summarise(total_count = sum(Count)) %>%
            # print()
            ggplot(mapping = aes(x = time, y = total_count, color = Case)) +
            geom_line() + geom_point() +
            scale_color_manual(values = c("#FF7F50", "#8B4513", "#A2CD5A")) + 
            # scale_y_log10() + 
            labs(y = "Count", caption = paste("accessed date:", time(cn_hist)))
        }
        trend_prov_c
    })

    output$trend_prov_r <- renderPlot({
        if(as.character(input$city) == "Total in province")
        { # plot trend of all cities in the province
            prov_death_heal %>%
            filter(province == as.character(input$province)) %>%
            ggplot(aes(time, Rate, color = CaseRate)) +
            geom_line() + geom_point() +
            scale_color_manual(values = c("#666666", "#00CD00")) +
            labs(y = "Percents", caption = paste("accessed date:",
                                                 time(cn_hist)))        
        }
        else{ # plot trend of selected city
            city_death_heal %>%
                filter(city == as.character(input$city)) %>%
                ggplot(aes(time, Rate, color = CaseRate)) +
                geom_line() + geom_point() +
                scale_color_manual(values = c("#666666", "#00CD00")) +
                labs(y = "Percents", caption = paste("accessed date:",
                                                     time(cn_hist)))
        }
    })
    
# Tab 3 Finantial Influence
    
    dataInput <- reactive({
        getSymbols(input$symb, src = "yahoo", 
                   from = input$dates[1],
                   to = input$dates[2],
                   auto.assign = FALSE)
    })
    
    output$plot <- renderPlot({
        chartSeries(dataInput(), theme = chartTheme("white"), 
                    type = "line", log.scale = input$log, TA = NULL)
    })
}

shinyApp(ui,server)
