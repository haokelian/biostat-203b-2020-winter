# install.packages("shinythemes")
# remotes::install_github("GuangchuangYu/nCov2019")
# remotes::install_github("GuangchuangYu/chinamap")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(nCov2019)
library(shiny)
library(lubridate)

# Load data updated today ----
# source: JHU #
# load accumulated global cases ny different types
glob_c <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
glob_d <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
glob_r <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
# source: Weixin #
cov_data <- get_nCov2019(lang = 'en')
cov_hist <- load_nCov2019(lang = 'en')
data_glob <- cov_data['global', ]
max_glob = data_glob[1,2]

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

glob_deathrate <- glob_confirmed_number %>%
    left_join(glob_death_number, by = "Date") %>%
    mutate(deathrate = 100*death_count/confirmed_count)

glob_healrate <- glob_confirmed_number %>%
    right_join(glob_recovered_number, by = "Date") %>%
    mutate(healrate = 100*recovered_count/confirmed_count)

glob_death_heal <- glob_deathrate %>%
    right_join(glob_healrate, by = "Date") %>%
    pivot_longer(ends_with("rate"), 
                 names_to = "Case", 
                 values_to = "Rate") %>%
    select(Date, ends_with("count"), Case, ends_with("rate"))

# store the update time of data
lastdate = glob_timeseries %>% tail(1)
last_date = lastdate[1] %>% pull(Date)



# User interface ----
ui = navbarPage(
        "Cronavirus Telescope",
        
# Tab1: Global Map and Trend 
        tabPanel("Homepage", icon=icon("home"),
                 titlePanel("Global Map and Trend"),
                 sidebarPanel(
                     helpText("World map and data table of coronavirus cases"),
                     dateInput('date_glob',
                               label = 'Date to display',
                               value = Sys.Date()),
                     selectInput("type_glob", 
                                label = "Type of cases to display",
                                choices = c("confirmed", "recovered", "death"),
                                selected = "confirmed"),
                 ),
                 mainPanel(plotOutput("map_glob")),
                 h4("Data Table of Global Cases in Map"),
                 DT::dataTableOutput("table_glob"),
                 
                 h4("Trend"),
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
    
    # plot the worldmap
    mapworld <-
        borders("world", colour = "gray75", fill = "White",
                        show.legend = FALSE) #basic map
    output$map_glob <- renderPlot({
        ggplot() + mapworld + ylim(-60,90) +
            labs(caption = paste("accessed date:", time(cov_data)))
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
            ggplot(mapping = aes(x = Date, y = Rate, color = Case)) +
            geom_line() + geom_point() +
            scale_color_manual(values = c("#666666", "#00CD00")) +
            labs(y = "Percents", caption = paste("accessed date:", last_date))
    })

}

shinyApp(ui,server)
