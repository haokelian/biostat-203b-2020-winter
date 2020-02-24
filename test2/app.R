# install.packages("shinythemes")
# remotes::install_github("GuangchuangYu/nCov2019")
# remotes::install_github("GuangchuangYu/chinamap")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(nCov2019)
library(shiny)

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
glob_timeseries <- glob_tbl %>% group_by(Date, Case) %>%  
    summarise(total_count = sum(Count))



# User interface ----
ui = navbarPage(
        "Cronavirus Telescope",
        
# Tab1: Global Map and Trend 
        tabPanel("Homepage", icon=icon("home"),
                 titlePanel("Global Map and Trend"),
                 sidebarPanel(
                     helpText("World map of coronavirus cases"),
                     selectInput("type_glob", 
                                label = "Type of cases to display",
                                choices = c("confirmed", "recovered", "death"),
                                selected = "confirmed"),
                     sliderInput("range_glob", 
                        label = "Number of cases to display in log10 scale",
                        min = 0, max = max_glob, value = c(0, log10(max_glob)))
                 ),
                 mainPanel(plotOutput("map_glob")),
                 h4("Trend"),
                 tabsetPanel(
                     tabPanel("Confirmed and Suspected Cases", 
                              plotOutput("trend_glob_c")),
                     tabPanel("Added Cases"),
                     tabPanel("Death and Cure Cases"),
                     id = NULL, selected = NULL, type = c("tabs", "pills"),
                     position = NULL),
                 h4("Data Table"),
                 DT::dataTableOutput("table_glob")
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
    output$table_glob <- DT::renderDataTable(DT::datatable({glob_c}))
    # fillter the tibble based on input
    glob_distribution <- glob_tbl %>%
        filter(Date == max(Date)) %>%
        filter(Case == as.character(input$type_glob)) %>%
        # filter(count)
        group_by(`Country/Region`) %>%  
        summarise(total_count = sum(Count))
    # plot the worldmap
    mapworld <-
        glob_distribution %>% 
        borders("world", colour = "gray75", fill = Count,
                        show.legend = FALSE) #basic map
    
    ##
    output$dateRangeText2 <- renderText({
        paste("input$range_glob is", 
              paste(as.character(input$range_glob), collapse = " to ")
        )
    })
    ##
    output$map_glob <- renderPlot({
        ggplot() + mapworld + ylim(-60,90) +
            labs(caption = paste("accessed date:", time(cov_data)))
    })
    output$trend_glob_c <- renderPlot({
        ggplot()
    })
    
}

shinyApp(ui,server)
