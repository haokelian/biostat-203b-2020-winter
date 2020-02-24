# install.packages("shinythemes")
# remotes::install_github("GuangchuangYu/nCov2019")
# remotes::install_github("GuangchuangYu/chinamap")library(dplyr)

library(tidyverse)
library(ggplot2)
library(nCov2019)
library(shiny)

# load data updated today ----
# source: JHU #
# load accumulated global cases ny different types
glob_c <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
glob_d <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
glob_r <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
# source: Weixin #
cov_data <- get_nCov2019(lang = 'en')
cov_hist <- load_nCov2019(lang = 'en')
data_glob <- cov_data['global', ]

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
                                choices = c("Confirmed", "Recovered", "Deaths"),
                                selected = "Confirmed"),
                     sliderInput("range_glob", 
                        label = "Number of cases to display in log scale",
                        min = 0, max = max_glob, value = c(0, log(max_glob)))
                 ),
                 mainPanel(plotOutput("map_glob")),
                 h4("Trend"),
                 tabsetPanel(
                     tabPanel("Confirmed and Suspected Cases", 
                              plotOutput("trend_glob_c")),
                     tabPanel("Added Cases"),
                     tabPanel("Death Rate"),
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
    # estract today's data
    glob_c_t <- glob_c[,ncol(glob_c)]
    test <- glob_c %>% group_by(`Country/Region`)
    test <- summarise(test, Total_today = test[,ncol(test)])
    output$table_glob <- DT::renderDataTable(DT::datatable({data_glob_}))
    # plot the worldmap
    mapworld <- borders("world", colour = "gray75", fill="white",
                        show.legend = FALSE) #basic map
    output$map_glob <- renderPlot({
        ggplot() + mapworld + ylim(-60,90) +
            labs(caption = paste("accessed date:", time(cov_data)))
    })
    output$trend_glob_c <- renderPlot({
        ggplot(summary[cov_data], aes(as.Date(date, "%m.%d"),
        as.numeric(confirm))) + geom_col(fill='firebrick') 
        + theme_minimal(base_size = 14) + xlab(NULL) + ylab(NULL) 
        + labs(caption = paste("accessed date:", time(cov_data)))
    })
    
}

shinyApp(ui,server)
