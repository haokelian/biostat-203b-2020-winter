# install.packages("shinythemes")
# remotes::install_github("GuangchuangYu/nCov2019")
# remotes::install_github("GuangchuangYu/chinamap")
library(dplyr)
library(ggplot2)
library(nCov2019)
library(shiny)
library(shinythemes)

ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
        theme = "darkly", "Cronavirus Telescope",
        tabPanel("Homepage", icon=icon("home"),
                 titlePanel("Global Map and Trend"),
                 sidebarPanel(
                     helpText("Select type and range of cases"),
                     
                     dateInput('date_glob',
                               label = 'Date input',
                               value = Sys.Date()
                     ),
                 # actionButton("action2", "Action button",
                 #              class = "btn-primary")
                 ),
                 mainPanel(plotOutput("map_glob")),
                 h4("Trend"),
                 tabsetPanel(
                     tabPanel("Confirmed and Suspected Cases", icon=icon("home")),
                     tabPanel("Added Cases"),
                     tabPanel("Death Rate"),
                     id = NULL, selected = NULL, type = c("tabs", "pills"),
                     position = NULL),
                 h4("Data Table"),
                 DT::dataTableOutput("table_glob")
        ),
        navbarMenu(
            title = "China",
            tabPanel("Total"),
            tabPanel("Provinces")
        ),
        tabPanel("Other Countries", "This panel is intentionally left blank"),
        tags$head(
            tags$style(".tab-content .tab-content {
                       min-height:400px;}")
        )
    )
)

server = function(input, output) {
    cov_data <- get_nCov2019(lang = 'en')
    cov_hist <- load_nCov2019(lang = 'en')
    data_glob <- cov_data['global', ]
    # add names of some countries according to the intro of this package
    # Diamond Pricess Cruise landed in Japn is separated in the original data
    data_glob <- mutate(data_glob, countries = as.character(name))
    data_glob[2,10] = "Diamond Princess Cruise"
    data_glob[5,10] = "Japan"
    data_glob[7,10] = "Iran"
    # change the sequence of columes
    data_glob_ <- select(data_glob, countries, confirm:deadRate, heal, healRate)
    # output$txtout <- renderText({
    #     paste(input$txt, input$slider, format(input$date), sep = ", ")
    # })
    output$table_glob <- DT::renderDataTable(DT::datatable({data_glob_}))
    # require(chinamap)
    # cn = get_map_china()
    # plot(cov_hist, region='china', chinamap=cn, font.size=2,
         # date = test)
    ## translate province of polygons in map
    # cn$province <- trans_province(cn$province)
    test = as.character(input$date_glob)
    test = c("2020-02-23")
    # plot the worldmap
    mapworld <- borders("world", colour = "gray75", fill="white",
                        show.legend = FALSE) #basic map
    output$map_glob <- renderPlot({
        ggplot() + mapworld + ylim(-60,90) +
            labs(caption = paste("accessed date:", time(cov_data)))
    })
}

shinyApp(ui,server)

