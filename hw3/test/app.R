# install.packages("shinythemes")
library(shiny)
library(shinythemes)

ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
        theme = "darkly", "Cronavirus Telescope",
        tabPanel("Worldwide Summary", icon=icon("home"),
                 
                 sidebarPanel(
                     textInput("txt", "Text input:", "general"),
                     sliderInput("slider", "Slider input:", 1, 100, 30),
                     tags$h5("Deafult actionButton:"),
                     actionButton("action", "Search"),
                     
                     tags$h5("actionButton with CSS class:"),
                     actionButton("action2", "Action button", class = "btn-primary")
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Tab 1",
                                  h4("Table"),
                                  tableOutput("table"),
                                  h4("Verbatim text output"),
                                  verbatimTextOutput("txtout"),
                                  h1("Header 1"),
                                  h2("Header 2"),
                                  h3("Header 3"),
                                  h4("Header 4"),
                                  h5("Header 5")
                         ),
                         tabPanel("Tab 2", "This panel is intentionally left blank"),
                         tabPanel("Tab 3", "This panel is intentionally left blank")
                     )
                 ),
                 h4("Trend"),
                 tabsetPanel(
                     tabPanel("Confirmed and Suspected Cases", icon=icon("home")),
                     tabPanel("Added Cases"),
                     tabPanel("Death Rate"),
                     id = NULL, selected = NULL, type = c("tabs", "pills"),
                     position = NULL),
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
    output$txtout <- renderText({
        paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
        head(cars, 4)
    })
}

shinyApp(ui,server)

