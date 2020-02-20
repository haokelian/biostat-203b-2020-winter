#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


    ui = navbarPage(
        title="导航条风格",
        tabPanel("One", icon=icon("home")),
        tabPanel("Two"),
        navbarMenu(
            title = "Three",
            tabPanel("Four"),
            tabPanel("Five")
        ),
        navlistPanel(
            # title = "导航面板风格",
            widths = c(3, 9),
            tabPanel("One", icon=icon("home")),
            tabPanel("Two"),
            navbarMenu(
                title = "Three",
                tabPanel("Four"),
                tabPanel("Five")
            )
        ),
        h4("标签页风格"),
        tabsetPanel(
            type = "pills",
            tabPanel("One", icon=icon("home")),
            tabPanel("Two"),
            navbarMenu(
                title = "Three",
                tabPanel("Four"),
                tabPanel("Five")
            )
        ),
        tags$head(
            tags$style(".tab-content .tab-content {border: 1px solid gray; min-height:200px;}")
        )
    )
    
    server = function(session, input, output) {
    }

shinyApp(ui,server)