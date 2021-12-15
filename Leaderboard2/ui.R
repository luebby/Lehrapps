#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    useShinyjs(debug=TRUE),
    # Application title
    titlePanel("Leaderboard - First Test"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput(
              "NutzerInnenName",
              "Ihr Name:"
            ),
            numericInput(
              "NutzerInnenWert",
              "Ihr Score:",
              value = 0,
              min = 0,
              max = 100000,
              step = 0.01
            ),
            actionButton(
              "submitScore",
              "Score aktualisieren!" #, icon("rsync")
            ),
            verbatimTextOutput("value"),
            fluidRow(
              DT::dataTableOutput("leaderboard")
            )
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
              plotOutput("plot1", click = "plot_click"),
              textOutput("check"), # not useful
            )
        )
    )
))
