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
    titlePanel("Verbrauchprognose"),
    h3("Ihr Auftrag:"),
    p("Sie sind Einkäufer:in in einem Energieunternehmen. Sie kaufen die Energie ein, die Ihre Kund:innen benötigen. 
    Der Energieverbrauch hängt ab von der Temperatur. Diesen Zusammenhang wollen Sie ausnutzen um bedarfsgerecht einzukaufen.
      Kaufen Sie zu viel ein, müssen Sie mit Verlust weiterkaufen, kaufen Sie zu wenig müssen Sie mit Verlust nachkaufen."),
    p("Mit Hilfe Ihrer menschlichen Intelligenz: Finden Sie den Zusammenhang zwischen der Temperatur und dem Verbrauch! 
      Kaufen Sie optimal ein, mit möglichst geringem Verlust (Score)"),
    p("Tun Sie dies indem Sie zwei Punkte auf der Abbildung klicken und diese werden mit einer Gerade verbunden. 
     Eine neue Linie entsteht wenn Sie noch einmal klicken. Je niedriger Ihr Score desto besser!"),
    p("Wenn Sie zufrieden sind, geben Sie einen Namen ein und klicken Sie auf 'Score aktualisieren!'. Wie gut ist Ihr Modell? Sind Sie besser als die Künstliche Intelligenz von Dr. Stat?"),
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
              "Score aktualisieren!", icon("rsync")
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
