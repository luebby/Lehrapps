# ===========================================================================
# app.R (Release 0.1)
# =====----------------------------------------------------------------------
#
# Check Vorhersagen CDV
# ---------------------
#
# (W) by Norman Markgraf in 2020
#
# 06. Jul. 2020  (nm)  Aller erste Version (0.1)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
#   (C)opyleft Norman Markgraf (nmarkgraf@hotmail.com) in 2020
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#  Einstein’s Dictum:
#
#     “Everything should be as simple as possible, but no simpler.”
#
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
library(shiny)

options(shiny.maxRequestSize = 100*1024^2)
DEBUG <- FALSE


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Test"),
    
    sidebarLayout(
        sidebarPanel(
            navbarPage("Input", id = "allResults",
                       shiny::tabPanel(
                           value = 'inputData', 
                           title = 'Data Import',
                           br(),
                           
                           h4("Import data"),
                           
                           shiny::fileInput(inputId = "inFile", "Choose a CSV File",
                                            accept = c(
                                                "text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv"
                                            )
                           ),
                           
                           checkboxInput("header", "Header", TRUE)
                       )
            )
        ),
        mainPanel(
            # tableOutput("dispTable")
            htmlOutput("checkTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$checkTable <- renderText({
        inFile <- input$inFile
        correctColNumber = 2
        correctRowNumber = 100
        if (is.null(inFile)) {
            return("<p class=\"error\">Noch keine Datei hochgeladen!<p>")
        }
        df <- read.csv2(inFile$datapath, header = input$header)
        ausgabe <- ""
        x <- 0
        if (nrow(df) != correctRowNumber) {
            ausgabe <- paste("<p class=\"error\">Die Antwort muss", 
                             correctRowNumber, 
                             "Beobachtungen (Zeilen) enthalten!</p>")
            x <- x + 1
        }
        if (ncol(df) != correctColNumber) {
            ausgabe <- paste(ausgabe, 
                             paste("<p class=\"error\">Die Antwort muss",
                                    correctColNumber, 
                                    "Variabeln (Spalten) enthalten!</p>"), 
                             sep = "\r\n")
            x <- x + 2
        }
        if (x == 0) {
            ausgabe <- "<p class=\"okay\">Alles okay!</p>"
        }
        if (DEBUG) {
            paste(ausgabe, nrow(df), ncol(df), sep = "\r\n")
        } else {
            ausgabe
        }
    }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
