library(shiny)
library(shinydashboard)
library(shinyjs)
library(googlesheets4)
library(dplyr)
library(knitr)
library(kableExtra)

# Authentifizierung für google-sheet
gs4_auth(cache = ".secrets", email = "karlue74@gmail.com", use_oob = TRUE)
ss <- "https://docs.google.com/spreadsheets/d/1Ex-yumPZGtpky2CntTEJDUUyJa-2_8KKV20EbPfvpW8"
lotto <- read.csv2("Data/lotto.csv")

# Hilsfunktionn zum Speichern der Abgabe
saveData <- function(data) {
  # The data must be a dataframe rather than a named vector
  data <- data %>% as.list() %>% data.frame()
  # Add the data as a new row
  sheet_append(ss, data)
}
# Hilfsfunktion zum Berechnen der Anzahl Richtigen
Richtige <- function(data) {
  richtige <- lotto %>%
    rowwise(ziehung) %>%
    summarise(richtige = sum(data %in% c(z1,z2,z3,z4,z5,z6))) %>%
    ungroup() %>%
    count(richtige) 
  return(richtige)
}


# Anpassungen um die Zahlen 1 bis 49 als "Matrix" darzustellen
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 180px;
                                   -webkit-column-count: 7; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 7;    /* Firefox */ 
                                   column-count: 7; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))

all_rows <- as.numeric(t(matrix(1:49, ncol = 7)))
controls <-
  list(tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'numSelector', 
                                   label = NULL,
                                   choices  = all_rows,
                                   inline   = FALSE))) 

# UI
# ui <-fluidPage(tweaks,
#                fluidRow(column(width = 12, controls),
#                  verbatimTextOutput("test_box"),
#                  actionButton("go", "Abgabe"),
#                  column(12,tableOutput('table'))
#                 ))

ui <- dashboardPage(
  dashboardHeader(title = "Auswahl 6 aus 49"),
  dashboardSidebar(
    useShinyjs(),
    h3("Hinweis:"),
    p("Wählen Sie 6 aus 49 Zahlen aus. Ihre Auswahl wird bei einem Klick auf Abgabe gespeichert. 
      Dabei werden keine personenbeziehbaren Daten gespeichert."),
    p("Die ausgewählten Zahlen werden zu Lehr- und Forschungszwecken ausgewertet."),
    verbatimTextOutput("test_box"),
    p("Ihre gewählten Zahlen werden mit den Lottozahlen der Jahre 1955 bis 2021 abgeglichen."),
    p("Datenbasis: https://www.westlotto.de"),
    actionButton("go", "Abgabe"),
    actionButton("refresh", "Zurücksetzen")
  ),
  
  dashboardBody(
    tweaks,
    fluidRow(
      h3("Auswahl 6 aus 49:"), 
      column(width = 12, controls),
      h3("Auswertung Abgabe:"),
      column(6,tableOutput('table')),
      column(3,tableOutput('mean')),
      column(3,tableOutput('prop')),
    )
  )
)




# Server
server <- function(input, output) {
  
    lotto_data <- reactive(input$numSelector)
    
    # Wenn Abgabe, dann speichern und Ergebnis berichten
    observeEvent(input$go, {
      if(length(input$numSelector)==6)
        {saveData(lotto_data())
        richtige <- Richtige(lotto_data())
        output$table <- function()
          {richtige %>%
            rename('Anzahl Richtige' = 'richtige',
                   'Anzahl Ziehungen' = 'n') %>%
            kable("html") %>%
            kable_styling("striped", full_width = F)}
        # output$table <- renderTable(richtige)
        
        output$mean <- function()
        {
          richtige %>%
            summarise("Mittelwert Anzahl Richtige" = weighted.mean(richtige, n)) %>%
            kable("html") %>%
            kable_styling("striped", full_width = F)
        }
        
        output$prop <- function()
        {
          gesamt <- richtige %>%
            summarise(ziehungen = sum(n))
          
          richtige %>%
            filter(richtige >=3) %>%
            summarise(gewinne = sum(n)) %>%
            mutate("Anteil Ziehungen mit Gewinn" = gewinne / gesamt$ziehungen) %>%
            select(-gewinne) %>%
            kable("html") %>%
            kable_styling("striped", full_width = F)
        }
        
        
        }
      })
    
    observeEvent(input$refresh, {
      refresh()
    })


    # Ausgabe bis 6 Zahlen ausgewählt wurden.
    output$test_box <- renderText({
      validate(need(length(input$numSelector)==6, "Bitte 6 Zahlen wählen."))
    })
    }



shinyApp(ui = ui, server = server)