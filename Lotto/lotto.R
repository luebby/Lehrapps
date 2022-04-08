library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders) # für das Einbauen eines Wartezeichens bei Update der Eingabeparameter
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
      ## withSpinner: Wartezeichen beim Update des Output hinzufügen mit Ausblendeoption vor erstmaliger Auswertung
      column(2, hidden(div(id = 'outp4', withSpinner(tableOutput('zahlen'), type = 7)))),
      column(4, hidden(div(id = 'outp1', withSpinner(tableOutput('table'), type = 7)))),
      column(3, hidden(div(id = 'outp2', withSpinner(tableOutput('mean'), type = 7)))),
      column(3, hidden(div(id = 'outp3', withSpinner(tableOutput('prop'), type = 7)))),
    )
  )
)




# Server
server <- function(input, output) {
  
    lotto_data <- reactive(input$numSelector)
    
    # Wenn Abgabe, dann speichern und Ergebnis berichten
    observeEvent(input$go, {
      ## vor der erstmaligen Parameterauswahl das Wartezeichen ausblenden
      toggle(id = 'outp1', condition = FALSE)
      toggle(id = 'outp2', condition = FALSE)
      toggle(id = 'outp3', condition = FALSE)
      toggle(id = 'outp4', condition = FALSE)
      
      if(length(input$numSelector)==6)
        {saveData(lotto_data())
        richtige <- Richtige(lotto_data())
        zahlen <- sort(lotto_data()) %>% data.frame() %>% rename("Zahlen" = ".")
        
        ## Auswahl nach dem Abschicken wieder entfernen, um eine komplette Neuauswahl starten zu können
        updateCheckboxGroupInput(getDefaultReactiveDomain(), "numSelector",
                                 label = NULL,
                                 choices  = all_rows,
                                 inline   = FALSE,
                                 selected = NULL)
        
        ## Wartezeichen anzeigen während die Ausgabe geupdated wird
        show('outp1')
        show('outp2')
        show('outp3')
        show('outp4')
        
        toggle(id = 'outp1', condition = TRUE)
        toggle(id = 'outp2', condition = TRUE)
        toggle(id = 'outp3', condition = TRUE)
        toggle(id = 'outp4', condition = TRUE)
        
        output$zahlen <- function()
        {
          data.frame(zahlen) %>%
            kable("html") %>%
            kable_styling("striped", full_width = F)
        }
        
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