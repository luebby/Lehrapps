library(googlesheets4)
library(shiny)
library(shinyTime)
library(lubridate)
library(hms)
library(dplyr)

gs4_deauth()
results <- read_sheet("https://docs.google.com/spreadsheets/d/1rNdph1RbTv15lKTNzoex6_Uhw4t9Lp9Bvzi3Q1ShAS4/edit#gid=1588966171")
results <- results %>%
  rename(Sicherheit = `Wie sicher sind Sie, die Lüge erkannt zu haben?`,
         Richtig = `Lagen Sie richtig?`) %>%
  mutate(Datum = date(Zeitstempel)) %>%
  mutate(Zeit = as_hms(Zeitstempel)) %>%
  mutate(Korrekt = ifelse(Richtig == "Ja", 1, 0)) %>%
  select(Datum, Zeit, Korrekt)

ui <- fluidPage(
    h3("Auswertung Zwei Wahrheiten, eine Lüge"),
    p("Hinweis: Für Datenaktualisierung App schließen und neu starten."),
    dateInput("date", "Datum:"),
    timeInput("time1", "Startzeit:", value = Sys.time()-15*60, seconds = FALSE),
    timeInput("time2", "Endzeit:", value = Sys.time(), seconds = FALSE),
    tableOutput('table')
    )
  

shinyApp(ui, server = function(input, output) { 
  output$table <- renderTable(
    results %>%
      filter(Datum == input$date) %>%
      filter(Zeit >= as_hms(input$time1) & Zeit <= as_hms(input$time2)) %>%
      summarise(n = n(), richtig=sum(Korrekt)))
  })
