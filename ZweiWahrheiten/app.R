library(googlesheets4)
library(shiny)
library(lubridate)
library(dplyr)

gs4_deauth()
results <- read_sheet("https://docs.google.com/spreadsheets/d/1rNdph1RbTv15lKTNzoex6_Uhw4t9Lp9Bvzi3Q1ShAS4/edit#gid=1588966171")
results <- results %>%
  rename(Sicherheit = `Wie sicher sind Sie, die Lüge erkannt zu haben?`,
         Richtig = `Lagen Sie richtig?`) %>%
  mutate(Datum = date(Zeitstempel)) %>%
  mutate(Korrekt = ifelse(Richtig == "Ja", 1, 0)) %>%
  select(Datum, Korrekt)

ui <- fluidPage(
    h3("Auswertung Zwei Wahrheiten, eine Lüge"),
    dateInput("date", "Datum:"),
    tableOutput('table')
    )
  

shinyApp(ui, server = function(input, output) { 
  output$table <- renderTable(
    results %>%
      filter(Datum == input$date) %>%
      summarise(n = n(), richtig=sum(Korrekt)))
  })
