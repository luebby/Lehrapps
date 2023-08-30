library(shiny)
library(shinydashboard)
library(knitr)
library(dplyr)
library(kableExtra)
library(shinyjs)
library(googlesheets4)

gs4_auth(cache = ".secrets", email = "karlue74@gmail.com")
ss <- "https://docs.google.com/spreadsheets/d/1uj5e61UJvr2f1LDRAJX-WLgBG89lMEW6t-uZmt9GEGY"

vorlage <- data.frame(
  Versuch = 0,
  Zahl1 = 2,
  Zahl2 = 4,
  Zahl3 = 8,
  Regel = "Erfüllt!"
)

checkregel <- function(x){
  if (x[1]<x[2] & x[2]<x[3]) erg <- "Erfüllt!"
  else erg <- "Nicht erfüllt."
  return(erg)
}

saveData <- function(data, regel){
  n <- nrow(data) - 1
  nnicht <- sum(data$Regel == "Nicht erfüllt.")
  erg <- data.frame(
  n = n,
  nnicht = nnicht,
  regel = regel)
  
  sheet_append(ss, erg)
}
  

ui <- dashboardPage(
  dashboardHeader(title = "Regel finden!"),
  dashboardSidebar(
    h3("Aufgabenstellung:"),
    p("Wir haben eine Regel ausgewählt, die für einige Folgen von drei Zahlen gilt - und für andere nicht."),
    p("Ihre Aufgabe ist es, zu erraten, wie die Regel lautet."),
    p("Wir fangen damit an, dass die Folge 2, 4, 8 der Regel gehorcht."),
    p("Jetzt sind Sie an der Reihe."),
    p("Geben Sie eine Zahlenfolge in die Felder ein, und ich sage Ihnen, ob sie die Regel erfüllt oder nicht."),
    p("Sie können so viele Folgen testen, wie Sie möchten."),
    p("Ihre Aufgabe ist es die Regel zu finden!"),
    p("Hinweis: Die ausgewählten Zahlen sowie Ihre Regel wird zu Lehr- und Forschungszwecken anonym ausgewertet.")
  ),
  
  dashboardBody(
    useShinyjs(),
    fluidRow(
          splitLayout(
            numericInput("num1", "Erste Zahl", value = 2),
            numericInput("num2", "Zweite Zahl", value = 4),
            numericInput("num3", "Dritte Zahl", value = 8)
            ),
      actionButton("show", "Zahlen prüfen!"),
      h3(" Auswertung:"),
      tableOutput("auswertung"),
      p(" Wenn Sie meinen die Regel zu kennen, geben Sie sie bitte hier ein:"),
      textInput("regel", "Regel", "", width = "100%"),
      p("Bevor Sie abgeben:"),
      p("Sind Sie sich sicher? Sie haben nur eine Chance!"),
      actionButton("abgabe", "Regel abgeben!"),
      hidden(div(id='text_div',
                 verbatimTextOutput("text")))
    )
  )
)





# Erstellen Sie eine Serverfunktion
server <- function(input, output) {
  
  # Erstellen Sie eine reaktive Variable, um die eingegebenen Zahlen zu speichern
  values <- reactiveValues(data = vorlage)
  
  # Beobachten Sie die Änderungen in den Eingaben und fügen Sie sie der Variablen hinzu
  observeEvent(input$show, {
    # Erhöhen Sie die Anzahl der Versuche um eins
    values$trial <- ifelse(is.null(values$trial), 1, values$trial + 1)
    # Fügen Sie die Eingaben mit der Versuchsnummer der Variablen hinzu
    values$data <- rbind(values$data, c(values$trial, input$num1, input$num2, input$num3,
                                        checkregel(c(input$num1, input$num2, input$num3))))
  })
  
  observeEvent(input$abgabe, {
    toggle('text_div')
    output$text <- renderText({"Die Regel lautet: Die folgende Zahl ist größer! 
      Viele (Sie auch?) sagen etwas wie: Die Zahlen verdoppeln sich. 
      Sie vermuten diese Regel, und testen Zahlen, die dieser Regel gehochen, z.B. 3,6,12. 
      Dies ist aber eine Form des Confirmation Bias (Bestätigungsfehler)."})
    saveData(values$data, input$regel)
  })
  
  output$auswertung <- function() {
    values$data %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
}

# Starten Sie die Shiny App
shinyApp(ui = ui, server = server)