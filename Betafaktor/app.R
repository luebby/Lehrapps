##############################################################################
# Pakete
library(readxl)
library(quantmod)
library(dplyr)
library(ggformula)
library(shinydashboard)
library(markdown)


##############################################################################
# Hilfsfunktionen
CreateSubset <- function(iIdx, iConst){
  # iIdx: Index aus der Liste (1 bis 5)
  # iConst: Konstituent aus der Liste (1 bis 30)
  # Check Indices
  if(iIdx < 1 || iIdx > 5){
    print("Fehler bei der Auswahl des Vergleichsindex!")
    return()}
  
  if(iConst < 1 || iConst > 30){
    print("Fehler bei der Auswahl des Konstituenten!")
    return()}
  
  # Auswahl erzeugen und NAs löschen
  merge(Indexxts[,iIdx], DAX30xts[,iConst]) %>% na.omit()
}

GetDates <- function(period = "monatlich (36 Monate)"){
  # period: "monatlich", "wöchentlich" oder "täglich"
  
  xtsdat <- DAX30xts
  
  if(period == "monatlich (36 Monate)"){
    # maximale Länge der möglichen Daten bestimmen
    # Anzahl letzte Tage im Monat
    
    nRet <- 36
    
    nL <- xtsdat[endpoints(xtsdat, "months")] %>% nrow()
    if(nL < (nRet + 1)){
      print(paste0("Nicht genügend Daten vorhanden, verringern Sie die Anzahl auf ", nL - 1, " oder weniger!"))
    }
    else{
      # index: gibt Datum zurück
      # Auswahl von Anzahl geforderter Renditen + 1 bis Ende
      # substr: nur Datum ohne Angabe Zeitzone ausgeben
      (xtsdat[endpoints(xtsdat, "months")] %>% index())[nL:(nRet+1)] %>% substr(1,10) %>% na.omit()
    }
  }else if(period == "wöchentlich (52 Wochen)"){
    # maximale Länge der möglichen Daten bestimmen
    # Anzahl Freitage
    
    nRet <- 52
    
    nL <- xtsdat[.indexwday(xtsdat) == 5] %>% nrow()
    if(nL < (nRet + 1)){
      print(paste0("Nicht genügend Daten vorhanden, verringern Sie die Anzahl auf ", nL - 1, " oder weniger!"))
    }
    else{
      (xtsdat[.indexwday(xtsdat) == 5] %>% index())[(nL:nRet+1)] %>% substr(1,10) %>% na.omit()
    }
  }else{ # daily
    # maximale Länge der möglichen Daten bestimmen
    # Anzahl Tage
    
    nRet <- 250
    
    nL <- xtsdat %>% nrow()
    if(nL < (nRet + 1)){
      print(paste0("Nicht genügend Daten vorhanden, verringern Sie die Anzahl auf ", nL - 1, " oder weniger!"))
    }
    else{
      (xtsdat %>% index())[(nL:nRet+1)] %>% substr(1,10) %>% na.omit()
    }
  }
}



CalculateReturn <- function(xtsdat, date = endDate, period = "monatlich", type = "diskret"){
  ### Hinweis: hier keine Überprüfung der Funktionsparameter mehr ###
  # xtsdat: Datensatz mit Kursen von DAX und ausgewähltem Einzelwert
  # date: gewähltes Enddatum im Format "YYYY-MM-DD"
  # period: "monatlich", "wöchentlich" oder "täglich"
  # type: "arithmetic" (diskret) oder "log" (stetig)
  
  type <- ifelse(type == "diskret", "arithmetic", "log")
  
  if(period == "monatlich (36 Monate)"){
    nRet <- 36
    # Monatsrenditen bis zum Enddatum berechnen
    IndexRets <- monthlyReturn(xtsdat[, 1], subset = paste0("/", date), type = type)  # geht leider nur einzeln
    StockRets <- monthlyReturn(xtsdat[, 2], subset = paste0("/", date), type = type)
    nL <- nrow(IndexRets)
    IndexRets <- IndexRets[(nL-nRet+1):nL]
    StockRets <- StockRets[(nL-nRet+1):nL]
  }else if(period == "wöchentlich (52 Wochen)"){
    nRet <- 52
    # Wochenrenditen bis zum Enddatum berechnen
    IndexRets <- weeklyReturn(xtsdat[, 1], subset = paste0("/", date), type = type)  # geht leider nur einzeln
    StockRets <- weeklyReturn(xtsdat[, 2], subset = paste0("/", date), type = type)
    nL <- nrow(IndexRets)
    IndexRets <- IndexRets[(nL-nRet+1):nL]
    StockRets <- StockRets[(nL-nRet+1):nL]
  }else{ # daily
    # Tagesrenditen bis zum Enddatum berechnen
    nRet <- 250
    IndexRets <- dailyReturn(xtsdat[, 1], subset = paste0("/", date), type = type)  # geht leider nur einzeln
    StockRets <- dailyReturn(xtsdat[, 2], subset = paste0("/", date), type = type)
    nL <- nrow(IndexRets)
    IndexRets <- IndexRets[(nL-nRet+1):nL]
    StockRets <- StockRets[(nL-nRet+1):nL]
  }
  # coef(lm(StockRets ~ IndexRets))[2]
  Rets <- merge(IndexRets, StockRets)
  colnames(Rets) <- c("Index", "Stock")
  Rets
}



##############################################################################
# Vorverarbeitung
DAX30 <- read_excel("DAX30.xlsx", sheet = "Constituents", guess_max = 10000) 
Index <- read_excel("DAX30.xlsx", sheet = "Indices", guess_max = 10000) 


# guess_max notwendig, sonst wird aus Datenreihen, die erst nach 1000 Werten anfangen, logical
# in xts-Objekt umwandeln

DAX30xts <- xts(DAX30[-1], order.by = DAX30$Date)
Indexxts <- xts(Index[-1], order.by = Index$Date)
endDate <- DAX30xts %>% index() %>% last() %>% substr(1, 10)

# Selektiert aus dem vollständigen Datensatz die Namen der Konstituenten
aktien <- colnames(DAX30xts)
indices <- colnames(Indexxts)


##############################################################################
### User Interface

ui <- dashboardPage(
  dashboardHeader(title = "DAX Aktienbeta"),
  dashboardSidebar(
    selectInput("aktie", "Bitte wählen Sie eine Aktie:",
                choices = as.list(aktien)),
    selectInput("index", "Bitte wählen Sie den Vergleichsindex:",
                choices = as.list(indices)),
    selectInput("period", "Bitte wählen Sie das Berechnungsintervall:",
                choices = as.list(c("täglich (250 Tage)", "wöchentlich (52 Wochen)", "monatlich (36 Monate)"))),
    uiOutput("dateSelection"),
    selectInput("rendite", "Bitte wählen Sie die Art der Renditeberechnung:",
                choices = as.list(c("diskret", "stetig"))),
    actionButton("go", "Berechne!")
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("plotStock", height = 300)),
      box(plotOutput("plotAktie", height = 300)),
      box(plotOutput("plotStockReturn", height = 300)),
      box(plotOutput("plotAktieReturn", height = 300)),
      box(plotOutput("plotBeta", height = 450), title = "Streudiagramm Renditen", footer = "Aktienbeta (blau), Betafaktor = 1 (Referenz, rot)"),
      box(verbatimTextOutput("lmBeta"), title = "R Ausgabe"),
      box(withMathJax(includeMarkdown("Hintergrund.md")), width = 12, title = "Hintergrund")
    )
  )
)


######################################################
### Start: Server
######################################################

server <- function(input, output) {
  
  output$dateSelection <- renderUI({
    selectInput("date", "Bitte wählen Sie das Betrachtungsdatum", choices = GetDates(input$period))})
  
  
  data <- reactiveValues()
  
  observeEvent(input$go,{ 
    myAktie <- which(aktien == input$aktie)
    myDax <- which(indices == input$index)
    myStock <- CreateSubset(myDax, myAktie)
    
    myReturns <- CalculateReturn(myStock, input$date, input$period, input$rendite)
    
    mylm <- lm(Stock ~ Index, data = myReturns)
    
    myStock2Plot <- myStock[paste0(index(myReturns[1, ]),"/",index(myReturns[nrow(myReturns), ]))]
    
    isolate({
      data$myStock <- myStock2Plot
      data$myReturns <- myReturns
      data$mylm <- mylm
      })
    })
  
  output$plotStock <- renderPlot({
    if (input$go){
      autoplot(data$myStock[,1], main = "Kursverlauf Index") %>% gf_labs(x = "", y = "")
    }
  })
  
  output$plotAktie <- renderPlot({
    if (input$go){
      autoplot(data$myStock[,2], main = "Kursverlauf Aktie") %>% gf_labs(x = "", y = "")
    }
  })

  output$plotStockReturn <- renderPlot({
    if (input$go){
      autoplot(data$myReturns[,1], main = "Renditeverlauf Index") %>% gf_labs(x = "", y = "")
    }
  })
    
    output$plotAktieReturn <- renderPlot({
      if (input$go){
        autoplot(data$myReturns [,2], main = "Renditeverlauf Aktie") %>% gf_labs(x = "", y = "")
      }
  })

    
    output$plotBeta <- renderPlot({
      if (input$go) {
        gg <- gf_point(Stock ~ Index, data = as.data.frame(data$myReturns)) %>%
          gf_lm(interval = "prediction") %>%
          gf_vline(xintercept = ~ 0, color = "grey") %>%
          gf_hline(yintercept = ~ 0, color = "grey") %>%
          gf_abline(slope = ~ 1, intercept = ~ coef(data$mylm)[1], color = "red") %>%#
          gf_labs(title = paste0("Aktienbeta = ", round(coef(data$mylm)[2],3)),
                  subtitle = paste0("Modell: Aktienrendite = ",round(coef(data$mylm)[1],3), " + ", round(coef(data$mylm)[2],3)," * Marktrendite"))
       gg 
      }
    })
    
    output$lmBeta <- renderPrint({
      if (input$go) {
        summary(data$mylm)
        }
    })

  
  
  }

shinyApp(ui, server)
  
