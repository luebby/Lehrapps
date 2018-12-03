#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mosaic)
library(plotly)
library(DT)
library(openintro)


data("marioKart")

daten.org <- marioKart %>%
  filter(totalPr <= 80) %>%
  group_by(wheels) %>% 
  summarise(Preis=mean(totalPr)) %>%
  ungroup() %>%
  rename(Steuer=wheels)  
    

xlim <- c(-1,5)
ylim <- c(20,80)

lm.ols <- lm(Preis~Steuer, data = daten.org)
sse.ols <- sum(resid(lm.ols)^2)

ui <- navbarPage(title = "Kleinste Quadrate",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Kleinste Quadrate"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Das Verkaufspreis einer ebay Auktion variiert. Manchmal beträgt er 30$, manchmal 60$.")),
           fluidRow(column(12, "Hängt das vielleicht mit der Austattung des angebotenen Produktes zusammen?")),
           fluidRow(column(12, "Wie können wir ein gutes Modell konstruieren?")),
           fluidRow(column(12, "Dazu betrachten wir ein einfaches lineares Modell: y=b_0+b_1*x+e.")),
           fluidRow(column(12, h4("Datensatz"))),
           fluidRow(column(12, "Es wurden n=141 Beobachtungen von Mario Kart für Nintendo Wii Auktionen im Oktober 2009 analysiert.")),
           fluidRow(column(12, "Die Daten stammen aus dem Datensatz marioKart aus dem R Paket openintro.")),
           fluidRow(column(12, "Siehe auch http://www.openintro.org/.")),
           fluidRow(column(12, "Dabei wurde der arithmetische Mittelwert des erzielten Preises je Anzahl der mitgelieferten Steuerräder gebildet.")),
           fluidRow(column(12, "Beachten Sie das wichtige Kovariablen (z.B. Artikelzustand) nicht berücksichtigt wurden.")),
           fluidRow(column(12, h3("Modellierung"))),
           fluidRow(column(12, "Versuchen Sie den mittleren Preis je Anzahl Steuerräder linear zu modellieren.")),
           fluidRow(column(12, "Hier können sie verschiedene Steigungen (b_1) und Achsenabschnitte (b_0) ausprobieren.")),
           fluidRow(column(12, "Ziel ist, eine möglichst geringe Summe an quadratischen Abweichungen vom Modell zu den Beobachtungen zu bekommen.")),
           fluidRow(column(12, h3("Lösung"))),
           fluidRow(column(12, "Hier finden Sie die Kleinste-Quadrate Lösung der Linearen Regression.")))
      ),
                 
tabPanel("Modellierung", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(sliderInput("b0", "Achsenabschnitt b_0", 25, 75, 55, 0.05)),
               fluidRow(sliderInput("b1", "Steigung b_1", 0, 15, 0, 0.05))),
             mainPanel(
               fluidRow(column(12, h3("Ihr Modell"))),
               fluidRow(column(12, plotOutput("PlotModel"))),
               fluidRow(column(12, plotOutput("PlotSSE")))
               )
         ))),

tabPanel("Lösung", 
         fluidPage(
           fluidRow(column(12, plotOutput("PlotOls"))),
           fluidRow(column(12, verbatimTextOutput("ErgOls")))
         ))

)




server <- function(input, output) {
  
  daten <- reactive({
    daten.mod <- daten.org %>%
      mutate(ydach = input$b0+input$b1*Steuer) %>%
      mutate(residual = Preis - ydach)
    return(daten.mod)
  })
  
  
  
  output$PlotModel <- renderPlot({
    gf_point(Preis ~ Steuer, data = daten()) %>%
      gf_lims(x=xlim, y=ylim) %>%
      gf_abline(intercept = input$b0, slope = input$b1, color = "red") %>%
      gf_segment(Preis+ydach ~ Steuer + Steuer, color="red", alpha = 0.5)
    })
  
  output$PlotSSE <- renderPlot({
    sse.mod <- sum((daten()$residual)^2)
    Ergebnis <- c("Ihr Modell", "Bestes Modell")
    Ergebnis.SSE <- c(sse.mod, sse.ols)
    sse.data <- data.frame(Vergleich=Ergebnis, Fehlerquadratsumme=Ergebnis.SSE)
    gf_col(Fehlerquadratsumme ~ Vergleich, data=sse.data, fill = c("blue", "red"),
           title = paste0("Ihre Fehlerquadratsumme: ",round(sse.mod,1)),
           subtitle = paste0("Bestes Modell: ",round(sse.ols,1)))
  })
  
  
  output$PlotOls <- renderPlot({
    gf_point(Preis  ~ Steuer, data = daten.org) %>%
      gf_abline(intercept = coef(lm.ols)[1], slope = coef(lm.ols)[2], color = "blue")
    }) 
  
  output$ErgOls <- renderPrint({
     summary(lm.ols)
     })  
   
}





# Run the application 
shinyApp(ui = ui, server = server)

