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



ui <- navbarPage(title = "Verteilungen",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Verteilungen"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Der Wert der Verteilungsunktion F(x) gibt an, wie wahrscheinlich es ist, dass eine Zufallsvariable X einen Wert kleiner oder gleich x annimmt.")),
           fluidRow(column(12, "Die Quantilsfunktion (die Umkehrfunktion der Verteilungsfunktion) gibt an, welcher Wert mit einer gegebenen Wahrscheinlichkeit p (Anteil) nicht überschritten wird.")),
           fluidRow(column(12, "Diese Wahrscheinlichkeiten bzw. Werte hängen von der zugrundeliegenden Wahrscheinlichkeitsfunktion ab.")),
           fluidRow(column(12, "Diese wiederum hängen oft von Parametern, z. B. Mittelwert oder Anteilswert ab.")),
           fluidRow(column(12, h4("Normalverteilung"))),
           fluidRow(column(12, "Diese hängt von Mittelwert und Standardabweichung ab.")),
           fluidRow(column(12, "Hier können Sie verschiedene Parameter, Werte x und Anteile p ausprobieren")),
           fluidRow(column(12, h4("Binomialverteilung"))),
           fluidRow(column(12, "Diese hängt von der Erolgswahrscheinlichkeit und Anzahl Beobachtungen n ab.")),
           fluidRow(column(12, "Hier können Sie verschiedene Parameter, Werte x und Anteile p ausprobieren")))
      ),
                 
tabPanel("Normalverteilung", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(12, h4("Parameter"))),
               fluidRow(numericInput("mu", "Mittelwert", value = 100)),
               fluidRow(numericInput("sd", "Standardabweichung", value = 15, min = 0)),
               fluidRow(column(12, h4("x bzw. p"))),
               fluidRow(numericInput("q", "Wert x", value = 115)),
               fluidRow(numericInput("p", "Anteil p", value = 0.9, min=0, max=1, step=0.01))),
             mainPanel(
               fluidRow(column(12, h3("Verteilungsfunktion"))),
               fluidRow(column(12, plotOutput("Plotpnorm"))),
               fluidRow(column(12, verbatimTextOutput("pnorm"))),
               fluidRow(column(12, h3("Quantilsfunktion"))),
               fluidRow(column(12, plotOutput("Plotqnorm"))),
               fluidRow(column(12, verbatimTextOutput("qnorm")))
               )
         ))),

tabPanel("Binomialverteilung", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(12, h4("Parameter"))),
               fluidRow(numericInput("pi", "Erfolgswahrscheinlichkeit pi", value = 1/3, min=0.001, max=0.999, step=0.01)),
               fluidRow(numericInput("n", "Anzahl Beobachtungen", value = 34, min = 1)),
               fluidRow(column(12, h4("x bzw. p"))),
               fluidRow(numericInput("q2", "Wert x", value = 12)),
               fluidRow(numericInput("p2", "Anteil p", value = 0.9, min=0, max=1, step = 0.01))),
             mainPanel(
               fluidRow(column(12, h3("Verteilungsfunktion"))),
               fluidRow(column(12, plotOutput("Plotpbinom"))),
               fluidRow(column(12, verbatimTextOutput("pbinom"))),
               fluidRow(column(12, h3("Quantilsfunktion"))),
               fluidRow(column(12, plotOutput("Plotqbinom"))),
               fluidRow(column(12, verbatimTextOutput("qbinom")))
             )
           )))


)





server <- function(input, output) {
  

  
  
  output$Plotpnorm <- renderPlot({
    xpnorm(q=input$q, mean=input$mu, sd = input$sd, return = "plot", verbose = FALSE)
    })
  
  output$pnorm <- renderPrint({
    pnorm(q=input$q, mean=input$mu, sd = input$sd)
  })
  
  output$Plotqnorm <- renderPlot({
    xqnorm(p=input$p, mean=input$mu, sd = input$sd, return = "plot", verbose = FALSE)
  })
  
  output$qnorm <- renderPrint({
    qnorm(p=input$p, mean=input$mu, sd = input$sd)
  })
  
  
  output$Plotpbinom <- renderPlot({
    xpbinom(q=input$q2, size=input$n, prob=input$pi, return = "plot", verbose = FALSE)
  })
  
  output$pbinom <- renderPrint({
    pbinom(q=input$q2, size=input$n, prob=input$pi)
  })
  
  output$Plotqbinom <- renderPlot({
    xqbinom(p=input$p2, size=input$n, prob=input$pi, return = "plot", verbose = FALSE)
  })
  
  output$qbinom <- renderPrint({
    qbinom(p=input$p2, size=input$n, prob=input$pi)
  })
  


}





# Run the application 
shinyApp(ui = ui, server = server)

