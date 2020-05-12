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



ui <- navbarPage(title = "Konfidenzintervall",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Konfidenzintervall"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Ein Konfidenzintervall soll die Unsicherheit bei der Schätzung eines unbekannten Wertes, 
                           z.B. Mittelwert oder Anteilswert in der Population berücksichtigen.")),
           fluidRow(column(12, "Diese Unsicherheit entsteht u.a. dadurch, dass wir nur eine zufällige Stichprobe vorliegen haben.")),
           fluidRow(column(12, "Ein Konfidenzintervall soll den wahren, unbekannten und festen Wert mit einer vorgegebenen Sicherheit überdecken.")),
           fluidRow(column(12, "Die Sicherheit bezieht sich aber auf das Verfahren, d.h. es ist eine Aussage über die Performance bzw. Fehlerwahrscheinlichkeit des Verfahrens 
                           - nicht direkt des Wertes in der Population.")),
           fluidRow(column(12, "Was für die konkrete, vorliegende Stichprobe gilt wissen wir nicht.")),
           fluidRow(column(12, "In dieser App werden Stichproben gemäß einer bekannten Verteilung in der Population simuliert.")),
           fluidRow(column(12, h4("Mittelwert"))),
           fluidRow(column(12, "Hier wird eine Normalverteilung zugrundegelegt.")),           
           fluidRow(column(12, "Diese hängt von Mittelwert und Standardabweichung ab.")),
           fluidRow(column(12, "Hier können Sie verschiedene Parameter, Stichprobenumfänge und Sicherheiten ausprobieren.")),
           fluidRow(column(12, h4("Anteilswert"))),
           fluidRow(column(12, "Hier wird je Beobachtung eine Bernoulli-Verteilung zugrundegelegt.")),   
           fluidRow(column(12, "Diese hängt von der Erfolgswahrscheinlichkeit ab.")),
           fluidRow(column(12, "Hier können Sie verschiedene Parameter, Stichprobenumfänge und Sicherheiten ausprobieren.")),
           fluidRow(column(12, h4("Hinweise:"))),
           fluidRow(column(12, "Wie verändert sich die Breite der Konfidenzintervalle bei den verschiedenen Einstellungen?")),
           fluidRow(column(12, "Was verändert sich nicht?"))
      )),
                 
tabPanel("Mittelwert", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(12, h4("Parameter Verteilung"))),
               fluidRow(sliderInput("mu", "Mittelwert", min = -10, max = 10, value = 0)),
               fluidRow(sliderInput("sd", "Standardabweichung", value = 1, min = 0, max = 10)),
               fluidRow(column(12, h4("Stichprobenumfang und Sicherheit"))),
               fluidRow(sliderInput("nnorm", "Stichprobenumfang", value = 10, min = 2, max = 100)),
               fluidRow(sliderInput("alphanorm", "Sicherheit", min = 0.8, max =0.999, value = 0.95, step=0.01)),
               fluidRow(actionButton("SimNorm", "Los!"))),
             mainPanel(
               fluidRow(column(12, plotOutput("CInorm")))
               )
         ))),

tabPanel("Anteilswert", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(12, h4("Parameter Verteilung"))),
               fluidRow(sliderInput("pi", "Anteilswert", min = 0.01, max = 0.99, value = 0.5, step = 0.01)),
               fluidRow(column(12, h4("Stichprobenumfang und Sicherheit"))),
               fluidRow(sliderInput("nbinom", "Stichprobenumfang", value = 10, min = 2, max = 100)),
               fluidRow(sliderInput("alphabinom", "Sicherheit", min = 0.8, max =0.999, value = 0.95, step=0.01)),
               fluidRow(actionButton("SimBinom", "Los!"))),
             mainPanel(
               fluidRow(column(12, plotOutput("CIbinom")))
             )
           )))



)





server <- function(input, output) {
  

  simNorm <-  eventReactive(input$SimNorm,
                            {
                              CIsim(n=input$nnorm, conf.level = input$alphanorm,
                                    args = list(mean=input$mu, sd=input$sd),
                                    estimand = input$mu,
                                    verbose = FALSE)
                            })
  
  simBinom <-  eventReactive(input$SimBinom,
                            {
                              CIsim(n=input$nbinom, 
                                    rdist = rbinom,
                                    conf.level = input$alphabinom,
                                    args = list(size=1, prob=input$pi),
                                    estimand = input$pi,
                                    method = binom.test,
                                    verbose = FALSE)
                            })
  
  output$CInorm <- renderPlot({
    simNorm()
  })
  
  output$CIbinom <- renderPlot({
    simBinom()
  })



}





# Run the application 
shinyApp(ui = ui, server = server)

