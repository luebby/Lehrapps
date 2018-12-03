#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lsr)
library(mosaic)


ui <- navbarPage(title = "Mittelwertsvergleich",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Mittelwertsvergleich"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Häufig sollen die Mittelwerte zweier Gruppen (A/B) verglichen werden.")),
           fluidRow(column(12, "Das Ergebnis varriert aus vielfältigen Gründen:")),
           fluidRow(column(12, "Der Abstand der beiden Mittelwerte, die Streuung, der Stichprobenumfang - und die Stichprobe.")),
           fluidRow(column(12, h3("Mittelwertsvergleich"))),
           fluidRow(column(12, "Beide Gruppen (A/B) sind Zufallsstichproben einer Normalverteilung.")),
           fluidRow(column(12, "Der Mittelwert in der Population der Gruppe A (µ_A) ist 0.")),
           fluidRow(column(12, "Für den Mittelwert in der Population der Gruppe B (µ_B) können Sie verschiedene Werte ausprobieren.")),
           fluidRow(column(12, "Ebenfalls für die gemeinsame Standardabweichung (sd=sd_A=sd_B) sowie die jeweiligen Gruppengrößen (n=n_A=n_B)."))
         )
        ),
                 

tabPanel("Mittelwertsvergleich", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(sliderInput("mub", "Mittelwert Population B (µ_B)", 0, 2, 1, 0.1)),
               fluidRow(sliderInput("sd", "Streuung (sd)", 0.2, 2, 1, 0.1)),
               fluidRow(sliderInput("n", "Beobachtungen (n)", 20, 200, 50, 10)),
               fluidRow(actionButton("SamplenGo", "Los!")),
               fluidRow(column(12,h4("Hinweis:"))),
               fluidRow(column(12,"Versuchen Sie verschiedene Mittelwerte der Population für B aus.")),
               fluidRow(column(12,"Versuchen Sie verschiedene Streuungen aus.")),
               fluidRow(column(12,"Versuchen Sie verschiedene Stichprobenumfänge aus.")),
               fluidRow(column(12,"Wie stark variieren welche Ergebnisse?"))
               ),
             mainPanel(
               fluidRow(column(12, h3("Verteilungen"))),
               fluidRow(column(12, plotOutput("PlotDens"))),
               fluidRow(column(12, h3("t-Test"))),
               fluidRow(column(12, verbatimTextOutput("t")))
               )
         )))
)




server <- function(input, output) {

xlim <- c(-7.5,10)

Stipro <- eventReactive(input$SamplenGo, {
  A <- rnorm(input$n, mean=0, sd = input$sd)
  B <- rnorm(input$n, mean=input$mub, sd = input$sd)
  x <- c(A,B)
  Gruppe <- rep(c("A", "B"), each = input$n)
  daten <- data.frame(Gruppe = Gruppe, x =x)
  return(daten)
  })
   output$PlotDens <- renderPlot({
     daten <- Stipro()
     gf_dhistogram(~ x, fill = ~Gruppe,
                alpha = 0.2, data = daten,
                title = paste0(" Differnz Mittelwerte: ", round(diffmean(x~Gruppe, data = daten),2), 
                               ", Effektgröße Cohen's D: ",round(cohensD(x~Gruppe, data = daten),2))) %>%
       gf_lims(x=xlim) %>%
       gf_fitdistr(dist = "dnorm", col=~Gruppe)
       
   })
  
   
   
   output$t <- renderPrint({
     t.test(x~Gruppe, data = Stipro())
     })

}



# Run the application 
shinyApp(ui = ui, server = server)

