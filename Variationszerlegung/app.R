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


ui <- navbarPage(title = "Variationszerlegung",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Variationszerlegung"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Das allgemeine Modell lautet: Daten = Modell + Rest.")),
           fluidRow(column(12, "Da Daten i.a. variieren, stellt sich die Frage wie viel der Variation kann modelliert werden.")),
           fluidRow(column(12, "Dazu betrachten wir eine einfache lineare Regression.")),
           fluidRow(column(12, h3("Lineare Regression"))),
           fluidRow(column(12, "Das zugrundeliegende Modell lautet: y=2+1*x+e.")),
           fluidRow(column(12, "Für e können Sie verschiedene Standardabweichungen einstellen und so gucken welche Anteile der Variation von y modelliert werden können.")),
           fluidRow(column(12, "In blau sehen Sie die Abweichungen der Beobachtungen von y zum Mittelwert von y. Die Gesamtvariation.")),
           fluidRow(column(12, "In grün sehen Sie die Abweichungen der modellierten Werte von y zum Mittelwert von y. Die modellierte Variation.")),
           fluidRow(column(12, "In rot sehen Sie die Abweichungen der Beobachtungen von y zu den modellierten Werten von y. Die nicht erklärte Variation, der Rest."))
         )
        ),
                 

tabPanel("Lineare Regression", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(sliderInput("Error", "Fehlervariation (se)", 0, 10, 5, 0.1)),
               fluidRow(actionButton("SamplenGo", "Los!")),
               fluidRow(column(12,h4("Hinweis:"))),
               fluidRow(column(12,"Versuchen Sie verschiedene Fehlervariationen aus.")),
               fluidRow(column(12,"Wie stark streuen die Ergebnisse?"))
               ),
             mainPanel(
               fluidRow(column(6, h3("Variation insgesamt")), column(6, h3("Variation Modelliert"))),
               fluidRow(column(6, plotOutput("PlotTotal")), column(6, plotOutput("PlotModel"))),
               fluidRow(column(6, h3("Variation Rest")), column(6, h3("Variationszerlegung"))),
               fluidRow(column(6, plotOutput("PlotRest")), column(6, plotOutput("PlotVariation"))),
               fluidRow(column(12, h3("Regressionausgabe"))),
               fluidRow(column(12, verbatimTextOutput("Erglm")))
               )
         )))
)




server <- function(input, output) {

   
Stipro <- eventReactive(input$SamplenGo, {
     
                           x <- 1:10
                           y <- 2+1*x
                           e <- scale(rnorm(n=length(x)))
                           y <- 2+1*x+input$Error*e
                           
                           daten <- data.frame(x=x, y=y)
                           erglm <- lm(y~x, data = daten)
                           daten.ext <-data.frame(x=x, y=y, y0=mean(y), yd=fitted(erglm), e=resid(erglm))
                           return(daten.ext)
                           }
                           
                           
                           )
   
   output$PlotTotal <- renderPlot({
     daten <- Stipro()
     gf_point(y~x, data = daten) %>%
       gf_lm(y~x, data = daten, color = "darkgreen", alpha = 0.5) %>%
       gf_hline(yintercept = mean(~y, data = daten), color = "blue") %>%
       gf_segment(y+y0 ~ x+x, color = "blue")
   })
   
   output$PlotModel <- renderPlot({
     daten <- Stipro()
     gf_point(y~x, data = daten) %>%
       gf_lm(y~x, data = daten, color = "darkgreen", alpha = 1) %>%
       gf_hline(yintercept = mean(~y, data = daten), color = "blue", alpha = 0.5) %>%
       gf_segment(yd+y0 ~ x+x, color = "darkgreen")
   })
  
   output$PlotRest <- renderPlot({
     daten <- Stipro()
     gf_point(y~x, data = daten) %>%
       gf_lm(y~x, data = daten, color = "darkgreen", alpha = 0.5) %>%
       gf_hline(yintercept = mean(~y, data = daten), color = "blue", alpha = 0.5) %>%
       gf_segment(yd+y ~ x+x, color = "red")
   })
   
   output$PlotVariation <- renderPlot({
     daten <- Stipro()
     
     ###
     sst <- sum((daten$y-daten$y0)^2)
     ssg <- sum((daten$yd-daten$y0)^2)
     sse <- sum((daten$y-daten$yd)^2)
     ###
     Variation <- c("Daten", "Modell", "Rest")
     Quadratsumme <- c(sst, ssg, sse)
     ###
     daten.aov <- data.frame(Variation = Variation, Quadratsumme = Quadratsumme)
     
     gf_col(Quadratsumme ~ Variation, data=daten.aov, fill = c("blue", "darkgreen", "red"))
   })
   
   
   output$Erglm <- renderPrint({
     erglm <- lm(y~x, data = Stipro())
     summary(erglm)
     })

}



# Run the application 
shinyApp(ui = ui, server = server)

