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

daten <- read.csv2("results-survey372953.csv", 
                   fileEncoding = "UTF-8")

daten <- daten %>%
  filter(Gewicht >= 50 & Gewicht <= 150) %>%
  filter(Groesse >= 150 & Groesse <= 215) %>%
  mutate(Geschlecht = case_when(Geschlecht=="Weiblich" ~ "Frau",
                                Geschlecht=="Männlich" ~ "Mann")) %>%
  na.omit()

lmorg <- lm(Gewicht ~ Groesse*Geschlecht, data = daten)

xlim <- c(125,225)
ylim <- c(40,160)

ui <- navbarPage(title = "Modellierung",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Modellierung und Simulation"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Das Gewicht variiert. Manche wiegen 80kg, andere 60kg")),
           fluidRow(column(12, "Hängt das vielleicht mit der Größe und dem Geschlecht zusammen?")),
           fluidRow(column(12, "Dazu betrachten wir eine lineare Regression mit Wechselwirkung, 
                           d.h. der evt. Zusammenhang zwischen Größe und Gewicht wird evt. durch das Geschlecht morderiert.")),
           fluidRow(column(12, h3("Gesamtdatensatz"))),
           fluidRow(column(12, "Hier sehen Sie die insgesamt zur Verfügung stehende Stichprobe.")),
           fluidRow(column(12, h3("Stichprobe"))),
           fluidRow(column(12, "Hier können Sie aus den zur Verfügung stehenden Daten den Vorgang des Stichprobenziehens simulieren 
                           und das Ergebnis vergleichen.")),
           fluidRow(column(12, h3("Resample"))),
           fluidRow(column(12, "Hier können Sie das Re-Samplen simulieren und die Ergebnisse vergleichen.")),
           fluidRow(column(12, h3("Permutation"))),
           fluidRow(column(12, "Hier können Sie eine zufällige Zuordnung simulieren und Ergebnisse gemäß verschiedener
                           Nullmodelle (kein Zusammenhang) vergleichen.")),
           fluidRow(column(12, h3("Einfach Regression"))),
           fluidRow(column(12, "Hier sehen Sie das Ergebnis wenn Sie nur die Größe bzw. nur das Geschlecht als Modellierung des Gewichts heranziehen."))
         )
        ),
                 
tabPanel("Gesamtdatensatz", 
         fluidPage(
           titlePanel("Größe, Geschlecht und Gewicht"),
           fluidRow(column(12, h3("Regression Gesamtdatensatz"))),
           fluidRow(column(12, plotlyOutput("PlotOriginal"))),
           fluidRow(column(12, verbatimTextOutput("ErgOriginal"))),
           fluidRow(column(12, h3("Gesamtdatensatz"))),
           fluidRow(column(12, dataTableOutput("DatOriginal")))
           )
         ),
tabPanel("Stichprobe", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(sliderInput("Samplen", "Anzahl Beobachtungen", 5, 100, 50)),
               fluidRow(actionButton("SamplenGo", "Los!")),
               fluidRow(column(12,h4("Hinweis:"))),
               fluidRow(column(12,"Versuchen Sie verschiedene Stichprobenumfänge aus.")),
               fluidRow(column(12,"Wie stark variiert das Ergebnis?")),
             fluidRow(column(12,"Wie stark weicht es vom Original ab?"))),
             mainPanel(
               fluidRow(column(12, h3("Regression Stichprobe"))),
               fluidRow(column(12, plotlyOutput("PlotStipro"))),
               fluidRow(column(12, verbatimTextOutput("ErgStipro"))),
               fluidRow(column(12, h3("Stichprobe"))),
               fluidRow(column(12, dataTableOutput("DatStipro")))
               )
         ))),
tabPanel("Resample", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(actionButton("ResamplenGo", "Los!")),
               fluidRow(column(12,h4("Hinweis:"))),
               fluidRow(column(12,"Wie stark variiert das Ergebnis?")),
               fluidRow(column(12,"Wie stark weicht es vom Original ab?")),
               fluidRow(column(12,"Welche Beobachtungen sind im Resample?"))),
             mainPanel(
               fluidRow(column(12, h3("Regression Resample"))),
               fluidRow(column(12, plotlyOutput("PlotResample"))),
               fluidRow(column(12, verbatimTextOutput("ErgResample"))),
               fluidRow(column(12, h3("Resample"))),
               fluidRow(column(12, dataTableOutput("DatResample")))
               )
          ))),
tabPanel("Permutation", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(radioButtons("Shufflen", "Welche Variable soll permutiert werden?", c("Gewicht", "Größe", "Geschlecht"))),
               fluidRow(actionButton("ShuffleGo", "Los!")),
               fluidRow(column(12,h4("Hinweis:"))),
               fluidRow(column(12,"Wie verändern sich Achsenabschnitt und Steigung?")),
               fluidRow(column(12,"Wie starkt variiert das Ergebnis?")),
               fluidRow(column(12,"Gibt es zufällig signifikante Effekte?")),
               fluidRow(column(12,"Welche Werte wurden vertauscht?"))),
             mainPanel(
               fluidRow(column(12, h3("Regression Permutation"))),
               fluidRow(column(12, plotlyOutput("PlotPermutation"))),
               fluidRow(column(12, verbatimTextOutput("ErgPermutation"))),
               fluidRow(column(12, h3("Permutation"))),
               fluidRow(column(12, dataTableOutput("DatPermutation")))
             )
           ))),
tabPanel("Einfache Regression", 
         fluidPage(
           fluidRow(column(12, h3("Regression Gesamtdatensatz: nur Größe und Gewicht"))),
           fluidRow(column(12, plotlyOutput("PlotTeil"))),
           fluidRow(column(12, verbatimTextOutput("ErgTeil"))),
           fluidRow(column(12, h3("Regression Gesamtdatensatz: nur Geschlecht und Gewicht"))),
           fluidRow(column(12, plotlyOutput("PlotGeschlecht"))),
           fluidRow(column(12, verbatimTextOutput("ErgGeschlecht")))
             )
           )
)




server <- function(input, output) {

  # Original
  
   output$DatOriginal <- renderDataTable(daten)
   output$PlotOriginal <- renderPlotly({
     plmorg <- gf_point(Gewicht ~ Groesse, col = ~ Geschlecht, data=daten) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.5) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.5) 
  
     ggplotly(plmorg)
     })
   output$ErgOriginal <- renderPrint({
     summary(lmorg)
   })
   
  # Stichprobe
   
   Stipro <- eventReactive(input$SamplenGo, sample(daten, input$Samplen))
   
   output$DatStipro <- renderDataTable(Stipro())
   output$PlotStipro <- renderPlotly({
     lmStipro <- lm(Gewicht ~ Groesse*Geschlecht, data = Stipro())
     plmorg <- gf_point(Gewicht ~ Groesse, col = ~Geschlecht, data=Stipro()) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = coef(lmStipro)[2], color = "red", alpha = 0.5) %>%
       gf_abline(intercept = coef(lmStipro)[1]+coef(lmStipro)[3], slope = coef(lmStipro)[2]+coef(lmStipro)[4], color = "blue", alpha = 0.5) 
     ggplotly(plmorg)
   })
   output$ErgStipro <- renderPrint({
     summary(lm(Gewicht ~ Groesse*Geschlecht, data = Stipro()))
   })
   
   
   # Resample
   
   Resample <- eventReactive(input$ResamplenGo, resample(daten))
   
   output$DatResample <- renderDataTable(Resample())
   output$PlotResample <- renderPlotly({
     lmStipro <- lm(Gewicht ~ Groesse*Geschlecht, data = Resample())
     plmorg <- gf_point(Gewicht ~ Groesse, col = ~Geschlecht, data=Resample()) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = coef(lmStipro)[2], color = "red", alpha = 0.5) %>%
       gf_abline(intercept = coef(lmStipro)[1]+coef(lmStipro)[3], slope = coef(lmStipro)[2]+coef(lmStipro)[4], color = "blue", alpha = 0.5) 
     ggplotly(plmorg)
   })
   output$ErgResample <- renderPrint({
     summary(lm(Gewicht ~ Groesse*Geschlecht, data = Resample()))
   })
   
   
   # Permutation
   
   Shuffle <- eventReactive(input$ShuffleGo, { 
                              if (input$Shufflen=="Größe") daten_perm <- sample(daten, shuffled="Groesse")
                              if (input$Shufflen=="Gewicht") daten_perm <- sample(daten, shuffled="Gewicht")
                              if (input$Shufflen=="Geschlecht") daten_perm <- sample(daten, shuffled="Geschlecht")
                              daten_perm
                            })
   
   output$DatPermutation <- renderDataTable(Shuffle())
   output$PlotPermutation <- renderPlotly({
     lmStipro <- lm(Gewicht ~ Groesse*Geschlecht, data = Shuffle())
     plmorg <- gf_point(Gewicht ~ Groesse, col = ~Geschlecht, data=Shuffle()) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = coef(lmStipro)[2], color = "red", alpha = 0.5) %>%
       gf_abline(intercept = coef(lmStipro)[1]+coef(lmStipro)[3], slope = coef(lmStipro)[2]+coef(lmStipro)[4], color = "blue", alpha = 0.5) 
     ggplotly(plmorg)
   })
   output$ErgPermutation <- renderPrint({
     summary(lm(Gewicht ~ Groesse*Geschlecht, data = Shuffle()))
   })
   
# Einfachregression

   output$PlotTeil <- renderPlotly({
     lmStipro <- lm(Gewicht ~ Groesse, data = daten)
     plmorg <- gf_point(Gewicht ~ Groesse, data=daten) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = coef(lmStipro)[2], color = "black", alpha = 0.8) 
     ggplotly(plmorg)
   })   
   
   output$ErgTeil <- renderPrint({
     summary(lm(Gewicht ~ Groesse, data = daten))
   })
   
   output$PlotGeschlecht <- renderPlotly({
     lmStipro <- lm(Gewicht ~ Geschlecht, data = daten)
     plmorg <- gf_point(Gewicht ~ Geschlecht, col= ~ Geschlecht, data=daten,
                        position = "jitter", width = 0.05, height = 0) %>%
       gf_lims(y =ylim) %>%
       gf_hline(yintercept = coef(lmStipro)[1], color = "red", alpha = 0.5) %>%
       gf_hline(yintercept = coef(lmStipro)[1]+coef(lmStipro)[2], color = "blue", alpha = 0.5)
     ggplotly(plmorg)
   })   
   output$ErgGeschlecht <- renderPrint({
     summary(lm(Gewicht ~ Geschlecht, data = daten))
   })
   
   
}





# Run the application 
shinyApp(ui = ui, server = server)

