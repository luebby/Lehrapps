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

daten <- marioKart %>%
  filter(totalPr <= 80) %>%
  mutate(Zustand = case_when(cond=="new" ~ "neu",
                             cond=="used" ~ "gebraucht")) %>%
  rename(Dauer=duration) %>%
  rename(Preis=totalPr) %>%
  select(Preis, Dauer, Zustand) %>%
  na.omit()

lmorg <- lm(Preis ~ Dauer*Zustand, data = daten)

xlim <- c(0,12)
ylim <- c(25,80)

ui <- navbarPage(title = "Modellierung",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Modellierung und Simulation"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Der Verkaufspreis einer ebay Auktion variiert. Manchmal beträgt er 30$, manchmal 60$.")),
           fluidRow(column(12, "Hängt das vielleicht mit der Auktionsdauer und dem Zustand (neu/ gebraucht) zusammen?")),
           fluidRow(column(12, "Dazu betrachten wir eine lineare Regression mit Wechselwirkung, 
                           d.h. der vermutete Zusammenhang zwischen Dauer und Preis wird evtl. durch den Zustand modelliert.")),
           fluidRow(column(12, "Beachten Sie bitte, dass wichtige Kovariablen wie Ausstattungsmerkmale etc. hier nicht berücksichtigt werden.")),
           fluidRow(column(12, h3("Gesamtdatensatz"))),
           fluidRow(column(12, "Hier sehen Sie die insgesamt zur Verfügung stehende Stichprobe. 
                           Diese besteht aus n=141 Beobachtungen von Mario Kart für Nintendo Wii Auktionen im Oktober 2009.")),
           fluidRow(column(12, "Die Daten stammen aus dem Datensatz marioKart aus dem R Paket openintro.")),
           fluidRow(column(12, "Siehe auch http://www.openintro.org/.")),
           fluidRow(column(12, h3("Stichprobe"))),
           fluidRow(column(12, "Hier können Sie aus den zur Verfügung stehenden Daten den Vorgang des Stichprobenziehens simulieren 
                           und das Ergebnis vergleichen.")),
           fluidRow(column(12, h3("Resample"))),
           fluidRow(column(12, "Hier können Sie das Re-Samplen simulieren und die Ergebnisse vergleichen.")),
           fluidRow(column(12, h3("Permutation"))),
           fluidRow(column(12, "Hier können Sie eine zufällige Zuordnung simulieren und Ergebnisse gemäß verschiedener
                           Nullmodelle (kein Zusammenhang) vergleichen.")),
           fluidRow(column(12, "Achtung: durch Permutation einer einzelnen erklärenden Variable x wird auch der mögliche Zusammenhang 
                           mit den anderen erklärenden Variablen zerstört.")),
           fluidRow(column(12, h3("Weitere Regressionsmodelle"))),
           fluidRow(column(12, "Hier sehen Sie das Ergebnis, wenn Sie nur den Achsenabschnitt, nur die Dauer bzw. nur den Zustand als Modellierung des Preises heranziehen.
                           Auch das Ergebnis ohne Wechselwirkung wird gezeigt."))
         )
        ),
                 
tabPanel("Gesamtdatensatz", 
         fluidPage(
           titlePanel("Dauer, Zustand und Preis"),
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
               fluidRow(sliderInput("Sampeln", "Anzahl Beobachtungen", 2, 141, 50)),
               fluidRow(actionButton("SampelnGo", "Los!")),
               fluidRow(column(12,h4("Hinweis:"))),
               fluidRow(column(12,"Probieren Sie verschiedene Stichprobenumfänge aus.")),
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
               fluidRow(actionButton("ResampelnGo", "Los!")),
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
               fluidRow(radioButtons("Shufflen", "Welche Variable soll permutiert werden?", c("Preis", "Dauer", "Zustand"))),
               fluidRow(actionButton("ShuffleGo", "Los!")),
               fluidRow(column(12,h4("Hinweis:"))),
               fluidRow(column(12,"Wie verändern sich Achsenabschnitt und Steigung?")),
               fluidRow(column(12,"Wie starkt variiert das Ergebnis?")),
               fluidRow(column(12,"Gibt es zufällig signifikante Effekte?")),
               fluidRow(column(12,"Welche Werte wurden vertauscht?")),
               fluidRow(column(12,"Hinweis: Bei Permutation einer erklärenden wird ein möglicher Zusammenhang 
                               mit den anderen erklärenden Variablen ebenfalls zerstört."))),
             mainPanel(
               fluidRow(column(12, h3("Regression Permutation"))),
               fluidRow(column(12, plotlyOutput("PlotPermutation"))),
               fluidRow(column(12, verbatimTextOutput("ErgPermutation"))),
               fluidRow(column(12, h3("Permutation"))),
               fluidRow(column(12, dataTableOutput("DatPermutation")))
             )
           ))),
tabPanel("Weitere Regressionsmodelle", 
         fluidPage(
           fluidRow(column(12, h3("Regression Gesamtdatensatz: Preis~1"))),
           fluidRow(column(12, plotOutput("PlotI"))),
           fluidRow(column(12, verbatimTextOutput("ErgI"))),
           fluidRow(column(12, h3("Regression Gesamtdatensatz: Preis~Dauer"))),
           fluidRow(column(12, plotOutput("PlotTeil"))),
           fluidRow(column(12, verbatimTextOutput("ErgTeil"))),
           fluidRow(column(12, h3("Regression Gesamtdatensatz: Preis~Zustand"))),
           fluidRow(column(12, plotOutput("PlotZustand"))),
           fluidRow(column(12, verbatimTextOutput("ErgZustand"))),
           fluidRow(column(12, h3("Regression Gesamtdatensatz: Preis~Dauer+Zustand"))),
           fluidRow(column(12, plotOutput("PlotMain"))),
           fluidRow(column(12, verbatimTextOutput("ErgMain")))
             )
           )
)




server <- function(input, output) {

  # Original
  
   output$DatOriginal <- renderDataTable(daten)
   output$PlotOriginal <- renderPlotly({
     plmorg <- gf_point(Preis ~ Dauer, col = ~ Zustand, data=daten) %>%
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
     lmStipro <- lm(Preis ~ Dauer*Zustand, data = Stipro())
     plmorg <- gf_point(Preis ~ Dauer, col = ~Zustand, data=Stipro()) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = coef(lmStipro)[2], color = "red", alpha = 0.5) %>%
       gf_abline(intercept = coef(lmStipro)[1]+coef(lmStipro)[3], slope = coef(lmStipro)[2]+coef(lmStipro)[4], color = "blue", alpha = 0.5) 
     ggplotly(plmorg)
   })
   output$ErgStipro <- renderPrint({
     summary(lm(Preis ~ Dauer*Zustand, data = Stipro()))
   })
   
   
   # Resample
   
   Resample <- eventReactive(input$ResamplenGo, resample(daten))
   
   output$DatResample <- renderDataTable(Resample())
   output$PlotResample <- renderPlotly({
     lmStipro <- lm(Preis ~ Dauer*Zustand, data = Resample())
     plmorg <- gf_point(Preis ~ Dauer, col = ~Zustand, data=Resample()) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = coef(lmStipro)[2], color = "red", alpha = 0.5) %>%
       gf_abline(intercept = coef(lmStipro)[1]+coef(lmStipro)[3], slope = coef(lmStipro)[2]+coef(lmStipro)[4], color = "blue", alpha = 0.5) 
     ggplotly(plmorg)
   })
   output$ErgResample <- renderPrint({
     summary(lm(Preis ~ Dauer*Zustand, data = Resample()))
   })
   
   
   # Permutation
   
   Shuffle <- eventReactive(input$ShuffleGo, { 
                              if (input$Shufflen=="Dauer") daten_perm <- sample(daten, shuffled="Dauer")
                              if (input$Shufflen=="Preis") daten_perm <- sample(daten, shuffled="Preis")
                              if (input$Shufflen=="Zustand") daten_perm <- sample(daten, shuffled="Zustand")
                              daten_perm
                            })
   
   output$DatPermutation <- renderDataTable(Shuffle())
   output$PlotPermutation <- renderPlotly({
     lmStipro <- lm(Preis ~ Dauer*Zustand, data = Shuffle())
     plmorg <- gf_point(Preis ~ Dauer, col = ~Zustand, data=Shuffle()) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = coef(lmStipro)[2], color = "red", alpha = 0.5) %>%
       gf_abline(intercept = coef(lmStipro)[1]+coef(lmStipro)[3], slope = coef(lmStipro)[2]+coef(lmStipro)[4], color = "blue", alpha = 0.5) 
     ggplotly(plmorg)
   })
   output$ErgPermutation <- renderPrint({
     summary(lm(Preis ~ Dauer*Zustand, data = Shuffle()))
   })
   
# Einfachregression

   output$PlotI <- renderPlot({
     lmStipro <- lm(Preis ~ 1, data = daten)
     plmorg <- gf_point(Preis ~ Dauer, data=daten) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = 0, color = "black", alpha = 0.8) 
    plmorg
   })   
   
   output$ErgI <- renderPrint({
     summary(lm(Preis ~ 1, data = daten))
   })
   
   output$PlotTeil <- renderPlot({
     lmStipro <- lm(Preis ~ Dauer, data = daten)
     plmorg <- gf_point(Preis ~ Dauer, data=daten) %>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = coef(lmStipro)[2], color = "black", alpha = 0.8) 
    plmorg
   })   
   
   output$ErgTeil <- renderPrint({
     summary(lm(Preis ~ Dauer, data = daten))
   })
   
   output$PlotZustand <- renderPlot({
     lmStipro <- lm(Preis ~ Zustand, data = daten)
     plmorg <- gf_point(Preis ~ Zustand, col= ~ Zustand, data=daten,
                        position = "jitter", width = 0.05, height = 0) %>%
       gf_lims(y =ylim) %>%
       gf_hline(yintercept = coef(lmStipro)[1], color = "red", alpha = 0.5) %>%
       gf_hline(yintercept = coef(lmStipro)[1]+coef(lmStipro)[2], color = "blue", alpha = 0.5)
     plmorg
   })   
   output$ErgZustand <- renderPrint({
     summary(lm(Preis ~ Zustand, data = daten))
   })

   output$ErgTeil <- renderPrint({
     summary(lm(Preis ~ Dauer, data = daten))
   })
   
   output$PlotMain <- renderPlot({
     lmStipro <- lm(Preis ~ Dauer+Zustand, data = daten)
     plmorg <- gf_point(Preis ~ Dauer, col= ~ Zustand, data=daten)%>%
       gf_lims(x = xlim, y =ylim) %>%
       gf_abline(intercept = coef(lmorg)[1], slope = coef(lmorg)[2], color = "red", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmorg)[1]+coef(lmorg)[3], slope = coef(lmorg)[2]+coef(lmorg)[4], color = "blue", alpha = 0.2) %>%
       gf_abline(intercept = coef(lmStipro)[1], slope = coef(lmStipro)[2], color = "red", alpha = 0.5) %>%
       gf_abline(intercept = coef(lmStipro)[1]+coef(lmStipro)[3], slope = coef(lmStipro)[2], color = "blue", alpha = 0.5) 
     plmorg
   })   
   output$ErgMain <- renderPrint({
     summary(lm(Preis ~ Dauer+Zustand, data = daten))
   })  
   
}





# Run the application 
shinyApp(ui = ui, server = server)

