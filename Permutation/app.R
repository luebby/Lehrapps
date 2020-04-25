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
library(DT)
library(readxl)

daten <- read_excel("Daten_SoSe2018_FOM_Wacker.xlsx")

daten <- daten %>%
  na.omit() %>%
  sample_n(48) %>%
  mutate(Obs = 1:n()) %>%
  mutate(Geschlecht = case_when(Geschlecht=="w" ~ "Frau",
                                Geschlecht=="m" ~ "Mann")) %>%
  select(Obs, Geschlecht, ZeitBad)


xlims <- c(max(0,floor(min(~ZeitBad, data = daten))-2), ceiling(max(~ZeitBad, data = daten))+2)

datmean <- mean(ZeitBad~Geschlecht, data = daten)
datmeandiff <- diffmean(ZeitBad~Geschlecht, data = daten)
xlims_conf <- c(-round(abs(datmeandiff))-5, round(abs(datmeandiff))+5)

DmeanOrg <- data.frame(x1=datmean[1], x2=datmean[2], y1=1, y2=1, Geschlecht = NA)

erg <- list()
erg$mean <- NULL

library(shiny)
library(ggplot2)


# ui section
ui <- navbarPage(title = "Permutation",
                 tabPanel("Hintergrund", 
                          fluidPage(
                            titlePanel("FOMshiny: Permutation"),
                            fluidRow(column(12, h3("Hintergrund"))),
                            fluidRow(column(12, "Wie viel Zeit verbringen FOM-Studierende nach eigener Auskunt durchschnittlich morgens im Bad?")),
                            fluidRow(column(12, "Unterscheidet sich der Mittelwert zwischen Frauen und Männern?")),
                            fluidRow(column(12, "Dazu simulieren wir die Verteilung anhand der Nullhypothese: es gibt keinen Unterschied im Mittelwert der Population zwischen Frauen und Männern in der Zeit im Bad.")),
                            fluidRow(column(12, "Wenn wir von diesem Nullmodell (es gibt keinen Unterschied) ausgehen können wir das Geschlecht permutieren (mischen, vertauschen).")),
                            fluidRow(column(12, h3("Stichprobe"))),
                            fluidRow(column(12, "Hier sehen Sie die Verteilung der Zeit im Bad einer zufälligen Stichprobe von n=48 Studierenden.")),
                            fluidRow(column(12, "In diversen Vorlesungen wurden die Daten anonym online unter der Leitung von Fr. Dipl.-Psych. Eva Wacker erhoben.")),
                            fluidRow(column(12, h3("Permutation"))),
                            fluidRow(column(12, "Hier können Sie nacheinander das Geschlecht permutieren.")),
                            fluidRow(column(12, "So können Sie die Permutations-Verteilung der Differenz der Mittelwerte entwickeln.")),
                            fluidRow(column(12, "Der rote bzw. blaue Strich zeigt Ihnen die jeweiligen Mittelwerte an.")),
                            fluidRow(column(12, "Der schwarze Pfeil und Strich die in der Stichprobe beobachtete Differenz.")),
                            fluidRow(column(12, "Der grüne Pfeil und Strich die in der permutierten Stichprobe beobachtete Differenz.")),
                            fluidRow(column(12, h4("Hinweise:"))),
                            fluidRow(column(12, "Für welche Beobachtungen wurde das Geschlecht permutiert?")),
                            fluidRow(column(12, "Wie entwickelt sich die simulierte Verteilung der Differenz der Mittelwerte unter dem Modell der Nullhypothese?"))
                          )),
                 

                 tabPanel("Stichprobe", 
                          fluidPage(
                            titlePanel("Verteilung Stichprobe"),
                            fluidRow(column(12, h3("Verteilung Zeit im Badezimmer"))),
                            fluidRow(column(12, plotOutput("PlotOriginal"))),
                            fluidRow(column(12, h3("Stichprobe"))),
                            fluidRow(column(12, dataTableOutput("DatOriginal")))
                          )
                 ),
                 
                 tabPanel("Permutation", 
                          fluidRow(actionButton("ShuffleGo", "Permutiere!", icon = icon("refresh"))),
                          splitLayout(
                          fluidPage(
                            titlePanel("Permutation"),
                            fluidRow(column(12, h3("Verteilung permutierte Stichprobe"))),
                            fluidRow(column(12, plotOutput("PlotDist"))),
                            fluidRow(column(12, h3("Verteilung Stichprobe"))),
                            fluidRow(column(12, plotOutput("PlotDistRes")))
                            ),
                          fluidPage(
                            titlePanel("Permutations-Verteilung"),
                            fluidRow(column(12, h3("Permutationsverteilung Differenz Mittelwerte"))),
                            fluidRow(column(12, plotOutput("PlotMeanDist"))),
                            fluidRow(column(12, h3("p-Wert Permutationstest"))),
                            fluidRow(column(12, h4("Nullhypothese: Gleichheit der Verteilungen"))),
                            fluidRow(column(12, "Anteil Simulationen mit mindestens so großer absoluten Abweichung")),
                            fluidRow(column(12, verbatimTextOutput("Pwert"))),
                            fluidRow(column(12, h3("t-Test"))),
                            fluidRow(column(12, verbatimTextOutput("ttest")))
                            )
                          )
                          )
)



# server section
server = function(input, output) {
  
options(warn=-1)
  
werte <- reactiveValues()

res <- observe({
  if (input$ShuffleGo) {
    resb <- sample(daten, shuffled = "Geschlecht")
    
    isolate({
      werte$resb <- resb
      werte$mean <- c(werte$mean, diffmean(ZeitBad ~ Geschlecht, data=resb))
      
    })
  }
})

  
  output$PlotOriginal <- renderPlot({
    gf_dotplot(~ZeitBad | Geschlecht, data = daten, binpositions = "all", fill = ~ Geschlecht, binwidth = 5, 
               stackgroups = T, dotsize = 0.3, method = "histodot") %>%
      gf_lims(x = xlims) %>%
      gf_labs(x="Zeit im Bad") %>%
      gf_vline(xintercept = datmean[2], col = "blue") %>%
      gf_vline(xintercept = datmean[1], col = "red") %>%
      gf_theme(legend.position = "bottom")
    }) 
  
  output$DatOriginal <- renderDataTable(daten)


  output$PlotDistRes <- renderPlot({
    gf_dotplot(~ZeitBad, data = daten, binpositions = "all", fill = ~ Geschlecht, binwidth = 5, 
               stackgroups = T, dotsize = 0.3, method = "histodot",
               title = paste0("Differenz Mittelwerte: ",round(datmeandiff,1)) ) %>%
      gf_lims(x = xlims) %>%
      gf_labs(x="Zeit im Bad") %>%
      gf_vline(xintercept = datmean[2], col = "blue") %>%
      gf_vline(xintercept = datmean[1], col = "red") %>%
      gf_segment(y1+y2~x1+x2, data=DmeanOrg, arrow=arrow()) %>%
      gf_theme(legend.position = "bottom")
  })
  
  
  output$PlotDist <- renderPlot({
    if(input$ShuffleGo) {
      shufmean <- mean(ZeitBad ~ Geschlecht, data = werte$resb)
      
      Dmean <- data.frame(x1=shufmean[1], x2=shufmean[2], y1=1, y2=1, Geschlecht = NA)
      
      gf_dotplot(~ZeitBad, data = werte$resb, binpositions = "all", fill = ~ Geschlecht, binwidth = 5, 
                 stackgroups = T, dotsize = 0.3, method = "histodot",
                 title = paste0("Differenz Mittelwerte: ",round(tail(werte$mean,1),1))) %>%
        gf_lims(x = xlims) %>%
        gf_labs(x="Zeit im Bad") %>%
        gf_vline(xintercept = shufmean[2], col = "blue") %>%
        gf_vline(xintercept = shufmean[1], col = "red") %>%
        gf_segment(y1+y2~x1+x2, data=Dmean, arrow=arrow(), col="darkgreen") %>%
        gf_theme(legend.position = "bottom")}
  })

  output$PlotMeanDist <- renderPlot({
    if(input$ShuffleGo) {
    dat <- data.frame(dm=werte$mean, dl=abs(werte$mean)>=abs(datmeandiff))
    gf_dotplot( ~ dm, data=dat,
                title = paste0("Anzahl Permutationen: ",length(werte$mean)), 
                binwidth = 1,dotsize = 0.5, 
                fil= ~ dl ) %>%
      # gf_lims(x=xlims_conf) %>%
      gf_labs(x="Differenz Mittelwerte") %>%
      gf_vline(xintercept = tail(werte$mean,1), col = "darkgreen" ) %>%
      gf_vline(xintercept = 0, col = "grey" ) %>%
      gf_vline(xintercept = datmeandiff, col = "black") %>%
      gf_theme(legend.position = "bottom")} %>%
      gf_refine(guides(fill=guide_legend(title="Abweichung größer als in Stichprobe?")))
  })
  
  # output$PlotMeanHist <- renderPlot({
  #   if(input$ShuffleGo) {
  #   ki <- quantile(~werte$mean, probs = c(0.025, 0.975))
  #   gf_dhistogram( ~ x, data=data.frame(x=werte$mean),
  #               title = paste0("Kritische Werte (5%) :",round(ki[1],3), 
  #                              " und ", round(ki[2],3)),
  #               binwidth = 1) %>%
  #     gf_lims(x=xlims_conf) %>%
  #     gf_vline(xintercept = datmeandiff, col = "black") %>%
  #     gf_fitdistr(dist = "dnorm")}
  # })
  
  output$Pwert <- renderPrint({if(input$ShuffleGo) round(prop(abs(werte$mean)>=abs(datmeandiff)),4)})
  output$ttest <- renderPrint({t.test(ZeitBad~Geschlecht, data = daten)})
}
# Run the application 
shinyApp(ui = ui, server = server)


