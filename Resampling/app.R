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

daten <- read.csv2("results-survey372953.csv", 
                   fileEncoding = "UTF-8")

daten <- daten %>%
  na.omit() %>%
  filter(Gewicht >= 50 & Gewicht <= 150) %>%
  filter(Groesse >= 150 & Groesse <= 215) %>%
  mutate(BMI = round(Gewicht/(Groesse/100)^2,1)) %>%
  sample_n(42) %>%
  mutate(Obs = 1:n()) %>%
  select(Obs, BMI)


xlims <- c(floor(min(~BMI, data = daten))-2, ceiling(max(~BMI, data = daten))+2)
confi <- confint(t.test(~BMI, data=daten, conf.level=0.999))
xlims_conf <- unlist(c((floor(confi[2]*10)/10-0.2), (ceiling(confi[3]*10)/10)+0.2))
datmean <- mean(~BMI, data = daten)

erg <- list()
erg$mean <- NULL

library(shiny)
library(ggplot2)


# ui section
ui <- navbarPage(title = "Resampling",
                 tabPanel("Hintergrund", 
                          fluidPage(
                            titlePanel("FOMshiny: Resampling"),
                            fluidRow(column(12, h3("Hintergrund"))),
                            fluidRow(column(12, "Der Body-Mass-Index (BMI) variiert zwischen den Personen.")),
                            fluidRow(column(12, "Durch eine zufällige Stichprobe variiert auch der Mittelwert des BMI.")),
                            fluidRow(column(12, "Wie können wir die Unsicherheit in der Schätzung des Mittelwert des BMI bestimmen?")),
                            fluidRow(column(12, h3("Stichprobe"))),
                            fluidRow(column(12, "Hier sehen Sie die Verteilung des BMI eine zufälligen Stichprobe von n=42 Studierenden.")),
                            fluidRow(column(12, "In diversen Vorlesungen wurden anonym Größe und Gewicht erhoben und daraus wurde der BMI berechnet.")),
                            fluidRow(column(12, h3("Resampling"))),
                            fluidRow(column(12, "Hier können Sie nacheinander die vorhandene Stichprobe re-samplen.")),
                            fluidRow(column(12, "So können Sie die Bootstrap-Verteilung des Mittelwertes entwickeln.")),
                            fluidRow(column(12, "Der rote Strich zeigt Ihnen den Mittelwert der aktuellen Bootstrap Stichprobe.")),
                            fluidRow(column(12, "Der blaue Strich zeigt Ihnen den Mittelwert der Original-Stichprobe.")),
                            fluidRow(column(12, h4("Hinweise:"))),
                            fluidRow(column(12, "Rekonstruieren Sie die Zusammensetzung der Bootstrap-Stichprobe.")),
                            fluidRow(column(12, "Wie entwickelt sich die Bootstrap Verteilung des Mittelwertes?"))
                          )),
                 

                 tabPanel("Stichprobe", 
                          fluidPage(
                            titlePanel("Verteilung Stichprobe"),
                            fluidRow(column(12, h3("Verteilung BMI"))),
                            fluidRow(column(12, plotOutput("PlotOriginal"))),
                            fluidRow(column(12, h3("Stichprobe"))),
                            fluidRow(column(12, dataTableOutput("DatOriginal")))
                          )
                 ),
                 
                 tabPanel("Resampling", 
                          fluidRow(actionButton("ResamplenGo", "Resample!", icon = icon("refresh"))),
                          splitLayout(
                          fluidPage(
                            titlePanel("Verteilung Resample"),
                            fluidRow(column(12, h3("Verteilung Resample"))),
                            fluidRow(column(12, plotOutput("PlotDist"))),
                            fluidRow(column(12, h3("Verteilung Sample"))),
                            fluidRow(column(12, plotOutput("PlotDistRes")))
                            ),
                          fluidPage(
                            titlePanel("Bootstrap Verteilung"),
                            fluidRow(column(12, h3("Bootstrap Verteilung Mittelwert"))),
                            fluidRow(column(12, plotOutput("PlotMeanDist"))),
                            fluidRow(column(12, h3("Histogram Bootstrap Verteilung"))),
                            fluidRow(column(12, plotOutput("PlotMeanHist")))
                            )
                          )
                          )
)



# server section
server = function(input, output) {
  
options(warn=-1)
  
werte <- reactiveValues()

res <- observe({
  if (input$ResamplenGo) {
    resb <- resample(daten)
    
    isolate({
      werte$resb <- resb
      werte$mean <- c(werte$mean, mean(~BMI, data=resb))
      
      dd <- resb %>% 
        group_by(Obs) %>% 
        mutate(n=n()) %>% 
        ungroup() %>% 
        select(Obs, n) %>%
        unique()
      
      daten2 <- daten %>% 
        left_join(dd, by = "Obs") %>%
        mutate(n = ifelse(is.na(n), 0,n)) %>%
        mutate(B = factor(n))
      
      werte$resb2 <- daten2
    })
  }
})

  
  output$PlotOriginal <- renderPlot({
  gf_dhistogram( ~ BMI, data = daten,
                title = paste0(" Mittelwert: ", round(mean(~BMI, data = daten),2), 
                               ", Standardabweichung: ",round(sd(~BMI, data = daten),2)),
                binwidth = 1) %>%
    gf_lims(x = xlims) %>%
      gf_fitdistr(dist = "dnorm")
    })
  
  output$DatOriginal <- renderDataTable(daten)


  output$PlotDistRes <- renderPlot({
    if(input$ResamplenGo) {
    gf_dotplot(~BMI, data = werte$resb2, binpositions = "all", fill = ~ B, binwidth = 1, 
               stackgroups = T, dotsize = 0.6, method = "histodot") %>%
      gf_lims(x = xlims) %>%
      gf_vline(xintercept = datmean, col = "blue") %>%
      gf_vline(xintercept = tail(werte$mean,1), col = "red" ) %>%
      gf_theme(legend.position = "bottom")} %>%
      gf_refine(guides(fill=guide_legend(title="Häufigkeit im Resample")))
  })
  
  
  output$PlotDist <- renderPlot({
    if(input$ResamplenGo) {
    gf_dotplot( ~ BMI, data = werte$resb, binpositions = "all", binwidth = 1,
                stackgroups = T, dotsize = 0.6, method = "histodot",
                  title = paste0("Bootstrap Stichprobe: ", length(werte$mean), 
                                 ", Mittelwert: ", round(tail(werte$mean,1),2))) %>%
      gf_lims(x = xlims) %>%
      gf_vline(xintercept = tail(werte$mean,1), col = "red" )}
  })

  output$PlotMeanDist <- renderPlot({
    if(input$ResamplenGo) {
    gf_dotplot( ~ x, data=data.frame(x=werte$mean),
                title = paste0("Anzahl Bootstrap Stichproben: ",length(werte$mean), 
                               ", Standardfehler: ", round(sd(werte$mean),2)), 
                binwidth = 0.1) %>%
      gf_lims(x=xlims_conf) %>%
      gf_vline(xintercept = tail(werte$mean,1), col = "red" )}
  })
  
  output$PlotMeanHist <- renderPlot({
    if(input$ResamplenGo) {
    ki <- quantile(~werte$mean, probs = c(0.025, 0.975))
    gf_dhistogram( ~ x, data=data.frame(x=werte$mean),
                title = paste0("95% Resampling Konfidenzintervall: ",round(ki[1],3), 
                               " - ", round(ki[2],3)),
                binwidth = 0.1) %>%
      gf_lims(x=xlims_conf) %>%
      gf_vline(xintercept = datmean, col = "blue") %>%
      gf_vline(xintercept = tail(werte$mean,1), col = "red" ) %>%
      gf_vline(xintercept = ki) %>%
      gf_fitdistr(dist = "dnorm")}
  })
}
# run the app
runApp(list(ui = ui, server = server))


