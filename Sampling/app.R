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
library(nycflights13)
data(flights)


flights <- flights %>%
  filter(origin=="JFK") %>%
  select(arr_delay, distance) %>%
  mutate(Verspätet=factor(ifelse(arr_delay>10, "Ja", "Nein"))) %>%
  rename(Entfernung=distance) %>%
  select(-arr_delay) %>%
  na.omit()

verpop <- prop(~Verspätet, success = "Ja", data = flights)
entpop <- mean(~Entfernung, data = flights)

xlims <- c(0,5500)


# ui section
ui <- navbarPage(title = "Sampling",
                 tabPanel("Hintergrund", 
                          fluidPage(
                            titlePanel("FOMshiny: Sampling"),
                            fluidRow(column(12, h3("Hintergrund"))),
                            fluidRow(column(12, "Im R Paket nycflights13 liegen alle abgehenden Flüge aus New York City innerhalb der USA aus dem Jahr 2013 vor.")),
                            fluidRow(column(12, "Der Datensatz wurde eingeschränkt auf den Flughafen JFK, so dass insgesamt N=109079 Beobachtungen vorliegen.")),
                            fluidRow(column(12, "Ein Flug wurde als verspätet klassifiziert, wenn er mehr als 10min Verspätung hatte.")),
                            fluidRow(column(12, h3("Population"))),
                            fluidRow(column(12, "Hier sehen Sie die Verteilung der Entfernung des Fluges, so wie ob dieser verspätet ankam.")),
                            fluidRow(column(12, h3("Stichprobe n=50"))),
                            fluidRow(column(12, "Auf der linken Seite sehen Sie die Verteilung einer zufälligen Stichprobe mit n=50.")),
                            fluidRow(column(12, "Auf der rechten Seite können Sie die Stichprobenverteilung, d.h. die Verteilung des Mittelwertes bzw. des Anteils über die zufälligen Stichproben entwickeln.")),
                            fluidRow(column(12, "Der rote Strich ist der Mittelwert bzw. Anteil der aktuellen Stichprobe, der blaue der der Population.")),
                            fluidRow(column(12, h3("Stichprobe n=500"))),
                            fluidRow(column(12, "Auf der linken Seite sehen Sie die Verteilung einer zufälligen Stichprobe mit n=500.")),
                            fluidRow(column(12, "Auf der rechten Seite können Sie die Stichprobenverteilung, d.h. die Verteilung des Mittelwertes bzw. des Anteils über die zufälligen Stichproben entwickeln.")),
                            fluidRow(column(12, "Der rote Strich ist der Mittelwert bzw. Anteil der aktuellen Stichprobe, der blaue der der Population.")),
                            fluidRow(column(12, h4("Hinweise:"))),
                            fluidRow(column(12, "Worin unterscheiden sich die Stichprobenverteilung mit n=50 und n=500?")),
                            fluidRow(column(12, "Gegen welche Verteilungsform entwickeln sich die Stichprobenverteilungen?"))
                          )),
                 

                 tabPanel("Population", 
                          fluidPage(
                            titlePanel("Verteilung Population"),
                            fluidRow(column(12, h3("Verteilung Entfernung"))),
                            fluidRow(column(12, plotOutput("PlotOriginalEnt"))),
                            fluidRow(column(12, h3("Verteilung Verspätung"))),
                            fluidRow(column(12, plotOutput("PlotOriginalVer")))
                          )
                 ),
                 
                 tabPanel("Stichprobe n=50", 
                          fluidRow(actionButton("SampleGo50", "Sample!", icon = icon("refresh"))),
                          splitLayout(
                          fluidPage(
                            titlePanel("Stichprobe n=50"),
                            fluidRow(column(12, h3("Entfernung Stichprobe"))),
                            fluidRow(column(12, plotOutput("PlotEnt50"))),
                            fluidRow(column(12, h3("Verspätung Stichprobe"))),
                            fluidRow(column(12, plotOutput("PlotVer50")))
                            ),
                          fluidPage(
                            titlePanel("Stichprobenverteilung n=50"),
                            fluidRow(column(12, h3("Verteilung Mittelwert Entfernung"))),
                            fluidRow(column(12, plotOutput("PlotMeanEnt50"))),
                            fluidRow(column(12, h3("Verteilung Anteil Verspätung"))),
                            fluidRow(column(12, plotOutput("PlotMeanVer50")))
                            )
                          )
                    ),
                 tabPanel("Stichprobe n=500", 
                          fluidRow(actionButton("SampleGo500", "Sample!", icon = icon("refresh"))),
                          splitLayout(
                            fluidPage(
                              titlePanel("Stichprobe n=500"),
                              fluidRow(column(12, h3("Entfernung Stichprobe"))),
                              fluidRow(column(12, plotOutput("PlotEnt500"))),
                              fluidRow(column(12, h3("Verspätung Stichprobe"))),
                              fluidRow(column(12, plotOutput("PlotVer500")))
                            ),
                            fluidPage(
                              titlePanel("Stichprobenverteilung n=500"),
                              fluidRow(column(12, h3("Verteilung Mittelwert Entfernung"))),
                              fluidRow(column(12, plotOutput("PlotMeanEnt500"))),
                              fluidRow(column(12, h3("Verteilung Anteil Verspätung"))),
                              fluidRow(column(12, plotOutput("PlotMeanVer500")))
                            )
                          )
                 )                
)



# server section
server = function(input, output) {
  
options(warn=-1)
  
werte50 <- reactiveValues()
werte500 <- reactiveValues()

res50 <- observe({
  if (input$SampleGo50) {
    sam50 <- sample(flights,50)
    
    isolate({
      werte50$sam50 <- sam50
      werte50$Ent50 <- c(werte50$Ent50, mean(~Entfernung, data=sam50))
      werte50$Ver50 <- c(werte50$Ver50, prop(~Verspätet, success = "Ja", data=sam50))
      })
  }
})

res500 <- observe({
  if (input$SampleGo500) {
    sam500 <- sample(flights,500)
    
    isolate({
      werte500$sam500 <- sam500
      werte500$Ent500 <- c(werte500$Ent500, mean(~Entfernung, data=sam500))
      werte500$Ver500 <- c(werte500$Ver500, prop(~Verspätet, success = "Ja", data=sam500))
    })
  }
})

  
  output$PlotEnt50 <- renderPlot({
    if (input$SampleGo50) {
      gf_histogram( ~ Entfernung, data = werte50$sam50,
                    title = paste0(" Mittelwert Entfernung: ", round(tail(werte50$Ent50,1),2)),
                    binwidth = 250) %>%
        gf_lims(x = xlims)}
    })
  
  output$PlotVer50 <- renderPlot({
    if (input$SampleGo50) {
      gf_bar( ~ Verspätet, data = werte50$sam50 ,
              title = paste0(" Anteil Verspätet: ", round(tail(werte50$Ver50,1),2)))}
  })
  
  
  output$PlotEnt500 <- renderPlot({
    if (input$SampleGo500) {
      gf_histogram( ~ Entfernung, data = werte500$sam500,
                    title = paste0(" Mittelwert Entfernung: ", round(tail(werte500$Ent500,1),2)),
                    binwidth = 250) %>%
        gf_lims(x = xlims)}
  })
  
  output$PlotVer500 <- renderPlot({
    if (input$SampleGo500) {
      gf_bar( ~ Verspätet, data = werte500$sam500 ,
              title = paste0(" Anteil Verspätet: ", round(tail(werte500$Ver500,1),2)))}
  })

  output$PlotOriginalEnt <- renderPlot({
    gf_histogram( ~ Entfernung, data = flights,
                  title = paste0(" Mittelwert Entfernung: ", round(entpop,2)),
                  binwidth = 250) %>%
      gf_lims(x = xlims)
  })
  
  output$PlotOriginalVer <- renderPlot({
    gf_bar( ~ Verspätet, data = flights,
            title = paste0(" Anteil Verspätet: ", round(verpop,2)))
  })
  

  output$PlotMeanEnt50 <- renderPlot({
    if (input$SampleGo50) {
      gf_dotplot( ~ x, data=data.frame(x=werte50$Ent50),
                  title = paste0("Anzahl Stichproben: ",length(werte50$Ent50), 
                                 ", Standardfehler: ", round(sd(werte50$Ent50),2))) %>%
        gf_vline(xintercept = tail(werte50$Ent50,1), col = "red" ) %>%
        gf_vline(xintercept = entpop, col = "blue")}
  })
  
  output$PlotMeanVer50 <- renderPlot({
    if (input$SampleGo50) {
      gf_dotplot( ~ x, data=data.frame(x=werte50$Ver50),
                  title = paste0("Anzahl Stichproben: ",length(werte50$Ver50), 
                                 ", Standardfehler: ", round(sd(werte50$Ver50),2))) %>%
        gf_vline(xintercept = tail(werte50$Ver50,1), col = "red") %>%
        gf_vline(xintercept = verpop, col = "blue")}
  })
  
  output$PlotMeanEnt500 <- renderPlot({
    if (input$SampleGo500){
      gf_dotplot( ~ x, data=data.frame(x=werte500$Ent500),
                  title = paste0("Anzahl Stichproben: ",length(werte500$Ent500), 
                                 ", Standardfehler: ", round(sd(werte500$Ent500),2))) %>%
        gf_vline(xintercept = tail(werte500$Ent500,1), col = "red") %>%
        gf_vline(xintercept = entpop, col = "blue")}
  })
  
  output$PlotMeanVer500 <- renderPlot({
    if (input$SampleGo500){
      gf_dotplot( ~ x, data=data.frame(x=werte500$Ver500),
                  title = paste0("Anzahl Stichproben: ",length(werte500$Ver500), 
                                 ", Standardfehler: ", round(sd(werte500$Ver500),2))) %>%
        gf_vline(xintercept = tail(werte500$Ver500,1), col = "red") %>%
        gf_vline(xintercept = verpop, col = "blue")}
  })
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)


