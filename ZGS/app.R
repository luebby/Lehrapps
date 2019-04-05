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



ui <- navbarPage(title = "Zentraler Grenzwertsatz",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Zentraler Grenzwertsatz"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Die Normalverteilung ist eine der wichtigsten (stetigen) Verteilungen der Statistik.")),
           fluidRow(column(12, "Warum?")),
           fluidRow(column(12, "Sie entsteht z.B. häufig als (asymptotische, approximative) Grenzverteilung, wenn man viele Zufallsvariablen addiert.")),
           fluidRow(column(12, "Theoretisch wird dies im Zentralen Grenzwertsatz untersucht und unter recht allgemeinen Annahmen belegt.")),
           fluidRow(column(12, h4("Annäherung Normalverteilung"))),
           fluidRow(column(12, "Z.B. ist das Ergebnis eines Münzwurfs zufällig, die Variable X: *Anzahl Kopf* ist damit eine Zufallsvariable.")),
           fluidRow(column(12, "Wirft man einmal, so hat man mit einer Wahrscheinlichkeit von 1/2 Kopf, mit 1/2 Zahl.")),
           fluidRow(column(12, "Wirft man zweimal, so ist die Wahrscheinlichkeit für 0x Kopf 1/4, für 1x Kopf 1/2 und für 2x Kopf 1/4.")),  
           fluidRow(column(12, "Wie entwickelt sich die Verteilung von der Anzahl Kopf (einer Summe!), wenn die Münze immer öfter geworfen wird?"))
      )),
                 
tabPanel("Annäherung Normalverteilung", 
         fluidPage(
           fluidRow(sliderInput("n", "Anzahl Münzwürfe", value = 1, min=1, max=300, step=1, animate = TRUE)),
           fluidRow(plotOutput("Plotpbinom"))
           )
         )
)




server <- function(input, output) {
  

  output$Plotpbinom <- renderPlot({
    n <- input$n
    gf_dist("binom", size=n, prob=0.5) %>%
      gf_labs(x="Anzahl Kopf", y="Wahrscheinlichkeit",
              title = paste0("Verteilung Anzahl Kopf bei ",n," Würfen")) %>%
      gf_theme(theme_classic(22*1.01))
    })
  


}





# Run the application 
shinyApp(ui = ui, server = server)

