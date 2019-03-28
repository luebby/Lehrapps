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



ui <- navbarPage(title = "Six Sigma",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Six Sigma"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Überall gibt es Variation, z.B. auch bei der Herstellung von Produkten.")),
           fluidRow(column(12, "Wird ein Produkt innerhalb der zulässigen Toleranzen produziert, wird es akzeptiert, andernfalls muss es nachgearbeitet oder weggeworfen werden.")),
           fluidRow(column(12, "Wie oft ein Produkt außerhalb der Toleranzen liegt hängt von der Standardabweichung ab.")),
           fluidRow(column(12, "Ist diese klein, wird wenig Ausschuss produziert, ist diese groß wird viel Ausschuss produziert.")),
           fluidRow(column(12, "Dafür kann man die sogenannten sigma-Bereiche angeben: wie viele Standardabweichungen nach oben oder unten von der Sollgröße (Mittelwert) können (noch) akzeptiert werden?")),
           fluidRow(column(12, h4("Prozessqualität"))),
           fluidRow(column(12, "Angenommen die Produktionsschwankungen seien normalverteilt.")),
           fluidRow(column(12, "Die Solllänge eines Stahlrohres sei 100mm, der Toleranzbereich gehe von 99.95mm bis 100.05mm.")),
           fluidRow(column(12, "Wie viel Ausschuss wird in Abhängigkeit der Prozessqualität produziert?"))
      )),
                 
tabPanel("Prozessqualität", 
         fluidPage(
           fluidRow(sliderInput("sigma", "Prozessqualität sigma", value = 3, min=0.5, max=6, step=0.5)),
           fluidRow(plotOutput("Plotpnorm"))
           )
         )
)




server <- function(input, output) {
  

  
  output$Plotpnorm <- renderPlot({
    
    

    toleranz <- c(100-0.05, 100+0.05)
    
    
    sigma <- input$sigma
    
    ausschuss <- round(2*pnorm(-sigma)*1000000,2)
    
    sigmabereiche <- c(1:sigma) * 0.05/sigma
    sigmabereiche <- sort(c(100 - sigmabereiche, 100 + sigmabereiche))
    
    gf_dist("norm", mean = 100, sd = 0.05/sigma,
            fill = ~ (abs(x-100) <= 0.05), geom = "area") %>%
      gf_lims(x=c(100-3*0.05, 100+3*0.05), 
              y=c(0, 50)) %>%
      gf_refine(guides(fill=guide_legend(title="Akzeptiert"))) %>%
      gf_vline(xintercept = sigmabereiche, color = "lightgray") %>%
      gf_vline(xintercept = toleranz, color = "red") %>%
      gf_vline(xintercept = 100) %>%
      gf_labs(title = bquote(Prozess~funktioniert~zum~Niveau~.(sigma)~sigma), 
              subtitle = paste("Erwarterer Ausschuss bei 1.000.000 Teile: ", ausschuss),
              caption = "ohne Berücksichtigung einer evt. Mittelwertsverschiebung") %>%
      gf_theme(theme_classic(22*1.01)) %>%
      gf_theme(legend.position = "bottom")
    })
  


}





# Run the application 
shinyApp(ui = ui, server = server)

