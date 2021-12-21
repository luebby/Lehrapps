#
# Kneipe statt Hörsaal - shiny-app
#
# server.r
# ========--------------------------------------------------------------------
# Maintainer: Tanja Kistler
#

library(shiny)
library(pwr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$Plotpwr <- renderPlot({
    h <- floor(ES.h(p1 = input$p1, p2 = input$p2)*10000)/10000
    p.out <- pwr.p.test(h = h,
                        sig.level = input$alpha, 
                        power = input$power, 
                        alternative = "greater")
    plot(p.out)
  })
})
