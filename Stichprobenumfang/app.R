#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pwr)



ui <- navbarPage(title = "Stichprobenumfang",

tabPanel("Hintergrund", 
         fluidPage(
           titlePanel("FOMshiny: Stichprobenumfang"),
           fluidRow(column(12, h3("Hintergrund"))),
           fluidRow(column(12, "Das Signifikanzniveau alpha einer Hypothesenprüfung sagt, wie oft Sie maximal die Nullhypothese verwerfen wollen, obwohl sie gilt.")),
           fluidRow(column(12, "Die Nullhypothese zu verwerfen, obwohl sie gilt nennt man alpha-Fehler oder Fehler 1. Art.")),
           fluidRow(column(12, "Ein beta-Fehler oder Fehler 2. Art liegt vor, wenn man H_0 nicht verwirft, obwohl H_0 nicht gilt.")),
           fluidRow(column(12, "Die Power (1-beta) eines Test ist die Wahrscheinlichkeit eine Nullhypothese zu verwerfen, die in der Tat nicht stimmt.")),
           fluidRow(column(12, "Die Power hängt ab von dem wahren Wert (Effekt), alpha - und dem Stichprobenumfang n.")),
           fluidRow(column(12, "Allgemein gilt: je größer n, desto größer ist die Power (da der Standardfehler sinkt), je größer die Abweichung zu H_0 (Effekt), desto größer die Power,")),
           fluidRow(column(12, "je größer alpha, desto größer die Power (da H_0 leichter verworfen wird). Die genaue Funktion des Zusammenhangs hängt von der angenommenen Verteilung ab.")),
           fluidRow(column(12, "Man kann aber auch anhand der gewünschten Power und Annahmen über den wahren Wert den nötigen Stichprobenumfang ausrechnen.")),
           fluidRow(column(12, h4("Münzwurf"))),
           fluidRow(column(12, "Stellen Sie sich vor: Sie wollen zeigen, dass Sie die Kursentwicklung vorhersagen können.")),
           fluidRow(column(12, "Wann können Sie sich wirklich sicher sein besser zu sein als ein Münzwurf?")),
           fluidRow(column(12, "Die Nullhypothese lautet dementsprechend: Ihre Erfolgswahrscheinlichkeit liegt bei höchstens 50%.")),
           fluidRow(column(12, "Angenommen Sie hätten eine Erfolgswahrscheinlichkeit p von 60%.")),
           fluidRow(column(12, "Angenommen Sie verwenden das übliche Signifikanzniveau alpha=5% und Sie wünschen sich eine Power von 80%,")),           
           fluidRow(column(12, "d.h. Sie wollen sich vorab zu 80% sicher sein, dass das Verfahren H_0 korrekterweise verwerfen wird.")),
           fluidRow(column(12, "Was glauben Sie: wie viele Münzwurfe (Kursvorhersagen, Stichprobenumfang) sollten Sie einplanen?"))
           )
      ),
                 
tabPanel("Münzwurf", 
         fluidPage(
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(12, h4("Parameter"))),
               fluidRow(sliderInput("p", "Wahrer Anteil p", value = 0.6, min=0.501, max=0.9, step=0.01)),
               fluidRow(sliderInput("alpha", "Signifikanzniveau alpha", value = 0.05, min=0.01, max=0.15, step=0.01)),
               fluidRow(sliderInput("power", "Power", value = 0.8, min=0.15, max=1, step=0.01))),
             mainPanel(
               fluidRow(column(12, plotOutput("Plotpwr"))),
               fluidRow(column(12, "Stephane Champely (2018). pwr: Basic Functions for Power Analysis. R package version 1.2-2. https://CRAN.R-project.org/package=pwr"))
               )
         )))

)





server <- function(input, output) {
  

  
  
  output$Plotpwr <- renderPlot({
    h <- floor(ES.h(p1 = input$p, p2 = 0.50)*10000)/10000
    p.out <- pwr.p.test(h = h,
                        sig.level = input$alpha, 
                        power = input$power, 
                        alternative = "greater")
    plot(p.out)
    })


}





# Run the application 
shinyApp(ui = ui, server = server)

