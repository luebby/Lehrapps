##############################################################################
# Pakete
library(mosaic)
library(shinydashboard)


##############################################################################
### User Interface

ui <- dashboardPage(
  dashboardHeader(title = "Münzwurf"),
  dashboardSidebar(
    sliderInput("p", "Wahrscheinlichkeit Kopf", 0.01, 0.99, 0.5, 0.01),
    sliderInput("n", "Anzahl Würfe", 1, 100, 8, 1),
    sliderInput("s", "Anzahl Wiederholungen", 1, 1000, 1, 1),

    actionButton("go", "Los!")
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("plotSimu")),
    )
  )
)


######################################################
### Start: Server
######################################################

server <- function(input, output) {
  
  
  data <- reactiveValues()
  
  observeEvent(input$go,{ 
    
    simu <- do(input$s) * rflip(input$n, input$p) 
    kopf <- factor(simu$heads, levels=c(0:input$n))
    
    isolate({
      data$simu <- kopf
      })
    })
  
  output$plotSimu <- renderPlot({
    if (input$go){
      gf_bar( ~ data$simu) %>%
        gf_refine(scale_x_discrete(drop=FALSE)) %>%
        gf_labs(x="Anzahl Kopf", y="Häufigkeit")
      }
  })
  
  
  }

shinyApp(ui, server)
  
