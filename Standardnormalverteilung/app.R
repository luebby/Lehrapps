library(shiny)
library(ggplot2)
library(latex2exp)

# Define UI for app
ui <- fluidPage(
  withMathJax(),
  sidebarLayout(
    sidebarPanel(
      h4("Eingabe:"),
      sliderInput("q", "Quantil (q):", min = -3, max = 3, value = 1, step = 0.05),
      sliderInput("p", "Wahrscheinlichkeit (p):", min = 0, max = 1, value = 0.25 , step = 0.005)
    ),
    mainPanel(
      h4("Plot"),
      plotOutput("cdf_plot"),
      h4("Verteilungsfunktion", style = "color:red"),
      uiOutput("eqn1"),
      uiOutput("eqn1b"),
      verbatimTextOutput("text_output1"),
      h4("Quantilsfunktion", style = "color:blue"),
      uiOutput("eqn2"),
      uiOutput("eqn2b"),
      verbatimTextOutput("text_output2"),
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Calculate CDF values for standard normal distribution
  x <- seq(-5, 5, by = 0.01)
  y_cdf <- pnorm(x)
  
  # Render CDF plot
  output$cdf_plot <- renderPlot({
    df <- data.frame(x, y_cdf)
    ggplot(df, aes(x = x, y = y_cdf)) +
      geom_line() +
      geom_segment(aes(x = input$q, y = 0, xend = input$q, yend = pnorm(input$q)),
                   arrow = arrow(length = unit(0.2, "cm")), color = "red") +
      geom_segment(aes(x = input$q, y = pnorm(input$q), xend = -5, yend = pnorm(input$q)),
                   arrow = arrow(length = unit(0.2, "cm")), color = "red") +
      geom_segment(aes(x = -5, y = input$p, xend = qnorm(input$p), yend = input$p),
                   arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
      geom_segment(aes(x = qnorm(input$p), y = input$p, xend = qnorm(input$p), yend = 0),
                   arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
      ggtitle("Standardnormalverteilung") + 
      xlab(expression(paste("q = ",F^-1,"(p)"))) +
      ylab(expression(paste("p = ",F,"(q)"))) +
      annotate("text", x=-4.5, y=pnorm(input$q) + 0.05, label= "pnorm()", color = "red") +
      annotate("text", x= qnorm(input$p) + 0.75 , y = 0.05, label= "qnorm()", color = "blue")
  })

  
  # Render equation 
  output$eqn1 <- renderUI({
    withMathJax("$$p= F(q) = Pr(X \\leq q) = \\int_{-\\infty}^q \\frac{1}{\\sqrt{2\\pi}} e^{-\\frac{t^2}{2}}dt$$")
  })
  output$eqn1b <- renderUI({
    withMathJax(sprintf('$$p= F(%.3f) = Pr(X \\leq %.3f) = \\int_{-\\infty}^{%.3f} \\frac{1}{\\sqrt{2\\pi}} e^{-\\frac{t^2}{2}}dt = %.3f$$',
                        input$q, input$q, input$q, pnorm(input$q))  
    )
  })
  
  # Render text output
  output$text_output1 <- renderText({
    paste0("pnorm(",input$q,", mean = 0, sd = 1)\n","[1] ", pnorm(input$q), "\n# Für q = ", input$q, ", liegt der Wert der Verteilungsfunktion bei p = ", round(pnorm(input$q), 3),".")
  })
  
  
  # Render equation
  output$eqn2 <- renderUI({
    withMathJax("$$q=F^{-1}(p)$$")
  })
  output$eqn2b <- renderUI({
    withMathJax(sprintf('$$q=F^{-1}( %.3f)= %.3f$$', input$p, qnorm(input$p)))
  })
  
  # Render text output
  output$text_output2 <- renderText({
    paste0("qnorm(",input$p,", mean = 0, sd = 1)\n","[1] ", qnorm(input$p), "\n# Für p = ", input$p, ", liegt der Wert der Quantilsfunktion bei q = ", round(qnorm(input$p), 3),".")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

