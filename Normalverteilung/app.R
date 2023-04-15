library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu", "Mittelwert:",
                  min = -5, max = 5, value = 0, step = 0.1),
      sliderInput("sd", "Standardabweichung:",
                  min = 0.2, max = 2, value = 1, step = 0.1),
      radioButtons("plot_type", "Funktion:",
                   choices = c("Dichtefunktion" = "pdf", "Verteilungsfunktion" = "cdf", "Beides" = "both"),
                   selected = "pdf")
    ),
    mainPanel(
      plotOutput("Plot")
    )
  )
)


server <- function(input, output) {
  
  # Create a sequence of x values for plotting
  x_seq <- seq(-10, 10, length.out = 1000)
  
  # Create a reactive data frame containing the PDF and CDF values
  dist_data <- reactive({
    data.frame(x = x_seq,
               pdf = dnorm(x_seq, input$mu, input$sd),
               cdf = pnorm(x_seq, input$mu, input$sd))
  })
  
  # Render the plot
  output$Plot <- renderPlot({
    mu_val <- input$mu
    sd_val <- input$sd
    
    plot_type <- input$plot_type
    if (plot_type == "pdf") {
      ggplot(dist_data(), aes(x)) +
        geom_line(aes(y = pdf), linewidth = 1.5, color = "blue") +
        xlab("x") +
        ylab("f(x)") +
        ggtitle(bquote(paste("Dichtefunktion Normalverteilung (", mu == .(mu_val), ", ", sigma == .(sd_val), ")")))
    } else if (plot_type == "cdf") {
      ggplot(dist_data(), aes(x)) +
        geom_line(aes(y = cdf), linewidth = 1.5, color = "red") +
        xlab("x") +
        ylab("F(x)") +
        ggtitle(bquote(paste("Verteilungsfunktion Normalverteilung (", mu == .(mu_val), ", ", sigma == .(sd_val), ")"))) + 
        theme(legend.position = "bottom")
    }
    else {
        ggplot(dist_data(), aes(x)) +
          geom_line(aes(y = pdf, color = "Dichtefunktion f(x)"), linewidth = 1) +
          geom_line(aes(y = cdf, color = "Verteilungsfunktion F(x)"), linewidth = 1) +
          xlab("x") +
          ylab("Funktionswert") +
          ggtitle(bquote(paste("Normalverteilung (", mu == .(mu_val), ", ", sigma == .(sd_val), ")"))) +
          scale_color_manual(name = "Funktion: ", values = c("Dichtefunktion f(x)" = "blue", "Verteilungsfunktion F(x)" = "red")) +
          theme(legend.position = "bottom")
    }
  })
  
}

shinyApp(ui, server)
