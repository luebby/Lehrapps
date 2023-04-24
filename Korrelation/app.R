library(shiny)
library(mosaic)
library(mvtnorm)

ui <- bootstrapPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "fom.css")
  ),
  sliderInput("r", "Korrelationskoeffzient", 
              value = 0, min = -1, max = 1, step = 0.01),
  plotOutput("Plotcor", height = "500px", width = "500px"))

server <- function(input, output) {
  
  output$Plotcor <- renderPlot({
    r <- input$r
    sigma <- matrix(c(1,r,r,1), 2)
    rmvnorm(96, sigma = sigma) %>%  # " |> . => " geht leider noch nicht.
      tibble(
        x = .[,1],
        y = .[,2]
      ) -> xy

    options(repr.plot.width = 5, repr.plot.height = 5)
    gf_point(x ~ y, data = xy, color = "darkgreen", size = 2) |>
      gf_lims(x = c(-3.5, 3.5), y = c(-3.5, 3.5)) |>
      gf_vline(xintercept = ~ 0, color = "darkgrey") |>
      gf_hline(yintercept = ~ 0, color = "darkgrey") |>
      gf_theme(theme_void())
    })
}


shinyApp(ui = ui, server = server)

