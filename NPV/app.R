## app.R ##
library(shiny)
library(ggformula)

theme.fom <- theme_classic(22*1.02)
theme.fom <- theme.fom
theme_set(theme.fom)


ui <- fluidPage(
    withMathJax(),
    titlePanel("Investitionsrechnung"),
    fluidRow(column(4, numericInput("invest", HTML("Investition \\(C_0\\):"), 2000, 0, 10e6)),
             column(4, numericInput("r1",  HTML("Cashflow 1. Jahr \\(C_1\\):"), 1000, 0, 10e6)),
             column(4, numericInput("r2", HTML("Cashflow 2. Jahr \\(C_2\\):"), 800, 0, 10e6))),
    fluidRow(column(4, numericInput("r3", HTML("Cashflow 3. Jahr \\(C_3\\):"), 600, 0, 10e6)),
             column(4, numericInput("r4", HTML("Cashflow 4. Jahr \\(C_4\\):"), 200, 0, 10e6)),
             column(4, numericInput("r5", HTML("Cashflow 5. Jahr \\(C_4\\):"), 0, 0, 10e6))),
    fluidRow(column(4, sliderInput("i", HTML("Diskontierungsfaktor (Zinssatz) \\(r\\) in %:"), 0, 20, 10, 0.01)),
             column(4, uiOutput("npv"))),
    fluidRow(column(12, uiOutput('npv1'))),
    fluidRow(column(12, uiOutput('npv2'))),
    plotOutput("intern")
    
    )

server <- function(input, output, session) {
    output$npv <- renderUI({
        q <- 1+input$i/100
        npv <- -input$invest + input$r1/q + input$r2/q^2 + input$r3/q^3 + input$r4/q^4 + input$r5/q^5
        h3(withMathJax(sprintf("NPV=%.2f", npv)))
    })
    
    output$intern <- renderPlot({ 
        q <- seq(1, 1.2, by = 0.0001)
        i <- q-1
        npv <- -input$invest + input$r1/q + input$r2/q^2 + input$r3/q^3 + input$r4/q^4 + input$r5/q^5
        
        q <- 1+input$i/100
        npvr <- -input$invest + input$r1/q + input$r2/q^2 + input$r3/q^3 + input$r4/q^4 + input$r5/q^5
        
        D1 <- data.frame(x1 = input$i/100, x2 = input$i/100, y1=0, y2=npvr)
        D2 <- data.frame(x1 = input$i/100, x2 = 0 , y1=npvr, y2=npvr)
        
        gf_line(npv ~ i) %>%
          gf_labs(x="Diskontierungsfaktor (Zinssatz)", y="Net Present Value (Kapitalwert)") %>%
          gf_vline(xintercept = ~ 0, color ="grey") %>%
          gf_hline(yintercept = ~ 0, color ="grey") %>%
          gf_segment(y1 + y2 ~ x1 + x2, data = D1, color = "red", arrow=arrow()) %>%
          gf_segment(y1 + y2 ~ x1 + x2, data = D2, color = "red", arrow=arrow())
    })
    
    output$npv1 <- renderUI({
      withMathJax(helpText('$$NPV=-C_0 + \\sum_{t=1}^T \\frac{C_t}{(1+r)^t}$$'),
      helpText('$$NPV=-C_0 + \\frac{C_1}{(1+r)^1} + \\frac{C_2}{(1+r)^2} + \\frac{C_3}{(1+r)^3} + \\frac{C_4}{(1+r)^4} + \\frac{C_5}{(1+r)^5}$$'))
      })
   
    output$npv2 <- renderUI({
      q <- 1+input$i/100
      npv <- -input$invest + input$r1/q + input$r2/q^2 + input$r3/q^3 + input$r4/q^4 + input$r5/q^5
      withMathJax(sprintf('$$NPV= -%.2f + \\frac{%.2f}{(1+%.4f)^1} + \\frac{%.2f}{(1+%.4f)^2} + \\frac{%.2f}{(1+%.4f)^3} + \\frac{%.2f}{(1+%.4f)^4} + \\frac{%.2f}{(1+%.4f)^5}=%.2f$$',
                          input$invest, input$r1, input$i/100, input$r2, input$i/100, input$r3, input$i/100, input$r4, input$i/100, input$r5, input$i/100, npv))
    }) 
}

shinyApp(ui, server)
