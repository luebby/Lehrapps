# ===========================================================================
# app.R (Release 0.1)
# =====----------------------------------------------------------------------
#
# Sekantenverfahren
# -----------------
#
# (W) by Norman Markgraf in 2019
#
# 13. Apr. 2019  (nm)  Aller erste Version (0.1)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
#   (C)opyleft Norman Markgraf (nmarkgraf@hotmail.com) in 2019
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#  Einstein’s Dictum: 
#
#     “Everything should be as simple as possible, but no simpler.”
#
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

library(shiny)
library(mosaic)
library(mosaicCalc)
library(xtable)
library(shinycssloaders) # Added package for spinner (see below)
library(kableExtra)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Sekantenverfahren"),
  
  # Enable math in output!
  withMathJax(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("fkt", 
                "Funktion f(x)=",
                "(x-0.9)^2-1"),
      
      numericInput("x_alt", "x_0=", 0, min= -10, max = 10),

      numericInput("x",     "x_1=", 1, min= -10, max = 10),
      
            
      numericInput("eps", "eps=", 5*10^-6, min=1*10^-10, max=0.1),
      
      actionButton(inputId="update", label="Nächster Schritt", icon=icon("refresh")),
      helpText("Nächster Schritt der Iteration"),
      
      actionButton(inputId="reset", label="Reset", icon=icon("caret-square-up")),
      helpText("Zurücksetzen auf (aktuelle) Startwerte")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("functionPlot") %>% withSpinner(color = '#387F72')),
        tabPanel("Table", tableOutput("functionTable")  %>% withSpinner(color = '#387F72')),
        tabPanel("Hintergrund",
                 fluidPage(
                   titlePanel("FOMshiny: Sekantenverfahren"),
                   helpText(h3("Hintergrund")) # ,
                   # helpText('Die Idee des Bisektionsverfahren ist es ausgehend von einem Intervall \\([a, b]\\) mit \\(f(a) \\cdot f(b)<0\\), also mit mindestens einer Nullstelle im Intervall, das Intervall an der Stelle \\(c=\frac{a+b}{2}\\) zu halbieren und falls \\(f(a) \\cdot f(c) < 0\\) ist im Intervall \\([a, c]\\) weiter zu suchen oder alternativ im Intervall \\([b, c]\\) bis die Nullstelle (hinreichend genau) gefunden wurde.')
                 )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  FOMgreen <- '#387F72'
  FOMblue <- '#387F72'
  
  acolor <- "blue"
  bcolor <- "red"
  ccolor <- FOMgreen
  fktlinecolor <- "coral3"
  tangentlinecolor <- FOMgreen
  
  rv <- reactiveValues(
    fkt   = NULL,
    x_alt = NULL,
    df   = tribble(~"x", ~"x_alt", ~"f(x)", ~"m_S(x)", ~"x_neu", ~"|f(x_neu)-0|")
  )     
  
  observeEvent(input$reset, {
    fkt <- makeFun( as.formula(paste(input$fkt, "~ x")))
    x <- input$x
    x_alt <- input$x_alt
    rv$fkt <- fkt
    rv$x <- x
    rv$x_alt <- x_alt
    
    fx <- fkt(x)
    fx_alt <- fkt(x_alt)
    m_s <- (fx-fx_alt)/(x - x_alt)
    x_neu <- x - fkt(x)/m_s
    
    afx_neu <- abs(fkt(x_neu))
    rv$df <-  tribble(~"x", ~"x_alt", ~"f(x)", ~"m_S(x)", ~"x_neu", ~"|f(x_neu)-0|")
    add_row( rv$df,
             x=x,
             x_alt=x_alt,
             "f(x)"=fkt(x),
             "m_S(x)"=m_s,
             "x_neu"=x_neu,
             "|f(x_neu)-0|"=afx_neu
    )
  })
  
  observeEvent(input$update, {
    fkt <- rv$fkt
    if (is.null(fkt)) {
      fkt <- makeFun( as.formula(paste(input$fkt, "~ x")))
      rv$fkt <- fkt
    }
    flag <- FALSE
    x <- rv$x
    if (is.null(x)) {
      x <- input$x
      flag <- TRUE
    }
    x_alt <- rv$x_alt
    if (is.null(x_alt)) {
      x_alt <- input$x_alt
      flag <- TRUE
    }
    
    fx <- fkt(x)
    fx_alt <- fkt(x_alt)
    m_s <- (fx-fx_alt)/(x - x_alt)
    
    x_neu <- x - fkt(x)/m_s
    afx_neu <- abs(fkt(x_neu))
    
    if (afx_neu > input$eps/2) {
      rv$df <- add_row( rv$df,
                        x=x,
                        "x_alt"=x_alt,
                        "f(x)"=fkt(x),
                        "m_S(x)"=m_s,
                        "x_neu"=x_neu,
                        "|f(x_neu)-0|"=afx_neu
      )
      rv$x <- x_neu
      rv$x_alt <- x
    } else {
      rv$x <- x
      rv$x_alt <- x_alt
    }
  })
  
  
#  output$functionTable <- renderTable({
#  },
#  align = "r",
#  digits = 4,
#  display = rep("G", 8)
#  )
 
  output$functionTable <- function() {   
    req(input$update)
    req(rv)
    tibble::rowid_to_column(as.data.frame(rv$df)) %>%
      knitr::kable("html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive"), fixed_thead = T)
  } 
  
  
  output$functionPlot <- renderPlot({
    input$update
    fkt <- rv$fkt
    if (is.null(fkt)) {
      fkt <- makeFun( as.formula(paste(input$fkt, "~ x")))
      rv$fkt <- fkt
    }
    flag <- FALSE
    xx <- rv$x
    if (is.null(xx)) {
      xx <- input$x
      flag <- TRUE
    }
    x_alt <- rv$x_alt
    if (is.null(x_alt)) {
      x_alt <- input$x_alt
      flag <- TRUE
    }
    
    fx <- fkt(xx)
    fx_alt <- fkt(x_alt)
    m_s <- (fx-fx_alt)/(xx - x_alt)
    b <- fx-m_s*xx
    
    x_neu <- xx - fx/m_s
    
    m <- round(log(max(abs(x_neu), abs(xx)))/log(10))-1
    
    xmin <- min(x_neu, xx, x_alt) - 0.1 * 10^(m)
    xmax <- max(x_neu, xx, x_alt) + 0.1 * 10^(m)
    
    x <- seq(xmin, xmax, 0.001*10^m)
    
    D <- data.frame (x1 = x_alt, x2 = xx, y1= fx_alt, y2 = fx)
    
    gf_line(y ~ x, 
            data=data.frame(y = fkt(x), x = x),
            color = fktlinecolor,
            alpha = .5
    ) %>%
      gf_lims(x = c(xmin, xmax)) %>%
      gf_abline(slope = m_s, 
                intercept = b,
                color = tangentlinecolor,
                linetype = "dashed" ) %>%
      gf_vline(xintercept = xx, color=ccolor) %>%
      gf_hline(yintercept = 0) %>%
      gf_theme(theme_bw(base_size = 18)) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

