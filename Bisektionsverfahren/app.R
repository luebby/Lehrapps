# ===========================================================================
# app.R (Release 0.4)
# =====----------------------------------------------------------------------
#
# Bisektionsverfahren
# -------------------
#
# (W) by Norman Markgraf in 2019
#
# 30. Mrz. 2019  (nm)  Allererste Version (0.1)
# 05. Apr. 2019  (nm)  Man lernt stetig dazu (0.2)
# 07. Apr. 2019  (nm)  Etwas mehr (FOM-)Farbe (0.3)
# 11. Apr. 2019  (nm)  Etwas mehr math. Schriften (0.4)
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


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bisektionsverfahren"),
   
   # Enable math in output!
   withMathJax(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("fkt", 
                   "Funktion f(x)=",
                   "(x-0.9)^2-1"),
         
         numericInput("a", "a=", -1.0, min= -10, max = 10),
         
         numericInput("b", "b=",  1.0, min= -10, max = 10),
         
         numericInput("eps", "eps=", 5*10^-4, min=1*10^-10, max=0.1),
         
         actionButton(inputId="update", label="Nächster Schritt", icon=icon("refresh")),
         helpText("Nächster Schritt der Iteration"),

         actionButton(inputId="reset", label="Reset", icon=icon("caret-square-up")),
         helpText("Zurücksetzen auf (aktuelle) Startwerte")
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
         tabPanel("Plot", plotOutput("functionPlot") %>% withSpinner(color = '#387F72')),
         tabPanel("Table", tableOutput("functionTable") %>% withSpinner(color = '#387F72')),
         tabPanel("Hintergrund",
                  fluidPage(
                    titlePanel("FOMshiny: Bisektionsverfahren"),
                    helpText(h3("Hintergrund")),
                    helpText('Die Idee des Bisektionsverfahren ist es ausgehend von einem Intervall \\([a, b]\\) mit \\(f(a) \\cdot f(b)<0\\), also mit mindestens einer Nullstelle im Intervall, das Intervall an der Stelle \\(c=\frac{a+b}{2}\\) zu halbieren und falls \\(f(a) \\cdot f(c) < 0\\) ist im Intervall \\([a, c]\\) weiter zu suchen oder alternativ im Intervall \\([b, c]\\) bis die Nullstelle (hinreichend genau) gefunden wurde.')
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
   
   rv <- reactiveValues(
     fkt=NULL,
     a=NULL, 
     b=NULL,
     c=NULL,
     df=tribble(~"a", ~"c", ~"b", ~"f(a)", ~"f(c)", ~"f(b)", ~"f(a)*f(c)", ~"|b-a|", ~"|f(b)-f(a)|")
     )     

   observeEvent(input$reset, {
     fkt <- makeFun( as.formula(paste(input$fkt, "~ x")))
     a <- input$a
     b <- input$b
     c <- (a + b)/2
     fac <- fkt(a)*fkt(c)
     rv$fkt=fkt
     rv$a=a
     rv$b=b
     rv$c=c
     fafb <- abs(fkt(b)-fkt(a))
     ab <- abs(b-a)
     rv$df=tribble(~"a", ~"c", ~"b", ~"f(a)", ~"f(c)", ~"f(b)", ~"f(a)*f(c)", ~"|b-a|", ~"|f(b)-f(a)|")
     add_row( rv$df,
              a=a,
              c=c,
              b=b,
              "f(a)"=fkt(a),
              "f(c)"=fkt(c),
              "f(b)"=fkt(b),
              "f(a)*f(c)"=fac,
              "|b-a|"=ab,
              "|f(b)-f(a)|"=fafb
     )
   })
   
   observeEvent(input$update, {
      fkt <- rv$fkt
      if (is.null(fkt)) {
        fkt <- makeFun( as.formula(paste(input$fkt, "~ x")))
        rv$fkt <- fkt
      }
      flag <- FALSE
      a <- rv$a
      b <- rv$b
      if (is.null(a) | is.null(b)) {
        a <- input$a
        b <- input$b
        flag <- TRUE
      }
      if (a > b) { t <- a; a <- b; b <- a}
      c <- (a + b)/2
      fac = fkt(a)*fkt(c)
      old_a <- a
      old_b <- b
      if (!flag) {
        if (fac < 0) {
          b <- c
        } else {
          a <- c
        }
        c <- (a + b)/2
      }
      fafb <- abs(fkt(b)-fkt(a))
      ab <- abs(b-a)
      if (fafb > input$eps/2) {
        rv$df <- add_row( rv$df,
          a=a,
          c=c,
          b=b,
          "f(a)"=fkt(a),
          "f(c)"=fkt(c),
          "f(b)"=fkt(b),
          "f(a)*f(c)"=fac,
          "|b-a|"=ab,
          "|f(b)-f(a)|"=fafb
        )
        rv$a <- a
        rv$b <- b
        rv$c <- c
      } else {
       rv$a <- old_a
       rv$b <- old_b
       rv$c <- (old_a + old_b)/2
      }
   })
     
   output$functionTable <- renderTable({
     input$update
     tibble::rowid_to_column(as.data.frame(rv$df))
   },
   align = "r",
   digits = 4,
   display = rep("G", 11)
   )
    
   output$functionPlot <- renderPlot({
     input$update
     fkt <- rv$fkt
     if (is.null(fkt)) {
       fkt <- makeFun( as.formula(paste(input$fkt, "~ x")))
       rv$fkt <- fkt
     }
     
     if (is.null(rv$a) | is.null(rv$b)) {
       a <- input$a
       b <- input$b
     } else {
     a <- rv$a
     b <- rv$b
     }
     c <- (a+b)/2
       # 
       m <- round(log(abs(b-a))/log(10))-1
       
       xmin <- a - 7 * 10^m
       xmax <- b + 7 * 10^m
       
       x <- seq(xmin, xmax, 0.001*10^m)
       
       fadata <- tribble(    ~y,   ~x,
                         fkt(a),    a,
                         fkt(a), xmin)
       
       fbdata <- tribble(    ~y,   ~x,
                         fkt(b),    b,
                         fkt(b), xmin)

       fcdata <- tribble(    ~y,   ~x,
                         fkt(c),    c,
                         fkt(c), xmin)
       
       
       gf_line(y ~ x, 
               data=data.frame(y = fkt(x), x = x),
               color = fktlinecolor,
               alpha = .5
               ) %>%
           gf_lims(x = c(xmin, xmax)) %>%
           gf_vline(xintercept = c(a,b), color=c(acolor, bcolor)) %>%
           gf_vline(xintercept = c, color=ccolor) %>%
           gf_hline(yintercept = 0) %>%
           gf_theme(theme_bw(base_size = 18)) + 
        geom_line(aes(y=y, x=x), data=fadata, linetype = "dashed", size=0.5, color=acolor) +
        geom_line(aes(y=y, x=x), data=fcdata, linetype = "dashed", size=0.5, color=ccolor) +
        geom_rug(aes(y=fkt(a), x=NULL), linetype = "dashed", size=0.5, color=acolor) +
        geom_rug(aes(y=fkt(c), x=NULL), linetype = "dashed", size=0.5, color=ccolor) +
        geom_rug(aes(y=fkt(b), x=NULL), linetype = "dashed", size=0.5, color=bcolor) +
        geom_line(aes(y=y, x=x), data=fbdata, linetype = "dashed", size=0.5, color=bcolor)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

