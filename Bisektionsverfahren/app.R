# ===========================================================================
# app.R (Release 0.2)
# =====----------------------------------------------------------------------
#
# Bisektionsverfahren
# -------------------
#
# (W) by Norman Markgraf in 2019
#
# 30. Mrz. 2019  (nm)  Allererste Version (0.1)
# 05. Apr. 2019  (nm)  Man lernt stetig dazu (0.2)
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
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


library(shiny)
library(mosaic)
library(mosaicCalc)
library(xtable)

#fkttab <<- data.frame(integer(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric())
#names(fkttab) = c("iter","a", "c", "b", "f(a)", "f(c)", "f(b)", "f(a)*f(c)", "|a-b|", "|f(a)-f(b)|")

fkttab <<- tribble(~"a", ~"c", ~"b", ~"f(a)", ~"f(c)", ~"f(b)", ~"f(a)*f(c)", ~"|a-b|", ~"|f(a)-f(b)|")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bisektionsverfahren"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("fkt", 
                   "Funktion f(x)=",
                   "(x-0.9)^2-1"),
         
         numericInput("a", "a=", -1.0, min= -10, max = 10),
         
         numericInput("b", "b=",  1.0, min= -10, max = 10),
         
         numericInput("eps", "eps=", 5*10^-4, min=1*10^-10, max=0.1),
         
#        submitButton("Aktualisieren", icon("refresh")),
         actionButton(inputId="update", label="Aktualisieren", icon=icon("refresh")),
         helpText("Aktualisieren der Ausgaben")
         #submitButton("Reset", icon("caret-square-up")),
         #helpText("Reset der Ausgaben")
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("functionPlot"),
         tableOutput("functionTable")
        )
   )
)

# Define server logic required to draw a histogram
 server <- function(input, output, session) {

   rv <- reactiveValues(
     a=NULL, 
     b=NULL,
     c=NULL,
     df=tribble(~"a", ~"c", ~"b", ~"f(a)", ~"f(c)", ~"f(b)", ~"f(a)*f(c)", ~"|a-b|", ~"|f(a)-f(b)|")
     )     

   observeEvent(input$update, {
      fkt <- makeFun( as.formula(paste(input$fkt, "~ x")))
      if (is.null(rv$a) | is.null(rv$b)) {
        rv$a <- input$a
        rv$b <- input$b
        rv$c <- (rv$a + rv$b)/2
      } else {
        if (rv$a > rv$b) { t <- rv$a; rv$a <- rv$b; rv$b <- rv$a}
        
        rv$c <- (rv$a + rv$b)/2
        
        fac = fkt(rv$a)*fkt(rv$c)
        old_a <- rv$a
        old_b <- rv$b
        if (fac < 0) {
          rv$b <- rv$c
        } else {
          rv$a <- rv$c
        }
      }
      if (abs(fkt(rv$b)-fkt(rv$a)) > input$eps/2) {
        rv$df <- add_row( rv$df,
          a=rv$a,
          c=rv$c,
          b=rv$b,
          "f(a)"=fkt(rv$a),
          "f(c)"=fkt(rv$c),
          "f(b)"=fkt(rv$b),
          "f(a)*f(c)"=fkt(rv$a)*fkt(rv$c),
          "|a-b|"=abs(rv$a-rv$b),
          "|f(a)-f(b)|"=abs(fkt(rv$a)-fkt(rv$b))
        )
      } else {
       rv$a <- old_a
       rv$b <- old_b
       rv$c <- (rv$a + rv$b)/2
      }
   })
     
   output$functionTable <- renderTable({
     tibble::rowid_to_column(as.data.frame(rv$df))
   },
   align = "r",
   digits = 4,
   display = rep("G", 11)
   )
    
   output$functionPlot <- renderPlot({
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
       
       fkt <- makeFun( as.formula(paste(input$fkt, "~ x")))
       
       xmin <- a - 8 * 10^m
       xmax <- b + 8 * 10^m
       
       x <- seq(xmin, xmax, 0.001*10^m)
       
       gf_line(y ~ x, data=data.frame(y=fkt(x), x=x)) %>%
           gf_lims(x = c(xmin, xmax)) %>%
           gf_vline(xintercept = c(a,b), color="gray40") %>%
           gf_vline(xintercept = c, color="green") %>%
           gf_hline(yintercept = 0)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

