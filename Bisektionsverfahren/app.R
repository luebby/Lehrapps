# ===========================================================================
# app.R (Release 0.1)
# =====----------------------------------------------------------------------
#
# Bisektionsverfahren
# -------------------
#
# (W) by Norman Markgraf in 2019
#
# 30. Mrz. 2019  (nm)  Allererste Version (0.1)
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

fkttab <<- data.frame(integer(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric())
names(fkttab) = c("iter","a", "c", "b", "f(a)", "f(c)", "f(b)", "f(a)*f(c)", "|a-b|", "|f(a)-f(b)|")

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
         
         submitButton("Aktualisieren", icon("refresh")),
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
     
    observeEvent(input$Reset, {
            #fkttab <<- data.frame(integer(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric(), numeric())
            #names(fkttab) = c("iter","a", "c", "b", "f(a)", "f(c)", "f(b)", "f(a)*f(c)")
            
            print("RESET!")
            
            updateTextInput(session, "fkt", value = "(x-0.9)^2-1")
            updateNumericInput(session, "a", value = -1)
            updateNumericInput(session, "b", value = 1)
    })
   
     
   output$functionTable <- renderTable({
       input$nextStep
       
       a <- input$a
       b <- input$b
       c <- (a+b)/2
       fkt <- makeFun( as.formula(paste(input$fkt, "~ x")))
       
       df = data.frame(
           iter=ifelse(max(fkttab$iter) < 0, 1, as.integer(trunc(max(fkttab$iter))+1)),
           a=a,
           c=c,
           b=b,
           fa=fkt(a),
           fc=fkt(c),
           fb=fkt(b),
           fac=fkt(a)*fkt(c),
           amb=abs(a-b),
           fab=abs(fkt(a)-fkt(b))
       )
       names(df) = c("iter", "a", "c", "b", "f(a)", "f(c)", "f(b)", "f(a)*f(c)", "|a-b|", "|f(a)-f(b)|")
       fkttab <<- bind_rows(fkttab, df)
       fkttab
   },
   align = "r",
   digits=4,
   display = c("d","d","G","G","G","G","G","G","G","G","G")
   )
    
   output$functionPlot <- renderPlot({
       input$nextStep
       
       a <- input$a
       b <- input$b
       if (a>b) { t <- a; a <- b; b <- a}
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

