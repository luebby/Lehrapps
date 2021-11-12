# Source: https://www.tandfonline.com/doi/full/10.1080/26939169.2021.1997128
# https://github.com/JacopoDior/htgaws
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  titlePanel("Mission Vorhersage"),
  
  # Sidebar with a slider input for number of bins 
  h3("Ihr Auftrag:"),
  h5("Mit Hilfe Ihrer menschlichen Intelligenz: Finden Sie den Zusammenhang zwischen x und y, 
     zwischen der Anzahl Kontakte bei Facebook und Instagram!"),
  h5("Tun Sie dies indem Sie zwei Punkte auf der Abbildung klicken und diese werden mit einer Gerade verbunden. 
     Eine neue Linie entsteht wenn Sie noch einmal klicken. Je niedriger Ihr Score desto besser!"),
  h5("Sind Sie besser als die Künstliche Intelligenz von Dr. Stat?"),
  # 
  # h5("IF YOU WANT TO LOAD YOUR DATA..."),
  # h6("Check the box if your data file has a header. Choose a CSV File with at least two numerical columns and then click on the plot. 
  #    If there are more than two numerical columns the app will use the first two"),
  # checkboxInput("header", "Header", TRUE),
  # fileInput("file1", "Choose a CSV file and click on the plot",
  #           multiple = TRUE,
  #           accept = c(".csv", ".RData", ".text" )),
  hr(),
  plotOutput("plot1", click = "plot_click"),
  textOutput("check"), # not useful
  hr(),
  h5("Zufrieden? Dann schreiben Sie Ihren Score in den Chat!"),
  # shiny::actionButton(inputId='ab1', label="Submit!", 
  #                     icon = icon("th"), 
  #                     onclick ="window.open('https://jacopodiiorio.typeform.com/to/PU9zHY', '_blank')")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # available data
  mydata <- read.table(file="data/socialdata.csv", sep=',', header=TRUE)
  colnames(mydata) <- c('x','y') 
  click_saved <- reactiveValues(singleclick = NULL) #niente cliccato
  
  # plot is available
  output$plot1 <- renderPlot({
    if(!is.null(input$file1)){
      mydata <- read.table(input$file1$datapath,
                           header = input$header,
                           row.names = NULL,
                           sep = ',')
      mydata <- mydata %>% dplyr::select(where(is.numeric))
      mydata <- mydata[,c(1,2)]
      colnames(mydata) <- c('x','y')
    }
    prova <- dim(mydata)[1]
    plot(mydata$x, mydata$y, xlab='x', ylab='y', pch=16, asp=1)
  })
  
  # if you click
  observeEvent(eventExpr = input$plot_click, handlerExpr = { 

    click_saved$singleclick <- rbind(click_saved$singleclick, c(input$plot_click[1], input$plot_click[2])) 
    if(dim(click_saved$singleclick)[1]>2)
      click_saved$singleclick <- as.data.frame(click_saved$singleclick[dim(click_saved$singleclick)[1],])
      
    if(!is.null(input$file1)){
      mydata <- read.table(input$file1$datapath,
                           header = input$header,
                           row.names = NULL,
                           sep = ',')
      mydata <- mydata %>% dplyr::select(where(is.numeric))
      mydata <- mydata[,c(1,2)]
      colnames(mydata) <- c('x','y')
    }
    
    output$plot1 <- renderPlot({
      plot(mydata$x, mydata$y, xlab='x', ylab='y', pch=16, asp=1)
      
      points(click_saved$singleclick[,1], click_saved$singleclick[,2], col='red', pch=16)
      if(dim(click_saved$singleclick)[1]==2){
        ys <- as.vector(unlist(click_saved$singleclick[,2]))
        xs <- as.vector(unlist(click_saved$singleclick[,1]))
        fit <- lm(ys~xs)
        abline(fit, col='red')
        
        # projections
        proj <- fit$coefficients[2] * mydata$x + fit$coefficients[1] 
        points(mydata$x, proj, col='purple')
        
        # residuals
        myscore <- NULL
        
        for(k in 1:dim(mydata)[1]){
          residuals <- rbind( mydata[k,], c(mydata[k,1], proj[k]) )
          lines(residuals, col='purple')
          
          myscore <- rbind(myscore,(residuals[1,] -residuals[2,])^2)
        }
        mtext(paste("Ihr Score: ", round(sum(colSums(myscore))/(dim(mydata)[1]),2)), side=3)
        
        output$check <- renderText(paste("Ihre Gleichung: y = ", round(fit$coefficients[1],2), 
                                         "+", 
                                         round(fit$coefficients[2],2),
                                         "* x"))
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

### Bester Score:

# > d <- read.csv("Regressioncontest/data/socialdata.csv")
# > erg <- lm(instagram ~ facebook, data = d)
# > summary(erg)
# 
# Call:
#   lm(formula = instagram ~ facebook, data = d)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -364.15  -82.37  -12.45   76.64  409.66 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 62.34441   29.96115   2.081   0.0425 *  
#   facebook     0.44103    0.03162  13.946   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 155.7 on 51 degrees of freedom
# Multiple R-squared:  0.7922,	Adjusted R-squared:  0.7882 
# F-statistic: 194.5 on 1 and 51 DF,  p-value: < 2.2e-16
# 
# > mean(erg$residuals^2)
# [1] 23318.25