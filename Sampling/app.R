#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mosaic)
library(nycflights13)
library(shinycssloaders) # Added package for spinner (see below)
library(shinydashboard)
data(flights)


flights <- flights %>%
  filter(origin=="JFK") %>%
  select(arr_delay, distance) %>%
  mutate(Verspätet=factor(ifelse(arr_delay>10, "Ja", "Nein"))) %>%
  rename(Entfernung=distance) %>%
  select(-arr_delay) %>%
  na.omit()


verpop <- prop(~Verspätet, success = "Ja", data = flights)
entpop <- mean(~Entfernung, data = flights)
# Calculate true standard deviation from population data
verpop_sdtrue <- (verpop*(1-verpop))
entpop_sdtrue <- sd(~Entfernung, data = flights)
# Calculate standard error for 50-Obs Sample data
verpop_se50 =  verpop_sdtrue / sqrt(50)
entpop_se50 =  entpop_sdtrue / sqrt(50)
# Calculate standard error for 500-Obs Sample data
verpop_se500 =  verpop_sdtrue / sqrt(500)
entpop_se500 =  entpop_sdtrue / sqrt(500)


xlims <- c(0,5500)

# Calculate fixed x-limits as 6 times the standard error for 50-Obs Sample data
xlims2ver50 <- c((verpop-6*verpop_se50),(verpop+6*verpop_se50))
xlims2ent50 <- c((entpop-6*entpop_se50),(entpop+6*entpop_se50))
# Calculate fixed x-limits as 6 times the standard error for 500-Obs Sample data
xlims2ver500 <- c((verpop-6*verpop_se500),(verpop+6*verpop_se500))
xlims2ent500 <- c((entpop-6*entpop_se500),(entpop+6*entpop_se500))


# ui section
ui = dashboardPage(
  dashboardHeader(title = "FOM-Lehrapp: Sampling",titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Population", tabName = "Population",icon = icon('tachometer')),
      menuItem("Stichprobe n=50", tabName = "sample_50",icon = icon('tachometer')),
      menuItem("Stichprobe n=500", tabName = "sample_500",icon = icon('tachometer'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Population",
        fluidRow(
          box(
            width = 12,
            status = "info", solidHeader = TRUE,
            title = "Hintergrund",
            p("Im R Paket nycflights13 liegen alle abgehenden Flüge aus New York City innerhalb der USA aus dem Jahr 2013 vor. 
              Der Datensatz wurde eingeschränkt auf den Flughafen JFK, so dass insgesamt N=109079 Beobachtungen vorliegen.
              Ein Flug wurde als verspätet klassifiziert, wenn er mehr als 10 Min. Verspätung hatte. Hier sehen Sie die Verteilung
              der Entfernung des Fluges, sowie ob dieser verspätet ankam.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary", solidHeader = TRUE,
            title = "Verteilung der Entfernungen",
            plotOutput("PlotOriginalEnt") %>% withSpinner(color = '#387F72')
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary", solidHeader = TRUE,
            title = "Verteilung der Verspätungen",
            plotOutput("PlotOriginalVer") %>% withSpinner(color = '#387F72')
          )
        )
      ),
      tabItem("sample_50",
        fluidRow(
          box(
            width = 12,
            status = "info", solidHeader = TRUE,
            title = "Stichprobe n=50",
            p("Auf der linken Seite sehen Sie die Verteilung einer zufälligen Stichprobe mit n=50. Auf der rechten Seite können 
              Sie die Stichprobenverteilung, d. h. die Verteilung des Mittelwertes bzw. des Anteils über die zufälligen Stichproben entwickeln. 
              Der rote Strich ist der Mittelwert bzw. Anteil der aktuellen Stichprobe, der blaue der der Population."),
            p("Probieren Sie aus und ziehen Sie Stichproben von 50 Flügen:"),
            actionButton("SampleGo50", "Sample!", icon = icon("refresh"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary", solidHeader = TRUE,
            title = "Entfernung von Flügen",
            column(6, 
                   h4("Entfernung Stichprobe", align = "center"),
                   plotOutput("PlotEnt50")),
            column(6, 
                   h4("Verteilung Mittelwert Entfernung", align = "center"),
                   plotOutput("PlotMeanEnt50"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary", solidHeader = TRUE,
            title = "Verspätung von Flügen",
            column(6, 
                   h4("Verspätung Stichprobe", align = "center"),
                   plotOutput("PlotVer50")),
            column(6, 
                   h4("Verteilung Anteil Verspätung", align = "center"),
                   plotOutput("PlotMeanVer50"))
          )
        )
      ),
      tabItem("sample_500",
        fluidRow(
          box(
            width = 12,
            status = "info", solidHeader = TRUE,
            title = "Stichprobe n=500",
            p("Auf der linken Seite sehen Sie die Verteilung einer zufälligen Stichprobe mit n=500. Auf der rechten 
              Seite können Sie die Stichprobenverteilung, d. h. die Verteilung des Mittelwertes bzw. des Anteils über 
              die zufälligen Stichproben entwickeln. Der rote Strich ist der Mittelwert bzw. Anteil der aktuellen 
              Stichprobe, der blaue der der Population."),
            p("Hinweise: Worin unterscheiden sich die Stichprobenverteilungen mit n=50 und n=500? Gegen welche 
              Verteilungsform entwickeln sich die Stichprobenverteilungen?"),
            p("Probieren Sie aus und ziehen Sie Stichproben von 500 Flügen:"),
            actionButton("SampleGo500", "Sample!", icon = icon("refresh"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary", solidHeader = TRUE,
            title = "Entfernung von Flügen",
            column(6, 
                   h4("Entfernung Stichprobe", align = "center"),
                   plotOutput("PlotEnt500")),
            column(6, 
                   h4("Verteilung Mittelwert Entfernung", align = "center"),
                   plotOutput("PlotMeanEnt500"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary", solidHeader = TRUE,
            title = "Verspätung von Flügen",
            column(6, 
                   h4("Verspätung Stichprobe", align = "center"),
                   plotOutput("PlotVer500")),
            column(6, 
                   h4("Verteilung Anteil Verspätung", align = "center"),
                   plotOutput("PlotMeanVer500"))
          )
        )
      )
    )
  )
)

# server section
server = function(input, output) {
  
options(warn=-1)
  
werte50 <- reactiveValues()
werte500 <- reactiveValues()

res50 <- observe({
  if (input$SampleGo50) {
    sam50 <- sample(flights,50)
    
    isolate({
      werte50$sam50 <- sam50
      werte50$Ent50 <- c(werte50$Ent50, mean(~Entfernung, data=sam50))
      werte50$Ver50 <- c(werte50$Ver50, prop(~Verspätet, success = "Ja", data=sam50))
      })
  }
})

res500 <- observe({
  if (input$SampleGo500) {
    sam500 <- sample(flights,500)
    
    isolate({
      werte500$sam500 <- sam500
      werte500$Ent500 <- c(werte500$Ent500, mean(~Entfernung, data=sam500))
      werte500$Ver500 <- c(werte500$Ver500, prop(~Verspätet, success = "Ja", data=sam500))
    })
  }
})

  
  output$PlotEnt50 <- renderPlot({
    if (input$SampleGo50) {
      gf_histogram( ~ Entfernung, data = werte50$sam50,
                    title = paste0(" Mittelwert Entfernung: ", round(tail(werte50$Ent50,1),2)),
                    binwidth = 250,
                    color = 'white',  # White spaces between bars for better separation of bars
                    fill = '#387F72', # FOM-Blue for corporate design
                    alpha = .5) %>%   # Transparancy to better read exact values
      gf_refine(scale_x_continuous(limits = xlims, breaks = seq(xlims[1],xlims[2],250))) %>% # Breaks set manually to better read the exact values of the bars
      gf_labs(x = 'Entfernung',         
              y = 'Anzahl Flüge') %>% 
      gf_theme(theme = theme_bw())    # Black-white theme only little distraction in the background 
    }
  })
  
  output$PlotVer50 <- renderPlot({
    if (input$SampleGo50) {
      gf_bar( ~ Verspätet, data = werte50$sam50 ,
              title = paste0(" Anteil Verspätet: ", round(tail(werte50$Ver50,1),2)),
              color = 'white',        # White spaces between bars for better separation of bars
              fill = '#387F72',       # FOM-Blue for corporate design
              alpha = .5) %>%         # Transparancy to better read exact values
      gf_labs(x = 'Verspätung',        
              y = 'Anzahl Flüge') %>%  
      gf_theme(theme = theme_bw())    # Black-white theme only little distraction in the background 
    }
  })
  
  
  output$PlotEnt500 <- renderPlot({
    if (input$SampleGo500) {
      gf_histogram( ~ Entfernung, data = werte500$sam500,
                    title = paste0(" Mittelwert Entfernung: ", round(tail(werte500$Ent500,1),2)),
                    binwidth = 250,
                    color = 'white',   # White spaces between bars for better separation of bars
                    fill = '#387F72',  # FOM-Blue for corporate design
                    alpha = .5) %>%    # Transparancy to better read exact values
        gf_refine(scale_x_continuous(limits = xlims, breaks = seq(xlims[1],xlims[2],250))) %>%  # Breaks set manually to better read the exact values of the bars
        gf_labs(x = 'Entfernung',
                y = 'Anzahl Flüge') %>%
        gf_theme(theme = theme_bw())   # Black-white theme only little distraction in the background 
    }
  })
  
  output$PlotVer500 <- renderPlot({
    if (input$SampleGo500) {
      gf_bar( ~ Verspätet, data = werte500$sam500 ,
              title = paste0(" Anteil Verspätet: ", round(tail(werte500$Ver500,1),2)),
              color = 'white',          # White spaces between bars for better separation of bars
              fill = '#387F72',         # FOM-Blue for corporate design
              alpha = .5) %>%           # Transparancy to better read exact values
      gf_labs(x = 'Verspätung',
              y = 'Anzahl Flüge') %>%
      gf_theme(theme = theme_bw())      # Black-white theme only little distraction in the background 
    }
  })

  output$PlotOriginalEnt <- renderPlot({
    gf_histogram( ~ Entfernung, data = flights,
                  title = paste0(" Mittelwert Entfernung: ", round(entpop,2)),
                  binwidth = 250,       
                  color = 'white',      # White spaces between bars for better separation of bars
                  fill = '#387F72',     # FOM-Blue for corporate design
                  alpha = .5) %>%       # Transparancy to better read exact values
    gf_refine(scale_x_continuous(limits = xlims, breaks = seq(xlims[1],xlims[2],250))) %>%  # Breaks set manually to better read the exact values of the bars
    gf_labs(x = 'Entfernung',
            y = 'Anzahl Flüge') %>%
    gf_theme(theme = theme_bw())   # Black-white theme only little distraction in the background 
  })
  
  output$PlotOriginalVer <- renderPlot({
    gf_bar( ~ Verspätet, data = flights,
            title = paste0(" Anteil Verspätet: ", round(verpop,2)),
            color = 'white',       # White spaces between bars for better separation of bars
            fill = '#387F72',      # FOM-Blue for corporate design
            alpha = .5) %>%        # Transparancy to better read exact values
    gf_labs(x = 'Verspätung',
            y = 'Anzahl Flüge') %>%
    gf_theme(theme = theme_bw())   # Black-white theme only little distraction in the background 
  })
  

  output$PlotMeanEnt50 <- renderPlot({
    if (input$SampleGo50) {
      p <- gf_dotplot( ~ x, data=data.frame(x=werte50$Ent50),
                  title = paste0("Anzahl Stichproben: ",length(werte50$Ent50), 
                                 ", Standardfehler: ", round(sd(werte50$Ent50),2)),
                  color = '#12342E',        # FOM-Blue for corporate design
                  fill = '#387F72') %>%     # FOM-Blue for corporate design
      gf_vline(xintercept = tail(werte50$Ent50,1), col = "red" ) %>%
      gf_vline(xintercept = entpop, col = "blue") %>%
      gf_lims(x = xlims2ent50) %>%          # Fixed x-limits to better visualize what happens when new observations join the existing ones
      gf_labs(x = 'Mittelwert Entfernung',
              y = 'Häufigkeit') %>%
      gf_theme(theme = theme_bw())          # Black-white theme only little distraction in the background 
      
      p <- p + annotate("text", x = entpop, y = .80, label = "Wahrer Mittelwert (Population)", size = 4)            # Label the vertical line as expectation value from the  population
      p <- p + annotate("text", x = tail(werte50$Ent50,1), y = .70, label = "Mittelwert der aktuellen SP", size = 4)  # Lable the vertical line as Sample mean 
      p
    }
  })
  
  output$PlotMeanVer50 <- renderPlot({
    if (input$SampleGo50) {
      p <- gf_dotplot( ~ x, data=data.frame(x=werte50$Ver50),
                  title = paste0("Anzahl Stichproben: ",length(werte50$Ver50), 
                                 ", Standardfehler: ", round(sd(werte50$Ver50),2)),
                  color = '#12342E',     # FOM-Blue for corporate design
                  fill = '#387F72') %>%  # FOM-Blue for corporate design
      gf_vline(xintercept = tail(werte50$Ver50,1), col = "red") %>%
      gf_vline(xintercept = verpop, col = "blue") %>%
      gf_lims(x = xlims2ver50) %>%       # Fixed x-limits to better visualize what happens when new observations join the existing ones
      gf_labs(x = 'Anteil Verspätungen',
              y = 'Häufigkeit') %>%
      gf_theme(theme = theme_bw())       # Black-white theme only little distraction in the background 
      
      p <- p + annotate("text", x = verpop, y = .80, label = "Wahrer Anteil (Population)", size = 4)            # Label the vertical line as expectation value from the  population
      p <- p + annotate("text", x = tail(werte50$Ver50,1), y = .70, label = "Anteil der aktuellen SP", size = 4)  # Lable the vertical line as Sample mean 
      p
    }
  })
  
  output$PlotMeanEnt500 <- renderPlot({
    if (input$SampleGo500){
      p <- gf_dotplot( ~ x, data=data.frame(x=werte500$Ent500),
                title = paste0("Anzahl Stichproben: ",length(werte500$Ent500), 
                               ", Standardfehler: ", round(sd(werte500$Ent500),2)),
                color = '#12342E',         # FOM-Blue for corporate design
                fill = '#387F72') %>%      # FOM-Blue for corporate design
      gf_vline(xintercept = tail(werte500$Ent500,1), col = "red") %>%
      gf_vline(xintercept = entpop, col = "blue") %>%
      gf_lims(x = xlims2ent500) %>%        # Fixed x-limits to better visualize what happens when new observations join the existing ones
      gf_labs(x = 'Mittelwert Entfernung',
              y = 'Häufigkeit') %>%
      gf_theme(theme = theme_bw())         # Black-white theme only little distraction in the background 
      
      p <- p + annotate("text", x = entpop, y = .80, label = "Wahrer Mittelwert (Population)", size = 4)               # Label the vertical line as expectation value from the  population
      p <- p + annotate("text", x = tail(werte500$Ent500,1), y = .70, label = "Mittelwert der aktuellen SP", size = 4)   # Lable the vertical line as Sample mean 
      p
    }
  })
  
  output$PlotMeanVer500 <- renderPlot({
    if (input$SampleGo500){
      p <- gf_dotplot( ~ x, data=data.frame(x=werte500$Ver500),
                title = paste0("Anzahl Stichproben: ",length(werte500$Ver500), 
                               ", Standardfehler: ", round(sd(werte500$Ver500),2)),
                color = '#12342E',          # FOM-Blue for corporate design
                fill = '#387F72') %>%       # FOM-Blue for corporate design
      gf_vline(xintercept = tail(werte500$Ver500,1), col = "red") %>%
      gf_vline(xintercept = verpop, col = "blue") %>%
        gf_lims(x = xlims2ver500) %>%       # Fixed x-limits to better visualize what happens when new observations join the existing ones
      gf_labs(x = 'Anteil Verspätungen',
              y = 'Häufigkeit') %>%
      gf_theme(theme = theme_bw())          # Black-white theme only little distraction in the background 
      
      p <- p + annotate("text", x = verpop, y = .80, label = "Wahrer Anteil (Population)", size = 4)               # Label the vertical line as expectation value from the  population
      p <- p + annotate("text", x = tail(werte500$Ver500,1), y = .70, label = "Anteil der aktuellen SP", size = 4)   # Lable the vertical line as Sample mean 
      p
    }
  })
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)


