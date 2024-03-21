library(shiny)
library(ggplot2)
library(ggExtra)
library(googlesheets4)
library(shinyjs)


gs4_auth(cache = ".secrets", email = "karlue74@gmail.com")
ss <- "https://docs.google.com/spreadsheets/d/1hv9BSVuXJVpWNrBG30tVsiddjHTSuF7SKGv2oRFKDQc"

saveData <- function(m){
  erg <- data.frame(
    si = si,
    m = m
  )
  
  sheet_append(ss, erg)
}


# Zufälliger Startwert für Slider
si <- sample(0:16,1)/2
# Beispieldaten
PatientInnen <- data.frame(
  NoShows = c(rep(0,70), rep(1,40), rep(2,10), rep(3,20), rep(4,20), rep(5,40))
)

# Grafikparameter
# Quelle:
# https://stackoverflow.com/questions/49330742/change-y-axis-of-dot-plot-to-reflect-actual-count-using-geom-dotplot
yheight <- 100
binwidth <- 0.5
ratio <- 1/8
dotsize <- ratio

ui <- navbarPage(
  title = "Daten nutzen",
  tabPanel("Daten zusammenfassen",
           withMathJax(),
           useShinyjs(),
           tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
           sidebarLayout(
             sidebarPanel(
               h3("Aufgabenstellung:"),
               p("Die Abbildung zeigt, wie viele Patient:innen in 200 Tage jeweils nicht zum vereinbarten Termin erschienen sind."),
               p("Sie sollen diese Daten durch eine Kennzahl zusammenfassen; 
                 ein Modell für die beobachtete Anzahl der No-Shows."),
               p("Welche Kennzahl wählen Sie?"),
               h4("Eingabe:"),
               sliderInput("k0", "Kennzahl:", min = 0, max = 8, value = si, step = 0.5),
               br(),
               h4("Bitte um Datenspende:"),
               p("Ihre gewählte Zahl können Sie zu Lehr- und Forschungszwecken anonym spenden."),
               p("Bitte nur einmal spenden, nachdem Sie Ihre Wahl getroffen haben."),
               actionButton("abgabe", "Spenden!"),
               hidden(div(id='text_div',
                          verbatimTextOutput("text"))),
               br(),br(),
               h4("Hinweis:"),
               p("Dieses Beispiel basiert auf dem Paper:"),
               p("Melinda Miller Holt & Stephen M. Scariano (2009) Mean, Median and Mode from a Decision Perspective, 
               Journal of Statistics Education, 17:3"),
               a("DOI: 10.1080/10691898.2009.11889533", 
                 href="https://doi.org/10.1080/10691898.2009.11889533")
             ),
             mainPanel(
               h4("Daten"),
               plotOutput("k0")
             )
           )),
  tabPanel("1. Szenario",
           sidebarLayout(
             sidebarPanel(
               h3("Aufgabenstellung:"),
               p("Die Praxis verdient pro Patient:in Geld. Bei Leerlauf also weniger."),
               p("Außerdem nehmen wir an, dass bei zu vielen Patient:innen andere Praxen kostenpflichtig beauftragt werden müssen."),
               p("Daher ist die Praxis sehr daran interessiert, die Anzahl der No-Shows vorherzusagen."),
               p("Dies ist Ihr Auftrag. Sie bekommen dafür 200 Euro. Wenn Sie die Anzahl genau richtig vorhersagen bekommen Sie einen Bonus von 600 Euro."),
               p("Wenn Sie 2 vorhersagen und es kommmen 2 nicht, bekommen Sie den Bonus, 
                 wenn 3 nicht kommen gibt es keinen Bouns."),
               p("Welche Zahl sollten Sie jetzt wählen? Bei welcher Zahl ist Ihr erwarteter Lohn maximal?"),
               h4("Eingabe:"),
               sliderInput("k1", "Ihre Vorhersage:", min = 0, max = 8, value = si, step = 0.5),
               h4("Ergebnis:"),
               textOutput("t1"),
               br(),
               h4("Hinweis:"),
               p("Dieses Beispiel basiert auf dem Paper:"),
               p("Melinda Miller Holt & Stephen M. Scariano (2009) Mean, Median and Mode from a Decision Perspective, 
               Journal of Statistics Education, 17:3")
             ),
             mainPanel(
               h4("Daten"),
               plotOutput("k1"),
               h4("Berechnung erwarteter Lohn:"),
               uiOutput("eqn1"),
               # https://stackoverflow.com/questions/54876731/inline-latex-equations-in-shiny-app-with-mathjax
               tags$div(HTML("<script type='text/x-mathjax-config' >
               MathJax.Hub.Config({
               tex2jax: {inlineMath: [['$','$']]}
               });
               </script >
                             ")),
               uiOutput("h1")
             )
             )),
  tabPanel("2. Szenario",
           sidebarLayout(
             sidebarPanel(
               h3("Aufgabenstellung:"),
               p("Die Praxis verdient pro Patient:in Geld. Bei Leerlauf also weniger."),
               p("Außerdem nehmen wir an, dass bei zu vielen Patient:innen andere Praxen kostenpflichtig beauftragt werden müssen."),
               p("Daher ist die Praxis sehr daran interessiert, die Anzahl der No-Shows vorherzusagen."),
               p("Dies ist Ihr Auftrag. Sie bekommen dafür 800 Euro. Für jede Patient:in, die Sie daneben liegen werden Ihnen 50 Euro abgezogen."),
               p("Egal ob Sie zu wenig oder zu viel No-Shows vorhersagen. Liegen Sie 2 daneben bekommen Sie 2 * 50 = 100 Euro weniger, 
                 bei einer Abweichung von 3 gibt es 150 Euro weniger."),
               p("Welche Zahl sollten Sie jetzt wählen? Bei welcher Zahl ist Ihr erwarteter Lohn maximal?"),
               h4("Eingabe:"),
               sliderInput("k2", "Ihre Vorhersage:", min = 0, max = 8, value = si, step = 0.5),
               h4("Ergebnis:"),
               textOutput("t2"),
               br(),
               h4("Hinweis:"),
               p("Dieses Beispiel basiert auf dem Paper:"),
               p("Melinda Miller Holt & Stephen M. Scariano (2009) Mean, Median and Mode from a Decision Perspective, 
               Journal of Statistics Education, 17:3")
             ),
             mainPanel(
               h4("Daten"),
               plotOutput("k2"),
               h4("Berechnung erwarteter Lohn:"),
               uiOutput("eqn2")
             )
           )),
  
  tabPanel("3. Szenario",
           sidebarLayout(
             sidebarPanel(
               h3("Aufgabenstellung:"),
               p("Die Praxis verdient pro Patient:in Geld. Bei Leerlauf also weniger."),
               p("Außerdem nehmen wir an, dass bei zu vielen Patient:innen andere Praxen kostenpflichtig beauftragt werden müssen."),
               p("Daher ist die Praxis sehr daran interessiert, die Anzahl der No-Shows voherzusagen."),
               p("Dies ist Ihr Auftrag. Sie bekommen dafür 1000 Euro."),
               p("Da große Abweichungen schlimmer sind, bekommen Sie für die Abweichung ins Quadrat 30 Euro abgezogen."),
               p("Egal ob Sie zu wenig oder zu viel No-Shows vorhersagen. Liegen Sie 2 daneben bekommen Sie 2² * 30 = 120 Euro weniger, 
                 bei einer Abweichung von 3 dann 270 Euro."),
               p("Welche Zahl sollten Sie jetzt wählen? Bei welcher Zahl ist Ihr erwarteter Lohn maximal?"),
               h4("Eingabe:"),
               sliderInput("k3", "Ihre Vorhersage:", min = 0, max = 8, value = si, step = 0.5),
               h4("Ergebnis:"),
               textOutput("t3"),
               br(),
               h4("Hinweis:"),
               p("Dieses Beispiel basiert auf dem Paper:"),
               p("Melinda Miller Holt & Stephen M. Scariano (2009) Mean, Median and Mode from a Decision Perspective, 
               Journal of Statistics Education, 17:3")
             ),
             mainPanel(
               h4("Daten"),
               plotOutput("k3"),
               h4("Berechnung erwarteter Lohn:"),
               uiOutput("eqn3")
             )
           )),
  tabPanel("Lagemaße",
           fluidPage(
             plotOutput("k4"),
             h4("Modus"),
             uiOutput("modus"),
             h4("Median"),
             uiOutput("median"),
             h4("Mittelwert"),
             uiOutput("mittelwert"),
           ))
  
  
  
  )

server <- function(input, output) {
  
  observeEvent(input$abgabe, {
    toggle('text_div')
    output$text <- renderText({"Vielen Dank für Ihre Datenspende!"})
    saveData(input$k0)
  })
  
  observeEvent({input$k0}, {
    updateSliderInput(session = getDefaultReactiveDomain(), "k1", value =  input$k0)
    updateSliderInput(session = getDefaultReactiveDomain(), "k2", value =  input$k0)
    updateSliderInput(session = getDefaultReactiveDomain(), "k3", value =  input$k0)
    })
  
  output$k0 <- renderPlot({
    # basic dotplot (binwidth = the accuracy of the data)
    dotchart <- ggplot(PatientInnen, aes(x=NoShows), dpi = 800) + 
      geom_dotplot(binwidth=binwidth, method="histodot", dotsize = dotsize, fill="#00998A")
    
    # use coor_fixed(ratio=binwidth*dotsize*max frequency) to setup the right y axis height.
    dotchart <- dotchart + 
      theme_classic(20*1.04) + 
      coord_fixed(ratio=binwidth*dotsize*yheight*ratio)
    # tweak the theme a little bit
    dotchart <- dotchart + theme(panel.background=element_blank(),
                                 panel.border = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 #plot.margin=unit(c(1,-5,1,-5), "cm"),
                                 axis.line = element_line(colour = "black"),
                                 axis.line.y = element_blank()
    )
    
    # add more tick mark on x axis
    dotchart = dotchart + scale_x_continuous(breaks = seq(0,8,1), limits = c(0,8))
    # add tick mark on y axis to reflect frequencies. Note yheight is max frequency.
    dotchart = dotchart + scale_y_continuous(limits=c(0, 1/ratio), expand = c(0, 0), 
                                             breaks = seq(0, 1/ratio, 1/yheight*10*1/ratio), 
                                             labels=seq(0,yheight,10))
    # Add Slider Choice
    dotchart = dotchart + geom_vline(xintercept = input$k0)
    dotchart = dotchart + annotate("text", x = input$k0 + 1, y = 0.90*1/ratio, 
                                   label = paste0("Ihr Modell: ", input$k0))
    
    # remove x y lables and remove vertical grid lines
    dotchart = dotchart + labs(x="Anzahl", y="Häufigkeit", title = "Patient:Innen die nicht erscheinen") + removeGridX()
    dotchart
    })
  
  
  
  output$k1 <- renderPlot({
    # basic dotplot (binwidth = the accuracy of the data)
    dotchart <- ggplot(PatientInnen, aes(x=NoShows), dpi = 800) + 
      geom_dotplot(binwidth=binwidth, method="histodot", dotsize = dotsize, fill="#00998A")
    
    # use coor_fixed(ratio=binwidth*dotsize*max frequency) to setup the right y axis height.
    dotchart <- dotchart + 
      theme_classic(20*1.04) + 
      coord_fixed(ratio=binwidth*dotsize*yheight*ratio)
    # tweak the theme a little bit
    dotchart <- dotchart + theme(panel.background=element_blank(),
                                 panel.border = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 #plot.margin=unit(c(1,-5,1,-5), "cm"),
                                 axis.line = element_line(colour = "black"),
                                 axis.line.y = element_blank()
    )
    
    # add more tick mark on x axis
    dotchart = dotchart + scale_x_continuous(breaks = seq(0,8,1), limits = c(0,8))
    # add tick mark on y axis to reflect frequencies. Note yheight is max frequency.
    dotchart = dotchart + scale_y_continuous(limits=c(0, 1/ratio), expand = c(0, 0), 
                                             breaks = seq(0, 1/ratio, 1/yheight*10*1/ratio), 
                                             labels=seq(0,yheight,10))
    # Add Slider Choice
    dotchart = dotchart + geom_vline(xintercept = input$k1, color = "#FF8811")
    dotchart = dotchart + annotate("text", x = input$k1 + 1.5, y = 0.90*1/ratio, 
                                   label = paste0("Ihre Prognose: m=", input$k1))
    
    # remove x y lables and remove vertical grid lines
    dotchart = dotchart + labs(x="Anzahl", y="Häufigkeit", title = "Patient:Innen die nicht erscheinen") + removeGridX()
    dotchart
  })
  
  output$t1 <- renderText({ 
    g1 <- 200 + 600 * mean(PatientInnen$NoShows == input$k1)
    paste0("Bei einer Prognose von ", input$k1, " liegt der erwartete Lohn bei ", g1, " Euro.")
    })
  
  output$eqn1 <- renderUI({
    g1 <- 200 + 600 * mean(PatientInnen$NoShows == input$k1)
    withMathJax(sprintf(
      '$$200 + 
      (\\frac{70}{200} \\cdot 1_{0 = %.1f}  + 
      \\frac{40}{200} \\cdot 1_{1 = %.1f}  +
      \\frac{10}{200} \\cdot 1_{2 = %.1f}  +
      \\frac{20}{200} \\cdot 1_{3 = %.1f}  +
      \\frac{20}{200} \\cdot 1_{4 = %.1f}  +
      \\frac{40}{200} \\cdot 1_{5 = %.1f}) \\cdot 600 =
      %.1f $$', input$k1, input$k1, input$k1, input$k1, input$k1, input$k1, g1))
  })
  
  output$h1 <- renderUI({
    withMathJax(
      HTML("Hinweis: $1_{A}$ ist die Indikatorfunktion. Sie nimmt den Wert $1$ an, wenn die Aussage $A: y = m$, stimmt. 
           Hier also $1_{A}=1$, wenn $y$ den von Ihnen gewählten Wert $m$ annimmt. Ansonsten ist $1_{A}=0$.")
    )
  })
  
  output$k2<- renderPlot({
    # basic dotplot (binwidth = the accuracy of the data)
    dotchart <- ggplot(PatientInnen, aes(x=NoShows), dpi = 800) + 
      geom_dotplot(binwidth=binwidth, method="histodot", dotsize = dotsize, fill="#00998A")
    
    # use coor_fixed(ratio=binwidth*dotsize*max frequency) to setup the right y axis height.
    dotchart <- dotchart + 
      theme_classic(20*1.04) + 
      coord_fixed(ratio=binwidth*dotsize*yheight*ratio)
    # tweak the theme a little bit
    dotchart <- dotchart + theme(panel.background=element_blank(),
                                 panel.border = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 #plot.margin=unit(c(1,-5,1,-5), "cm"),
                                 axis.line = element_line(colour = "black"),
                                 axis.line.y = element_blank()
    )
    
    # add more tick mark on x axis
    dotchart = dotchart + scale_x_continuous(breaks = seq(0,8,1), limits = c(0,8))
    # add tick mark on y axis to reflect frequencies. Note yheight is max frequency.
    dotchart = dotchart + scale_y_continuous(limits=c(0, 1/ratio), expand = c(0, 0), 
                                             breaks = seq(0, 1/ratio, 1/yheight*10*1/ratio), 
                                             labels=seq(0,yheight,10))
    # Add Slider Choice
    dotchart = dotchart + geom_vline(xintercept = input$k2, color = "#DA70D6")
    dotchart = dotchart + annotate("text", x = input$k2 + 1.5, y = 0.90*1/ratio, 
                                   label = paste0("Ihre Prognose: m=", input$k2))
    
    # remove x y lables and remove vertical grid lines
    dotchart = dotchart + labs(x="Anzahl", y="Häufigkeit", title = "Patient:Innen die nicht erscheinen") + removeGridX()
    dotchart
  })
  
  output$t2 <- renderText({ 
    g2 <- 800 - 50 * mean(abs(PatientInnen$NoShows-input$k2))
    paste0("Bei einer Prognose von ", input$k2, " liegt der erwartete Lohn bei ", g2, " Euro.")
  })
  
  output$eqn2 <- renderUI({
    g2 <- 800 - 50 * mean(abs(PatientInnen$NoShows-input$k2))
    withMathJax(sprintf(
      '$$800 - 
      (\\frac{70}{200} \\cdot |0 - %.1f|  + 
      \\frac{40}{200}  \\cdot|1 - %.1f|  +
      \\frac{10}{200} \\cdot |2 - %.1f|  +
      \\frac{20}{200} \\cdot |3 - %.1f|  +
      \\frac{20}{200} \\cdot |4 - %.1f|  +
      \\frac{40}{200} \\cdot |5 - %.1f|) \\cdot 50 =
      %.1f $$', input$k2, input$k2, input$k2, input$k2, input$k2, input$k2, g2))
  })
  
  output$k3<- renderPlot({
    # basic dotplot (binwidth = the accuracy of the data)
    dotchart <- ggplot(PatientInnen, aes(x=NoShows), dpi = 800) + 
      geom_dotplot(binwidth=binwidth, method="histodot", dotsize = dotsize, fill="#00998A")
    
    # use coor_fixed(ratio=binwidth*dotsize*max frequency) to setup the right y axis height.
    dotchart <- dotchart + 
      theme_classic(20*1.04) + 
      coord_fixed(ratio=binwidth*dotsize*yheight*ratio)
    # tweak the theme a little bit
    dotchart <- dotchart + theme(panel.background=element_blank(),
                                 panel.border = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 #plot.margin=unit(c(1,-5,1,-5), "cm"),
                                 axis.line = element_line(colour = "black"),
                                 axis.line.y = element_blank()
    )
    
    # add more tick mark on x axis
    dotchart = dotchart + scale_x_continuous(breaks = seq(0,8,1), limits = c(0,8))
    # add tick mark on y axis to reflect frequencies. Note yheight is max frequency.
    dotchart = dotchart + scale_y_continuous(limits=c(0, 1/ratio), expand = c(0, 0), 
                                             breaks = seq(0, 1/ratio, 1/yheight*10*1/ratio), 
                                             labels=seq(0,yheight,10))
    # Add Slider Choice
    dotchart = dotchart + geom_vline(xintercept = input$k3, color = "#808000")
    dotchart = dotchart + annotate("text", x = input$k3 + 1.5, y = 0.90*1/ratio, 
                                   label = paste0("Ihre Prognose: m=", input$k3))
    
    # remove x y lables and remove vertical grid lines
    dotchart = dotchart + labs(x="Anzahl", y="Häufigkeit", title = "Patient:Innen die nicht erscheinen") + removeGridX()
    dotchart
  })
  

  
  
  output$t3 <- renderText({ 
    g3 <- 1000 - 30 * mean((PatientInnen$NoShows-input$k3)^2)
    paste0("Bei einer Prognose von ", input$k3, " liegt der erwartete Lohn bei ", g3, " Euro.")
  })
  
  output$eqn3 <- renderUI({
    g3 <- 1000 - 30 * mean((PatientInnen$NoShows-input$k3)^2)
    withMathJax(sprintf(
      '$$1000 - 
      (\\frac{70}{200} \\cdot (0 - %.1f)^2  + 
      \\frac{40}{200} \\cdot (1 - %.1f)^2   +
      \\frac{10}{200} \\cdot (2 - %.1f)^2   +
      \\frac{20}{200} \\cdot (3 - %.1f)^2   +
      \\frac{20}{200} \\cdot (4 - %.1f)^2   +
      \\frac{40}{200} \\cdot (5 - %.1f)^2  \\cdot 30 =
      %.1f $$', input$k3, input$k3, input$k3, input$k3, input$k3, input$k3, g3))
  })
  
  output$modus <- renderUI({
    withMathJax(
    HTML("Der Modus oder Modalwert ist der Wert der Beobachtung, der am häufigsten vorkommt. 
    In unserem Fall kommt der Wert 0 am häufigsten vor: $$y_{mod}=0$$")
    )
  })
  
  output$median <- renderUI({
    withMathJax(
      HTML("Der Median oder Zentralwert, ist der Wert, der bei aufsteigend sortierten Beobachtungen in der Mitte liegt.
           In unserem Fall zwischen der 100 und 101 sortierten Beobachtung.  
           $$\\underbrace{\\underbrace{0,0, \\ldots, 0}_{70 \\times} , \\underbrace{1,1, \\ldots, 1}_{30 \\times}}_{100},
           \\underbrace{\\underbrace{1,1, \\ldots, 1}_{10 \\times}, \\underbrace{2,2, \\ldots, 2}_{10 \\times},
           \\underbrace{3,3, \\ldots, 3}_{20 \\times}, \\underbrace{4,4, \\ldots, 4}_{20 \\times},
           \\underbrace{5,5, \\ldots, 5}_{40 \\times}}_{100}$$
           Diese sind hier beides Einsen:
           $$y_{(0.5)}=1$$")
    )
  })
  
  output$mittelwert <- renderUI({
    withMathJax(
      HTML("Der arithmetische Mittelwert ist die Summe aller Beobachtungswerte geteilt durch die Anzahl Beobachtungen.
           $$\\bar{y}= \\frac{70 \\cdot 0 + 40 \\cdot 1 + 10 \\cdot 2 + 20 \\cdot 3 + 20 \\cdot 4 + 40 \\cdot 5}{70+40+10+20+20+40}=\\frac{0+40+20+60+80+200}{200}=2$$")
    )
  })
  
  output$k4<- renderPlot({
    # basic dotplot (binwidth = the accuracy of the data)
    dotchart <- ggplot(PatientInnen, aes(x=NoShows), dpi = 800) + 
      geom_dotplot(binwidth=binwidth, method="histodot", dotsize = dotsize, fill="#00998A")
    
    # use coor_fixed(ratio=binwidth*dotsize*max frequency) to setup the right y axis height.
    dotchart <- dotchart + 
      theme_classic(20*1.04) + 
      coord_fixed(ratio=binwidth*dotsize*yheight*ratio)
    # tweak the theme a little bit
    dotchart <- dotchart + theme(panel.background=element_blank(),
                                 panel.border = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 #plot.margin=unit(c(1,-5,1,-5), "cm"),
                                 axis.line = element_line(colour = "black"),
                                 axis.line.y = element_blank()
    )
    
    # add more tick mark on x axis
    dotchart = dotchart + scale_x_continuous(breaks = seq(0,8,1), limits = c(0,8))
    # add tick mark on y axis to reflect frequencies. Note yheight is max frequency.
    dotchart = dotchart + scale_y_continuous(limits=c(0, 1/ratio), expand = c(0, 0), 
                                             breaks = seq(0, 1/ratio, 1/yheight*10*1/ratio), 
                                             labels=seq(0,yheight,10))
    # Add Central Tendency
    dotchart = dotchart + geom_vline(xintercept = 2, color = "#808000")
    dotchart = dotchart + annotate("text", x = 2.75, y = 0.70*1/ratio, 
                                   label = "Mittelwert")
    dotchart = dotchart + geom_vline(xintercept = 1, color = "#DA70D6")
    dotchart = dotchart + annotate("text", x = 1.5, y = 0.8*1/ratio, 
                                   label = "Median")
    dotchart = dotchart + geom_vline(xintercept = 0, color = "#FF8811")
    dotchart = dotchart + annotate("text", x = 0.5, y = 0.9*1/ratio, 
                                   label = "Modus")
    
    # remove x y lables and remove vertical grid lines
    dotchart = dotchart + labs(x="Anzahl", y="Häufigkeit", title = "Patient:Innen die nicht erscheinen") + removeGridX()
    dotchart
  })
  
  
}

shinyApp(ui = ui, server = server)