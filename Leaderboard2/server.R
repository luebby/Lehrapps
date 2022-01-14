#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mosaic)
library(DT)
library(lubridate)
library(readr)
library(googlesheets4)
library(shinyjs)
library(here)

# Leaderboard von Google Spreadsheet laden

# Zurücksetzen der Google-OAuth:
gs4_deauth()

# OAuth Konfigurieren:
google_app <- httr::oauth_app(
  "my-first-test-pr-1566813817446",
  key = "869420690012-i6e2vd9jghpal8g73fr5q2n803rt6i6h.apps.googleusercontent.com",
  secret = "OCSPX-K0CdHa86Z5wuKr7cKYt0a9gTizAX"
)
gs4_auth_configure(google_app)
#
path_to_json <- here("gs4","my-first-test-pr-1566813817446-d3d1ca8b1622.json")
print(path_to_json)
gs4_auth(path = path_to_json)
df_leaderboard <<- read_sheet(
  "1S8FTzl44t7MgW6lTJcHgBuYfk2VUZHe1oTzy0DTl3JE",
  sheet = "Regressionscontest",
  col_types =  "ctn") %>%
  filter(score > 0) %>%
  filter(submitter != "") %>%
  filter(!is.na(submission_date))

print(head(df_leaderboard))

update_leaderboard <- function(input) {
  submitted_input <- input$NutzerInnenName
  if (!(is.null(submitted_input) || submitted_input == "")) {
    score <- input$NutzerInnenWert
    submitter <- input$NutzerInnenName
    submit_date <- strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%d %H:%M:%S%z")
    df_leaderboard <<- rbind(
      df_leaderboard,
      c(list(submitter = submitter, submission_date = submit_date, score = score))) %>%
      filter(score > 0)
    print(df_leaderboard)
    write_sheet(
      df_leaderboard,
      ss = "1S8FTzl44t7MgW6lTJcHgBuYfk2VUZHe1oTzy0DTl3JE",
      sheet = "Regressionscontest")
  }

  # Nur die Daten vom aktuellen Tag anzeigen!
  filterdate <- round_date(today(), "day")

  return(
    df_leaderboard %>%
      filter(round_date(date(submission_date), "day") == filterdate)
  )

}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observeEvent(input$NutzerInnenWert, {
    shinyjs::disable("NutzerInnenWert")
  })

  # Data to show
  read.csv("data/energy.csv",
           colClasses = c("numeric", "numeric"),
           col.names = c("x", "y")
  ) %>% mutate( type = "data") -> mydata

  click_saved <- reactiveValues(singleclick = NULL)

  output$leaderboard <- DT::renderDataTable({
    input$submitScore
    table <- isolate(update_leaderboard(input))
    DT::datatable(
      table,
      colnames = c("Nutzer:in", "Datum", "Score")
      ) %>%
      formatDate(
        columns = 2,
        method =  "toLocaleTimeString",
        params = list(
          'de-DE',
          list(
            year = 'numeric',
            month = 'numeric',
            day = 'numeric',
            hours = 'numeric',
            minutes = 'numeric',
            seconds = 'numeric' )
        )
      )
  })

  # Plot is available
  output$plot1 <- renderPlot({
    gf_point(y ~ x, data = filter(mydata, type == "data")) %>%
      gf_labs(x = "Temperatur", y = "Verbrauch") %>%
      gf_theme(theme_classic())
  })

  # if you click
  observeEvent(eventExpr = input$plot_click, handlerExpr = {

    click_saved$singleclick <- rbind(click_saved$singleclick, c(input$plot_click[1], input$plot_click[2]))
    if (dim(click_saved$singleclick)[1] > 2) {
      click_saved$singleclick <- as.data.frame(click_saved$singleclick[dim(click_saved$singleclick)[1],])
    }

    output$plot1 <- renderPlot({
      ys <- as.vector(unlist(click_saved$singleclick[,2]))
      xs <- as.vector(unlist(click_saved$singleclick[,1]))
      clicked_points <- data.frame(
        x = xs,
        y = ys) %>% mutate(type = "clicked")

      plt <- gf_point(
        y ~ x,
        color = ~type,
        show.legend = FALSE,
        data = rbind(mydata, clicked_points))

      if (dim(click_saved$singleclick)[1] == 2) {
        if (xs[1] == xs[2] && ys[1] == ys[2]) {
          # Falls jemand zu schnell auf einer Stelle klickt gibt es einen Fehler.
          # Hiermit umgehen wir dieses in dem wir eine echte Steigung erzeugen!
          xs[2] = xs[2] + 10
          ys[2] = ys[2] + 10
        }
        # Erzeuge ein Gerade durch die zwei geklickten Punkte
        fit <- lm(ys ~ xs)

        # Berechne in proj die durch das Modell geschätzen Werte:
        proj <- fit$coefficients[2] * mydata$x + fit$coefficients[1]

        proj_data <- data.frame(
          x = mydata$x,
          y = proj
        ) %>% mutate( type = "residuals" )
        plt <- gf_point(
          y ~ x,
          color = ~type,
          show.legend = FALSE,
          data = rbind(mydata, clicked_points, proj_data))

        # Zeichne Gerade durch die zwei geklickten Punkte:
        plt %>%
          gf_coefline(coef = fit$coefficients, color = "red") -> plt

        # residuals
        myscore <- 0
        n <- dim(mydata)[1]
        for (k in 1:n) {
          residuals <- rbind( mydata[k,], c(mydata[k,1], proj[k]) )
          # print(str(residuals))
          plt %>% gf_line(y ~ x, color = "purple", data = residuals) -> plt
          # lines(residuals, col = 'purple')

          myscore <- myscore + (mydata[k,2] - proj[k])^2
        }


        plt %>% gf_labs(
          title = paste("Ihr Score: ",
                        format(round(myscore/n,2),
                               big.mark = ".",
                               decimal.mark = ","))
        ) -> plt

        updateNumericInput(session, "NutzerInnenWert", value = round(myscore/n,2))

        output$check <- renderText(
          paste("Ihre Gleichung: y = ",
                format(round(fit$coefficients[1],2),
                       big.mark = ".",
                       decimal.mark = ","),
                "+",
                format(round(fit$coefficients[2],2),
                       ig.mark = ".",
                       ecimal.mark = ","),
                intToUtf8(183), # UTF-8 int für &centerdot;
                "x"))
      }
      our_colors <- c("data" = "black", "clicked" = "red", "residuals" = "purple")
      plt %>%
        gf_refine(
          scale_color_manual(values = our_colors)
        ) %>%
        gf_theme(theme_classic())
    })
  })
})
