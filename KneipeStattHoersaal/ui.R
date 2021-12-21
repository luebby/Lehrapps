#
# Kneipe statt Hörsaal - shiny-app
#
# ui.r
# =====-----------------------------------------------------------------------
# Maintainer: Tanja Kistler
#


library(shiny)

# Define UI for application
shinyUI(
  navbarPage(
    title = "Stichprobenumfang des Dreieckstest",
    tabPanel(
      "Hintergrund",
      fluidPage(
        titlePanel(
          "FOMshiny: Stichprobenumfang"),
          fluidRow(
            column(
              12,
              h3("Hintergrund"),
              p("Das Signifikanzniveau alpha einer Hypothesenprüfung sagt, wie oft Sie maximal die Nullhypothese verwerfen wollen, obwohl sie gilt.",
                "Die Nullhypothese zu verwerfen, obwohl sie gilt nennt man $\\alpha$-Fehler oder Fehler 1. Art.",
                "Ein $\\beta$-Fehler oder Fehler 2. Art liegt vor, wenn man $H_0$ nicht verwirft, obwohl $H_0$ nicht gilt.",
                "Die Power ($1-\\beta$) eines Test ist die Wahrscheinlichkeit eine Nullhypothese zu verwerfen, die in der Tat nicht stimmt.",
                "Die Power hängt ab von dem wahren Wert (Effekt), alpha - und dem Stichprobenumfang $n$.",
                "Allgemein gilt: je größer $n$, desto größer ist die Power (da der Standardfehler sinkt), je größer die Abweichung zu $H_0$ (Effekt), desto größer die Power,",
                "je größer $\\alpha$, desto größer die Power (da $H_0$ leichter verworfen wird). Die genaue Funktion des Zusammenhangs hängt von der angenommenen Verteilung ab.",
                "Man kann aber auch anhand der gewünschten Power und Annahmen über den wahren Wert den nötigen Stichprobenumfang ausrechnen."
              ),
              h4("Kneipe statt Hörsaal"),
              p("Im Beispiel Kneipe statt Hörsaal, führte eine Stichprobe von $n = 34$ einen Dreieckstest durch.",
                "12 der TeilnehmerInnen tippten das andere Bier korrekt.",
                "Somit gehen wir vorläufig davon aus, dass der Geschmacksunterschied nicht groß war.",
                "Wie groß hätte die Stichprobe sein müssen, dass ein kleiner Effekt des Geschmacksunterschieds dennoch hätte eruiert werden können?",
                "Wie ändert sich die Stichprobengröße, wenn sich der Anteil unter $H_0$ ($\\pi_0$) oder der wahre Anteil $\\pi$ verändert?",
                "Was passiert, wenn die Power ($1-\\beta$) höher oder niedriger wird?"
              )
          )))),
         tabPanel(
           "Kneipe statt Hörsaal",
            withMathJax(),
            # section below allows in-line LaTeX via $ in mathjax. Replace less-than-sign with <
            # and grater-than-sign with >
            tags$div(
              HTML(
                "<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                  tex2jax: {inlineMath: [['$','$']]}
                });
                </script>")),
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  fluidRow(
                    column(
                      12,
                      h4("Parameter"),
                      sliderInput("p1", "Wahrer Anteil ($\\pi$)", value = 12/34, min = 0.1, max = 0.9, step = 0.001),
                      sliderInput("p2", "Anteil $H_0$ ($\\pi_0$)", value = 1/3, min = 0.1, max = 0.9, step = 0.001),
                      sliderInput("alpha", "Signifikanzniveau ($\\alpha$)", value = 0.05, min = 0.01, max = 0.15, step = 0.01),
                      sliderInput("power", "Power ($1-\\beta$)", value = 0.8, min = 0.15, max = 0.9, step = 0.01)
                    )
                  )
                ),
                mainPanel(
                  fluidRow(
                    column(
                     12,
                     plotOutput("Plotpwr"),
                     p("Stephane Champely (2018). pwr: Basic Functions for Power Analysis. R package version 1.2-2. ",
                       a(
                         "https://CRAN.R-project.org/package=pwr",
                         href = "https://CRAN.R-project.org/package=pwr"
                       ),
                       "")
                    )
                  )
                )
              )
            )
   )
  )
)
