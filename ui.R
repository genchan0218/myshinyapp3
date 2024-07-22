source("global.R")


ui = dashboardPage(
  skin = "midnight",
  tags$head(includeCSS(file.path('www', 'style.css'))),
  header = source("header.R", local = TRUE)$value,
  sidebar = source("sidebar.R", local = TRUE)$value,
  body = dashboardBody(
    fluidRow(
      uiOutput("boxes")),
    fluidRow(
      uiOutput("plot_region")),
    fluidRow(
      uiOutput("plot_out_ui")),
    fluidRow(
      uiOutput("queries_out"))
  ),
  controlbar = NULL,
  footer = dashboardFooter(left = HTML("<span style='font-size:12px;'><a href='https://www.loankimrobinson.com'>Author: Loan Kim Robinson </a></span><br>
                                        <span style='font-size:12px;'>Email: loankimrobinson@gmail.com</span><br>"),
                           right = "San Diego, Feb 7th 2019"),
)      







