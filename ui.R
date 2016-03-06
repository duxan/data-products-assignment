shinyUI(pageWithSidebar(
  headerPanel('EM clustering'),
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", ".shiny-input-container { width:50%; display:inline-block; }"),
      tags$style(type="text/css", "#iter { width:150% !important;}")
    ),
    
    h3("First cluster params"),
    span("X-axis: "),
    numericInput('mu1x', 'mean', min = 1, max = 6, value = 4, step = 0.05),
    numericInput('std1x', 'std.dev range', min = 0.5, max = 4, value = 2, step = 0.5),
    br(),
    span("Y-axis: "),
    numericInput('mu1y', '', min = 40, max = 100, value = 40, step = 2),
    numericInput('std1y', '', min = 2, max = 30, value = 10, step = 2),
    
    h3("Second cluster params"),
    span("X-axis: "),
    numericInput('mu2x', 'mean', min = 1, max = 6, value = 2, step = 0.05),
    numericInput('std2x', 'std.dev range', min = 0.5, max = 4, value = 2, step = 0.5),
    br(),
    span("Y-axis: "),
    numericInput('mu2y', '', min = 40, max = 100, value = 50, step = 2),
    numericInput('std2y', '', min = 2, max = 30, value = 10, step = 2),
    
    hr(),
    h3("# EM algorithm iterations run"),
    selectInput("iter", "EM iteration:", c(1:50))
  ),
  mainPanel(
    plotOutput('plotEMinitial'),
    plotOutput('plotEMiter')
  )
))
