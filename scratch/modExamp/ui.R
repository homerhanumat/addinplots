shinyUI(
  fluidPage(
  sidebarLayout(
    sidebarPanel(
      cloudRotationInput("numvars", "Choose rotations:")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Plot 1",
                 plotOutput("plot1")),
        tabPanel(title = "Plot 2",
                 plotOutput("plot2"))
      )
    ) # end mainPanel
  ) # end sidebarLayout
) # end fluidPage
) # end shinyUI