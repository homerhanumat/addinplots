library(shiny)
library(lattice)

shinyServer(function(input, output, session) {
  
  myplot <- callModule(cloudRotation, "numvars")
  output$plot1 <- renderPlot({
    myplot()
  })
  output$plot2 <- renderPlot({
    myplot()
  })
})