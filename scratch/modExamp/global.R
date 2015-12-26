
cloudRotationInput <- function(id, label = "Rotation") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("yScreen"),"Around z-axis",0,360,value=0,step=1),
    sliderInput(ns("xScreen"),"Around x-axis",0,360,value=90,step=1),
    sliderInput(ns("zScreen"),"Around y-axis",0,360,value=0,step=1)
    )
}

cloudRotation <- function(input, output, session) {
  cloudplot <- reactive({
    lattice::cloud(Petal.Length ~ Sepal.Length * Petal.Width,
                 data = iris
                 ,
                 screen = list(x = -input$xScreen,
                               y = input$yScreen,
                               z = input$zScreen)
                 )
  })
  
  return(cloudplot)
}


