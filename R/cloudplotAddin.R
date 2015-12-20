#' Make a \code{lattice} cloud plot.
#'
#' Interactively make a \code{lattice} cloud plot. The resulting
#' code will be emitted as a call to \code{lattice::cloud}.
#' function.
#'
#' The intended way to use this is as follows:
#'
#' 1. Highlight a symbol naming a \code{data.frame} in your R session,
#'    e.g. \code{mtcars},
#' 2. Execute this addin, to interactively subset it.
#'
#' When you're done, the code performing this operation will be emitted
#' at the cursor position.
#'
#' @export
cloudplotAddin <- function() {

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text

  # Generate UI for the gadget.
  ui <- gadgetPage(
    titlebar("Make a Cloud Plot"),
    contentPanel(
      sidebarPanel(
        textInput("data", "Data", value = defaultData),
        helpText("Choose your variables."),
        textInput("zVar", "z", value = "Petal.Length"),
        textInput("xVar", "x", value = "Sepal.Length"),
        textInput("yVar", "y", value = "Sepal.Width"),
        helpText("Use these sliders to rotate the plot."),
        sliderInput("zScreen","z",0,360,value=0,step=1),
        sliderInput("xScreen","x",0,360,value=90,step=1),
        sliderInput("yScreen","y",0,360,value=0,step=1)
      ),
      mainPanel(
        uiOutput("pending"),
        plotOutput("output")
        )
    )
  )


  # Server code for the gadget.
  server <- function(input, output, session) {

    reactiveData <- reactive({

      # Collect inputs.
      dataString <- input$data
      
      # Check to see if there is data called 'data',
      # and access it if possible.
      if (!nzchar(dataString))
        return(errorMessage("data", "No dataset available."))

      if (!exists(dataString, envir = .GlobalEnv))
        return(errorMessage("data", paste("No dataset named '", dataString, "' available.")))

      data <- get(dataString, envir = .GlobalEnv)

      eval(call, envir = .GlobalEnv)
    })

    output$pending <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })

    output$output <- renderPlot({
      data <- reactiveData()
      if (isErrorMessage(data))
        return(NULL)
      form <- as.name(paste(input$zVar,"~", input$xVar, "*", input$yVar))
      lattice::cloud(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris,
                     screen = list(x=-input$xScreen,y=input$yScreen,z=input$zScreen))
    })

    # Listen for 'done'.
    observeEvent(input$done, {

      # Emit a subset call if a dataset has been specified.
      # if (nzchar(input$data) && nzchar(input$subset)) {
      #   code <- paste("subset(", input$data, ", ", input$subset, ")", sep = "")
      #   rstudioapi::insertText(text = code)
      # }

      invisible(stopApp())
    })
  }

  # Use a modal dialog as a viewr.
  viewer <- browserViewer()
  runGadget(ui, server, viewer = viewer)

}
