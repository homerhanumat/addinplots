#' Make a \code{lattice} cloud plot.
#'
#' Interactively make a \code{lattice} cloud plot. The resulting
#' code will be emitted as a call to \code{lattice::cloud}.
#' function.
#'
#' Here's how you use it:
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
  
  # utility
  find_numeric_vars <- function(data) {
    isNum <- function(name, data) {
      is.numeric(get(name, envir = as.environment(data)))
    }
    numNames <- sapply(names(data), isNum, data = data)
    names(data)[numNames]
  }

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
        uiOutput("zVar"),
        uiOutput("xVar"),
        uiOutput("yVar"),
        helpText("Use these sliders to rotate the plot."),
        sliderInput("yScreen","Around z-axis",0,360,value=0,step=1),
        sliderInput("xScreen","Around x-axis",0,360,value=90,step=1),
        sliderInput("zScreen","Around y-axis",0,360,value=0,step=1)
      ),
      mainPanel(
        uiOutput("pending"),
        plotOutput("output"),
        uiOutput("main"),
        uiOutput("pch")
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
      
      # find the numerical variables in the data
      
      
      data
    })
    
    reactiveVarCheck <- reactive({
      xvar <- input$xVar
      yvar <- input$yVar
      zvar <- input$zVar
      xcheck <- !is.null(xvar) && nzchar(xvar)
      ycheck <- !is.null(yvar) && nzchar(yvar)
      zcheck <- !is.null(zvar) && nzchar(zvar)
      return(xcheck && ycheck && zcheck)
    })
    
    output$zVar <- renderUI({
      data <- reactiveData()
      selectInput(inputId = "zVar", label = "z",
                  choices = c("", find_numeric_vars(data)),
                  selected = "")
    })
    
    output$xVar <- renderUI({
      data <- reactiveData()
      selectInput(inputId = "xVar", label = "x",
                  choices = c("", find_numeric_vars(data)),
                  selected = "")
    })
    
    output$yVar <- renderUI({
      data <- reactiveData()
      selectInput(inputId = "yVar", label = "y",
                  choices = c("", find_numeric_vars(data)),
                  selected = "")
    })
    
    output$main <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      textInput(inputId = "main","Graph Title", value = "")
    })
    
    output$pch <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "pch","Point Type", 
                   min = 1, max = 25, step =1, value = 19, width = "100px")
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
      
      if (!reactiveVarCheck()) {
        return(NULL)
      } else {
        xvar <- input$xVar
        yvar <- input$yVar
        zvar <- input$zVar
        command <- paste0("lattice::cloud(",zvar," ~ ",xvar," * ",yvar,", data = ",
                          input$data, ", screen = list(x = -",input$xScreen,", y = ",
                          input$yScreen,", z = ",input$zScreen,")")
        if (!is.null(input$main) && nzchar(input$main)) {
          command <- paste0(command,", main = \"",input$main,"\"")
        }
        if (!is.null(input$pch)) {
          command <- paste0(command,",pch = ",input$pch,")")
        } else {
          command <- paste0(command,",pch = 19)")
        }
        eval(parse(text = command))
      }
    })

    # Listen for 'done'.
    observeEvent(input$done, {
      
      # Emit a cloud call.
      if (reactiveVarCheck()) {
          xvar <- input$xVar
          yvar <- input$yVar
          zvar <- input$zVar
           code <- paste0("cloud(",zvar," ~ ",xvar," * ",yvar,", data = ",
                          input$data,",\n\tscreen = list(x = -",input$xScreen,", y = ",
                          input$yScreen,", z = ",input$zScreen,")")
           if (!is.null(input$main) && nzchar(input$main)) {
             code <- paste0(code, ",\n\tmain = \"",input$main, "\"")
           }
           code <- paste0(code,",\n\tpch = ",input$pch,")")
           rstudioapi::insertText(text = code)
      } else {
         return(NULL)
       }

      invisible(stopApp())
    })
  }

  # Use a browser as a viewer.
  viewer <- browserViewer()
  runGadget(ui, server, viewer = viewer)

}
