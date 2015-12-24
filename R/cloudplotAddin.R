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
  
  # utilities:
  
  find_numeric_vars <- function(data) {
    isNum <- function(name, data) {
      is.numeric(get(name, envir = as.environment(data)))
    }
    numNames <- sapply(names(data), isNum, data = data)
    names(data)[numNames]
  }
  
  find_factor_vars <- function(data) {
    isFac <- function(name, data) {
      is.factor(get(name, envir = as.environment(data)))
    }
    facNames <- sapply(names(data), isFac, data = data)
    names(data)[facNames]
  }

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text

  # Generate UI for the gadget.
  ui <- miniPage(
    miniTitleBar("Make a Cloud Plot"),
    miniContentPanel(
      sidebarPanel(width = 3,
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
        plotOutput("cloudplot")
        ,
        fluidRow(
          column(width = 7, uiOutput("main")),
          column(width = 2, uiOutput("pch"))
        )
        ,
        fluidRow(
          column(width = 4, uiOutput("group")),
          column(width = 5, uiOutput("keypos"))
        )
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
    
    reactiveCode <- reactive({
      xvar <- input$xVar
      yvar <- input$yVar
      zvar <- input$zVar
      code <- paste0("cloud(",zvar," ~ ",xvar," * ",yvar,", data = ",
                     input$data,",\n\tscreen = list(x = -",input$xScreen,", y = ",
                     input$yScreen,", z = ",input$zScreen,")")
      if (!is.null(input$main) && nzchar(input$main)) {
        code <- paste0(code, ",\n\tmain = \"",input$main, "\"")
      }
      if (!is.null(input$pch)) {
        code <- paste0(code,",\n\tpch = ",input$pch)
      }
      wantGroups <- !is.null(input$group) && nzchar(input$group)
      if ( wantGroups ) {
        code <- paste0(code,",\n\tgroups = ",input$group)
        if (input$keypos == "top") {
          code <- paste0(code, ",\n\tauto.key = TRUE")
        } else {
          code <- paste0(code, 
                  ",\n\tauto.key = list(space = \"",input$keypos,"\")")
        }
      }
      
      # add closing paren:
      code <- paste0(code,")")
      return(code)
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
    
    output$group <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      data <- reactiveData()
      factors <- find_factor_vars(data)
      if (length(factors) == 0) {
        return(NULL)
      }
      selectInput(inputId = "group", label = "Group by:",
                  choices = c("", factors),
                  selected = "")
    })
    
    output$keypos <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      noGroups <- is.null(input$group) || !nzchar(input$group)
      if ( noGroups ) {
        return(NULL)
      }
      selectInput(inputId = "keypos", label = "Legend position:",
                  choices = c("top","left","right","bottom"),
                  selected = "top")
    })

    output$pending <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })

    output$cloudplot <- renderPlot({
      data <- reactiveData()
      if (isErrorMessage(data))
        return(NULL)
      
      if (!reactiveVarCheck()) {
        return(NULL)
      } else {
        command <- reactiveCode()
        eval(parse(text = command))
      }
    })

    # Listen for 'done'.
    observeEvent(input$done, {
      
      # Emit a cloud call.
      if (reactiveVarCheck()) {
          code <- reactiveCode()
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
