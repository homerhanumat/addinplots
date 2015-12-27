#' Make a \code{lattice} density plot.
#'
#' Interactively make a \code{lattice} denisty plot. The resulting
#' code will be emitted as a call to \code{lattice::densityplot}.
#' function.
#'
#' Here's how you use it:
#'
#' 1. Highlight a symbol naming a \code{data.frame} in your R session,
#'    e.g. \code{mtcars},
#' 2. Execute this addin, to interactively build the plot..
#'
#' When you're done, the code performing this operation will be emitted
#' at the cursor position.
#'
#' @export
densityplotAddin <- function() {
  
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
  
  entered <- function(string) {
    !is.null(string) && nzchar(string)
  }

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text

  # Generate UI for the gadget.
  ui <- miniPage(
    miniTitleBar("Densityplot Code-Helper"),
    miniContentPanel(
    sidebarLayout(
      sidebarPanel(width = 3,
        textInput("data", "Data", value = defaultData),
        helpText("Choose the numerical variable."),
        uiOutput("xVar")
      ),
      mainPanel(width = 9,
        tabsetPanel(
          tabPanel(
            title = "Group",
            uiOutput("pending1"),
        fluidRow(
          column(width = 4,
                 h3("The Plot"),
                 plotOutput("plot1")),
          column(width = 5,
                 h3("The Code"),
                 br(),
                 verbatimTextOutput("code1"))
        )
        ,
        fluidRow(
          column(width = 4, uiOutput("group")),
          column(width = 5, uiOutput("keypos"))
        )
        ,
        fluidRow(
          column(width = 3, uiOutput("keytitle")),
          column(width = 3, uiOutput("keytitlesize")),
          column(width = 3, uiOutput("keycolumns"))
        )
          ), #end tabPanel "Group"
        tabPanel(
          title = "Facet",
          uiOutput("pending2"),
          fluidRow(
            column(width = 4,
                   h3("The Plot"),
                   plotOutput("plot2")),
            column(width = 5,
                   h3("The Code"),
                   br(),
                   verbatimTextOutput("code2"))
          )
          ,
          fluidRow(
            column(width = 4, uiOutput("facet1")),
            column(width = 4, uiOutput("facet2"))
          )
          ,
          fluidRow(
            column(width = 3, uiOutput("layrows")),
            column(width = 3, uiOutput("laycols")),
            column(width = 3, uiOutput("layvarnames"))
          )
        )  # end tabPanel "Facet"
        ,
        tabPanel(
          title = "Other",
          uiOutput("pending3"),
          fluidRow(
            column(width = 4,
                   h3("The Plot"),
                   plotOutput("plot3")),
            column(width = 5,
                   h3("The Code"),
                   br(),
                   verbatimTextOutput("code3"))
          )
          ,
          fluidRow(
            column(width = 3, uiOutput("points")),
            column(width = 3, uiOutput("adjust")),
            column(width = 3, uiOutput("kernel"))
          )
          ,
          fluidRow(
            column(width = 3, uiOutput("from")),
            column(width = 3, uiOutput("to")),
            column(width = 3, uiOutput("bw"))
          )
          ,
          fluidRow(
            column(width = 7, uiOutput("main")),
            column(width = 2, uiOutput("mainsize"))
            )
          ,
          fluidRow(
            column(width = 7, uiOutput("sub")),
            column(width = 2, uiOutput("subsize"))
          )
          ,
          fluidRow(
            column(width = 7, uiOutput("xlab")),
            column(width = 2, uiOutput("xlabsize"))
          )
        ) # end tabPanel "Other"
        ) # end tabsetPanel
      ) # end MainPanel
    ) # end sidebarLayout
    ) # end miniContentPanel
  ) # end miniPage


  # Server code for the gadget.
  server <- function(input, output, session) {


################################
## Reactive functions
################################
    
    # fetch the data frame
    reactiveData <- reactive({
      dataString <- input$data
      if (!nzchar(dataString)) {
        return(errorMessage("data", "No dataset available."))
        }

      if (!exists(dataString, envir = .GlobalEnv)) {
        return(errorMessage("data", paste("No dataset named '",
                                          dataString, "' available.")))
      }

      data <- get(dataString, envir = .GlobalEnv)
      data
    })
    
    # check to see if primary variables have been entered
    reactiveVarCheck <- reactive({
      entered(input$xVar)
    })
    
    # our code-maker
    reactiveCode <- reactive({
      xvar <- input$xVar
      if ( !reactiveVarCheck() ) {
        return("No code to show yet!")
      }
      
      # function and formula:
      code <- paste0("densityplot( ~ ",xvar)
      if (entered(input$facet1)) {
        code <- paste0(code, " | ", input$facet1)
      }
      if (entered(input$facet2)) {
        code <- paste0(code, " * ", input$facet2)
      }
      
      # the data argument
      code <- paste0(code, ",\n\tdata = ",input$data)
      
      # layout information
      if (entered(input$facet1) && !is.null(input$layrows) && nzchar(input$laycols)) {
        code <- paste0(code, ",\n\tlayout = c(",input$laycols,",",input$layrows,")")
      }
      
      # facet variable names?
      if (!is.null(input$layvarnames) && input$layvarnames) {
        code <- paste0(code, ",\n\tstrip = strip.custom(strip.names = c(TRUE, TRUE))")
      }
     
      # groups argument 
      if ( entered(input$group) ) {
        code <- paste0(code,",\n\tgroups = ",input$group)
        code <- paste0(code, ",\n\tauto.key = list(",
                       "\n\t\tspace = \"", input$keypos, "\"",
                       ",\n\t\ttitle = \"", input$keytitle,"\"",
                       ",\n\t\tcex.title = ", input$keytitlesize,
                       ",\n\t\tcolumns = ", input$keycolumns,
                       ")")
      }
      
      # main, ,sub, xlab
      if (entered(input$main)) {
        code <- paste0(code, ",\n\tmain = list(label=\"",input$main, "\"",
                       ",\n\t\tcex = ",input$mainsize,
                       ")")
      }
      
      if (entered(input$sub)) {
        code <- paste0(code, ",\n\tsub = list(label=\"",input$sub, "\"",
                       ",\n\t\tcex = ",input$subsize,
                       ")")
      }
      
      if (entered(input$xlab)) {
        code <- paste0(code, ",\n\txlab = list(label=\"",input$xlab, "\"",
                       ",\n\t\tcex = ",input$xlabsize,
                       ")")
      }
      
      # points, adjust bandwidth (all on one line)
      arg_prev <- FALSE
      if (!is.null(input$points) && !input$points) {
        code <- paste0(code,",\n\tplot.points = FALSE")
        arg_prev <- TRUE
      }
      
      if (!is.null(input$adjust) && input$adjust != 1) {
        if (arg_prev) {
          code <- paste0(code,", adjust = ",input$adjust)
        } else {
          code <- paste0(code,",\n\tadjust = ", input$adjust)
        }
      }
      
      # kernel
      if (!is.null(input$kernel) && input$kernel != "gaussian") {
          code <- paste0(code,",\n\tkernel = \"",input$kernel,"\"")
      }
      
      # from, to (on one line)
      arg_prev <- FALSE
      if (entered(input$from)) {
        code <- paste0(code,",\n\tfrom = ",as.numeric(input$from))
        arg_prev <- TRUE
      }
      
      if (entered(input$to)) {
        if (arg_prev) {
          code <- paste0(code,", to = ",as.numeric(input$to))
        } else {
          code <- paste0(code,",\n\tto = ",as.numeric(input$to))
        }
      }
      
      # theme argument
      wantBW <- !is.null(input$bw) && input$bw
      if ( wantBW ) {
        code <- paste0(code, 
            ",\n\tpar.settings = canonical.theme(color=FALSE)")
      }
      
      # add closing paren:
      code <- paste0(code,")")
      return(code)
    })
    
    # our plot-maker
    makeplot <- reactive({
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
    
    # compute a reasonable layout
    reactiveLayout <- reactive({
      
      getNumberLevels <- function(varName) {
        if (entered(varName)) {
          var <- get(varName, envir = as.environment(reactiveData()))
          return(length(levels(var)))
        } else {
          return(NULL)
        }
      }
      
      f1 <- input$facet1
      f2 <- input$facet2
      
      if (entered(f1) && !entered(f2)) {
        rows <- getNumberLevels(f1)
        cols <- 1
      }
      if (entered(f1) && entered(f2)) {
        rows <- getNumberLevels(f1)
        cols <- getNumberLevels(f2)
      }
      
      return(list(rows = rows, cols = cols))
      
    })
    
############################
## Primary Variables
############################
    
    output$xVar <- renderUI({
      data <- reactiveData()
      selectInput(inputId = "xVar", label = "x",
                  choices = c("", find_numeric_vars(data)),
                  selected = "")
    })
    
##############################
## For groups tab
#############################
    
    output$pending1 <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })
    
    output$plot1 <- renderPlot({
      makeplot()
    })
    
    output$code1 <- renderText({
      reactiveCode()
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
      availableFactors <- factors
      if (entered(input$facet1)) {
        availableFactors <- setdiff(availableFactors, input$facet1)
      }
      if (entered(input$facet2)) {
        availableFactors <- setdiff(availableFactors, input$facet2)
      }
      if (entered(input$group)) {
        selected <- input$group
      } else {
        selected <- ""
      }
      selectInput(inputId = "group", label = "Group by:",
                  choices = c("", availableFactors),
                  selected = selected)
    })
    
    output$keypos <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      if ( !entered(input$group) ) {
        return(NULL)
      }
      selectInput(inputId = "keypos", label = "Legend position:",
                  choices = c("top","left","right","bottom"),
                  selected = "top")
    })
    
    output$keytitle <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      if ( !entered(input$group) ) {
        return(NULL)
      }
      textInput(inputId = "keytitle", label = "Legend title:",
                value = input$group)
    })
    
    output$keytitlesize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      if ( !entered(input$group) ) {
        return(NULL)
      }
      numericInput(inputId = "keytitlesize", label = "Title Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$keycolumns <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      if ( !entered(input$group) ) {
        return(NULL)
      }
      numericInput(inputId = "keycolumns", label = "Key Columns",
                   min = 1, max = length(levels(input$group)), value = 1, step = 1)
    })
    

###############################
## for facets tab
###############################
    
    output$pending2 <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })
    
    output$plot2 <- renderPlot({
      makeplot()
    })
    
    output$code2 <- renderText({
      reactiveCode()
    })
    
    output$facet1 <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      data <- reactiveData()
      factors <- find_factor_vars(data)
      if (length(factors) == 0) {
        return(NULL)
      }
      availableFactors <- factors
      if (entered(input$group)) {
        availableFactors <- setdiff(availableFactors, input$group)
      }
      if (entered(input$facet2)) {
        availableFactors <- setdiff(availableFactors, input$facet2)
      }
      if (entered(input$facet1)) {
        selected <- input$facet1
      } else {
        selected <- ""
      }
      selectInput(inputId = "facet1", label = "Facet by:",
                  choices = c("", availableFactors),
                  selected = selected)
    })
    
    output$facet2 <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      data <- reactiveData()
      factors <- find_factor_vars(data)
      if (length(factors) == 0) {
        return(NULL)
      }
      availableFactors <- factors
      if (entered(input$group)) {
        availableFactors <- setdiff(availableFactors, input$group)
      }
      if (entered(input$facet1)) {
        availableFactors <- setdiff(availableFactors, input$facet1)
      }
      if (entered(input$facet2)) {
        selected <- input$facet2
      } else {
        selected <- ""
      }
      anyLeft <- length(availableFactors) > 0
      if ( entered(input$facet1) && anyLeft ) {
        selectInput(inputId = "facet2", label = "Also facet by:",
                    choices = c("", availableFactors),
                    selected = selected)
      }
    })
    
    output$layrows <- renderUI({
      if (!entered(input$facet1)) {
        return(NULL)
      }
      layout <- reactiveLayout()
      rows <- layout$rows
      numericInput(inputId = "layrows", label = "Rows in Layout",
                   min = 1, value = rows)
    })
    
    output$laycols <- renderUI({
      if (!entered(input$facet1)) {
        return(NULL)
      }
      layout <- reactiveLayout()
      cols <- layout$cols
      numericInput(inputId = "laycols", label = "Columns in Layout",
                   min = 1, value = cols)
    })
    
    output$layvarnames <- renderUI({
      if (!entered(input$facet1)) {
        return(NULL)
      }
      checkboxInput(inputId = "layvarnames", 
                    label = "Show Facet-Variable Names")
    })
    
##############################
## for "other" tab
##############################
    
    output$pending3 <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })
    
    output$plot3 <- renderPlot({
      makeplot()
    })
    
    output$code3 <- renderText({
      reactiveCode()
    })
    
    output$main <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      textInput(inputId = "main","Graph Title", value = "")
    })
    
    output$mainsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "mainsize","Graph Title Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$sub <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      textInput(inputId = "sub","Graph Sub-title", value = "")
    })
    
    output$subsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "subsize","Graph Sub-title Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$xlab <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      textInput(inputId = "xlab","x-Label", value = "")
    })
    
    output$xlabsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "xlabsize","x-Label Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$points <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      checkboxInput(inputId = "points", label = "Plot Points", value = TRUE,
                    width = "100px")
    })
    
    output$adjust <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "adjust", label = "Adjust Bandwidth", value = 1,
                    min = 0.1, width = "100px", ste = 0.1)
    })
    
    output$kernel <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      selectInput(inputId = "kernel", label = "Kernal-Type",
                  choices = c("gaussian", "rectangular", "triangular", 
                              "epanechnikov", "biweight", "cosine","optcosine"),
                   selected = "gaussian")
    })
    
    output$from <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      textInput(inputId = "from", label = "Start curve at")
    })
    
    output$to <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      textInput(inputId = "to", label = "End curve at")
    })
    
    output$bw <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      checkboxInput(inputId = "bw", label = "Bl & Wh", width = "100px")
    })
    
#######################
## Finish Up
#######################

    # Listen for Done.
    observeEvent(input$done, {
      
      # Get code to user:
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