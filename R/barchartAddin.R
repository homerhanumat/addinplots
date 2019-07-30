#' Make a \code{lattice} bar chart.
#'
#' Interactively make a \code{lattice} bar chart. The resulting
#' code will be emitted as a call to \code{lattice::barchart}.
#' function.
#'
#' Here's how you use it:
#'
#' 1. Highlight a symbol naming a \code{data.frame} in your R session,
#'    e.g. \code{mtcars},
#' 2. Execute this addin and interactively build the plot.
#'
#' When you're happy with the plot, press Done.  The code for
#' the plot  will be be placed at the cursor position.
#'
#' @export
barchartAddin <- function() {
  
  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()
  
  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text
  
  # UI for gadget ---------------------------------
  ui <- miniPage(
    useShinyCustom(),
    gadgetTitleBar("Bar Chart Code-Helper"),
    miniContentPanel(
      sidebarLayout(
        sidebarPanel(width = 2,
                     textInput("data", "Data", value = defaultData),
                     helpText("Choose the factor variable."),
                     uiOutput("xVar")
        ),
        mainPanel(width = 10,
                  tabsetPanel(id = "buildertabs",
                              tabPanel(
                                title = "Group",
                                uiOutput("pending1"),
                                fluidRow(
                                  column(width = 5,
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
                                  column(width = 5,
                                         h3("The Plot"),
                                         plotOutput("plot2")),
                                  column(width = 5,
                                         h3("The Code"),
                                         br(),
                                         verbatimTextOutput("code2"))
                                )
                                ,
                                fluidRow(
                                 htmlOutput("facetmessage") 
                                )
                                ,
                                fluidRow(
                                  column(width = 4, uiOutput("facet1")),
                                  column(width = 4, uiOutput("facet2"))
                                )
                                ,
                                fluidRow(
                                  column(width = 2, uiOutput("f1name")),
                                  column(width = 2, uiOutput("f1number")),
                                  column(width = 2, uiOutput("f1overlap"))
                                )
                                ,
                                fluidRow(
                                  column(width = 2, uiOutput("f2name")),
                                  column(width = 2, uiOutput("f2number")),
                                  column(width = 2, uiOutput("f2overlap"))
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
                                  column(width = 5,
                                         h3("The Plot"),
                                         plotOutput("plot3")),
                                  column(width = 5,
                                         h3("The Code"),
                                         br(),
                                         verbatimTextOutput("code3"))
                                )
                                ,
                                fluidRow(
                                  column(width = 2, uiOutput("stack")),
                                  column(width = 2, uiOutput("horizontal")),
                                  column(width = 3, uiOutput("ownlabs")),
                                  column(width = 3, uiOutput("rotatelabs"))
                                )
                                ,
                                fluidRow(
                                  uiOutput("customlabels")
                                )
                                ,
                                fluidRow(
                                  column(width = 5, uiOutput("main")),
                                  column(width = 2, uiOutput("mainsize"))
                                )
                                ,
                                fluidRow(
                                  column(width = 5, uiOutput("sub")),
                                  column(width = 2, uiOutput("subsize"))
                                )
                                ,
                                fluidRow(
                                  column(width = 5, uiOutput("xlab")),
                                  column(width = 2, uiOutput("xlabsize"))
                                )
                                ,
                                fluidRow(
                                  column(width = 5, uiOutput("ylab")),
                                  column(width = 2, uiOutput("ylabsize"))
                                )
                              ) # end tabPanel "Other"
                  ) # end tabsetPanel
        ) # end MainPanel
      ) # end sidebarLayout
    ) # end miniContentPanel
  ) # end miniPage
  
  
  # Server code for the gadget.
  server <- function(input, output, session) {
    
    ## Reactive Values ----------------
    ###########################
    
    rv <- reactiveValues(
      code = NULL,
      customlabels = NULL
    )
    
    ## Reactive functions -------------------
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
    observe({
      xvar <- input$xVar
      if ( !reactiveVarCheck() ) {
        return("No code to show yet!")
      }
      
      code <- ""
      
      # lead-up table-maker code:
      talliedVar <- ifelse(entered(input$group),
                           paste0(" + ",xvar), xvar)
      grouper <- ifelse(entered(input$group),
                        input$group,
                        "")
      facet1 <- ifelse(entered(input$facet1),
                       paste0(" + ",input$facet1),
                       "")
      facet2 <- ifelse(entered(input$facet2),
                       paste0(" + ",input$facet2),
                       "")
      code <- paste0(code, "contTable <- xtabs(~ ",
                     grouper,facet1,facet2,talliedVar, ", data = ",
                     input$data, ")\n")
      
      # function and formula:
      code <- paste0(code,"barchart(contTable")
      
      # layout information
      if (entered(input$facet1) && exists_as_numeric(input$layrows) && exists_as_numeric(input$laycols)) {
        code <- paste0(code, ",\n\tlayout = c(",input$laycols,",",input$layrows,")")
      }
      
      # facet variable names?
      if (!is.null(input$layvarnames) && input$layvarnames) {
        code <- paste0(code, ",\n\tstrip = strip.custom(strip.names = c(TRUE, TRUE))")
      }
      
      # groups argument 
      if ( entered(input$group) ) {
        code <- paste0(code, ",\n\tauto.key = list(",
                       "\n\t\tspace = \"", input$keypos, "\"",
                       ",\n\t\ttitle = \"", input$keytitle,"\"",
                       ",\n\t\tcex.title = ", input$keytitlesize,
                       ",\n\t\tcolumns = ", input$keycolumns,
                       ")")
      }
      
      # stacked and horizontal (on one line)
      prev_arg <- FALSE
      if (!is.null(input$stack) && !input$stack) {
        code <- paste0(code, ",\n\tstack = FALSE ")
        prev_arg <- TRUE
      }
      
      if (!is.null(input$horizontal) && !input$horizontal) {
        code <- ifelse(prev_arg,
                       paste0(code, ", horizontal = FALSE"),
                       paste0(code, ",\n\thorizontal = FALSE"))
        prev_arg <- TRUE
      }
      
      # customizing value-labels
      CustomLabels <- !is.null(input$ownlabs) && input$ownlabs &&entered(input$customlabels)
      rotateLabels <- exists_as_numeric(input$rotatelabs) && input$rotatelabs != 0
      labelAxis <- ifelse(input$horizontal, "y","x")
      
      if ( CustomLabels ) {
        cl <- unlist(strsplit(input$customlabels,split=","))
        cl <- cl[cl !=""]
        cl <- paste0(cl, collapse = '\",\"')
        cl <- paste0('\"',cl,'\"')
      }
      
      if ( CustomLabels && !rotateLabels ) {
        code <- paste0(code, ",\n\tscale = list(",labelAxis,
                       " = list(labels = c(",cl,")))")
      }
      
      
      if ( CustomLabels && rotateLabels ) {
        code <- paste0(code, ",\n\tscale = list(",labelAxis,
                       " = list(labels = c(",cl,"),\n\t\t\t      rot = ",
                       input$rotatelabs,"))")
      }
      
      if ( !CustomLabels && rotateLabels ) {
        code <- paste0(code, ",\n\tscale = list(",labelAxis,
                       " = list(rot = ",input$rotatelabs,"))")
      }
      
      
    
    # main, ,sub, xlab, ylab
      if (entered(input$main) && exists_as_numeric(input$mainsize) &&  input$mainsize == 1) {
        code <- paste0(code, ",\n\tmain = \"",input$main, "\"")
      }
      
      if (entered(input$main) && exists_as_numeric(input$mainsize) && input$mainsize != 1) {
        code <- paste0(code, ",\n\tmain = list(label=\"",input$main, "\"",
                       ",\n\t\tcex = ",input$mainsize,
                       ")")
      }
      
      if (entered(input$sub) && exists_as_numeric(input$subsize) && input$subsize == 1) {
        code <- paste0(code, ",\n\tsub = \"",input$sub,"\"")
      }
      
      if (entered(input$sub) && exists_as_numeric(input$subsize) && input$subsize != 1) {
        code <- paste0(code, ",\n\tsub = list(label=\"",input$sub, "\"",
                       ",\n\t\tcex = ",input$subsize,
                       ")")
      } 
      
      if (entered(input$xlab) && exists_as_numeric(input$xlabsize) && input$xlabsize == 1) {
        code <- paste0(code, ",\n\txlab = \"",input$xlab,"\"")
      }
      
      if (!entered(input$xlab) && exists_as_numeric(input$xlabsize) && input$xlabsize !=1) {
        code <- paste0(code, ",\n\txlab = list(cex = ",input$xlabsize,")")
      }
      
      if (entered(input$xlab) && exists_as_numeric(input$xlabsize) && input$xlabsize != 1) {
        code <- paste0(code, ",\n\txlab = list(label=\"",input$xlab, "\"",
                       ",\n\t\tcex = ",input$xlabsize,
                       ")")
      } 
      
      if (entered(input$ylab) && exists_as_numeric(input$ylabsize) && input$ylabsize == 1) {
        code <- paste0(code, ",\n\tylab = \"",input$ylab,"\"")
      }
      
      if (!entered(input$ylab) && exists_as_numeric(input$ylabsize) && input$ylabsize !=1) {
        code <- paste0(code, ",\n\tylab = list(cex = ",input$ylabsize,")")
      }
      
      if (entered(input$ylab) && exists_as_numeric(input$ylabsize) && input$ylabsize != 1) {
        code <- paste0(code, ",\n\tylab = list(label=\"",input$ylab, "\"",
                       ",\n\t\tcex = ",input$ylabsize,
                       ")")
      } 
      
      # theme argument
      wantBW <- !is.null(input$bw) && input$bw
      if ( wantBW ) {
        code <- paste0(code, 
                       ",\n\tpar.settings = canonical.theme(color=FALSE)")
      }
      
      # add closing paren:
      code <- paste0(code,")")
      #return(code)
      rv$code <- code
    })
    
    # hair-trigger plotting (code not isolated)
    makeplot <- reactive({
      data <- reactiveData()
      if (isErrorMessage(data))
        return(NULL)
      
      if (!reactiveVarCheck()) {
        return(NULL)
      } else {
        command <- rv$code
        eval(parse(text = command), envir = globalenv())
      }
    })
    
    # for use with Draw Plot button (code isolated)
    # makeplot2 <- reactive({
    #   input$draw # for dependecny on action button
    #   input$buildertabs # redraw plot when user changes tabs
    #   data <- reactiveData()
    #   if (isErrorMessage(data))
    #     return(NULL)
    #   
    #   if (!reactiveVarCheck()) {
    #     return(NULL)
    #   } else {
    #     command <- isolate(rv$code)
    #     eval(parse(text = command))
    #   }
    # })
    
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
        res <- list(rows = rows, cols = cols)
        return(res)
      }
      if (entered(f1) && entered(f2)) {
        rows <- getNumberLevels(f1)
        cols <- getNumberLevels(f2)
        res <- list(rows = rows, cols = cols)
        return(res)
      }
      # safety values
      res <- NULL
      return(res)
      
    })
    
    
    ## Primary Variables --------------------
    ############################
    
    output$xVar <- renderUI({
      data <- reactiveData()
      selectInput(inputId = "xVar", label = "Variable to tally:",
                  choices = c("", find_factor_vars(data)),
                  selected = "")
    })
    
    
    ## For groups tab -------------------------
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
      rv$code
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
      customTextInput(inputId = "keytitle", label = "Legend title:",
                value = input$xVar)
    })
    
    output$keytitlesize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      if ( !entered(input$group) ) {
        return(NULL)
      }
      customNumericInput(inputId = "keytitlesize", label = "Title Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$keycolumns <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      if ( !entered(input$group) ) {
        return(NULL)
      }
      customNumericInput(inputId = "keycolumns", label = "Key Columns",
                   min = 1, max = length(levels(input$group)), value = 1, step = 1)
    })
    
    
    
    ## for facets tab --------------------
    ###############################
    
    output$pending2 <- renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        h4(style = "color: #AA7732;", data$message)
    })
    
    output$plot2 <- renderPlot({
      makeplot()
    })
    
    outputOptions(output, "plot2", priority = -1)
    
    output$code2 <- renderText({
      rv$code
    })
    
    outputOptions(output, "code2", priority = -1)
    
    output$facetmessage <- renderText({
      if (!reactiveVarCheck()  || entered(input$group)) {
        return(NULL)
      }
      "<strong>Facetting requires prior choice of grouping variable.</strong>"
    })
    
    output$facet1 <- renderUI({
      if (!reactiveVarCheck()  || !entered(input$group)) {
        return(NULL)
      }
      data <- reactiveData()
      factors <- find_factor_vars(data)
      if (length(factors) == 0) {
        return(NULL)
      }
      available <- factors
      available <- setdiff(available, input$xVar)
      if (entered(input$group)) {
        available <- setdiff(available, input$group)
      }
      if (entered(input$facet2)) {
        available <- setdiff(available, input$facet2)
      }
      if (entered(input$facet1)) {
        selected <- input$facet1
      } else {
        selected <- ""
      }
      # revise rv
      selectInput(inputId = "facet1", label = "Facet by:",
                  choices = c("", available),
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
      available <- factors
      available <- setdiff(available, input$xVar)
      if (entered(input$group)) {
        available <- setdiff(available, input$group)
      }
      if (entered(input$facet1)) {
        available <- setdiff(available, input$facet1)
      }
      if (entered(input$facet2)) {
        selected <- input$facet2
      } else {
        selected <- ""
      }
      anyLeft <- length(available) > 0
      if ( entered(input$facet1) && anyLeft ) {
        selectInput(inputId = "facet2", label = "Also facet by:",
                    choices = c("", available),
                    selected = selected)
      }
    })

    
    output$layrows <- renderUI({
      if (!entered(input$facet1)) {
        return(NULL)
      }
      layout <- reactiveLayout()
      rows <- layout$rows
      customNumericInput(inputId = "layrows", label = "Rows in Layout",
                   min = 1, value = rows)
    })
    
    output$laycols <- renderUI({
      input$facet1
      input$facet2
      if (!entered(input$facet1)) {
        return(NULL)
      }
      layout <- reactiveLayout()
      if (is.null(layout)) {
        return(NULL)
      }
      cols <- layout$cols
      customNumericInput(inputId = "laycols", label = "Columns in Layout",
                   min = 1, value = cols)
    })
    
    output$layvarnames <- renderUI({
      if (!entered(input$facet1)) {
        return(NULL)
      }
      checkboxInput(inputId = "layvarnames", 
                    label = "Show Facet-Variable Names")
    })
    
    
    ## for "other" tab -------------------
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
      rv$code
    })
    
    output$stack <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      checkboxInput(inputId = "stack", label = "Stacked Bars", value = T)
    })
    
    output$horizontal <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      checkboxInput(inputId = "horizontal", label = "Horizontal Bars", value = T)
    })
    
    output$ownlabs <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      value <- ifelse(is.null(isolate(rv$customlabels)), FALSE, TRUE)
      checkboxInput(inputId = "ownlabs", 
                    label = paste0("Rename ",ifelse(input$horizontal,"y","x"),
                                   "-axis values?"), value = value)
    })
    
    observeEvent(input$ownlabs,{
      if (!is.null(input$ownlabs) && !input$ownlabs)
        rv$customlabels <- NULL
    })
    
    #keep custom lavels if the only thing you are oding is changing orientation
    observeEvent(input$horizontal,{
      rv$customlabels <- input$customlabels
    })
    
    # Remove custom names whenever grouping variable is changed:
    observe({
      input$group
      updateCheckboxInput(session, inputId = "ownlabs", value = FALSE)
      updateTextInput(session, inputId = "customlabels", value = NULL)
      updateNumericInput(session, inputId = "rotatelabs", value = 0)
      rv$customlabels <- NULL
    })
    
    output$rotatelabs <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      customNumericInput(inputId = "rotatelabs", 
                   label = paste0("Rotate ",ifelse(input$horizontal,"y","x"),
                                  "-axis values (degrees):"),
                   value = 0, step = 15)
    })
    
    output$customlabels <- renderUI({
      if (!reactiveVarCheck() || is.null(input$ownlabs) || !input$ownlabs) {
        return(NULL)
      }
      customTextInput(inputId = "customlabels", label = "Values (comma-separated)",
                value = rv$customlabels)
    })
    
    output$bw <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      checkboxInput(inputId = "bw", label = "Bl & Wh", width = "100px")
    })
    
    output$main <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      customTextInput(inputId = "main","Graph Title", value = "")
    })
    
    output$mainsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      customNumericInput(inputId = "mainsize","Graph Title Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$sub <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      customTextInput(inputId = "sub","Graph Sub-title", value = "")
    })
    
    output$subsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      customNumericInput(inputId = "subsize","Graph Sub-size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$xlab <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      customTextInput(inputId = "xlab","x-Label", value = "")
    })
    
    output$xlabsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      customNumericInput(inputId = "xlabsize","x-Label Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$ylab <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      customTextInput(inputId = "ylab","y-Label", value = "")
    })
    
    output$ylabsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      customNumericInput(inputId = "ylabsize","y-Label Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    
    ## Finish Up ----------------------
    #######################
    
    # Listen for Done.
    observeEvent(input$done, {
      
      # Get code to user:
      if (reactiveVarCheck()) {
        code <- rv$code
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
