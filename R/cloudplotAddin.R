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
#' 2. Execute this addin and interactively build the plot.
#'
#' When you're happy with the plot, press Done.  The code for
#' the plot  will be be placed at the cursor position.
#'
#' @export
cloudplotAddin <- function() {

  # Get the document context.
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text <- context$selection[[1]]$text
  defaultData <- text
  
  lrTextInput <- function(inputId, label, value = "") {
    tagList(tags$label(label, `for` = inputId), 
            tags$input(id = inputId, 
                      type = "text", value = value,
                      class="lrTextInput form-control shiny-bound-input"))
  }
  
#   code2 <- HTML(
#     '<script>
# $(document).ready( function () {
#   $("img").wrap("<a class = \'plotzoom\' href = \'#\' target=\'_blank\'></a>");
#   $(".plotzoom").bind(\'mouseover\', function() {
#     this.href = this.firstElementChild.src;
#   });
# });
#     </script>'
#   )

  # Generate UI for the gadget -------------------
  ui <- miniPage(
    #code,
    includeScript(system.file("js/custom.js", package = "addinplots")),
    gadgetTitleBar("Cloudplot Code-Helper"),
    miniContentPanel(
    sidebarLayout(
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
        ),
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
            column(width = 3, uiOutput("pch")),
            column(width = 3, uiOutput("bw")),
            column(width=3, uiOutput("zoom"))
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
          ,
          fluidRow(
            column(width = 7, uiOutput("ylab")),
            column(width = 2, uiOutput("ylabsize"))
          )
          ,
          fluidRow(
            column(width = 7, uiOutput("zlab")),
            column(width = 2, uiOutput("zlabsize"))
          )
        )
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
      shingle1 = FALSE,
      shingle2 = FALSE,
      code = NULL
    )


## Reactive functions ----------------------
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
      xvar <- input$xVar
      yvar <- input$yVar
      zvar <- input$zVar
      xcheck <- entered(xvar)
      ycheck <- entered(yvar)
      zcheck <- entered(zvar)
      return(xcheck && ycheck && zcheck)
    })
    
    # our code-maker
    observe({
      xvar <- input$xVar
      yvar <- input$yVar
      zvar <- input$zVar
      if ( !reactiveVarCheck() ) {
        return("No code to show yet!")
      }
      
      code <- ""
      
      # lead-up shingle-maker(s)
      if (entered(input$f1name)) {
        code <- paste0(code,input$f1name," <- equal.count(",input$data,
                       "$",input$facet1,
                       ", number = ",input$f1number,", overlap = ",input$f1overlap,
                       ")\n")
      }
      
      if (entered(input$f2name)) {
        code <- paste0(code,input$f2name,"<- equal.count(",input$data,
                       "$",input$facet2,
                       ", number = ",input$f2number,", overlap = ",input$f2overlap,
                       ")\n")
      }
      
      # function and formula:
      code <- paste0(code,"cloud(",zvar," ~ ",xvar," * ",yvar)
      if (entered(input$facet1) && !rv$shingle1) {
        code <- paste0(code, " | ", input$facet1)
      }
      if (entered(input$facet1) && rv$shingle1 && entered(input$f1name)) {
        code <- paste0(code, " | ", input$f1name)
      }
      if (entered(input$facet2) && !rv$shingle2) {
        code <- paste0(code, " * ", input$facet2)
      }
      if (entered(input$facet2) && rv$shingle2 && entered(input$f2name)) {
        code <- paste0(code, " * ", input$f2name)
      }
      
      # the data argument
      code <- paste0(code, ",\n\tdata = ",input$data)
      
      # layout information
      if (entered(input$facet1) && !is.null(input$layrows) && !is.null(input$laycols)) {
        code <- paste0(code, ",\n\tlayout = c(",input$laycols,",",input$layrows,")")
      }
      
      # facet variable names?
      if (!is.null(input$layvarnames) && input$layvarnames) {
        code <- paste0(code, ",\n\tstrip = strip.custom(strip.names = c(TRUE, TRUE))")
      }
      
      # the screen argument
      xscr <- input$xScreen; yscr <- input$yScreen; zscr <- input$zScreen
      
      # if I were to use default lattice ordering of x,y,z rotations:
      # atDefaultVals <- xscr == -60 && yscr == 0 && zscr == 40
      # if ( !atDefaultVals) ) {
      # code <- paste0(code, ",\n\tscreen = list(z = -",zscr,",\n\t\t\tx = ",
      #                xscr,",\n\t\t\ty = ",yscr,")")
      
      # But I think this is easier for the new user:
        code <- paste0(code, ",\n\tscreen = list(x = -",xscr,",\n\t\t\ty = ",
                      yscr,",\n\t\t\tz = ",zscr,")")
     
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
      
      # pch argument
      if (exists_as_numeric(input$pch) && input$pch !=1 && !input$bw) {
        code <- paste0(code,",\n\tpch = ",input$pch)
      }
      
      # theme argument
      wantBW <- !is.null(input$bw) && input$bw
      if ( wantBW ) {
        code <- paste0(code, 
                       ",\n\tpar.settings = canonical.theme(color=FALSE)")
      }
      
      # zoom argument
      wantZoom <- exists_as_numeric(input$zoom) && input$zoom > 0 && input$zoom != 1
      if ( wantZoom ) {
        code <- paste0(code, ",\n\tzoom = ",input$zoom)
      }
      
      # main, xlab, ylab. zlab
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
      
      if (entered(input$ylab) && exists_as_numeric(input$ylabsize) &&  input$ylabsize == 1) {
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
      
      if (entered(input$zlab) && exists_as_numeric(input$zlabsize) &&  input$zlabsize == 1) {
        code <- paste0(code, ",\n\tzlab = \"",input$zlab,"\"")
      }
      
      if (!entered(input$zlab) && exists_as_numeric(input$zlabsize) && input$zlabsize !=1) {
        code <- paste0(code, ",\n\tzlab = list(cex = ",input$zlabsize,")")
      }
      
      if (entered(input$zlab) && exists_as_numeric(input$zlabsize) && input$zlabsize != 1) {
        code <- paste0(code, ",\n\tzlab = list(label=\"",input$zlab, "\"",
                       ",\n\t\tcex = ",input$zlabsize,
                       ")")
      }
      
      # add closing paren:
      code <- paste0(code,")")
      rv$code <- code
    })
    
    # our plot-maker
    makeplot <- reactive({
      data <- reactiveData()
      if (isErrorMessage(data))
        return(NULL)
      
      if (!reactiveVarCheck()) {
        return(NULL)
      } else {
        command <- rv$code
        eval(parse(text = command))
      }
    })
    
    # compute a reasonable layout
    reactiveLayout <- reactive({
      getNumberLevels <- function(varName) {
        if (rv$shingle1 && varName == input$f1name) {
          return(input$f1number)
        }
        if (rv$shingle2 && varName == input$f2name) {
          return(input$f2number)
        }
        # if we get this far, the facetting variable is a factor
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
        if (rv$shingle1 && !entered(input$f1name)) {
          return(NULL)
        }
        var1 <- ifelse(rv$shingle1, input$f1name, f1)
        rows <- getNumberLevels(var1)
        cols <- 1
        res <- list(rows = rows, cols = cols)
        return(res)
      }
      if (entered(f1) && entered(f2)) {
        if (rv$shingle2 && !entered(input$f2name)) {
          return(NULL)
        }
        var1 <- ifelse(rv$shingle1, input$f1name, f1)
        var2 <- ifelse(rv$shingle2, input$f2name, f2)
        rows <- getNumberLevels(var1)
        cols <- getNumberLevels(var2)
        res <- list(rows = rows, cols = cols)
        return(res)
      }
      # safety values
      res <- NULL
      return(res)
      
    })
    

## Primary Variables -----------------
############################
    
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
    

## For groups tab ---------------------
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
      lrTextInput(inputId = "keytitle", label = "Legend title:",
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
    


## for facets tab -------------------
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
      rv$code
    })
    
    output$facet1 <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      data <- reactiveData()
      facNumVars <- find_facnum_vars(data)
      if (length(facNumVars) == 0) {
        return(NULL)
      }
      available <- facNumVars
      
      available <- setdiff(available, c(input$xVar, input$yVar, input$zVar))
      
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
      facNumVars <- find_facnum_vars(data)
      if (length(facNumVars) == 0) {
        return(NULL)
      }
      available <- facNumVars
      available <- setdiff(available, c(input$xVar, input$yVar, input$zVar))
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
    
    output$f1name <- renderUI({
      if (!entered(input$facet1)) {
        rv$shingle1 <- FALSE
        return(NULL)
      }
      data <- reactiveData()
      var <- get(input$facet1, envir = as.environment(data))
      if (is.factor(var)) {
        rv$shingle1 <- FALSE
        return(NULL)
      }
      rv$shingle1 <- TRUE
      lrTextInput(inputId = "f1name", label = "Shingle Name",
                value = suggestedName(input$facet1))
    })
    
    output$f1number <- renderUI({
      if (!entered(input$facet1)) {
        rv$shingle1 <- FALSE
        return(NULL)
      }
      data <- reactiveData()
      var <- get(input$facet1, envir = as.environment(data))
      if (is.factor(var)) {
        rv$shingle1 <- FALSE
        return(NULL)
      }
      rv$shingle1 <- TRUE
      numericInput(inputId = "f1number", label = "How Many?",
                   min = 2, value = 2)
    })
    
    output$f1overlap <- renderUI({
      if (!entered(input$facet1)) {
        rv$shingle1 <- FALSE
        return(NULL)
      }
      data <- reactiveData()
      var <- get(input$facet1, envir = as.environment(data))
      if (is.factor(var)) {
        rv$shingle1 <- FALSE
        return(NULL)
      }
      rv$shingle1 <- TRUE
      numericInput(inputId = "f1overlap", label = "Overlap",
                   min = 0, value = 0.1)
    })
    
    output$f2name <- renderUI({
      if (!entered(input$facet2)) {
        rv$shingle2 <- FALSE
        return(NULL)
      }
      data <- reactiveData()
      var <- get(input$facet2, envir = as.environment(data))
      if (is.factor(var)) {
        rv$shingle2 <- FALSE
        return(NULL)
      }
      rv$shingle2 <- TRUE
      lrTextInput(inputId = "f2name", label = "Shingle 2 Name",
                value = suggestedName(input$facet2))
    })
    
    output$f2number <- renderUI({
      if (!entered(input$facet2)) {
        rv$shingle2 <- FALSE
        return(NULL)
      }
      data <- reactiveData()
      var <- get(input$facet2, envir = as.environment(data))
      if (is.factor(var)) {
        rv$shingle2 <- FALSE
        return(NULL)
      }
      rv$shingle2 <- TRUE
      numericInput(inputId = "f2number", label = "How Many?",
                   min = 2, value = 2)
    })
    
    output$f2overlap <- renderUI({
      if (!entered(input$facet2)) {
        rv$shingle2 <- FALSE
        return(NULL)
      }
      data <- reactiveData()
      var <- get(input$facet2, envir = as.environment(data))
      if (is.factor(var)) {
        rv$shingle2 <- FALSE
        return(NULL)
      }
      rv$shingle2 <- TRUE
      numericInput(inputId = "f2overlap", label = "Overlap",
                   min = 0, value = 0.1)
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
      input$f1name
      input$f2name
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
    

## for "other" tab -----------------
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
    
    output$main <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      lrTextInput(inputId = "main","Graph Title", value = "")
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
      lrTextInput(inputId = "sub","Graph Sub-title", value = "")
    })
    
    output$subsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "subsize","Graph Sub-size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$xlab <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      lrTextInput(inputId = "xlab","x-Label", value = "")
    })
    
    output$xlabsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "xlabsize","x-Label Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$ylab <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      lrTextInput(inputId = "ylab","y-Label", value = "")
    })
    
    output$ylabsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "ylabsize","y-Label Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$zlab <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      lrTextInput(inputId = "zlab","z-Label", value = "")
    })
    
    output$zlabsize <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "zlabsize","z-Label Size",
                   min = 0, max = 4, value = 1, step = 0.1)
    })
    
    output$pch <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      if (input$bw) {
        return(NULL)
      }
      numericInput(inputId = "pch","Point Type", 
                   min = 1, max = 25, step =1, value = 1, width = "100px")
    })
    
    output$bw <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      checkboxInput(inputId = "bw", label = "Bl & Wh", width = "100px")
    })
    
    output$zoom <- renderUI({
      if (!reactiveVarCheck()) {
        return(NULL)
      }
      numericInput(inputId = "zoom","Zoom In/Out", 
                   min = 0, step = 0.1, value = 1, width = "100px")
    })

## Finish Up ----------------
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
