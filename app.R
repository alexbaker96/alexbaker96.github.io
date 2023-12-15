#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for the calculator application
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .result {
        margin-top: 20px;
        font-size: 20px;
      }
      .shiny-output-error { color: red; }
      body { background-color:#4000ff; }
    "))
  ),
  titlePanel("Calculator and Graph"),
  mainPanel(
    #numeric inputs to take entry from user.
    numericInput("Num1", "Enter Number 1", 0),
    numericInput("Num2", "Enter Number 2", 0),
    #conditional panel to display output.
    conditionalPanel(
      condition = "input.last_btn == 14", 
      numericInput("last_btn", label = "last_btn", value = 0)
    ),
    #Rows containing action buttons to perform operations.
    fluidRow(
      column(4, actionButton("ADD", "ADD")),
      column(4, actionButton("SUB", "SUB")),
      column(4, actionButton("MUL", "MUL"))
    ),
    fluidRow(
      column(4, actionButton("DIV", "DIV")),
      column(4, actionButton("MOD", "MOD")),
      column(4, actionButton("SQRT", "SQRT"))
    ),
    fluidRow(
      column(4, actionButton("SIN", "SIN")),
      column(4, actionButton("COS", "COS")),
      column(4, actionButton("TAN", "TAN"))
    ),
    fluidRow(
      column(4, actionButton("INVERSE-SIN", "INV SIN")),
      column(4, actionButton("INVERSE-COS", "INV COS")),
      column(4, actionButton("INVERSE-TAN", "INV TAN"))
    ),
    #output result of operation
    verbatimTextOutput("Result"),
    textInput("mathFunction", "Enter Mathematical Function (e.g., sin(x))"),
    fluidRow(
      #action buttons for graphing.
      column(6, actionButton("Graph", "Graph")),
      column(6, actionButton("ClearGraph", "Clear Graph"))
    ),
    #where graph will be displayed
    plotOutput("graph")
  )
)

# Define server logic required to perform the operations of the calculator when specific button is used.
server <- function(input, output, session) {
  observeEvent(input$ADD, {
    updateNumericInput(session, "last_btn", value = 1)
  })
  observeEvent(input$SUB, {
    updateNumericInput(session, "last_btn", value = 2)
  })
  observeEvent(input$MUL, {
    updateNumericInput(session, "last_btn", value = 3)
  })
  observeEvent(input$DIV, {
    updateNumericInput(session, "last_btn", value = 4)
  })
  observeEvent(input$MOD, {
    updateNumericInput(session, "last_btn", value = 5)
  })
  observeEvent(input$SQRT, {
    updateNumericInput(session, "last_btn", value = 6)
  })
  observeEvent(input$SIN, {
    updateNumericInput(session, "last_btn", value = 7)
  })
  observeEvent(input$COS, {
    updateNumericInput(session, "last_btn", value = 8)
  })
  observeEvent(input$TAN, {
    updateNumericInput(session, "last_btn", value = 9)
  })
  observeEvent(input$INVERSE.SIN, {
    updateNumericInput(session, "last_btn", value = 10)
  })
  observeEvent(input$INVERSE.COS, {
    updateNumericInput(session, "last_btn", value = 11)
  })
  observeEvent(input$INVERSE.TAN, {
    updateNumericInput(session, "last_btn", value = 12)
  })
  
  observeEvent(input$Graph, {
    updateNumericInput(session, "last_btn", value = 13)
    updateNumericInput(session, "graphValue", value = input$mathFunction)
  })
  
  observeEvent(input$ClearGraph, {
    updateNumericInput(session, "graphValue", value = 0)
    updateNumericInput(session, "last_btn", value = 0)
  })
  #result of the operation is rendered.
  output$Result <- renderText({
    result <- switch(as.character(input$last_btn),
                     "1" = input$Num1 + input$Num2,
                     "2" = input$Num1 - input$Num2,
                     "3" = input$Num1 * input$Num2,
                     "4" = input$Num1 / input$Num2,
                     "5" = input$Num1 %% input$Num2,
                     "6" = sqrt(input$Num1),
                     "7" = sin(input$Num1),
                     "8" = cos(input$Num1),
                     "9" = tan(input$Num1),
                     "10" = asin(input$Num1),
                     "11" = acos(input$Num1),
                     "12" = atan(input$Num1),
                     "13" = eval(parse(text = input$mathFunction)))
    
    if (is.numeric(result)) {
      paste("Result: ", result)
    } else {
      paste("Result: ", input$last_btn)
    }
  })
  #graph rendering
  output$graph <- renderPlot({
    x <- seq(-10, 10, length.out = 100)
    
    result <- switch(as.character(input$last_btn),
                     "1" = input$Num1 + input$Num2,
                     "2" = input$Num1 - input$Num2,
                     "3" = input$Num1 * input$Num2,
                     "4" = input$Num1 / input$Num2,
                     "5" = input$Num1 %% input$Num2,
                     "6" = sqrt(input$Num1),
                     "7" = sin(input$Num1),
                     "8" = cos(input$Num1),
                     "9" = tan(input$Num1),
                     "10" = asin(input$Num1),
                     "11" = acos(input$Num1),
                     "12" = atan(input$Num1),
                     "13" = eval(parse(text = input$mathFunction)))
    
    if (is.numeric(result)) {
      plot(x, rep(result, 100), type = 'l', col = 'blue', main = 'Graph', xlab = 'X', ylab = 'Y')
    } else {
      plot(NA, xlim = c(-10, 10), ylim = c(0, 1), main = paste("Invalid operation for graph:", input$last_btn))
    }
  })
}

# Run the application
shinyApp(ui, server)

  