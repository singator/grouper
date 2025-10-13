#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(magrittr)

ui <- fluidPage(
  textInput('data', 'Enter a dataset from the "datasets" package', 'cars'),
  p('(E.g. "cars", "mtcars", "pressure", "faithful")'),
  hr(),
  tableOutput('tbl'),
  selectInput("test1", "Display", c("A", "B")),
  selectInput("test2", "Display", c("C", "D")),
  textOutput("output1"),
  textOutput("output2")

)

server <- function(input, output, session) {

  value <- reactiveVal(0)       # rv <- reactiveValues(value = 0)

  # observeEvent(input$test1, {
  #   newValue <- value() - 1     # newValue <- rv$value - 1
  #   value(newValue)             # rv$value <- newValue
  # })
  bindEvent(observe({
    newValue <- value() - 1     # newValue <- rv$value - 1
    value(newValue)             # rv$value <- newValue
  }), input$test1)
  # observeEvent(input$test1, {
  #   newValue <- value() - 1     # newValue <- rv$value - 1
  #   value(newValue)             # rv$value <- newValue
  # })

  # r <- reactiveValues(x=NULL, y=NULL)
  #
  # # x <- reactive({
  # #     return(input$test2)
  # # })
  # # x <- reactive({
  # #   return(input$test1)
  # # })
  # reactive({
  #   print("test1")
  # }) %>% bindEvent(input$test1)
  #
  # reactive({
  #   r$x <- input$test2
  #   print("test2")
  # }) %>% bindEvent(input$test2)

  output$output1 <- renderText({
    #print(r$x())
    print(value())
    value()
  })

  output$tbl <- renderTable({

    ## to require that the user types something, use: `req(input$data)`
    ## but better: require that input$data is valid and leave the last
    ## valid table up
    req(exists(input$data, "package:datasets", inherits = FALSE),
        cancelOutput = TRUE)

    head(get(input$data, "package:datasets", inherits = FALSE))
  })
}

shinyApp(ui, server)
