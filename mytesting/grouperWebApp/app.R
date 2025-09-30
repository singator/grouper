#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

ui <- page_navbar(
  nav_panel("Optimise",
            h2("Model preparation"),
            fluidRow(
            tabsetPanel(
              tabPanel("Diversity-based",
                       fluidRow(
                        h3("Student information input"),
                        br(),
                        h3("Model parameters")
                       ),
              ),
              tabPanel("Preference-based", "Placeholder")
            )
            ),
            hr(),
            p("This is a new paragraph."),
            h3("Optimisation termination criteria")
            ),
  nav_panel("Documentation",
            includeMarkdown("help.md")),
  title = "grouper",
  id = "main_page",
  theme = bs_theme(bootswatch = "minty")
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)

# # Define UI for application that draws a histogram
# ui <- fluidPage(
#
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
#
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
#
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
#
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
#
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }

# Run the application
shinyApp(ui = ui, server = server)
