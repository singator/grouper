library(shiny)
library(bslib)
library(magrittr)
library(grouper)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

ui <- page_navbar(
  nav_panel("Optimise",
            h2("Model preparation"),
            tabsetPanel(
              tabPanel("Diversity-based",
                       fluidRow(
                        h3("Student information input"),
                        fileInput("stud_info", "Upload a file:"),
                        actionButton("prepare", "Prepare model:")
                       ),
              ),
              tabPanel("Preference-based", "Placeholder")
            ),
            hr(),
            p("This is a new paragraph."),
            actionButton("optimise", "Optimise"),
            h3("Optimisation termination criteria"),
            verbatimTextOutput("file1_contents"),
            textOutput("model_prep"),
            textOutput("optimised"),
            ),
  nav_panel("Documentation",
            includeMarkdown("help.md")),
  title = "grouper",
  id = "main_page",
  theme = bs_theme(bootswatch = "minty")
)

server <- function(input, output) {
  m4 <- reactive({
    df_ex004_list <- extract_student_info(dba_gc_ex004,
                                          skills = 2,
                                          self_formed_groups = 3,
                                          d_mat=matrix(0, 5, 5))
    yaml_ex004_list <- extract_params_yaml(system.file("extdata",
                                                       "dba_params_ex004.yml",
                                                       package = "grouper"),
                                           "diversity")
    prepare_model(df_ex004_list, yaml_ex004_list, w1=0.0, w2=1.0)
  }) %>%
    bindEvent(input$prepare)

  result <- reactive({
    solve_model(m4(), with_ROI(solver="glpk", verbose=TRUE))
  }) %>%
    bindEvent(input$optimise)

  output$file1_contents <- renderPrint({print(input$stud_info)})

  output$model_prep <- renderText({
    m4()
    return("Model prepared.")
  })

  output$optimised <- renderText({
    result()
    return("Model optimised!")
  })

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
