library(shiny)
library(bslib)
library(DT)
library(magrittr)
library(grouper)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

library(readxl)

ui <- page_navbar(
  nav_panel("Optimise",
            h2("Model preparation"),
            tabsetPanel(
              tabPanel("Diversity-based",
                        br(),
                        h3("Step 1: Student information input"),

                        p('Upload a csv or excel file that contains information on the
                          students in your course. There should be one row for each
                          student. There should also be a column containing the
                          groupings. For this diversity-based assignment, there
                          may also be columns corresponding to demographics, and
                          a single column corresponding to a skill factor.'),

                        fluidRow(
                          column(width=4, fileInput("stud_info", "Upload a file:",
                                              accept=c(".csv", ".xlsx")) ),
                          column(width=6, DTOutput("stud_info_preview"))
                        ),

                        fluidRow(
                          column(width=4,
                          "Select the column corresponding to the groups.",
                          uiOutput("group_selection")
                          ),

                          column(width=4,
                          "Select the column(s) corresponding to the demographic
                          variables:",
                          uiOutput("demographic_selection")
                          ),

                          column(width=4,
                          "Select the column corresponding to the skill:",
                          uiOutput("skill_selection")
                          ),

                        ),

                        fluidRow(
                          column(width=4, "Verify column selection"),
                          column(width=6, "Columns verified")
                        ),

                        hr(),
                        h3("Step 2: Model parameters"),
                        p('Select the parameters for your model here, e.g. number
                          of topics, number of members, etc.'),


                        actionButton("prepare", "Prepare model:")
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
            actionButton("merge", "Merge dataframes"),
            textOutput("merged"),
            ),
  nav_panel("Documentation",
            includeMarkdown("help.md")),
  title = "grouper",
  id = "main_page",
  theme = bs_theme(bootswatch = "minty")
)

server <- function(input, output, session) {

  stud_info_df <- reactive({
    fname <- input$stud_info$datapath
    f_ext <- tools::file_ext(fname)
    if(f_ext == "csv"){
      df <- read.csv(fname)
    } else if (f_ext == "xlsx"){
      df <- read_excel(fname)
    }
    df
  })

  output$stud_info_preview <- renderDT({
    df <- input$stud_info
    req(df)
    stud_info_df()
    # NULL
  }, options=list(scrollX = TRUE))

  output$group_selection <- renderUI({
    df <- input$stud_info
    req(df)
    col_names <- colnames(stud_info_df())
    selectizeInput("group_var", label="Column names:",
                   choices=col_names, multiple=FALSE)
  })

  output$demographic_selection <- renderUI({
    df <- input$stud_info
    req(df)
    col_names <- colnames(stud_info_df())
    selectizeInput("demographic_vars", label="Column names:",
                   choices=col_names, multiple=TRUE)
  })

  output$skill_selection <- renderUI({
    df <- input$stud_info
    req(df)
    col_names <- colnames(stud_info_df())
    selectizeInput("skill_var", label="Column names:",
                   choices=col_names, multiple=FALSE)
  })

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

  merged_df <- reactive({
    assign_groups(result(), "diversity", dba_gc_ex004,
                  group_names="self_groups")
  }) %>%
    bindEvent(input$merge)

  output$file1_contents <- renderPrint({print(input$stud_info)})

  output$model_prep <- renderText({
    m4()
    return("Model prepared.")
  })

  output$optimised <- renderText({
    result()
    return("Model optimised!")
  })

  output$merged <- renderText({
    merged_df()
    return("Df merged.")
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
