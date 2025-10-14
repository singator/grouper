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
library(DT)
library(magrittr)
library(grouper)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(ROI.plugin.gurobi)
library(readxl)

# Define UI for application that draws a histogram
ui <- fluidPage(
  h3("Step 1: Student information input"),
  p('Upload a csv or excel file that contains information on the
    students in your course. There should be one row for each
    student. There should also be a column containing the
    self-formed groupings.'),
  fluidRow(
    column(width=4, fileInput("stud_info_pba", "Upload a file:",
                        accept=c(".csv", ".xlsx")) ),
    column(width=6, DTOutput("stud_info_preview_pba"))
  ),
  fluidRow(
    column(width=4, uiOutput("group_selection_pba"))
  ),
  p('Upload a csv or excel file that contains information on the
    preference of each self-formed group for each topic. There should
    be one row for each self-formed group, and one column for each topic.
    The values in each topic should be nonnegative values, indicating the
    preference that a group has for a particular topic. Larger values indicate
    larger preference.'),
  fluidRow(
    column(width=4, fileInput("stud_pref_pba", "Upload a file:",
                        accept=c(".csv", ".xlsx")) ),
    column(width=4, textOutput("stud_pref_message_pba"))
  ),
  fluidRow(
    column(width=4, uiOutput("group_selection"))
  ),
  hr(),
  h3("Step 2: Model parameters"),
  p('Select the parameters for your model here, e.g. number of topics,
    number of members, etc.'),
  fluidRow(
    column(width=4,
           numericInput("num_reps", "No. of repetition of each topic:", 1, min=1, step=1)),
    column(width=4,
           numericInput("n_min", "Min. group size:", 1, min=1, step=1)),
    column(width=4,
           numericInput("n_max", "Max. group size:", 2, min=1, step=1))
  ),
  hr(),
  h3("Step 3: Prepare model"),
  p('Click on the button to prepare the model'),
  fluidRow(
    column(width=4,  actionButton("prepare", "Prepare model")),
    column(width=6,  textOutput("model_prepared"))
  ),
  hr(),
  h3("Step 4: Optimisation termination criteria"),
  p("Enter the termination criteria you wish to set, then click optimise.
     To learn more about the criteria below, please take a look at the
     following link:"),
  a("Gurobi optimisation parameters", href="https://docs.gurobi.com/projects/optimizer/en/current/concepts/parameters/groups.html#paramgrouptermination"),
  p("As long as one of the criteria below is reached, the optimisation will terminate.
     It is good to set one of the above during the initial runs, just to make
     sure that the model is running properly before allowing it to run to
     completion. If you do not wish to set both criteria, just leave the one you
     do not wish to set as 0."),
  fluidRow(
    column(width=4, numericInput("time_limit", "Time limit (sec)", 60, min=0, step=0.1)),
    column(width=4, numericInput("iteration_limit", "Iteration limit", 100, min=0, step=1))
    ),
  fluidRow( column(width=4, actionButton("optimise", "Optimise")),
            column(width=6, textOutput("optimisation_output"))
            ),
  hr(),
  h3("Step 5: Merge with original data"),
  fluidRow( column(width=4, actionButton("merge", "Merge dataframes")),
            column(width=4, textOutput("merged_output")),
            column(width=4, downloadLink("download_df", "Download"))
            )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## Step 1: PBA
  stud_info_df_pba <- reactive({
    fname <- input$stud_info_pba$datapath
    f_ext <- tools::file_ext(fname)
    if(f_ext == "csv"){
      df <- read.csv(fname)
    } else if (f_ext == "xlsx"){
      df <- read_excel(fname)
    }
    df
  })
  stud_pref_mat_pba <- reactive({
    fname <- input$stud_pref_pba$datapath
    f_ext <- tools::file_ext(fname)
    if(f_ext == "csv"){
      df <- read.csv(fname)
    } else if (f_ext == "xlsx"){
      df <- read_excel(fname)
    }
    as.matrix(df)
  })

  output$stud_info_preview_pba <- renderDT({
    df <- input$stud_info_pba
    req(df)
    stud_info_df_pba()
    # NULL
  }, options=list(scrollX = TRUE))


  output$group_selection_pba <- renderUI({
    df <- input$stud_info_pba
    req(df)
    col_names <- colnames(stud_info_df_pba())
    selectizeInput("group_var", label="Group column:",
                   choices=col_names, multiple=FALSE)
  })

  output$stud_pref_message_pba <- renderText({
    mat1 <- input$stud_pref_pba
    req(mat1)
    "Preference matrix read in"
  })

}

# Run the application
shinyApp(ui = ui, server = server)
