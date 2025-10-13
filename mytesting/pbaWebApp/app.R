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
