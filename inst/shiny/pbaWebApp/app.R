library(shiny)
library(bslib)
library(DT)
library(magrittr)
library(grouper)
library(ompr)
library(ompr.roi)
#library(ROI.plugin.glpk)
#library(ROI.plugin.gurobi)
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
    column(width=4, numericInput("iteration_limit", "Iteration limit", 100, min=0, step=1)),
    column(width=4, selectizeInput("solver", "Select solver", choices=c("glpk", "gurobi")))
    ),
  fluidRow( column(width=4, actionButton("optimise", "Optimise")),
            column(width=6, textOutput("optimisation_output"))
            ),
  hr(),
  h3("Step 5: Merge with original data"),
  fluidRow( column(width=4, actionButton("merge", "Merge dataframes")),
            column(width=4, textOutput("merged_output")),
            column(width=4, downloadLink("download_df", "Download"))
            ),
  theme = bs_theme(bootswatch = "minty")
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
    paste0("Preference matrix read in with ",
           NCOL(stud_pref_mat_pba()), " topics.")
  })

  yaml_list <- reactive({
    n_topics <- NCOL(stud_pref_mat_pba())
    list(n_topics = n_topics,
         B = 1,
         R = input$num_reps,
         rmin=input$num_reps, rmax=input$num_reps,
         nmin = matrix(input$n_min,
                       nrow=n_topics*input$num_reps,
                       ncol=1, byrow=TRUE),
         nmax = matrix(input$n_max,
                       nrow=n_topics*input$num_reps,
                       ncol=1, byrow=TRUE))
  })

  m4 <- reactive({
    df_col_names <- colnames(stud_info_df_pba())
    grouping_col_no <- match(input$group_var, df_col_names)

    df_list <- extract_student_info(stud_info_df_pba(), "preference",
                                    self_formed_groups = grouping_col_no,
                                    pref_mat = stud_pref_mat_pba())

    #browser()
   #print(yaml_list)

    prepare_model(df_list, yaml_list(), "preference")
    #return(1)
  }) %>%
    bindEvent(input$prepare)

  output$model_prepared <- renderText({
    m4()
    if(inherits(m4(), "character")){
      return(m4())
    }
    "Model prepared."
  })

  result <- reactive({
    if(input$time_limit == 0){
      time_limit = Inf
    } else {
      time_limit = input$time_limit
    }

    if(input$iteration_limit == 0){
      it_limit = Inf
    } else {
      it_limit = input$iteration_limit
    }

    if(input$solver == "glpk") {
      require(ROI.plugin.glpk)
      solve_model(m4(), with_ROI(solver=input$solver,
                                 verbose=TRUE))
    } else if(input$solver == "gurobi") {
      require(ROI.plugin.gurobi)
      solve_model(m4(), with_ROI(solver=input$solver,
                                 TimeLimit = time_limit,
                                 IterationLimit = it_limit,
                                 verbose=TRUE))
    }

  }) %>%
    bindEvent(input$optimise)

  output$optimisation_output <- renderText({
    result()
    return(paste("Model optimised! Status: ", result()$status, ".", sep=""))
  })

  merged_df <- reactive({
    assign_groups(result(), "preference",
                  dframe=stud_info_df_pba(),
                  yaml_list(),
                  group_names=input$group_var)
  }) %>%
    bindEvent(input$merge)

  output$merged_output <- renderText({
    merged_df()
    return("Df merged.")
  })

  output$download_df <- downloadHandler(
    filename = "model_output.csv",
    content = function(file) {
      write.csv(merged_df(), file, row.names=FALSE)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
