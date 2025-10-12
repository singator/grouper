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

source('utils.R')

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
                          uiOutput("group_selection")
                          ),

                          column(width=4,
                          uiOutput("demographic_selection")
                          ),

                          column(width=4,
                          uiOutput("skill_selection")
                          ),

                        ),

                        fluidRow(
                          column(width=4,
                                 actionButton("verify_col", "Verify columns")),
                          column(width=6,
                                 textOutput("col_verified"))
                        ),

                        hr(),
                        h3("Step 2: Model parameters"),
                        p('Select the parameters for your model here, e.g. number
                          of topics, number of members, etc.'),

                        fluidRow(
                          column(width=4,
                                 numericInput("num_topics", "No. of topics:",
                                              1, min=1, step=1)),
                          column(width=4,
                                 numericInput("n_min", "Min. group size:",
                                              1, min=1, step=1)),
                          column(width=4,
                                 numericInput("n_max", "Max. group size:",
                                              2, min=1, step=1)),
                          column(width=4,
                                 numericInput("w_1_input", "Demographics weight (w1)",
                                              0.5, min=0, max=1, step=0.001)),
                          column(width=4, textOutput("w_2_text"))
                        ),

                        hr(),
                        h3("Step 3: Prepare model"),
                        p('Click on the button to prepare the model'),

                        fluidRow(
                          column(width=4,
                                 actionButton("prepare", "Prepare model")),
                          column(width=6,
                                 textOutput("model_prepared"))
                        ),

              ),
              tabPanel("Preference-based",
                        br(),
                        h3("Step 1: Student information input"),

                        p('Upload a csv or excel file that contains information on the
                          students in your course. There should be one row for each
                          student. There should also be a column containing the
                          groupings.'),
                        fluidRow(
                          column(width=4, fileInput("stud_info_pba", "Upload a file:",
                                              accept=c(".csv", ".xlsx")) ),
                          column(width=6, DTOutput("stud_info_preview_pba"))
                        )
              )
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
              completion. If you do not wish to set both criteria, just
              leave the one you do not wish to set as 0."),
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
                      ),
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

  w_2 <- reactive({
    1 - w_1()
  })
  w_1 <- reactive({
    input$w_1_input
  })

  output$w_2_text <- renderText({
    paste("Skill weight (w2): ", sprintf("%.3f", w_2()))
  })

  output$model_prepared <- renderText({
    m4()
    if(inherits(m4(), "character")){
      return(m4())
    }
    "Model prepared."
  })

  output$optimisation_output <- renderText({
    result()
    return("Model optimised!")
  })

  output$merged_output <- renderText({
    merged_df()
    return("Df merged.")
  })



  output$col_verified <- renderText({
    output_string <- verify_columns(input$group_var,
                                    input$demographic_vars,
                                    input$skill_var)
    output_string
  }, sep="\n") %>%
    bindEvent(input$verify_col)

  output$stud_info_preview <- renderDT({
    df <- input$stud_info
    req(df)
    stud_info_df()
    # NULL
  }, options=list(scrollX = TRUE))
  output$stud_info_preview_pba <- renderDT({
    df <- input$stud_info_pba
    req(df)
    stud_info_df_pba()
    # NULL
  }, options=list(scrollX = TRUE))

  output$group_selection <- renderUI({
    df <- input$stud_info
    req(df)
    col_names <- colnames(stud_info_df())
    selectizeInput("group_var", label="Group column:",
                   choices=col_names, multiple=FALSE)
  })

  output$demographic_selection <- renderUI({
    df <- input$stud_info
    req(df)
    col_names <- c(colnames(stud_info_df()), "No demographics")
    selectizeInput("demographic_vars", label="Demographic column(s):",
                   choices=col_names, multiple=TRUE)
  })

  output$skill_selection <- renderUI({
    df <- input$stud_info
    req(df)
    col_names <- c(colnames(stud_info_df()), "No skills")
    selectizeInput("skill_var", label="Skill column:",
                   choices=col_names, multiple=FALSE)
  })

  m4 <- reactive({
    verification <- verify_params(input$demographic_vars,
                                  input$skill_var,
                                  w_1(),
                                  w_2())
    #print(verification)
    if(verification != "Parameter weights specified ok.") {
      return(verification)
    }
    df_col_names <- colnames(stud_info_df())

    grouping_col_no <- match(input$group_var, df_col_names)
    if(input$skill_var == "No skills") {
      skills_col_no <- NULL
      demo_col_no <- match(input$demographic_vars, df_col_names)
      df_list <- extract_student_info(stud_info_df(), assignment = "diversity",
                                      self_formed_groups = grouping_col_no,
                                      demographic_cols = demo_col_no,
                                      skills = skills_col_no)
    } else if(input$demographic_vars == "No demographics") {
      demo_col_no <- NULL
      skills_col_no <- match(input$skill_var, df_col_names)
      df_list <- extract_student_info(stud_info_df(), assignment = "diversity",
                                      self_formed_groups = grouping_col_no,
                                      d_mat = matrix(0, NROW(stud_info_df()),
                                                     NCOL(stud_info_df())),
                                      skills = skills_col_no)
    } else {
      skills_col_no <- match(input$skill_var, df_col_names)
      demo_col_no <- match(input$demographic_vars, df_col_names)
      df_list <- extract_student_info(stud_info_df(), assignment = "diversity",
                                    self_formed_groups = grouping_col_no,
                                    demographic_cols = demo_col_no,
                                    skills = skills_col_no)
    }

    yaml_list <- list(n_topics = input$num_topics,
                      R = 1,
                      rmin=1, rmax=1,
                      nmin = matrix(input$n_min,
                                    nrow=input$num_topics,
                                    ncol=1, byrow=TRUE),
                      nmax = matrix(input$n_max,
                                    nrow=input$num_topics,
                                    ncol=1, byrow=TRUE))

    prepare_model(df_list, yaml_list, w1=w_1(), w2=w_2())
  }) %>%
    bindEvent(input$prepare)

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

    solve_model(m4(), with_ROI(solver="gurobi",
                               TimeLimit = time_limit,
                               IterationLimit = it_limit,
                               verbose=TRUE))
  }) %>%
    bindEvent(input$optimise)

  merged_df <- reactive({
    assign_groups(result(), "diversity", stud_info_df(), group_names=input$group_var)
  }) %>%
    bindEvent(input$merge)

  output$download_df <- downloadHandler(
    filename = "model_output.csv",
    content = function(file) {
      write.csv(merged_df(), file, row.names=FALSE)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
