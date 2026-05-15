library(shiny)
library(bslib)
library(DT)
library(magrittr)
library(dplyr)
library(ggplot2)
library(readxl)
library(grouper)
library(ompr)
library(ompr.roi)

app_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(app_dir, "../dbaWebApp/utils.R"))
source(file.path(app_dir, "../phdWebApp/utils.R"))

global_styles <- "
:root {
  --brand-deep: #0f4c81;
  --brand-mid: #2b6cb0;
  --brand-light: #7fb3d5;
  --ink-900: #123a63;
  --ink-700: #355d80;
  --surface: #ffffff;
  --border: #d9e2ec;
  --accent: #e66100;
  --accent-dark: #c95300;
}

body {
  background: linear-gradient(180deg, #f4f8fc 0%, #edf4fa 50%, #f4f8fc 100%);
}

.navbar {
  border-bottom: 1px solid rgba(15, 76, 129, 0.2);
  box-shadow: 0 8px 24px rgba(18, 67, 115, 0.08);
}

.navbar-brand {
  font-weight: 700;
  letter-spacing: 0.3px;
}

.hero-banner {
  background: linear-gradient(115deg, var(--brand-deep) 0%, var(--brand-mid) 52%, var(--brand-light) 100%);
  color: #ffffff;
  border-radius: 16px;
  padding: 22px 26px;
  margin: 18px 0 14px;
  box-shadow: 0 12px 24px rgba(18, 67, 115, 0.18);
}

.hero-banner h1 {
  font-size: 32px;
  margin: 0 0 8px 0;
  letter-spacing: 0.2px;
}

.hero-banner p {
  font-size: 16px;
  margin: 0;
  opacity: 0.95;
}

.module-intro {
  background: linear-gradient(120deg, rgba(15, 76, 129, 0.1) 0%, rgba(127, 179, 213, 0.2) 100%);
  border: 1px solid rgba(15, 76, 129, 0.18);
  border-radius: 14px;
  padding: 14px 18px;
  margin: 16px 0 12px;
}

.module-intro h2 {
  margin: 0 0 6px 0;
  color: var(--ink-900);
  font-size: 24px;
}

.module-intro p {
  margin: 0;
}

.landing-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
  gap: 14px;
  margin-top: 10px;
}

.model-card {
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 14px;
  padding: 16px;
  box-shadow: 0 8px 20px rgba(15, 76, 129, 0.06);
  display: flex;
  flex-direction: column;
  gap: 10px;
  min-height: 230px;
}

.model-card h3 {
  margin: 0;
  color: var(--ink-900);
}

.model-card p {
  margin: 0;
  color: #35506b;
  flex-grow: 1;
}

.model-chip {
  display: inline-block;
  font-size: 12px;
  letter-spacing: 0.2px;
  font-weight: 600;
  color: var(--brand-deep);
  background: rgba(15, 76, 129, 0.09);
  border: 1px solid rgba(15, 76, 129, 0.2);
  border-radius: 999px;
  padding: 3px 9px;
}

.panel-card {
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 14px;
  padding: 14px 16px;
  margin-bottom: 12px;
  box-shadow: 0 8px 20px rgba(15, 76, 129, 0.06);
}

.panel-card h3 {
  margin-top: 0;
  margin-bottom: 10px;
  color: var(--ink-900);
}

.hint {
  color: var(--ink-700);
  font-size: 14px;
}

.status-ok {
  color: #0b7d44;
  font-weight: 600;
}

.status-warn {
  color: #a04a00;
  font-weight: 600;
}

.btn-accent {
  background-color: var(--accent);
  border-color: var(--accent-dark);
  color: #ffffff;
}

.btn-accent:hover,
.btn-accent:focus,
.btn-accent:active {
  background-color: var(--accent-dark);
  border-color: #b54a00;
  color: #ffffff;
}

.btn-open {
  width: 100%;
}

@media (max-width: 767px) {
  .hero-banner {
    padding: 18px 16px;
  }

  .hero-banner h1 {
    font-size: 27px;
  }
}
"

compact_header <- function(title, subtitle) {
  div(
    class = "module-intro",
    h2(title),
    p(class = "hint", subtitle)
  )
}

step_card <- function(title, ...) {
  div(
    class = "panel-card",
    h3(title),
    ...
  )
}

home_ui <- function() {
  tagList(
    div(
      class = "hero-banner",
      h1("grouper"),
      p("A single workspace for diversity-based, preference-based, and PhD workload allocation models.")
    ),
    step_card(
      "Choose A Model",
      p(class = "hint", "Start from one model card below."),
      div(
        class = "landing-grid",
        div(
          class = "model-card",
          span(class = "model-chip", "DBA"),
          h3("Diversity-Based Assignment"),
          p("Optimise team composition by balancing diversity and optional skill factors from uploaded student attributes."),
          actionButton("open_dba", "Open DBA", class = "btn btn-accent btn-open")
        ),
        div(
          class = "model-card",
          span(class = "model-chip", "PBA"),
          h3("Preference-Based Assignment"),
          p("Assign self-formed groups to topics by maximising expressed topic preference with configurable repetition and size bounds."),
          actionButton("open_pba", "Open PBA", class = "btn btn-accent btn-open")
        ),
        div(
          class = "model-card",
          span(class = "model-chip", "PhD"),
          h3("PhD Workload Allocation"),
          p("Allocate TA/GR/E workload under demand and policy constraints with validation, diagnostics, and reusable outputs."),
          actionButton("open_phd", "Open PhD", class = "btn btn-accent btn-open")
        )
      )
    ),
    step_card(
      "Quick Notes",
      tags$ul(
        tags$li("Use Gurobi for best performance where available."),
        tags$li("PhD tab includes a downloadable semester template."),
        tags$li("All outputs remain compatible with existing downstream usage.")
      )
    )
  )
}

dba_ui <- function(id) {
  ns <- NS(id)

  tagList(
    compact_header(
      "Diversity-Based Assignment (DBA)",
      "Upload student data, configure diversity parameters, optimise, and merge assignments back to the original dataframe."
    ),
    step_card(
      "Step 1: Student Information Input",
      p(
        "Upload a csv or excel file that contains information on the students in your
        course. There should be one row for each student. There should also be a
        column containing the groupings. For this diversity-based assignment, there
        may also be columns corresponding to demographics, and a single column
        corresponding to a skill factor."
      ),
      fluidRow(
        column(
          width = 4,
          fileInput(ns("stud_info"), "Upload a file:", accept = c(".csv", ".xlsx"))
        ),
        column(width = 6, DTOutput(ns("stud_info_preview")))
      ),
      fluidRow(
        column(width = 4, uiOutput(ns("group_selection"))),
        column(width = 4, uiOutput(ns("demographic_selection"))),
        column(width = 4, uiOutput(ns("skill_selection")))
      ),
      fluidRow(
        column(width = 4, actionButton(ns("verify_col"), "Verify columns", class = "btn btn-accent")),
        column(width = 6, textOutput(ns("col_verified")))
      )
    ),
    step_card(
      "Step 2: Model Parameters",
      p("Select the parameters for your model here, e.g. number of topics, number of members, etc."),
      fluidRow(
        column(width = 4, numericInput(ns("num_topics"), "No. of topics:", 1, min = 1, step = 1)),
        column(width = 4, numericInput(ns("n_min"), "Min. group size:", 1, min = 1, step = 1)),
        column(width = 4, numericInput(ns("n_max"), "Max. group size:", 2, min = 1, step = 1)),
        column(width = 4, numericInput(ns("w_1_input"), "Demographics weight (w1)", 0.5, min = 0, max = 1, step = 0.001)),
        column(width = 4, textOutput(ns("w_2_text")))
      )
    ),
    step_card(
      "Step 3: Prepare Model",
      p("Click on the button to prepare the model."),
      fluidRow(
        column(width = 4, actionButton(ns("prepare"), "Prepare model", class = "btn btn-accent")),
        column(width = 6, textOutput(ns("model_prepared")))
      )
    ),
    step_card(
      "Step 4: Optimisation Termination Criteria",
      p(
        "Enter the termination criteria you wish to set, then click optimise.
        To learn more about the criteria below, please take a look at the following link:"
      ),
      a(
        "Gurobi optimisation parameters",
        href = "https://docs.gurobi.com/projects/optimizer/en/current/concepts/parameters/groups.html#paramgrouptermination"
      ),
      p(
        "As long as one of the criteria below is reached, the optimisation will terminate.
        It is good to set one of the above during the initial runs, just to make
        sure that the model is running properly before allowing it to run to
        completion. If you do not wish to set both criteria, just leave the one you
        do not wish to set as 0."
      ),
      fluidRow(
        column(width = 4, numericInput(ns("time_limit"), "Time limit (sec)", 60, min = 0, step = 0.1)),
        column(width = 4, numericInput(ns("iteration_limit"), "Iteration limit", 100, min = 0, step = 1)),
        column(width = 4, selectizeInput(ns("solver"), "Select solver", choices = c("glpk", "gurobi")))
      ),
      fluidRow(
        column(width = 4, actionButton(ns("optimise"), "Optimise", class = "btn btn-accent")),
        column(width = 6, textOutput(ns("optimisation_output")))
      )
    ),
    step_card(
      "Step 5: Merge With Original Data",
      fluidRow(
        column(width = 4, actionButton(ns("merge"), "Merge dataframes", class = "btn btn-accent")),
        column(width = 4, textOutput(ns("merged_output"))),
        column(width = 4, downloadLink(ns("download_df"), "Download"))
      )
    )
  )
}

dba_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    stud_info_df <- reactive({
      fname <- input$stud_info$datapath
      f_ext <- tools::file_ext(fname)
      if (f_ext == "csv") {
        df <- read.csv(fname)
      } else if (f_ext == "xlsx") {
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

    output$col_verified <- renderText({
      output_string <- verify_columns(input$group_var,
                                      input$demographic_vars,
                                      input$skill_var)
      output_string
    }, sep = "\n") %>%
      bindEvent(input$verify_col)

    output$stud_info_preview <- renderDT({
      df <- input$stud_info
      req(df)
      stud_info_df()
    }, options = list(scrollX = TRUE))

    output$group_selection <- renderUI({
      df <- input$stud_info
      req(df)
      col_names <- colnames(stud_info_df())
      selectizeInput(ns("group_var"), label = "Group column:",
                     choices = col_names, multiple = FALSE)
    })

    output$demographic_selection <- renderUI({
      df <- input$stud_info
      req(df)
      col_names <- c(colnames(stud_info_df()), "No demographics")
      selectizeInput(ns("demographic_vars"), label = "Demographic column(s):",
                     choices = col_names, multiple = TRUE)
    })

    output$skill_selection <- renderUI({
      df <- input$stud_info
      req(df)
      col_names <- c(colnames(stud_info_df()), "No skills")
      selectizeInput(ns("skill_var"), label = "Skill column:",
                     choices = col_names, multiple = FALSE)
    })

    m4 <- reactive({
      verification <- verify_params(input$demographic_vars,
                                    input$skill_var,
                                    w_1(),
                                    w_2())
      if (verification != "Parameter weights specified ok.") {
        return(verification)
      }
      df_col_names <- colnames(stud_info_df())

      grouping_col_no <- match(input$group_var, df_col_names)
      if (input$skill_var == "No skills") {
        skills_col_no <- NULL
        demo_col_no <- match(input$demographic_vars, df_col_names)
        df_list <- extract_student_info(stud_info_df(), assignment = "diversity",
                                        self_formed_groups = grouping_col_no,
                                        demographic_cols = demo_col_no,
                                        skills = skills_col_no)
      } else if ((length(input$demographic_vars) == 1) && (input$demographic_vars == "No demographics")) {
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

      yaml_list <- list(
        n_topics = input$num_topics,
        R = 1,
        rmin = 1, rmax = 1,
        nmin = matrix(input$n_min,
                      nrow = input$num_topics,
                      ncol = 1, byrow = TRUE),
        nmax = matrix(input$n_max,
                      nrow = input$num_topics,
                      ncol = 1, byrow = TRUE)
      )

      prepare_model(df_list, yaml_list, w1 = w_1(), w2 = w_2())
    }) %>%
      bindEvent(input$prepare)

    output$model_prepared <- renderText({
      m4()
      if (inherits(m4(), "character")) {
        return(m4())
      }
      "Model prepared."
    })

    result <- reactive({
      if (input$time_limit == 0) {
        time_limit <- Inf
      } else {
        time_limit <- input$time_limit
      }

      if (input$iteration_limit == 0) {
        it_limit <- Inf
      } else {
        it_limit <- input$iteration_limit
      }
      if (input$solver == "glpk") {
        require(ROI.plugin.glpk)
        solve_model(m4(), with_ROI(solver = input$solver,
                                   verbose = TRUE))
      } else if (input$solver == "gurobi") {
        require(ROI.plugin.gurobi)
        solve_model(m4(), with_ROI(solver = input$solver,
                                   TimeLimit = time_limit,
                                   IterationLimit = it_limit,
                                   verbose = TRUE))
      }
    }) %>%
      bindEvent(input$optimise)

    output$optimisation_output <- renderText({
      result()
      return(paste("Model optimised! Status: ", result()$status, ".", sep = ""))
    })

    merged_df <- reactive({
      assign_groups(result(), "diversity", stud_info_df(), group_names = input$group_var)
    }) %>%
      bindEvent(input$merge)

    output$merged_output <- renderText({
      merged_df()
      return("Df merged.")
    })

    output$download_df <- downloadHandler(
      filename = "model_output.csv",
      content = function(file) {
        write.csv(merged_df(), file, row.names = FALSE)
      }
    )
  })
}

pba_ui <- function(id) {
  ns <- NS(id)

  tagList(
    compact_header(
      "Preference-Based Assignment (PBA)",
      "Upload student and preference files, configure repetition/size parameters, optimise, and merge assignments."
    ),
    step_card(
      "Step 1: Student Information Input",
      p(
        "Upload a csv or excel file that contains information on the
        students in your course. There should be one row for each
        student. There should also be a column containing the
        self-formed groupings."
      ),
      fluidRow(
        column(width = 4, fileInput(ns("stud_info_pba"), "Upload a file:",
                                    accept = c(".csv", ".xlsx"))),
        column(width = 6, DTOutput(ns("stud_info_preview_pba")))
      ),
      fluidRow(
        column(width = 4, uiOutput(ns("group_selection_pba")))
      ),
      p(
        "Upload a csv or excel file that contains information on the
        preference of each self-formed group for each topic. There should
        be one row for each self-formed group, and one column for each topic.
        The values in each topic should be nonnegative values, indicating the
        preference that a group has for a particular topic. Larger values indicate
        larger preference."
      ),
      fluidRow(
        column(width = 4, fileInput(ns("stud_pref_pba"), "Upload a file:",
                                    accept = c(".csv", ".xlsx"))),
        column(width = 4, textOutput(ns("stud_pref_message_pba")))
      )
    ),
    step_card(
      "Step 2: Model Parameters",
      p("Select the parameters for your model here, e.g. number of topics, number of members, etc."),
      fluidRow(
        column(width = 4,
               numericInput(ns("num_reps"), "No. of repetition of each topic:", 1, min = 1, step = 1)),
        column(width = 4,
               numericInput(ns("n_min"), "Min. group size:", 1, min = 1, step = 1)),
        column(width = 4,
               numericInput(ns("n_max"), "Max. group size:", 2, min = 1, step = 1))
      )
    ),
    step_card(
      "Step 3: Prepare Model",
      p("Click on the button to prepare the model."),
      fluidRow(
        column(width = 4, actionButton(ns("prepare"), "Prepare model", class = "btn btn-accent")),
        column(width = 6, textOutput(ns("model_prepared")))
      )
    ),
    step_card(
      "Step 4: Optimisation Termination Criteria",
      p(
        "Enter the termination criteria you wish to set, then click optimise.
        To learn more about the criteria below, please take a look at the following link:"
      ),
      a(
        "Gurobi optimisation parameters",
        href = "https://docs.gurobi.com/projects/optimizer/en/current/concepts/parameters/groups.html#paramgrouptermination"
      ),
      p(
        "As long as one of the criteria below is reached, the optimisation will terminate.
        It is good to set one of the above during the initial runs, just to make
        sure that the model is running properly before allowing it to run to
        completion. If you do not wish to set both criteria, just leave the one you
        do not wish to set as 0."
      ),
      fluidRow(
        column(width = 4, numericInput(ns("time_limit"), "Time limit (sec)", 60, min = 0, step = 0.1)),
        column(width = 4, numericInput(ns("iteration_limit"), "Iteration limit", 100, min = 0, step = 1)),
        column(width = 4, selectizeInput(ns("solver"), "Select solver", choices = c("glpk", "gurobi")))
      ),
      fluidRow(
        column(width = 4, actionButton(ns("optimise"), "Optimise", class = "btn btn-accent")),
        column(width = 6, textOutput(ns("optimisation_output")))
      )
    ),
    step_card(
      "Step 5: Merge With Original Data",
      fluidRow(
        column(width = 4, actionButton(ns("merge"), "Merge dataframes", class = "btn btn-accent")),
        column(width = 4, textOutput(ns("merged_output"))),
        column(width = 4, downloadLink(ns("download_df"), "Download"))
      )
    )
  )
}

pba_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    stud_info_df_pba <- reactive({
      fname <- input$stud_info_pba$datapath
      f_ext <- tools::file_ext(fname)
      if (f_ext == "csv") {
        df <- read.csv(fname)
      } else if (f_ext == "xlsx") {
        df <- read_excel(fname)
      }
      df
    })
    stud_pref_mat_pba <- reactive({
      fname <- input$stud_pref_pba$datapath
      f_ext <- tools::file_ext(fname)
      if (f_ext == "csv") {
        df <- read.csv(fname)
      } else if (f_ext == "xlsx") {
        df <- read_excel(fname)
      }
      as.matrix(df)
    })

    output$stud_info_preview_pba <- renderDT({
      df <- input$stud_info_pba
      req(df)
      stud_info_df_pba()
    }, options = list(scrollX = TRUE))

    output$group_selection_pba <- renderUI({
      df <- input$stud_info_pba
      req(df)
      col_names <- colnames(stud_info_df_pba())
      selectizeInput(ns("group_var"), label = "Group column:",
                     choices = col_names, multiple = FALSE)
    })

    output$stud_pref_message_pba <- renderText({
      mat1 <- input$stud_pref_pba
      req(mat1)
      paste0("Preference matrix read in with ",
             NCOL(stud_pref_mat_pba()), " topics.")
    })

    yaml_list <- reactive({
      n_topics <- NCOL(stud_pref_mat_pba())
      list(
        n_topics = n_topics,
        B = 1,
        R = input$num_reps,
        rmin = input$num_reps, rmax = input$num_reps,
        nmin = matrix(input$n_min,
                      nrow = n_topics * input$num_reps,
                      ncol = 1, byrow = TRUE),
        nmax = matrix(input$n_max,
                      nrow = n_topics * input$num_reps,
                      ncol = 1, byrow = TRUE)
      )
    })

    m4 <- reactive({
      df_col_names <- colnames(stud_info_df_pba())
      grouping_col_no <- match(input$group_var, df_col_names)

      df_list <- extract_student_info(stud_info_df_pba(), "preference",
                                      self_formed_groups = grouping_col_no,
                                      pref_mat = stud_pref_mat_pba())

      prepare_model(df_list, yaml_list(), "preference")
    }) %>%
      bindEvent(input$prepare)

    output$model_prepared <- renderText({
      m4()
      if (inherits(m4(), "character")) {
        return(m4())
      }
      "Model prepared."
    })

    result <- reactive({
      if (input$time_limit == 0) {
        time_limit <- Inf
      } else {
        time_limit <- input$time_limit
      }

      if (input$iteration_limit == 0) {
        it_limit <- Inf
      } else {
        it_limit <- input$iteration_limit
      }

      if (input$solver == "glpk") {
        require(ROI.plugin.glpk)
        solve_model(m4(), with_ROI(solver = input$solver,
                                   verbose = TRUE))
      } else if (input$solver == "gurobi") {
        require(ROI.plugin.gurobi)
        solve_model(m4(), with_ROI(solver = input$solver,
                                   TimeLimit = time_limit,
                                   IterationLimit = it_limit,
                                   verbose = TRUE))
      }
    }) %>%
      bindEvent(input$optimise)

    output$optimisation_output <- renderText({
      result()
      return(paste("Model optimised! Status: ", result()$status, ".", sep = ""))
    })

    merged_df <- reactive({
      assign_groups(result(), "preference",
                    dframe = stud_info_df_pba(),
                    yaml_list(),
                    group_names = input$group_var)
    }) %>%
      bindEvent(input$merge)

    output$merged_output <- renderText({
      merged_df()
      return("Df merged.")
    })

    output$download_df <- downloadHandler(
      filename = "model_output.csv",
      content = function(file) {
        write.csv(merged_df(), file, row.names = FALSE)
      }
    )
  })
}

phd_ui <- function(id) {
  ns <- NS(id)

  tagList(
    compact_header(
      "PhD Workload Allocation",
      "Validate semester files, run the model with configurable constraints, and export assignment outputs."
    ),
    step_card(
      "Step 1: Upload And Validate",
      p(
        class = "hint",
        "A template for current semester file is provided below. Please fill and submit files in the exact template style/format."
      ),
      fluidRow(
        column(
          width = 12,
          downloadButton(ns("download_template"), "Download sample dataset")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 6,
          fileInput(
            ns("current_file"),
            "Current semester file (XLSX)",
            accept = c(".xlsx")
          )
        ),
        column(
          width = 6,
          fileInput(
            ns("past_file"),
            "Previous semester model output (XLSX)",
            accept = c(".xlsx")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          actionButton(ns("validate_inputs"), "Validate Inputs", class = "btn btn-accent")
        )
      ),
      htmlOutput(ns("validation_message")),
      conditionalPanel(
        condition = sprintf("output['%s'] == 'true'", ns("has_validated")),
        hr(),
        tabsetPanel(
          tabPanel("Students preview", DTOutput(ns("students_preview"))),
          tabPanel("Demand preview", DTOutput(ns("demand_preview"))),
          tabPanel("Previous output preview", DTOutput(ns("past_preview")))
        )
      )
    ),
    step_card(
      "Step 2: Parameters And Run",
      p(class = "hint", "Set parameters after validation, then run optimisation."),
      fluidRow(
        column(
          width = 4,
          selectInput(
            ns("solver"),
            "Solver",
            choices = c("gurobi", "glpk", "highs"),
            selected = "gurobi"
          )
        ),
        column(
          width = 4,
          numericInput(ns("capacity"), "C (semester cap)", value = 4, min = 1, step = 1)
        ),
        column(
          width = 4,
          numericInput(ns("e_max"), "e_max", value = 1, min = 0, step = 1)
        )
      ),
      tags$details(
        tags$summary("Advanced Parameters"),
        br(),
        fluidRow(
          column(width = 3, numericInput(ns("t_max_y1"), "t_max_y1", value = 1, min = 0, step = 1)),
          column(width = 3, numericInput(ns("alpha"), "alpha", value = 2, min = 0, step = 0.1)),
          column(width = 3, numericInput(ns("beta"), "beta", value = 1, min = 0, step = 0.1)),
          column(width = 3, numericInput(ns("phi"), "phi", value = 1, min = 0, step = 0.1))
        ),
        fluidRow(
          column(width = 3, numericInput(ns("rho"), "rho", value = 10, min = 0, step = 0.1)),
          column(width = 3, numericInput(ns("ta_min"), "ta_min", value = NA, min = 0, step = 1)),
          column(width = 3, numericInput(ns("ta_max"), "ta_max", value = NA, min = 0, step = 1)),
          column(width = 3, numericInput(ns("gr_min"), "gr_min", value = NA, min = 0, step = 1))
        ),
        fluidRow(
          column(width = 3, numericInput(ns("gr_max"), "gr_max", value = NA, min = 0, step = 1)),
          column(width = 3, numericInput(ns("e_min"), "e_min", value = NA, min = 0, step = 1)),
          column(width = 3, numericInput(ns("time_limit"), "Time limit (sec, Gurobi)", value = 0, min = 0, step = 1)),
          column(width = 3, numericInput(ns("iteration_limit"), "Iteration limit (Gurobi)", value = 0, min = 0, step = 1))
        )
      ),
      br(),
      actionButton(ns("run_model"), "Run Optimisation", class = "btn btn-accent"),
      br(),
      br(),
      htmlOutput(ns("run_message"))
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 'true'", ns("has_run")),
      fluidRow(
        column(
          width = 4,
          step_card(
            "Step 3: Run Summary",
            DTOutput(ns("run_summary"))
          )
        ),
        column(
          width = 8,
          step_card(
            "Step 3: Workload Distribution",
            plotOutput(ns("workload_plot"), height = "460px")
          )
        )
      )
    ),
    conditionalPanel(
      condition = sprintf("output['%s'] == 'true'", ns("has_run")),
      step_card(
        "Step 4: Outputs",
        p(class = "hint", "Primary output uses assign_job format for direct reuse in future semesters."),
        fluidRow(
          column(width = 3, downloadButton(ns("download_assignment"), "Download Assignment XLSX"))
        ),
        br(),
        tabsetPanel(
          tabPanel("Assignment table", DTOutput(ns("assignment_table"))),
          tabPanel("Preference attainment", DTOutput(ns("preference_table")))
        )
      )
    )
  )
}

phd_server <- function(id, template_path) {
  moduleServer(id, function(input, output, session) {
    validated_data <- reactiveVal(NULL)
    run_data <- reactiveVal(NULL)

    validation_message <- reactiveVal("<span class='status-warn'>Upload both files and click Validate Inputs.</span>")
    run_message <- reactiveVal("<span class='status-warn'>No run has been executed yet.</span>")

    to_nullable_number <- function(x) {
      if (is.null(x) || is.na(x)) {
        return(NULL)
      }
      as.numeric(x)
    }

    observeEvent(list(input$current_file, input$past_file), {
      validated_data(NULL)
      run_data(NULL)
      validation_message("<span class='status-warn'>Inputs changed. Click Validate Inputs again.</span>")
      run_message("<span class='status-warn'>No run has been executed yet.</span>")
    }, ignoreInit = TRUE)

    output$validation_message <- renderUI({
      HTML(validation_message())
    })

    output$run_message <- renderUI({
      HTML(run_message())
    })

    output$has_run <- renderText({
      if (is.null(run_data())) {
        "false"
      } else {
        "true"
      }
    })
    outputOptions(output, "has_run", suspendWhenHidden = FALSE)

    output$has_validated <- renderText({
      if (is.null(validated_data())) {
        "false"
      } else {
        "true"
      }
    })
    outputOptions(output, "has_validated", suspendWhenHidden = FALSE)

    output$download_template <- downloadHandler(
      filename = function() {
        "current_semester_template.xlsx"
      },
      content = function(file) {
        if (!file.exists(template_path)) {
          stop("Template file is missing in app directory: current_semester_template.xlsx")
        }
        file.copy(template_path, file, overwrite = TRUE)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    observeEvent(input$validate_inputs, {
      run_data(NULL)
      run_message("<span class='status-warn'>No run has been executed yet.</span>")

      if (is.null(input$current_file$datapath) || is.null(input$past_file$datapath)) {
        validated_data(NULL)
        validation_message("<span class='status-warn'>Please upload both files before validation.</span>")
        return()
      }

      out <- tryCatch({
        current <- validate_current_semester_file(input$current_file$datapath)
        prev <- read_uploaded_table(input$past_file$datapath)

        list(students = current$students, demand = current$demand, past_output = prev)
      }, error = function(e) {
        validation_message(
          paste0("<span class='status-warn'>Validation failed: ", htmltools::htmlEscape(conditionMessage(e)), "</span>")
        )
        NULL
      })

      if (is.null(out)) {
        validated_data(NULL)
        return()
      }

      validated_data(out)
      validation_message(
        paste0(
          "<span class='status-ok'>Validation successful: ",
          nrow(out$students), " students, ",
          nrow(out$demand), " courses, ",
          nrow(out$past_output), " rows in previous output.</span>"
        )
      )
    })

    observeEvent(input$run_model, {
      req(validated_data())

      run_result <- tryCatch({
        prep <- prepare_phd_run_inputs(
          students = validated_data()$students,
          demand = validated_data()$demand,
          previous_output = validated_data()$past_output,
          C = input$capacity
        )

        model <- grouper::prepare_model(
          df_list = prep$df_list,
          assignment = "phd",
          t_max_y1 = as.numeric(input$t_max_y1),
          e_max = to_nullable_number(input$e_max),
          ta_min = to_nullable_number(input$ta_min),
          ta_max = to_nullable_number(input$ta_max),
          gr_min = to_nullable_number(input$gr_min),
          gr_max = to_nullable_number(input$gr_max),
          e_min = to_nullable_number(input$e_min),
          alpha = as.numeric(input$alpha),
          beta = as.numeric(input$beta),
          phi = as.numeric(input$phi),
          rho = as.numeric(input$rho),
          C = as.numeric(input$capacity)
        )

        result <- solve_phd_model(
          model = model,
          solver = input$solver,
          time_limit = input$time_limit,
          iteration_limit = input$iteration_limit
        )

        assignment_tbl <- grouper::assign_job(
          model_result = result,
          student_df = prep$students,
          course_codes = prep$course_codes,
          name_col = "Name"
        )

        alloc_summary <- summarise_assignment_from_job_output(assignment_tbl, prep$students)
        pref_attainment <- compute_preference_attainment(
          model_result = result,
          p_mat = prep$p_mat,
          total_ta_demand = sum(prep$demand$TA)
        )
        student_diag <- compute_student_diagnostics(
          alloc_summary = alloc_summary,
          t1 = prep$df_list$t1,
          g1 = prep$df_list$g1
        )

        list(
          summary_tbl = compute_run_summary(result),
          assignment_tbl = assignment_tbl,
          preference_tbl = pref_attainment,
          student_diag = student_diag,
          workload_plot = plot_workload_distribution(student_diag),
          solver_status = as.character(result$status)
        )
      }, error = function(e) {
        run_message(
          paste0("<span class='status-warn'>Run failed: ", htmltools::htmlEscape(conditionMessage(e)), "</span>")
        )
        NULL
      })

      if (is.null(run_result)) {
        run_data(NULL)
        return()
      }

      run_data(run_result)

      solver_note <- ""
      if (input$solver != "gurobi" && ((input$time_limit > 0) || (input$iteration_limit > 0))) {
        solver_note <- " Time/iteration limits are applied only for Gurobi in this app."
      }

      run_message(
        paste0(
          "<span class='status-ok'>Run completed. Solver status: ",
          htmltools::htmlEscape(run_result$solver_status),
          ".", solver_note, "</span>"
        )
      )
    })

    output$students_preview <- renderDT({
      req(validated_data())
      datatable(validated_data()$students, options = list(scrollX = TRUE, pageLength = 6))
    })

    output$demand_preview <- renderDT({
      req(validated_data())
      datatable(validated_data()$demand, options = list(scrollX = TRUE, pageLength = 6))
    })

    output$past_preview <- renderDT({
      req(validated_data())
      datatable(validated_data()$past_output, options = list(scrollX = TRUE, pageLength = 6))
    })

    output$run_summary <- renderDT({
      req(run_data())
      datatable(
        run_data()$summary_tbl,
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      )
    })

    output$workload_plot <- renderPlot({
      req(run_data())
      run_data()$workload_plot
    })

    output$assignment_table <- renderDT({
      req(run_data())
      datatable(run_data()$assignment_tbl, options = list(scrollX = TRUE, pageLength = 12))
    })

    output$preference_table <- renderDT({
      req(run_data())
      datatable(run_data()$preference_tbl, options = list(dom = "t", ordering = FALSE))
    })

    output$download_assignment <- downloadHandler(
      filename = function() {
        paste0("phd_assignment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        req(run_data())
        writexl::write_xlsx(list(allocation = run_data()$assignment_tbl), path = file)
      }
    )
  })
}

ui <- page_navbar(
  nav_panel("Home", value = "home", home_ui()),
  nav_panel("DBA", value = "dba", dba_ui("dba")),
  nav_panel("PBA", value = "pba", pba_ui("pba")),
  nav_panel("PhD", value = "phd", phd_ui("phd")),
  id = "main_nav",
  selected = "home",
  title = "grouper",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Source Sans 3"),
    heading_font = font_google("IBM Plex Sans")
  ),
  header = tags$head(tags$style(HTML(global_styles)))
)

server <- function(input, output, session) {
  observeEvent(input$open_dba, {
    bslib::nav_select("main_nav", selected = "dba")
  })
  observeEvent(input$open_pba, {
    bslib::nav_select("main_nav", selected = "pba")
  })
  observeEvent(input$open_phd, {
    bslib::nav_select("main_nav", selected = "phd")
  })

  dba_server("dba")
  pba_server("pba")
  phd_server("phd", template_path = file.path(app_dir, "../phdWebApp/current_semester_template.xlsx"))
}

shinyApp(ui = ui, server = server)
