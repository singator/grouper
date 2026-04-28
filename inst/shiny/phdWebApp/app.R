library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)
library(grouper)
library(ompr)
library(ompr.roi)

source("utils.R")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Source Sans 3"),
    heading_font = font_google("IBM Plex Sans")
  ),
  tags$head(
    tags$style(HTML(
      ".hero-banner {
         background: linear-gradient(115deg, #0f4c81 0%, #2b6cb0 52%, #7fb3d5 100%);
         color: #ffffff;
         border-radius: 16px;
         padding: 18px 22px;
         margin-bottom: 14px;
         box-shadow: 0 12px 24px rgba(18, 67, 115, 0.18);
       }
       .hero-banner h1 {
         font-size: 30px;
         margin: 0 0 6px 0;
         letter-spacing: 0.2px;
       }
       .hero-banner p {
         font-size: 16px;
         margin: 0;
         opacity: 0.95;
       }
       .panel-card {
         background: #ffffff;
         border: 1px solid #d9e2ec;
         border-radius: 14px;
         padding: 14px 16px;
         margin-bottom: 12px;
         box-shadow: 0 8px 20px rgba(15, 76, 129, 0.06);
       }
       .panel-card h3 {
         margin-top: 0;
         margin-bottom: 10px;
         color: #123a63;
       }
       .hint {
         color: #355d80;
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
         background-color: #e66100;
         border-color: #c95300;
         color: #ffffff;
       }
       .btn-accent:hover {
         background-color: #c95300;
         border-color: #b54a00;
         color: #ffffff;
       }
      "
    ))
  ),

  # ---- Step 1: Upload & Validate ----
  div(
    class = "hero-banner",
    h1("PhD Workload Allocation"),
    p("Template-driven upload, strict validation, parameterized optimisation, and direct downloadable outputs.")
  ),

  # ---- Step 2: Parameter Selection & Run ----
  div(
    class = "panel-card",
    h3("Step 1: Upload And Validate"),
        p(
          class = "hint",
          "Use exactly two inputs: current_semester.xlsx (fixed tabs/columns) and previous assign_job output (XLSX)."
        ),
    fluidRow(
      column(
        width = 6,
        downloadButton("download_template", "Download current_semester.xlsx")
      ),
      column(
        width = 6,
        actionButton("validate_inputs", "Validate Inputs", class = "btn btn-primary")
      )
    ),
    br(),
    fileInput(
      "current_file",
      "Current semester file (XLSX)",
      accept = c(".xlsx")
    ),
        fileInput(
          "past_file",
          "Previous semester model output (XLSX)",
          accept = c(".xlsx")
        ),
    htmlOutput("validation_message"),
    hr(),
    tabsetPanel(
      tabPanel("Students preview", DTOutput("students_preview")),
      tabPanel("Demand preview", DTOutput("demand_preview")),
      tabPanel("Previous output preview", DTOutput("past_preview"))
    )
  ),

  div(
    class = "panel-card",
    h3("Step 2: Parameters And Run"),
    p(class = "hint", "Set parameters after validation, then run optimisation."),
    fluidRow(
      column(
        width = 4,
        selectInput(
          "solver",
          "Solver",
          choices = c("gurobi", "glpk", "highs"),
          selected = "gurobi"
        )
      ),
      column(
        width = 4,
        numericInput("capacity", "C (semester cap)", value = 4, min = 1, step = 1)
      ),
      column(
        width = 4,
        numericInput("e_max", "e_max", value = 1, min = 0, step = 1)
      )
    ),

    tags$details(
      tags$summary("Advanced Parameters"),
      br(),
      fluidRow(
        column(width = 3, numericInput("t_max_y1", "t_max_y1", value = 1, min = 0, step = 1)),
        column(width = 3, numericInput("alpha", "alpha", value = 2, min = 0, step = 0.1)),
        column(width = 3, numericInput("beta", "beta", value = 1, min = 0, step = 0.1)),
        column(width = 3, numericInput("phi", "phi", value = 1, min = 0, step = 0.1))
      ),
      fluidRow(
        column(width = 3, numericInput("rho", "rho", value = 10, min = 0, step = 0.1)),
        column(width = 3, numericInput("ta_min", "ta_min", value = NA, min = 0, step = 1)),
        column(width = 3, numericInput("ta_max", "ta_max", value = NA, min = 0, step = 1)),
        column(width = 3, numericInput("gr_min", "gr_min", value = NA, min = 0, step = 1))
      ),
      fluidRow(
        column(width = 3, numericInput("gr_max", "gr_max", value = NA, min = 0, step = 1)),
        column(width = 3, numericInput("e_min", "e_min", value = NA, min = 0, step = 1)),
        column(width = 3, numericInput("time_limit", "Time limit (sec, Gurobi)", value = 0, min = 0, step = 1)),
        column(width = 3, numericInput("iteration_limit", "Iteration limit (Gurobi)", value = 0, min = 0, step = 1))
      )
    ),

    br(),
    actionButton("run_model", "Run Optimisation", class = "btn btn-accent"),
    br(),
    br(),
    htmlOutput("run_message")
  ),

  # ---- Step 3: Post-run Summary ----
  conditionalPanel(
    condition = "output.has_run == 'true'",
    fluidRow(
      column(
        width = 4,
        div(
          class = "panel-card",
          h3("Step 3: Run Summary"),
          DTOutput("run_summary")
        )
      ),
      column(
        width = 8,
        div(
          class = "panel-card",
          h3("Step 3: Workload Distribution"),
          plotOutput("workload_plot", height = "460px")
        )
      )
    )
  ),

  # ---- Step 4: Downloadable Outputs ----
  conditionalPanel(
    condition = "output.has_run == 'true'",
    div(
      class = "panel-card",
      h3("Step 4: Outputs"),
      p(class = "hint", "Primary output uses assign_job format for direct reuse in future semesters."),
      fluidRow(
        column(width = 3, downloadButton("download_assignment", "Download Assignment XLSX"))
      ),
      br(),
      tabsetPanel(
        tabPanel("Assignment table", DTOutput("assignment_table")),
        tabPanel("Preference attainment", DTOutput("preference_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  # ---- Reactive State ----
  validated_data <- reactiveVal(NULL)
  run_data <- reactiveVal(NULL)

  validation_message <- reactiveVal("<span class='status-warn'>Upload both files and click Validate Inputs.</span>")
  run_message <- reactiveVal("<span class='status-warn'>No run has been executed yet.</span>")

  # ---- Utility: convert optional numeric input to NULL when blank ----
  to_nullable_number <- function(x) {
    if (is.null(x) || is.na(x)) {
      return(NULL)
    }
    as.numeric(x)
  }

  # ---- Reset run state when uploads change ----
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

  # ---- Template download ----
  output$download_template <- downloadHandler(
    filename = function() {
      "current_semester_template.xlsx"
    },
    content = function(file) {
      template_path <- file.path(getwd(), "current_semester_template.xlsx")
      if (!file.exists(template_path)) {
        stop("Template file is missing in app directory: current_semester_template.xlsx")
      }
      file.copy(template_path, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  # ---- Validation step ----
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
      validate_previous_output_df(prev)

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

  # ---- Run step ----
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

      # Reuse assign_job output for student-level TA/GR/E summary.
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
}

shinyApp(ui = ui, server = server)
