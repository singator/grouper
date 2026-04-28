`%>%` <- dplyr::`%>%`

# ---- Text Normalisation Helpers ----
# Standardise names for robust join keys across uploads.
standardise_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_to_upper(x)
  x <- stringr::str_replace_all(x, "[^A-Z ]", "")
  stringr::str_squish(x)
}

# Normalise course codes to uppercase without outer whitespace.
norm_code <- function(x) {
  toupper(trimws(as.character(x)))
}

# Convert empty / sentinel preference values to NA.
clean_pref_code <- function(x) {
  out <- norm_code(x)
  out[out %in% c("", "-", "NIL", "NA", "N/A", "NONE")] <- NA_character_
  out
}


# ---- File Reading & Schema Validation ----
read_uploaded_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xlsx") {
    return(as.data.frame(
      readxl::read_excel(path, sheet = 1, .name_repair = "minimal"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  stop("Unsupported file type. Please upload XLSX.")
}

validate_exact_columns <- function(df, expected_cols, label) {
  actual <- names(df)
  missing_cols <- setdiff(expected_cols, actual)
  extra_cols <- setdiff(actual, expected_cols)

  if (length(missing_cols) > 0 || length(extra_cols) > 0 || !identical(actual, expected_cols)) {
    parts <- c()
    if (length(missing_cols) > 0) {
      parts <- c(parts, paste0("missing: ", paste(missing_cols, collapse = ", ")))
    }
    if (length(extra_cols) > 0) {
      parts <- c(parts, paste0("extra: ", paste(extra_cols, collapse = ", ")))
    }
    if (length(parts) == 0) {
      parts <- "column order does not match template"
    }

    stop(
      label, " tab has invalid columns (", paste(parts, collapse = " | "),
      "). Expected exactly: ", paste(expected_cols, collapse = ", "), "."
    )
  }

  invisible(TRUE)
}

validate_current_semester_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext != "xlsx") {
    stop("Current semester input must be an XLSX file.")
  }

  expected_sheets <- c("students", "demand")
  sheet_names <- readxl::excel_sheets(path)

  missing_sheets <- setdiff(expected_sheets, sheet_names)
  extra_sheets <- setdiff(sheet_names, expected_sheets)
  if (length(missing_sheets) > 0 || length(extra_sheets) > 0) {
    parts <- c()
    if (length(missing_sheets) > 0) {
      parts <- c(parts, paste0("missing sheets: ", paste(missing_sheets, collapse = ", ")))
    }
    if (length(extra_sheets) > 0) {
      parts <- c(parts, paste0("extra sheets: ", paste(extra_sheets, collapse = ", ")))
    }
    stop(
      "Current semester file must contain exactly two sheets: students, demand (",
      paste(parts, collapse = " | "), ")."
    )
  }

  students <- as.data.frame(
    readxl::read_excel(path, sheet = "students", .name_repair = "minimal"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  demand <- as.data.frame(
    readxl::read_excel(path, sheet = "demand", .name_repair = "minimal"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  validate_exact_columns(
    students,
    expected_cols = c("student_id", "Name", "year", "first", "second", "third"),
    label = "students"
  )
  validate_exact_columns(
    demand,
    expected_cols = c("course_code", "TA", "GR"),
    label = "demand"
  )

  list(students = students, demand = demand)
}

validate_previous_output_df <- function(df) {
  if (!"Name" %in% names(df)) {
    stop("Previous semester output must contain a Name column.")
  }

  ta_cols <- grep("-t$", names(df), value = TRUE, ignore.case = TRUE)
  gr_cols <- grep("-g$", names(df), value = TRUE, ignore.case = TRUE)
  if (length(ta_cols) == 0 || length(gr_cols) == 0) {
    stop(
      "Previous semester output must contain course-role columns ending in '-t' and '-g' ",
      "from assign_job() output."
    )
  }

  invisible(TRUE)
}


# ---- Core Parsing Helpers ----
coerce_nonneg_integer <- function(x, label) {
  num <- suppressWarnings(as.numeric(x))
  invalid_na <- is.na(num) & !(is.na(x) | trimws(as.character(x)) == "")
  if (any(invalid_na)) {
    stop(label, " contains non-numeric values.")
  }

  num[is.na(num)] <- 0
  if (any(num < 0)) {
    stop(label, " cannot contain negative values.")
  }
  if (any(abs(num - round(num)) > 1e-8)) {
    stop(label, " must contain integer unit values.")
  }

  as.integer(round(num))
}

summarise_past_workload <- function(previous_output) {
  validate_previous_output_df(previous_output)

  ta_cols <- grep("-t$", names(previous_output), value = TRUE, ignore.case = TRUE)
  gr_cols <- grep("-g$", names(previous_output), value = TRUE, ignore.case = TRUE)

  ta_vals <- lapply(ta_cols, function(col_nm) coerce_nonneg_integer(previous_output[[col_nm]], col_nm))
  gr_vals <- lapply(gr_cols, function(col_nm) coerce_nonneg_integer(previous_output[[col_nm]], col_nm))

  ta_sum <- if (length(ta_vals) > 0) {
    rowSums(as.data.frame(ta_vals, check.names = FALSE), na.rm = TRUE)
  } else {
    rep(0, nrow(previous_output))
  }
  gr_sum <- if (length(gr_vals) > 0) {
    rowSums(as.data.frame(gr_vals, check.names = FALSE), na.rm = TRUE)
  } else {
    rep(0, nrow(previous_output))
  }

  out <- data.frame(
    name_key = standardise_name(previous_output$Name),
    past_ta = ta_sum,
    past_gr = gr_sum,
    stringsAsFactors = FALSE
  )

  out <- out[out$name_key != "", , drop = FALSE]

  dplyr::as_tibble(out) %>%
    dplyr::group_by(.data$name_key) %>%
    dplyr::summarise(
      past_ta = sum(.data$past_ta, na.rm = TRUE),
      past_gr = sum(.data$past_gr, na.rm = TRUE),
      .groups = "drop"
    )
}

build_preference_matrix <- function(students_clean, course_codes) {
  ns <- nrow(students_clean)
  nj <- length(course_codes)

  p_mat <- matrix(-99L, nrow = ns, ncol = nj)
  colnames(p_mat) <- course_codes

  for (i in seq_len(ns)) {
    choices <- c(
      third = students_clean$third[[i]],
      second = students_clean$second[[i]],
      first = students_clean$first[[i]]
    )
    scores <- c(third = 1L, second = 2L, first = 3L)

    for (nm in names(choices)) {
      code <- choices[[nm]]
      if (is.na(code) || code == "") {
        next
      }
      j <- match(code, course_codes)
      if (!is.na(j)) {
        p_mat[i, j] <- scores[[nm]]
      }
    }
  }

  p_mat
}


# ---- Model Input Assembly ----
prepare_phd_run_inputs <- function(students, demand, previous_output, C = 4) {
  if (!is.numeric(C) || length(C) != 1 || is.na(C) || C <= 0) {
    stop("C must be a single positive number.")
  }
  if (abs(C - round(C)) > 1e-8) {
    stop("C must be an integer unit cap.")
  }
  C <- as.integer(round(C))

  students_clean <- students %>%
    dplyr::transmute(
      student_id = .data$student_id,
      Name = stringr::str_squish(as.character(.data$Name)),
      year = suppressWarnings(as.numeric(.data$year)),
      first = clean_pref_code(.data$first),
      second = clean_pref_code(.data$second),
      third = clean_pref_code(.data$third)
    )

  if (nrow(students_clean) == 0) {
    stop("students tab is empty.")
  }
  if (any(is.na(students_clean$student_id) | trimws(as.character(students_clean$student_id)) == "")) {
    stop("students.student_id cannot be empty.")
  }
  if (any(duplicated(students_clean$student_id))) {
    stop("students.student_id must be unique.")
  }
  if (any(is.na(students_clean$Name) | students_clean$Name == "")) {
    stop("students.Name cannot be empty.")
  }

  students_clean$name_key <- standardise_name(students_clean$Name)
  if (any(students_clean$name_key == "")) {
    stop("students.Name contains invalid values after standardisation.")
  }
  if (any(duplicated(students_clean$name_key))) {
    stop("Standardised student names are not unique; cannot safely match previous output by Name.")
  }

  if (any(is.na(students_clean$year))) {
    stop("students.year must be numeric.")
  }
  if (any(abs(students_clean$year - round(students_clean$year)) > 1e-8)) {
    stop("students.year must contain integer values.")
  }
  students_clean$year <- as.integer(round(students_clean$year))

  demand_clean <- demand %>%
    dplyr::transmute(
      course_code = norm_code(.data$course_code),
      TA = suppressWarnings(as.numeric(.data$TA)),
      GR = suppressWarnings(as.numeric(.data$GR))
    )

  if (nrow(demand_clean) == 0) {
    stop("demand tab is empty.")
  }
  if (any(is.na(demand_clean$course_code) | demand_clean$course_code == "")) {
    stop("demand.course_code cannot be empty.")
  }
  if (any(duplicated(demand_clean$course_code))) {
    stop("demand.course_code must be unique.")
  }
  if (any(is.na(demand_clean$TA)) || any(is.na(demand_clean$GR))) {
    stop("demand TA/GR values must be numeric.")
  }
  if (any(demand_clean$TA < 0) || any(demand_clean$GR < 0)) {
    stop("demand TA/GR values cannot be negative.")
  }
  if (any(abs(demand_clean$TA - round(demand_clean$TA)) > 1e-8) ||
      any(abs(demand_clean$GR - round(demand_clean$GR)) > 1e-8)) {
    stop("demand TA/GR values must be integers.")
  }

  demand_clean$TA <- as.integer(round(demand_clean$TA))
  demand_clean$GR <- as.integer(round(demand_clean$GR))

  ns <- nrow(students_clean)
  total_e <- ns * C - sum(demand_clean$TA) - sum(demand_clean$GR)
  if (total_e < 0) {
    stop(
      "Computed E is negative (Ns*C - sum(TA) - sum(GR) = ",
      total_e,
      "). Reduce demand or increase C."
    )
  }

  past_summary <- summarise_past_workload(previous_output)
  students_joined <- students_clean %>%
    dplyr::left_join(past_summary, by = "name_key")

  students_joined$past_ta[is.na(students_joined$past_ta)] <- 0
  students_joined$past_gr[is.na(students_joined$past_gr)] <- 0

  student_input <- students_joined %>%
    dplyr::transmute(
      student_id = .data$student_id,
      year = .data$year,
      past_ta = as.numeric(.data$past_ta),
      past_gr = as.numeric(.data$past_gr)
    )

  course_codes <- demand_clean$course_code
  p_mat <- build_preference_matrix(students_joined, course_codes)

  # Reuse package function for model-aligned preprocessing + E allocation.
  df_list <- grouper::extract_phd_info(
    student_df = as.data.frame(student_input, stringsAsFactors = FALSE),
    p_mat = p_mat,
    d_mat = as.matrix(demand_clean[, c("TA", "GR")]),
    e_mode = "rr",
    C = C
  )

  demand_clean$E <- as.integer(round(df_list$d[, "E"]))

  list(
    students = students_joined %>% dplyr::select("student_id", "Name", "year", "past_ta", "past_gr"),
    demand = demand_clean,
    course_codes = course_codes,
    p_mat = p_mat,
    d_mat = df_list$d,
    df_list = df_list,
    total_e = total_e
  )
}


# ---- Solver Wrapper ----
solve_phd_model <- function(model, solver = c("gurobi", "glpk", "highs"),
                            time_limit = 0, iteration_limit = 0) {
  solver <- match.arg(solver)

  plugin_pkg <- switch(
    solver,
    gurobi = "ROI.plugin.gurobi",
    glpk = "ROI.plugin.glpk",
    highs = "ROI.plugin.highs"
  )

  if (!requireNamespace(plugin_pkg, quietly = TRUE)) {
    stop(
      "Solver '", toupper(solver), "' requires package '", plugin_pkg,
      "'. Install it and retry."
    )
  }

  if (solver == "gurobi") {
    roi_args <- list(solver = solver, verbose = TRUE)

    if (is.numeric(time_limit) && length(time_limit) == 1 && !is.na(time_limit) && time_limit > 0) {
      roi_args$TimeLimit <- time_limit
    }
    if (is.numeric(iteration_limit) && length(iteration_limit) == 1 &&
        !is.na(iteration_limit) && iteration_limit > 0) {
      roi_args$IterationLimit <- as.integer(round(iteration_limit))
    }

    roi_ctl <- do.call(ompr.roi::with_ROI, roi_args)
    return(ompr::solve_model(model, roi_ctl))
  }

  roi_ctl <- ompr.roi::with_ROI(solver = solver, verbose = TRUE)
  ompr::solve_model(model, roi_ctl)
}


# ---- Post-solve Summaries & Diagnostics ----
# Build per-student TA/GR/E summary directly from assign_job() output.
summarise_assignment_from_job_output <- function(assignment_tbl, students_df) {
  ta_cols <- grep("-t$", names(assignment_tbl), value = TRUE, ignore.case = TRUE)
  gr_cols <- grep("-g$", names(assignment_tbl), value = TRUE, ignore.case = TRUE)
  e_cols <- grep("-e$", names(assignment_tbl), value = TRUE, ignore.case = TRUE)

  if (length(ta_cols) == 0 || length(gr_cols) == 0 || length(e_cols) == 0) {
    stop("assignment_tbl is missing one or more expected role columns (-t/-g/-e).")
  }

  ta_vals <- lapply(ta_cols, function(col_nm) coerce_nonneg_integer(assignment_tbl[[col_nm]], col_nm))
  gr_vals <- lapply(gr_cols, function(col_nm) coerce_nonneg_integer(assignment_tbl[[col_nm]], col_nm))
  e_vals <- lapply(e_cols, function(col_nm) coerce_nonneg_integer(assignment_tbl[[col_nm]], col_nm))

  ta_total <- rowSums(as.data.frame(ta_vals, check.names = FALSE), na.rm = TRUE)
  gr_total <- rowSums(as.data.frame(gr_vals, check.names = FALSE), na.rm = TRUE)
  e_total <- rowSums(as.data.frame(e_vals, check.names = FALSE), na.rm = TRUE)

  data.frame(
    student_id = students_df$student_id,
    student_name = students_df$Name,
    year = students_df$year,
    TA = as.integer(round(ta_total)),
    GR = as.integer(round(gr_total)),
    E = as.integer(round(e_total)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

compute_preference_attainment <- function(model_result, p_mat, total_ta_demand) {
  ta_sol <- ompr::get_solution(model_result, X[i, j, r])
  ta_sol <- ta_sol[ta_sol$r == 1 & ta_sol$value > 1e-8, c("i", "j", "value"), drop = FALSE]

  units <- c(First = 0, Second = 0, Third = 0, Unranked = 0)

  if (nrow(ta_sol) > 0) {
    for (k in seq_len(nrow(ta_sol))) {
      i <- ta_sol$i[[k]]
      j <- ta_sol$j[[k]]
      v <- ta_sol$value[[k]]

      score <- p_mat[[i, j]]
      if (is.na(score)) {
        units[["Unranked"]] <- units[["Unranked"]] + v
      } else if (score >= 3) {
        units[["First"]] <- units[["First"]] + v
      } else if (score == 2) {
        units[["Second"]] <- units[["Second"]] + v
      } else if (score == 1) {
        units[["Third"]] <- units[["Third"]] + v
      } else {
        units[["Unranked"]] <- units[["Unranked"]] + v
      }
    }
  }

  out <- data.frame(
    preference_rank = c("First", "Second", "Third", "Unranked"),
    TA_units = as.numeric(units[c("First", "Second", "Third", "Unranked")]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (is.numeric(total_ta_demand) && length(total_ta_demand) == 1 && total_ta_demand > 0) {
    out$pct_of_ta_demand <- round(100 * out$TA_units / total_ta_demand, 1)
  } else {
    out$pct_of_ta_demand <- NA_real_
  }

  out
}

compute_student_diagnostics <- function(alloc_summary, t1, g1) {
  student_df <- alloc_summary
  student_df$past_TA <- as.numeric(t1)
  student_df$past_GR <- as.numeric(g1)
  student_df$current_TA <- as.numeric(student_df$TA)
  student_df$current_GR <- as.numeric(student_df$GR)
  student_df$current_E <- as.numeric(student_df$E)

  student_df$yearly_TA <- student_df$past_TA + student_df$current_TA
  student_df$current_total <- student_df$current_TA + student_df$current_GR + student_df$current_E

  out <- student_df[, c(
    "student_id", "student_name", "year",
    "past_TA", "past_GR",
    "current_TA", "current_GR", "current_E",
    "current_total", "yearly_TA"
  )]

  out <- out[order(out$year, out$student_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

safe_extract_scalar <- function(x) {
  if (is.data.frame(x)) {
    if (!"value" %in% names(x) || nrow(x) == 0) {
      return(NA_real_)
    }
    return(as.numeric(x$value[[1]]))
  }

  val <- suppressWarnings(as.numeric(x))
  if (length(val) == 0) {
    return(NA_real_)
  }
  val[[1]]
}

extract_objective_value <- function(model_result) {
  obj <- suppressWarnings(as.numeric(model_result$objective_value))
  if (length(obj) > 0 && !is.na(obj[[1]])) {
    return(obj[[1]])
  }

  fallback <- tryCatch(
    as.numeric(model_result$additional_solver_output$ROI$message$objval),
    error = function(e) NA_real_
  )

  if (length(fallback) == 0 || is.na(fallback[[1]])) {
    return(NA_real_)
  }

  fallback[[1]]
}

compute_run_summary <- function(model_result) {
  tmax <- safe_extract_scalar(ompr::get_solution(model_result, Tmax))
  tmin <- safe_extract_scalar(ompr::get_solution(model_result, Tmin))
  ta_spread <- if (!is.na(tmax) && !is.na(tmin)) tmax - tmin else NA_real_

  data.frame(
    metric = c("Status", "Objective", "Tmax", "Tmin", "TA spread"),
    value = c(
      as.character(model_result$status),
      format(round(extract_objective_value(model_result), 4), trim = TRUE),
      format(round(tmax, 4), trim = TRUE),
      format(round(tmin, 4), trim = TRUE),
      format(round(ta_spread, 4), trim = TRUE)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

plot_workload_distribution <- function(student_diag) {
  required_cols <- c(
    "student_id", "student_name", "year",
    "past_TA", "past_GR",
    "current_TA", "current_GR", "current_E"
  )

  missing_cols <- setdiff(required_cols, names(student_diag))
  if (length(missing_cols) > 0) {
    stop("student_diag is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  plot_df <- student_diag[order(student_diag$year, student_diag$student_id), , drop = FALSE]
  plot_df$student_label <- paste0(plot_df$student_id, " - ", plot_df$student_name)
  plot_df$year <- factor(plot_df$year, levels = sort(unique(plot_df$year)))

  long_df <- rbind(
    data.frame(student_label = plot_df$student_label, year = plot_df$year,
               component = "Sem2_TA", units = plot_df$current_TA),
    data.frame(student_label = plot_df$student_label, year = plot_df$year,
               component = "Sem2_GR", units = plot_df$current_GR),
    data.frame(student_label = plot_df$student_label, year = plot_df$year,
               component = "Sem2_E", units = plot_df$current_E),
    data.frame(student_label = plot_df$student_label, year = plot_df$year,
               component = "Sem1_TA", units = plot_df$past_TA),
    data.frame(student_label = plot_df$student_label, year = plot_df$year,
               component = "Sem1_GR", units = plot_df$past_GR)
  )

  long_df$component <- factor(
    long_df$component,
    levels = c("Sem2_TA", "Sem2_GR", "Sem2_E", "Sem1_TA", "Sem1_GR")
  )

  fill_values <- c(
    Sem2_TA = "#0072B2",
    Sem2_GR = "#E66100",
    Sem2_E = "#0D9E00",
    Sem1_TA = "#8BBAD9",
    Sem1_GR = "#F2AA7A"
  )

  sem2_is_four <- all(abs(plot_df$current_TA + plot_df$current_GR + plot_df$current_E - 4) < 1e-8)

  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = .data$student_label, y = .data$units, fill = .data$component)
  ) +
    ggplot2::geom_col(
      width = 0.84,
      color = "white",
      linewidth = 0.2,
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::scale_fill_manual(
      values = fill_values,
      breaks = c("Sem2_TA", "Sem2_GR", "Sem2_E", "Sem1_TA", "Sem1_GR"),
      labels = c("Current TA", "Current GR", "Current E", "Past TA", "Past GR"),
      name = "Component",
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Student",
      y = "Workload Units",
      title = "Year-Long Workload Distribution by Student",
      subtitle = "Current semester allocation stacked below past semester workload"
    ) +
    ggplot2::facet_wrap(~year, scales = "free_x", ncol = 4) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "top"
    )

  if (sem2_is_four) {
    p <- p + ggplot2::geom_hline(yintercept = 4, linetype = "dashed", color = "grey35")
  }

  p
}
