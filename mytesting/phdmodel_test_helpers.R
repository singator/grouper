# PhD model testing helpers
# Usage:
# 1) source("phdmodel_test_helpers.R")
# 2) extract_phd_assignment() / extract_phd_ta_detail() for a solved model
# 3) extract_phd_results() or extract_phd_pool_results() for summary workflows
# 4) plot_phd_year_distribution() for workload visualisation

# -------------------------------------------------------------------------
# Result extraction helpers
# -------------------------------------------------------------------------

# Extract student-level TA/GR/E assignments from a solved PhD model.
# Note that course-level (j) information is aggregated here.
extract_phd_assignment <- function(model_result,
                                   student_df,
                                   id_col = "S/No.",
                                   name_col = "Name",
                                   seniority_col = 'seniority',
                                   role_names = c("TA", "GR", "E"),
                                   integer_output = TRUE) {
  if (!all(c(id_col, name_col) %in% names(student_df))) {
    stop("id_col and/or name_col not found in student_df.")
  }
  if (length(role_names) != 3) {
    stop("role_names must have length 3 in the order of model roles (TA, GR, E).")
  }

  student_map <- student_df %>%
    dplyr::transmute(
      i = dplyr::row_number(),
      student_id = .data[[id_col]],
      student_name = .data[[name_col]],
      seniority = .data[[seniority_col]]
    )

  alloc_df <- ompr::get_solution(model_result, X[i, j, r]) %>%
    dplyr::filter(.data$value > 1e-8) %>%
    dplyr::group_by(.data$i, .data$r) %>% # group by student and job type
    dplyr::summarise(units = sum(.data$value), .groups = "drop") %>%
    dplyr::mutate(
      role = factor(.data$r, levels = 1:3, labels = role_names)
    ) %>%
    dplyr::select("i", "role", "units") %>%
    tidyr::pivot_wider(
      names_from = .data$role,
      values_from = .data$units,
      values_fill = 0
    )

  out <- student_map %>%
    dplyr::left_join(alloc_df, by = "i")

  for (nm in role_names) {
    if (!nm %in% names(out)) {
      out[[nm]] <- 0
    }
  }

  if (isTRUE(integer_output)) {
    out[role_names] <- lapply(
      out[role_names],
      function(x) as.integer(round(replace(x, is.na(x), 0)))
    )
  } else {
    out[role_names] <- lapply(
      out[role_names],
      function(x) as.numeric(replace(x, is.na(x), 0))
    )
  }

  out %>%
    dplyr::select("student_id", "student_name", "seniority", dplyr::all_of(role_names)) %>%
    dplyr::arrange(.data$student_id)
}

# Extract TA assignment detail at student-course level (assigned pairs only)
extract_phd_ta_detail <- function(model_result, preference) {
  if (!is.data.frame(preference)) {
    stop("preference must be a data frame.")
  }

  # resolve student id/name columns
  if (all(c("S/No.", "Name") %in% names(preference))) {
    id_col <- "S/No."
    name_col <- "Name"
  } else if (all(c("student_id", "student_name") %in% names(preference))) {
    id_col <- "student_id"
    name_col <- "student_name"
  } else {
    stop("preference must contain either (S/No., Name) or (student_id, student_name).")
  }

  # resolve preference columns
  nm_l <- tolower(names(preference))
  find_col <- function(candidates) {
    idx <- match(candidates, nm_l, nomatch = 0L)
    idx <- idx[idx > 0]
    if (length(idx) == 0) {
      return(NA_character_)
    }
    names(preference)[idx[1]]
  }
  first_col <- find_col(c("first", "1st", "choice1", "pref1"))
  second_col <- find_col(c("second", "2nd", "choice2", "pref2"))
  third_col <- find_col(c("third", "3rd", "choice3", "pref3"))

  student_map <- preference %>%
    dplyr::transmute(
      i = dplyr::row_number(),
      student_id = .data[[id_col]],
      student_name = .data[[name_col]],
      first_choice = if (!is.na(first_col)) norm_code(.data[[first_col]]) else NA_character_,
      second_choice = if (!is.na(second_col)) norm_code(.data[[second_col]]) else NA_character_,
      third_choice = if (!is.na(third_col)) norm_code(.data[[third_col]]) else NA_character_
    )

  ta_sol <- ompr::get_solution(model_result, X[i, j, r]) %>%
    dplyr::filter(.data$r == 1, .data$value > 1e-8) %>%
    dplyr::transmute(i = .data$i, j = .data$j, units = as.numeric(.data$value))

  if (nrow(ta_sol) == 0) {
    return(tibble::tibble(
      student_id = student_map$student_id[0],
      student_name = student_map$student_name[0],
      course_code = character(0),
      preference_rank = character(0),
      units = numeric(0)
    ))
  }

  j_vals <- sort(unique(ta_sol$j))
  j_max <- max(j_vals)

  # course code inference:
  # 1) use attr(preference, "course_codes") if available
  # 2) else use object "courses" from caller env if available and aligned
  # 3) else use observed course index as fallback
  course_codes <- attr(preference, "course_codes", exact = TRUE)

  if (is.null(course_codes)) {
    course_codes <- get0("courses", envir = parent.frame(), inherits = TRUE, ifnotfound = NULL)
  }

  if (!is.null(course_codes)) {
    course_codes <- norm_code(course_codes)
  }

  if (is.null(course_codes) || length(course_codes) < j_max) {
    course_map <- tibble::tibble(
      j = j_vals,
      course_code = as.character(j_vals)
    )
    warning(
      "Unable to infer full course code mapping from preference alone; ",
      "using course index as course_code."
    )
  } else {
    course_map <- tibble::tibble(
      j = j_vals,
      course_code = course_codes[j_vals]
    )
  }

  ta_sol %>%
    dplyr::left_join(student_map, by = "i") %>%
    dplyr::left_join(course_map, by = "j") %>%
    dplyr::mutate(
      preference_rank = dplyr::case_when(
        is.na(.data$first_choice) | .data$first_choice == "" ~ "First",
        .data$course_code == .data$first_choice ~ "First",
        .data$course_code == .data$second_choice ~ "Second",
        .data$course_code == .data$third_choice ~ "Third",
        TRUE ~ "Unranked"
      )
    ) %>%
    dplyr::select(
      "student_id",
      "student_name",
      "course_code",
      "preference_rank",
      "units"
    ) %>%
    dplyr::arrange(.data$student_id, dplyr::desc(.data$units), .data$course_code)
}

# Wrapper extractor for key PhD model outputs
extract_phd_results <- function(model_result, preference) {
  extract_scalar <- function(x, nm) {
    if (is.data.frame(x)) {
      if (!("value" %in% names(x)) || nrow(x) == 0) {
        stop("Could not extract ", nm, " from model_result.")
      }
      return(as.numeric(x$value[[1]]))
    }
    xv <- as.numeric(x)
    if (length(xv) == 0 || is.na(xv[[1]])) {
      stop("Could not extract ", nm, " from model_result.")
    }
    xv[[1]]
  }

  tmax_val <- extract_scalar(ompr::get_solution(model_result, Tmax), "Tmax")
  tmin_val <- extract_scalar(ompr::get_solution(model_result, Tmin), "Tmin")
  obj_val <- as.numeric(model_result$objective_value)
  if (length(obj_val) == 0 || is.na(obj_val[[1]])) {
    # fallback to solver message payload when objective_value is unavailable
    obj_val <- as.numeric(
      model_result$additional_solver_output$ROI$message$objval
    )
  }
  if (length(obj_val) == 0 || is.na(obj_val[[1]])) {
    stop("Could not extract objective value from model_result.")
  }

  pref_for_assignment <- preference
  if (!"seniority" %in% names(pref_for_assignment)) {
    pref_for_assignment <- dplyr::mutate(pref_for_assignment, seniority = NA_real_)
  }

  list(
    objective_value = obj_val[[1]],
    ta_min = tmin_val,
    ta_max = tmax_val,
    phd_assignment = extract_phd_assignment(
      model_result = model_result,
      student_df = pref_for_assignment
    ),
    ta_detail = extract_phd_ta_detail(
      model_result = model_result,
      preference = preference
    )
  )
}

# Extract and dedupe solution-pool results from a solved model
extract_phd_pool_results <- function(result_pool, preference, k = NULL) {
  pool <- result_pool$additional_solver_output$ROI$message$pool
  if (is.null(pool) || length(pool) == 0) {
    stop("No solution pool found in result_pool.")
  }
  if (is.null(result_pool$solution) || is.null(names(result_pool$solution))) {
    stop("result_pool does not contain named base solution vector.")
  }

  if (!is.null(k)) {
    if (!is.numeric(k) || length(k) != 1 || is.na(k) || k < 1) {
      stop("k must be NULL or a single positive integer.")
    }
    k <- as.integer(k)
  }

  n_take <- if (is.null(k)) length(pool) else min(length(pool), k)
  pool <- pool[seq_len(n_take)]

  make_result_from_pool <- function(base_result, pool_entry) {
    out <- base_result
    out$solution <- stats::setNames(pool_entry$poolnx, names(base_result$solution))
    out$objective_value <- as.numeric(pool_entry$objval)
    out
  }

  make_key <- function(x) {
    ta_df <- x$ta_detail %>%
      dplyr::arrange(.data$student_id, .data$course_code, .data$preference_rank, .data$units)

    ta_df$student_id <- as.character(ta_df$student_id)
    ta_df$student_name <- as.character(ta_df$student_name)
    ta_df$course_code <- as.character(ta_df$course_code)
    ta_df$preference_rank <- as.character(ta_df$preference_rank)
    ta_df$units <- sprintf("%.10f", as.numeric(ta_df$units))

    detail_key <- if (nrow(ta_df) == 0) {
      "EMPTY"
    } else {
      paste(apply(ta_df, 1, paste, collapse = "|"), collapse = ";")
    }

    paste0(sprintf("%.10f", as.numeric(x$objective_value)), "||", detail_key)
  }

  out_list <- lapply(seq_along(pool), function(idx) {
    rk_result <- make_result_from_pool(result_pool, pool[[idx]])
    extract_phd_results(rk_result, preference)
  })

  keys <- vapply(out_list, make_key, FUN.VALUE = character(1))
  out_list[!duplicated(keys)]
}


# -------------------------------------------------------------------------
# Plot helper
# -------------------------------------------------------------------------

# Plot per-student workload distribution for current semester / full year.
plot_phd_year_distribution <- function(current_alloc_df,
                                       t1 = NULL,
                                       g1 = NULL,
                                       student_id_col = "student_id",
                                       student_name_col = "student_name",
                                       facet_by_seniority = FALSE,
                                       seniority_col = NULL,
                                       seniority = NULL,
                                       facet_ncol = 4) {
  required_cols <- c(student_id_col, student_name_col, "TA", "GR", "E")
  missing_cols <- setdiff(required_cols, names(current_alloc_df))
  if (length(missing_cols) > 0) {
    stop(
      "current_alloc_df is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  has_past <- !(is.null(t1) && is.null(g1))
  if (xor(is.null(t1), is.null(g1))) {
    stop("Provide both t1 and g1, or provide neither.")
  }
  if (has_past && (length(t1) != nrow(current_alloc_df) || length(g1) != nrow(current_alloc_df))) {
    stop("t1 and g1 must have length equal to nrow(current_alloc_df).")
  }

  if (!is.logical(facet_by_seniority) || length(facet_by_seniority) != 1 || is.na(facet_by_seniority)) {
    stop("facet_by_seniority must be TRUE or FALSE.")
  }
  if (!is.null(facet_ncol) && (!is.numeric(facet_ncol) || length(facet_ncol) != 1 || is.na(facet_ncol) || facet_ncol < 1)) {
    stop("facet_ncol must be NULL or a single positive integer.")
  }

  seniority_vec <- NULL
  if (isTRUE(facet_by_seniority)) {
    if (!is.null(seniority)) {
      if (length(seniority) != nrow(current_alloc_df)) {
        stop("seniority must have length equal to nrow(current_alloc_df).")
      }
      seniority_vec <- seniority
    } else {
      if (is.null(seniority_col)) {
        if ("seniority" %in% names(current_alloc_df)) {
          seniority_col <- "seniority"
        } else {
          stop("Set seniority_col (or provide seniority vector) when facet_by_seniority = TRUE.")
        }
      }
      if (!seniority_col %in% names(current_alloc_df)) {
        stop("seniority_col not found in current_alloc_df.")
      }
      seniority_vec <- current_alloc_df[[seniority_col]]
    }
  }

  base_df <- current_alloc_df %>%
    dplyr::transmute(
      student_id = .data[[student_id_col]],
      student_name = .data[[student_name_col]],
      current_TA = as.numeric(.data$TA),
      current_GR = as.numeric(.data$GR),
      current_E  = as.numeric(.data$E)
    ) %>%
    dplyr::arrange(.data$student_id) %>%
    dplyr::mutate(student_label = paste0(.data$student_id, " - ", .data$student_name))

  if (isTRUE(facet_by_seniority)) {
    ordered_idx <- order(current_alloc_df[[student_id_col]])
    seniority_vec <- seniority_vec[ordered_idx]

    if (is.numeric(seniority_vec)) {
      seniority_levels <- sort(unique(seniority_vec))
    } else {
      seniority_levels <- unique(as.character(seniority_vec))
    }

    base_df$seniority <- factor(as.character(seniority_vec), levels = as.character(seniority_levels))
  } else {
    base_df$seniority <- factor("All")
  }

  job_colors <- c(TA = "#0072B2", GR = "#e61b00", E = "#0d9e00")

  if (has_past) {
    plot_df <- base_df %>%
      dplyr::mutate(
        past_TA = as.numeric(t1),
        past_GR = as.numeric(g1)
      )

    long_df <- dplyr::bind_rows(
      plot_df %>% dplyr::transmute(student_label, seniority = .data$seniority, stack_component = "Sem2_TA", units = .data$current_TA),
      plot_df %>% dplyr::transmute(student_label, seniority = .data$seniority, stack_component = "Sem2_GR", units = .data$current_GR),
      plot_df %>% dplyr::transmute(student_label, seniority = .data$seniority, stack_component = "Sem2_E",  units = .data$current_E),
      plot_df %>% dplyr::transmute(student_label, seniority = .data$seniority, stack_component = "Sem1_TA", units = .data$past_TA),
      plot_df %>% dplyr::transmute(student_label, seniority = .data$seniority, stack_component = "Sem1_GR", units = .data$past_GR)
    ) %>%
      dplyr::mutate(
        stack_component = factor(
          .data$stack_component,
          levels = c("Sem2_TA", "Sem2_GR", "Sem2_E", "Sem1_TA", "Sem1_GR")
        )
      )

    fill_values <- c(
      Sem2_TA = job_colors[["TA"]],
      Sem2_GR = job_colors[["GR"]],
      Sem2_E  = job_colors[["E"]],
      Sem1_TA = job_colors[["TA"]],
      Sem1_GR = job_colors[["GR"]]
    )

    sem2_is_four <- all(abs(plot_df$current_TA + plot_df$current_GR + plot_df$current_E - 4) < 1e-8)
    subtitle_txt <- if (sem2_is_four) {
      "Bottom 4 units: Semester 2 (current) | Top 4 units: Semester 1 (past)"
    } else {
      "Bottom stack: Semester 2 (current) | Top stack: Semester 1 (past)"
    }

    p <- ggplot2::ggplot(
      long_df,
      ggplot2::aes(x = .data$student_label, y = .data$units, fill = .data$stack_component)
    ) +
      ggplot2::geom_col(
        width = 0.85,
        color = "white",
        linewidth = 0.2,
        position = ggplot2::position_stack(reverse = TRUE)
      ) +
      ggplot2::scale_fill_manual(
        values = fill_values,
        breaks = c("Sem2_TA", "Sem2_GR", "Sem2_E"),
        labels = c("TA", "GR", "E"),
        name = "Job",
        drop = FALSE
      ) +
      ggplot2::labs(
        x = "Student",
        y = "Workload Units",
        title = "Year-Long Work Distribution by Student",
        subtitle = subtitle_txt
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank()
      )

    if (sem2_is_four) {
      p <- p + ggplot2::geom_hline(yintercept = 4, linetype = "dashed", color = "grey35")
    }

    if (isTRUE(facet_by_seniority)) {
      p <- p + ggplot2::facet_wrap(~seniority, scales = "free_x", ncol = facet_ncol)
    }

    return(p)
  }

  long_df <- dplyr::bind_rows(
    base_df %>% dplyr::transmute(student_label, seniority = .data$seniority, job = "TA", units = .data$current_TA),
    base_df %>% dplyr::transmute(student_label, seniority = .data$seniority, job = "GR", units = .data$current_GR),
    base_df %>% dplyr::transmute(student_label, seniority = .data$seniority, job = "E",  units = .data$current_E)
  ) %>%
    dplyr::mutate(job = factor(.data$job, levels = c("TA", "GR", "E")))

  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = .data$student_label, y = .data$units, fill = .data$job)) +
    ggplot2::geom_col(
      width = 0.85,
      color = "white",
      linewidth = 0.2,
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::scale_fill_manual(values = job_colors, drop = FALSE) +
    ggplot2::labs(
      x = "Student",
      y = "Workload Units",
      fill = "Job",
      title = "Current-Semester Work Distribution by Student"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )

  if (isTRUE(facet_by_seniority)) {
    p <- p + ggplot2::facet_wrap(~seniority, scales = "free_x", ncol = facet_ncol)
  }

  p
}


# -------------------------------------------------------------------------
# General utilities used by PhD testing notebooks
# -------------------------------------------------------------------------

# Course-code normalisation helper
norm_code <- function(x) {
  toupper(trimws(as.character(x)))
}

# Name standardisation helper
standardise_name <- function(x) {
  x %>%
    as.character() %>%
    str_to_upper() %>%        
    str_replace_all("[^A-Z ]", "") %>% # remove funny letters
    str_squish()             
}

# Preference-code cleanup helper
clean_pref_code <- function(x) {
  out <- norm_code(x)
  out[out %in% c("", "-", "NIL", "NA")] <- NA_character_
  out
}

# Recompute objective components for manual cross-checks
compute_obj_from_components <- function(ta_by_student, e_by_student, ta_pref_sum,
                                        t1, s, t_max_y1, alpha, beta, phi, rho) {

  idx_non_y1 <- which(s >= 0)
  idx_y1 <- which(s == -1)

  year_ta <- t1 + ta_by_student
  ta_spread <- if (length(idx_non_y1) > 0) {
    max(year_ta[idx_non_y1]) - min(year_ta[idx_non_y1])
  } else {
    0
  }

  seniority_e_sum <- sum(s * e_by_student)
  y1_slack_sum <- if (length(idx_y1) > 0) {
    sum(pmax(0, ta_by_student[idx_y1] - t_max_y1))
  } else {
    0
  }

  tibble(
    ta_spread = ta_spread,
    ta_pref_sum = ta_pref_sum,
    seniority_e_sum = seniority_e_sum,
    y1_slack_sum = y1_slack_sum,
    objective_recalc = alpha * ta_spread -
      beta * ta_pref_sum -
      phi * seniority_e_sum +
      rho * y1_slack_sum
  )
}
