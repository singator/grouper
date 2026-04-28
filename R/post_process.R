#' Assigns model result to the original data frame.
#'
#' From the result of [ompr::solve_model()], this function attaches the
#' derived groupings to the original dataframe comprising students.
#'
#' @param model_result The output solution objection.
#' @param dframe The original dataframe used in [extract_student_info()].
#' @param assignment Character string indicating the type of model that this
#'   dataset is for. The argument is either 'preference' or 'diversity'. Partial
#'   matching is fine.
#' @param params_list The list of parameters from the YAML file, i.e. the output
#'   of [extract_params_yaml()]. This is only required for the preference-based
#'   assignment.
#' @param group_names A character string. It denotes the column name in the
#'   original dataframe containing the self-formed groups. Note that we need the
#'   string here, not the integer position, since we are going to join with it.
#'
#' @importFrom rlang .data
#'
#' @returns A data frame with the group assignments attached to the original
#' group composition dataframe.
#' @export
#'
assign_groups <- function(model_result,
                          assignment=c("diversity", "preference"),
                          dframe,
                          params_list,
                          group_names) {
  assignment <- match.arg(assignment)
  if(assignment == "diversity") {

    out_df <- ompr::get_solution(model_result, x[g,t,r]) %>%
      dplyr::filter(.data$value>0) %>%
      dplyr::select("t", "r", "g") %>%
      dplyr::rename("group"="g", "topic"="t", "rep"="r") %>%
      dplyr::arrange(.data$topic, .data$rep, .data$group) %>%
      dplyr::left_join(dframe, by=c("group"=group_names))

    return(out_df)
  } else if (assignment == "preference") {
    # message("incomplete")
    group_sizes <- dframe %>%
      dplyr::group_by(.data[[group_names]]) %>%
      dplyr::summarise(size = length(.data$id), .groups = "drop")
    n_topics <- params_list[["n_topics"]]
    B <- params_list[["B"]]

    topic_df <- data.frame(topic = 1:(n_topics*B),
                          topic2 = rep(1:n_topics, B),
                          subtopic=rep(1:B, each=n_topics))

    out_df <- ompr::get_solution(model_result, x[g,t,r]) %>%
                dplyr::filter(.data$value>0) %>%
                dplyr::select("t", "r", "g") %>%
                dplyr::rename("group"="g", "topic"="t", "rep"="r") %>%
                dplyr::left_join(group_sizes, by=c("group"=group_names)) %>%
                dplyr::left_join(topic_df, by="topic") %>%
                dplyr::select("topic2", "subtopic", "rep", "group", "size")  %>%
                dplyr::group_by(.data$topic2, .data$subtopic) %>%
                dplyr::mutate(rep=match(.data$rep, unique(.data$rep)))
    out_df <- as.data.frame(dplyr::ungroup(out_df))

    return(out_df)
  } else {
    stop("assignment argument should be either 'diversity' or 'preference'.")
  }
}


#' Convert PhD solver allocation to manual-style wide table
#'
#' Creates one row per student and one column per course-role pair, with units
#' allocated by the solver.
#'
#' @param model_result Result object from `ompr::solve_model()` for the PhD model.
#' @param student_df A data frame that contains student name information. Every
#'   row is a unique student.
#' @param course_codes Character vector of course codes in the same order as
#'   `p_mat` columns (and `d_mat` rows).
#' @param name_col Student name column name in `student_df`.
#'
#' @returns A data frame with columns:
#'   `Name`, then all `<course>-t`, all `<course>-g`, all `<course>-e`.
#'
#' @export
assign_job <- function(model_result,
                       student_df,
                       course_codes,
                       name_col = "Name") {
  if (!name_col %in% names(student_df)) {
    stop("name_col not found in student_df.")
  }

  Ns <- nrow(student_df)
  course_codes <- as.character(course_codes)
  Nj <- length(course_codes)

  alloc <- ompr::get_solution(model_result, X[i, j, r])
  alloc <- alloc[alloc$value > 1e-8, c("i", "j", "r", "value"), drop = FALSE]

  ta_mat <- matrix(0, nrow = Ns, ncol = Nj)
  gr_mat <- matrix(0, nrow = Ns, ncol = Nj)
  e_mat  <- matrix(0, nrow = Ns, ncol = Nj)

  if (nrow(alloc) > 0) {
    for (k in seq_len(nrow(alloc))) {
      i <- alloc$i[k]
      j <- alloc$j[k]
      r <- alloc$r[k]
      v <- alloc$value[k]

      if (r == 1) ta_mat[i, j] <- ta_mat[i, j] + v
      if (r == 2) gr_mat[i, j] <- gr_mat[i, j] + v
      if (r == 3) e_mat[i, j]  <- e_mat[i, j] + v
    }
  }

  ta_df <- as.data.frame(matrix(as.integer(round(ta_mat)), nrow = Ns, ncol = Nj))
  gr_df <- as.data.frame(matrix(as.integer(round(gr_mat)), nrow = Ns, ncol = Nj))
  e_df  <- as.data.frame(matrix(as.integer(round(e_mat)),  nrow = Ns, ncol = Nj))

  names(ta_df) <- paste0(course_codes, "-t")
  names(gr_df) <- paste0(course_codes, "-g")
  names(e_df)  <- paste0(course_codes, "-e")

  out <- cbind(
    data.frame(Name = student_df[[name_col]], stringsAsFactors = FALSE),
    ta_df,
    gr_df,
    e_df
  )

  rownames(out) <- NULL
  out
}
