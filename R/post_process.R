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

    return(dplyr::ungroup(out_df))
  } else {
    stop("assignment argument should be either 'diversity' or 'preference'.")
  }
}
