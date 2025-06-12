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
                          dframe, group_names) {
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
    message("incomplete")
  } else {
    stop("assignment argument should be either 'diversity' or 'preference'.")
  }
}
