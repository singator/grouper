#' Initialise optimisation model
#'
#' @param df_list The output list from extract_student_info().
#' @param yaml_list The output list from extract_params_yaml().
#' @param assignment Character string indicating the type of model that this
#'   dataset is for. The argument is either 'preference' or 'diversity'. Partial
#'   matching is fine.
#' @param w1,w2 Numeric values between 0 and 1. Should sum to 1. These weights
#'   correspond to the importance given to the diversity- and skill-based
#'   portions in the objective function.
#'
#' @returns An ompr model.
#' @export
#'
prepare_model <- function(df_list, yaml_list,
                          assignment=c("diversity", "preference"),
                          w1=0.5, w2=0.5) {

  assignment <- match.arg(assignment)

  if(assignment == "diversity") {
    N <- df_list$N
    G <- df_list$G
    m <- df_list$m
    d <- df_list$d
    s <- df_list$s

    n_topics <- yaml_list$n_topics
    R <- yaml_list$R
    nmin <- yaml_list$nmin
    nmax <- yaml_list$nmax
    rmin <- yaml_list$rmin
    rmax <- yaml_list$rmax

    model <- ompr::MIPModel() %>%
    # DEFINE DECISION VARIABLES
    ompr::add_variable(x[g,t,r], g=1:G, t=1:n_topics, r=1:R, type="binary") %>%
    ompr::add_variable(z[i,j,t,r], i=1:(N-1), j=(i+1):N, t=1:n_topics, r=1:R, type="binary") %>%
    ompr::add_variable(a[t,r], t=1:n_topics, r=1:R, type="binary")

    if(is.null(s)) {
      model <- model %>%
      # DEFINE OBJECTIVE FUNCTION
      ompr::set_objective(
      # MAXIMISE DIVERSITY
        ompr::sum_over(z[i,j,t,r]*d[i,j], i=1:(N-1), j=(i+1):N, t=1:n_topics, r=1:R), "max") %>%
      # DEFINE CONSTRAINTS (EACH GROUP ASSIGNED A TOPIC-REP)
      ompr::add_constraint(ompr::sum_over(x[g,t,r], t=1:n_topics, r=1:R)==1, g=1:G) %>%
      # DEFINE CONSTRAINTS (WHETHER 2 STUDENTS IN SAME TOPIC-REP)
      ompr::add_constraint(z[i,j,t,r]<=ompr::sum_over(m[i,g]*x[g,t,r], g=1:G), i=1:(N-1), j=(i+1):N, t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(z[i,j,t,r]<=ompr::sum_over(m[j,g]*x[g,t,r], g=1:G), i=1:(N-1), j=(i+1):N, t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(z[i,j,t,r]>=ompr::sum_over(m[i,g]*x[g,t,r], g=1:G) + ompr::sum_over(m[j,g]*x[g,t,r], g=1:G)-1, i=1:(N-1), j=(i+1):N, t=1:n_topics, r=1:R) %>%
      # DEFINE CONSTRAINTS (MIN AND MAX NO. OF REPETITIONS PER TOPIC)
      ompr::add_constraint(a[t,r]>=x[g,t,r], g=1:G, t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(a[t,r]<=ompr::sum_over(x[g,t,r], g=1:G), t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(ompr::sum_over(a[t,r], r=1:R)>=rmin, t=1:n_topics) %>%
      ompr::add_constraint(ompr::sum_over(a[t,r], r=1:R)<=rmax, t=1:n_topics) %>%
      # DEFINE CONSTRAINTS (MIN AND MAX NO. OF STUDENTS PER TOPIC-REPETITION)
      ompr::add_constraint(ompr::sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)>=a[t,r]*nmin[t,r], t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(ompr::sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)<=a[t,r]*nmax[t,r], t=1:n_topics, r=1:R)
    } else {
      model <- model %>%
      ompr::add_variable(smin, type="continuous", lb=0) %>%
      ompr::add_variable(smax, type="continuous", lb=0) %>%
      # DEFINE OBJECTIVE FUNCTION
      ompr::set_objective(
      # MAXIMISE DIVERSITY
        w1*ompr::sum_over(z[i,j,t,r]*d[i,j], i=1:(N-1), j=(i+1):N, t=1:n_topics, r=1:R)+
          # MINIMIZE SKILL VARIABILITY
          w2*(smin-smax), "max") %>%
      # DEFINE CONSTRAINTS (EACH GROUP ASSIGNED A TOPIC-REP)
      ompr::add_constraint(ompr::sum_over(x[g,t,r], t=1:n_topics, r=1:R)==1, g=1:G) %>%
      # DEFINE CONSTRAINTS (WHETHER 2 STUDENTS IN SAME TOPIC-REP)
      ompr::add_constraint(z[i,j,t,r]<=ompr::sum_over(m[i,g]*x[g,t,r], g=1:G), i=1:(N-1), j=(i+1):N, t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(z[i,j,t,r]<=ompr::sum_over(m[j,g]*x[g,t,r], g=1:G), i=1:(N-1), j=(i+1):N, t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(z[i,j,t,r]>=ompr::sum_over(m[i,g]*x[g,t,r], g=1:G) + ompr::sum_over(m[j,g]*x[g,t,r], g=1:G)-1, i=1:(N-1), j=(i+1):N, t=1:n_topics, r=1:R) %>%
      # DEFINE CONSTRAINTS (MIN AND MAX NO. OF REPETITIONS PER TOPIC)
      ompr::add_constraint(a[t,r]>=x[g,t,r], g=1:G, t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(a[t,r]<=ompr::sum_over(x[g,t,r], g=1:G), t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(ompr::sum_over(a[t,r], r=1:R)>=rmin, t=1:n_topics) %>%
      ompr::add_constraint(ompr::sum_over(a[t,r], r=1:R)<=rmax, t=1:n_topics) %>%
      # DEFINE CONSTRAINTS (MIN AND MAX NO. OF STUDENTS PER TOPIC-REPETITION)
      ompr::add_constraint(ompr::sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)>=a[t,r]*nmin[t,r], t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(ompr::sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)<=a[t,r]*nmax[t,r], t=1:n_topics, r=1:R) %>%
      # DEFINE CONSTRAINTS (SKILL VARIABILITY)
      ompr::add_constraint(ompr::sum_over(m[i,g]*x[g,t,r]*s[i], i=1:N, g=1:G)>=smin, t=1:n_topics, r=1:R) %>%
      ompr::add_constraint(ompr::sum_over(m[i,g]*x[g,t,r]*s[i], i=1:N, g=1:G)<=smax, t=1:n_topics, r=1:R)
    }

    return(model)

  } else if(assignment == "preference") {
    # message("incomplete function")
    N <- df_list$N
    G <- df_list$G
    m <- df_list$m
    n <- df_list$n
    p <- df_list$p

    T <- yaml_list$n_topics
    B <- yaml_list$B
    R <- yaml_list$R
    nmin <- yaml_list$nmin
    nmax <- yaml_list$nmax
    rmin <- yaml_list$rmin
    rmax <- yaml_list$rmax

    model <- ompr::MIPModel() %>%
    # DEFINE DECISION VARIABLES
    ompr::add_variable(x[g,t,r], g=1:G, t=1:(B*T), r=1:R, type="binary") %>%
    ompr::add_variable(a[t,r], t=1:(B*T), r=1:R, type="binary") %>%
    # DEFINE OBJECTIVE FUNCTION
    ompr::set_objective(ompr::sum_over(x[g,t,r]*n[g]*p[g,t], g=1:G, t=1:(B*T), r=1:R), "max") %>%
    # DEFINE CONSTRAINTS (EACH GROUP ASSIGNED A TOPIC-REP)
    ompr::add_constraint(ompr::sum_over(x[g,t,r], t=1:(B*T), r=1:R)==1, g=1:G) %>%
    # DEFINE CONSTRAINTS (MIN NO. OF REPETITIONS PER TOPIC)
    ompr::add_constraint(a[t,r]>=x[g,t,r], g=1:G, t=1:(B*T), r=1:R) %>%
    ompr::add_constraint(a[t,r]<=ompr::sum_over(x[g,t,r], g=1:G), t=1:(B*T), r=1:R) %>%
    ompr::add_constraint(ompr::sum_over(a[t,r], r=1:R)>=rmin, t=1:T) %>%
    # DEFINE CONSTRAINTS (BALANCED NO. OF REPETITIONS FOR SUBGROUPS)
    ompr::add_constraint(ompr::sum_over(a[t,r], r=1:R)==ompr::sum_over(a[(b*T+t),r], r=1:R), t=1:T, b=min(1,B-1):max(0,B-1)) %>%
    # DEFINE CONSTRAINTS (MIN AND MAX NO. OF STUDENTS PER TOPIC-REPETITION)
    ompr::add_constraint(ompr::sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)>=a[t,r]*nmin[t,r], t=1:(B*T), r=1:R) %>%
    ompr::add_constraint(ompr::sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)<=a[t,r]*nmax[t,r], t=1:(B*T), r=1:R)

    return(model)
  } else {
    stop("assignment model not found: should be 'diversity' or 'preference'.")
  }

}
