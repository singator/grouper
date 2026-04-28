#' Prepare the diversity-based assignment model
#'
#' @param df_list The output list from [extract_student_info()] for
#'   `assignment = "diversity"`.
#' @param yaml_list The output list from [extract_params_yaml()] for
#'   `assignment = "diversity"`.
#' @param w1,w2 Numeric values between 0 and 1. Should sum to 1. These weights
#'   correspond to the importance given to the diversity- and skill-based
#'   portions in the objective function.
#'
#' @returns An ompr model.
#' @export
prepare_diversity_model <- function(df_list, yaml_list, w1 = 0.5, w2 = 0.5) {
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

  model
}


#' Prepare the preference-based assignment model
#'
#' @param df_list The output list from [extract_student_info()] for
#'   `assignment = "preference"`.
#' @param yaml_list The output list from [extract_params_yaml()] for
#'   `assignment = "preference"`.
#'
#' @returns An ompr model.
#' @export
prepare_preference_model <- function(df_list, yaml_list) {
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

  ompr::MIPModel() %>%
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
}



#' Prepare the PhD workload allocation model
#'
#' Builds a mixed-integer optimisation model for assigning TA, GR, and E units
#' across students and courses.
#'
#' @param df_list A list of model inputs, typically from [extract_phd_info()].
#'   Required elements are:
#'   \itemize{
#'   \item \code{Ns}: number of students
#'   \item \code{Nj}: number of courses
#'   \item \code{P}: preference matrix \code{[i, j]}
#'   \item \code{d}: demand matrix \code{[j, r]} where \code{r = 1:3} for TA, GR, E
#'   \item \code{s}: seniority vector (offset form; e.g. year - 2)
#'   \item \code{t1}: past TA workload vector
#'   \item \code{g1}: past GR workload vector
#'   }
#' @param t_max_y1 Maximum current-semester TA load for Year-1 students
#'   (\code{s == -1}) before slack is used.
#' @param e_max Optional upper bound on per-student E units in current semester.
#' @param ta_min,ta_max Optional lower/upper bounds on per-student TA units in
#'   current semester.
#' @param gr_min,gr_max Optional lower/upper bounds on per-student GR units in
#'   current semester.
#' @param e_min Optional lower bound on per-student E units in current semester.
#' @param alpha Objective weight on TA spread \code{(Tmax - Tmin)}.
#' @param beta Objective weight on TA preference term.
#' @param phi Objective weight on seniority-weighted E term.
#' @param rho Objective weight on Year-1 TA slack penalties.
#' @param C Semester workload capacity per student. The model fixes annual
#'   workload at \code{2 * C} via \code{T_i + G_i + e_i^(2) == 2 * C}.
#'   Default is \code{4}.
#'
#' @details
#' Index alignment is critical: \code{P[i, j]}, \code{d[j, ]}, \code{s[i]},
#' \code{t1[i]}, and \code{g1[i]} must refer to the same student/course ordering.
#'
#' @return An \code{ompr} model object ready for \code{ompr::solve_model()}.
#' @export
prepare_phd_model <- function(df_list, t_max_y1 = 1, e_max = NULL,
                              ta_min = NULL, ta_max = NULL,
                              gr_min = NULL, gr_max = NULL,
                              e_min = NULL,
                              alpha = 2, beta = 1, phi = 1, rho = 10,
                              C = 4) {
  # keep role order fixed: 1 = TA, 2 = GR, 3 = E

  # extract inputs
  Ns <- df_list$Ns
  Nj <- df_list$Nj
  P  <- df_list$P   # preference matrix [i, j]
  d  <- df_list$d   # demand matrix [j, r], r = 1:3 for TA, GR, E
  s  <- df_list$s   # seniority
  t1 <- df_list$t1  # previous semester TA workload
  g1 <- df_list$g1  # previous semester GR workload

  idx_y1     <- which(s == -1)
  idx_non_y1 <- which(s >= 0)

  # check optional workload bounds
  validate_optional_bound <- function(x, nm) {
    if (!is.null(x) && (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0)) {
      stop(nm, " must be NULL or a single non-negative number.")
    }
  }
  validate_optional_bound(e_max, "e_max")
  validate_optional_bound(ta_min, "ta_min")
  validate_optional_bound(ta_max, "ta_max")
  validate_optional_bound(gr_min, "gr_min")
  validate_optional_bound(gr_max, "gr_max")
  validate_optional_bound(e_min, "e_min")

  if (!is.null(ta_min) && !is.null(ta_max) && ta_min > ta_max) {
    stop("ta_min cannot be greater than ta_max.")
  }
  if (!is.null(gr_min) && !is.null(gr_max) && gr_min > gr_max) {
    stop("gr_min cannot be greater than gr_max.")
  }
  if (!is.null(e_min) && !is.null(e_max) && e_min > e_max) {
    stop("e_min cannot be greater than e_max.")
  }

  model <- ompr::MIPModel() %>%

    # assignment vars
    ompr::add_variable(
      X[i, j, r],
      i = 1:Ns, j = 1:Nj, r = 1:3,
      type = "integer", lb = 0
    ) %>%

    # spread vars for yearly TA among non-Y1 students
    ompr::add_variable(Tmax, type = "continuous", lb = 0) %>%
    ompr::add_variable(Tmin, type = "continuous", lb = 0) %>%

    # slack for Y1 TA soft bound
    ompr::add_variable(w[i], i = idx_y1, type = "continuous", lb = 0) %>%

    # objective
    ompr::set_objective(
      alpha * (Tmax - Tmin) -
        beta * ompr::sum_over(P[i, j] * X[i, j, 1], i = 1:Ns, j = 1:Nj) -
        phi  * ompr::sum_over(s[i] * X[i, j, 3], i = 1:Ns, j = 1:Nj) +
        rho  * ompr::sum_over(w[i], i = idx_y1),
      sense = "min"
    ) %>%

    # demand satisfaction for each job and role
    ompr::add_constraint(
      ompr::sum_over(X[i, j, r], i = 1:Ns) == d[j, r],
      j = 1:Nj, r = 1:3
    ) %>%

    # yearly TA spread constraints for non-Year-1 students:
    # T_i = t1[i] + sum_j X[i,j,TA]
    ompr::add_constraint(
      t1[i] + ompr::sum_over(X[i, j, 1], j = 1:Nj) <= Tmax,
      i = idx_non_y1
    ) %>%
    ompr::add_constraint(
      t1[i] + ompr::sum_over(X[i, j, 1], j = 1:Nj) >= Tmin,
      i = idx_non_y1
    ) %>%

    # annual workload cap from semester capacity C:
    # T_i + G_i + e_i^(2) == 2 * C
    ompr::add_constraint(
      (t1[i] + ompr::sum_over(X[i, j, 1], j = 1:Nj)) +
      (g1[i] + ompr::sum_over(X[i, j, 2], j = 1:Nj)) +
               ompr::sum_over(X[i, j, 3], j = 1:Nj) == 2 * C,
      i = 1:Ns
    ) %>%

    # Year 1 soft TA bound on current semester TA workload
    ompr::add_constraint(
      ompr::sum_over(X[i, j, 1], j = 1:Nj) <= t_max_y1 + w[i],
      i = idx_y1
    )

  # optional per-student lower/upper bound on TA units
  if (!is.null(ta_min)) {
    model <- model %>%
      ompr::add_constraint(
        ompr::sum_over(X[i, j, 1], j = 1:Nj) >= ta_min,
        i = 1:Ns
      )
  }
  if (!is.null(ta_max)) {
    model <- model %>%
      ompr::add_constraint(
        ompr::sum_over(X[i, j, 1], j = 1:Nj) <= ta_max,
        i = 1:Ns
      )
  }

  # optional per-student lower/upper bound on GR units
  if (!is.null(gr_min)) {
    model <- model %>%
      ompr::add_constraint(
        ompr::sum_over(X[i, j, 2], j = 1:Nj) >= gr_min,
        i = 1:Ns
      )
  }
  if (!is.null(gr_max)) {
    model <- model %>%
      ompr::add_constraint(
        ompr::sum_over(X[i, j, 2], j = 1:Nj) <= gr_max,
        i = 1:Ns
      )
  }

  # optional per-student lower/upper bound on E units
  if (!is.null(e_min)) {
    model <- model %>%
      ompr::add_constraint(
        ompr::sum_over(X[i, j, 3], j = 1:Nj) >= e_min,
        i = 1:Ns
      )
  }
  if (!is.null(e_max)) {
    model <- model %>%
      ompr::add_constraint(
        ompr::sum_over(X[i, j, 3], j = 1:Nj) <= e_max,
        i = 1:Ns
      )
  }

  model
}


#' Initialise optimisation model (wrapper)
#'
#' @param df_list Model input list.
#' @param yaml_list Parameter list from [extract_params_yaml()]. Required for
#'   `assignment = "diversity"` and `assignment = "preference"`. Ignored for
#'   `assignment = "phd"`.
#' @param assignment Character string indicating model type. Must be one of
#'   `"diversity"`, `"preference"`, or `"phd"`.
#' @param w1,w2 Numeric values between 0 and 1. Should sum to 1. Used only for
#'   `assignment = "diversity"`.
#' @param ... Additional arguments passed to [prepare_phd_model()] when
#'   `assignment = "phd"`.
#'
#' @returns An ompr model.
#' @export
prepare_model <- function(df_list, yaml_list = NULL,
                          assignment = c("diversity", "preference", "phd"),
                          w1 = 0.5, w2 = 0.5, ...) {
  assignment <- match.arg(assignment)

  if (assignment == "diversity") {
    if (is.null(yaml_list)) {
      stop("yaml_list must be provided for assignment = 'diversity'.")
    }
    return(prepare_diversity_model(df_list, yaml_list, w1 = w1, w2 = w2))
  }

  if (assignment == "preference") {
    if (is.null(yaml_list)) {
      stop("yaml_list must be provided for assignment = 'preference'.")
    }
    return(prepare_preference_model(df_list, yaml_list))
  }

  prepare_phd_model(df_list, ...)
}