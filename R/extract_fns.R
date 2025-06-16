#' Extract student information
#'
#' Converts a dataframe with information on students to a list of parameters. This
#' list forms one half of the inputs to prepare_model(). The other half comes from
#' extract_params_yaml.
#'
#' @param dframe A dataframe with one row for each student. The columns could
#'   possibly contain demographic variables, an overall skill measure, and a
#'   column indicating self-formed groups. It is best to have an id column to
#'   identify each student.
#' @param assignment Character string indicating the type of model that this
#'   dataset is for. The argument is either 'preference' or 'diversity'. Partial
#'   matching is fine.
#' @param demographic_cols A set of integers indicating the columns
#'   corresponding to demographic information, e.g. major, year of study,
#'   gender, etc. This argument is only used by the diversity-based assignment.
#' @param skills A numeric measure of overall skill level (higher means more
#'   skilled). This argument is only used by the diversity-based assignment.
#'   This argument can be set to NULL. If this is done, then the model used only
#'   maximises the diversity.
#' @param self_formed_groups An integer column that identifies the self-formed
#'   groups, submitted by students.
#' @param pref_mat The preference matrix with dimensions equal to the
#'   num of groups x B*T, where T is the number of topics and B is the number
#'   of sub-groups per topic. This argument is only used in the preference-based
#'   assignment. See the Details section for more information.
#'
#' @details
#' For the diversity-based assignment, the demographic variables are converted
#' into an NxN dissimilarity matrix. By default, the dissimilarity metric used
#' is the Gower distance [cluster::daisy()].
#'
#' For the preference-based assignment, the preference matrix indicates the
#' preference that each group has for the project topics. For this model,
#' each topic has possibly B sub-groups. The number of columns of this matrix
#' must be B*T. Suppose there are T=3 topics and B=2 sub-groups per topic. Then
#' the order of the sub-topics should be:
#'
#' T1S1, T2S1, T3S1, T1S2, T2S2, and T3S2.
#'
#' Note that higher values in the preference matrix reflect a greater preference for a
#' particular topic-subtopic combination, since the objective function is set to be
#' maximised.
#'
#' @returns For the diversity-based assignment model, this function returns a
#' list containing:
#'
#' * N: number of students
#' * G: number of self-formed groups
#' * m: a (student x groups) matrix, indicating group membership for each student.
#' * d: dissimilarity matrix, NxN
#' * s: skills vector for each individual student (possibly NULL)
#'
#' For the preference-based assignment model, this function returns a list
#' containing:
#'
#' * N: number of students
#' * G: number of self-formed groups
#' * m: a (student x groups) matrix, indicating group membership for each student.
#' * n: a vector of length G, with the number of students in each self-formed group.
#' * p: The preference matrix from the input argument.
#'
#' @export
#'
extract_student_info <- function(dframe, assignment=c("diversity", "preference"),
                                 self_formed_groups,
                                 demographic_cols, skills,
                                 pref_mat) {
  assignment <- match.arg(assignment)
  if(assignment == "diversity") {
    self_formed_groups_vec <- dframe[[self_formed_groups]]
    N <- NROW(dframe)
    G <- max(self_formed_groups_vec)

    # form student x G matrix
    m <- matrix(0, nrow = N, ncol = G)
    for (i in 1:N) {
      m[i, self_formed_groups_vec[i]] <- 1
    }

    # form dissimilarity mx
    if(length(demographic_cols) == 1) {
      if(is.character(dframe[[demographic_cols]]))
        dframe[[demographic_cols]] <- as.factor(dframe[[demographic_cols]])
        d_cols <- dframe[demographic_cols]
    } else {
      d_cols <- dframe[demographic_cols]
      char_cols <- which(sapply(d_cols, is.character))
      for (col_id in char_cols) {
        d_cols[[col_id]] <- as.factor(d_cols[[col_id]])
      }
    }
    d <- as.matrix(cluster::daisy(d_cols, metric="gower"))

    # extract skills
    if(is.null(skills)) {
      s <- NULL
    } else {
      s <- dframe[[skills]]
    }

    return(list(N=N, G=G, m=m, d=d, s=s))

  } else if (assignment == "preference") {
    N <- NROW(dframe)
    G <- max(dframe[[self_formed_groups]])
    self_formed_groups_vec <- dframe[[self_formed_groups]]

    # form student x G matrix
    m <- matrix(0, nrow = N, ncol = G)
    for (i in 1:N) {
      m[i, self_formed_groups_vec[i]] <- 1
    }
    # number of students per group
    n <- colSums(m)

    # checks on matrix dimension?
    if(missing(pref_mat)){
      stop("Missing preference matrix")
    } else {
      if(NROW(pref_mat) != G){
        stop("Incorrect number of rows in preference matrix; does not match groups in dataframe\n")
      }
    }


    return(list(N=N, G=G, m=m, n=n, p=pref_mat))
  } else {
    stop("assignment argument should be either 'diversity' or 'preference'.\n")
  }

}


#' Extract parameters from a YAML file
#'
#' The remaining parameters for the models are retrieved from a YAML file, so as
#' not to clutter the argument list for [extract_student_info()].
#'
#' @param fname A YAML file containing the remaining parameters.
#' @param assignment Character string indicating the type of model that this
#'   dataset is for. The argument is either 'preference' or 'diversity'. Partial
#'   matching is fine.
#'
#' @returns  For the diversity+skill-based assignment, this function returns a
#' list containing:
#'
#' * n_topics: the number of topics
#' * R: the optimally desired number of repetitions per topic
#' * nmin: the minimum number of students per topic,
#' * nmax: the maximum number of students per topic,
#' * rmin: the minimum number of repetitions per topic,
#' * rmax: the maximum number of repetitions per topic.
#'
#' For the preference-based assignment, this function returns a list containing:
#'
#' * n_topics: the number of topics
#' * R: the optimally desired number of repetitions per topic
#' * nmin: the minimum number of students per topic,
#' * nmax: the maximum number of students per topic,
#' * rmin: the minimum number of repetitions per topic,
#' * rmax: the maximum number of repetitions per topic.
#'
#'
#' @export
extract_params_yaml <- function(fname, assignment=c("diversity", "preference")) {

  in_params <- yaml::yaml.load_file(fname)
  assignment <- match.arg(assignment)
  if(assignment == "diversity") {
    # MINIMUM NUMBER OF STUDENTS PER TOPIC
    nmin <- matrix(data=in_params$nmin,
                   nrow=in_params$n_topics,
                   ncol=in_params$R, byrow=TRUE)

    # MAXIMUM NUMBER OF STUDENTS PER TOPIC
    nmax <- matrix(data=in_params$nmax,
                   nrow=in_params$n_topics,
                   ncol=in_params$R, byrow=TRUE)

    return( list(n_topics = in_params$n_topics,
                 R = in_params$R,
                 nmin=nmin, nmax = nmax,
                 rmin=in_params$rmin,  rmax=in_params$rmax)
    )
  } else if (assignment == "preference"){
    # message("incomplete")
    B <- in_params$B

    # MINIMUM NUMBER OF STUDENTS PER TOPIC
    nmin <- matrix(data=in_params$nmin,
                   nrow=B*in_params$n_topics,
                   ncol=in_params$R, byrow=TRUE)

    # MAXIMUM NUMBER OF STUDENTS PER TOPIC
    nmax <- matrix(data=in_params$nmax,
                   nrow=B*in_params$n_topics,
                   ncol=in_params$R, byrow=TRUE)

    return(list(n_topics = in_params$n_topics, B = B,
                R = in_params$R,
                nmin=nmin, nmax = nmax,
                rmin=in_params$rmin, rmax=in_params$rmax
                )
          )
  } else {
    stop("assignment model not found: should be 'diversity' or 'preference'.")
  }

}
