#' Extract student information
#'
#' Converts a dataframe with information on students to a list of parameters. This
#' list forms one half of the inputs to prepare_model(). The other half comes from
#' extract_params_yaml.
#'
#' @param dframe A dataframe with one row for each student. The columns could
#'        possibly contain demographic variables, an overall skill measure, and
#'        a column indicating self-formed groups. There must be a column named
#'        id, for use by other functions in this package.
#' @param demographic_cols A set of integers indicating the columns corresponding
#'        to demographic information, e.g. major, year of study, gender, etc.
#' @param skills A numeric measure of overall skill level (higher means more
#'        skilled).
#' @param self_formed_groups An integer column that identifies the self-formed
#'        groups, submitted by students.
#'
#' @details
#' The demographic variables are converted into an NxN dissimilarity matrix.
#'
#'
#' @returns A list containing:
#'
#'   N - number of students
#'   G - number of self-formed groups
#'   m - a student x groups matrix, indicating which student is in which
#'       group
#'   d - dissimilarity matrix
#'   s - skills vector for each individual student
#'
#' @export
#'
extract_student_info <- function(dframe, demographic_cols,
                                 skills, self_formed_groups) {
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
  s <- dframe[[skills]]

  list(N=N, G=G, m=m, d=d, s=s)
}


#' Extract parameters from a YAML file
#'
#' The remaining parameters for model 1 are retrieved from a YAML file, so as not
#' to clutter the argument list for extract_student_info.
#'
#' @param fname A YAML file containing the remaining parameters.
#'
#' @returns A list containing:
#'   T    - the number of topics
#'   R    - the optimally desired number of repetitions per topic
#'   nmim - the minimum number of students per topic
#'   nmax - the maximum number of students per topic
#'   rmin - the minimum number of repetitions per topic
#'   rmax - the maximum number of repetitions per topic.
#'
#' @export
extract_params_yaml <- function(fname) {
  in_params <- yaml::yaml.load_file(fname)

  # MINIMUM NUMBER OF STUDENTS PER TOPIC
  nmin <- matrix(data=in_params$nmin,
                 nrow=in_params$n_studs,
                 ncol=in_params$R, byrow=TRUE)

  # MAXIMUM NUMBER OF STUDENTS PER TOPIC
  nmax <- matrix(data=in_params$nmax,
                 nrow=in_params$n_studs,
                 ncol=in_params$R, byrow=TRUE)

  list(T = in_params$T,
       R = in_params$R,
       nmin=nmin, nmax = nmax,
       rmin=in_params$rmin,
       rmax=in_params$rmax
       )
}
