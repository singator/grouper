#' DBA Group Composition Data Example 001
#'
#' An example dataset to use with the diversity-based assignment model.
#'
#' @format ## `dba_gc_ex001`
#' A data frame with 4 rows and 4 columns.
#'
#' * id: the student id of each students, simply the integers 1 to 4.
#' * major: the primary major of each student.
#' * skill: the skill level of each student.
#' * groups: the self-formed groups submitted by each student. In this case,
#'           student is in his/her own group.
#'
#' @source This dataset was constructed by hand.
"dba_gc_ex001"

#' PBA Group Composition Data Example 002
#'
#' An example dataset to use with the preference-based assignment model.
#'
#' @format ## `pba_gc_ex002`
#' A data frame with 8 rows and 2 columns.
#'
#' * id: the student id of each students, simply the integers 1 to 8.
#' * grouping: the self-formed groups submitted by each student. In this case,
#'             each self-formed group is of size 2.
#'
#' @source This dataset was constructed by hand.
"pba_gc_ex002"

#' PBA Group Preference Data Example 002
#'
#' An example dataset to use with the preference-based assignment model.
#'
#' @format ## `pba_prefmat_ex002`
#' A matrix with 4 rows and 4 columns
#'
#' Each row represents the preferences of each self-formed group in the
#' dataset `pba_gc_ex002`.
#'
#' @source This dataset was constructed by hand.
"pba_prefmat_ex002"

#' DBA Group Composition Data Example 003
#'
#' An example dataset to use with the diversity-based assignment model. It is
#' used to demonstrate the use of a custom dissimilarity matrix.
#'
#' @format ## `dba_gc_ex003`
#' A matrix with 4 rows and 4 columns
#'
#' * id: the student id of each students, simply the integers 1 to 4.
#' * self_groups: The self-formed groups
#' * year, major: demographics used in computing dissimilarities
#'
#' @source This dataset was constructed by hand.
"dba_gc_ex003"

#' DBA Group Composition Data Example 004
#'
#' An example dataset to use with the diversity-based assignment model. It is
#' used to demonstrate the use of a vectors to indicate individual group size
#' constraints for specific topics.
#'
#' @format ## `dba_gc_ex004`
#' A matrix with 5 rows and 4 columns
#'
#' * id: the student id of each students, simply the integers 1 to 4.
#' * self_groups: The self-formed groups
#' * python: Python skill level - 1 is lowest, 3 is highest.
#'
#' @source This dataset was constructed by hand.
"dba_gc_ex004"
