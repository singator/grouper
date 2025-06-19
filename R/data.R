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
