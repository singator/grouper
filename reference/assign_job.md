# Convert PhD solver allocation to manual-style wide table

Creates one row per student and one column per course-role pair, with
units allocated by the solver.

## Usage

``` r
assign_job(model_result, student_df, course_codes, name_col = "Name")
```

## Arguments

- model_result:

  Result object from
  [`ompr::solve_model()`](https://rdrr.io/pkg/ompr/man/solve_model.html)
  for the PhD model.

- student_df:

  A data frame that contains student name information. Every row is a
  unique student.

- course_codes:

  Character vector of course codes in the same order as `p_mat` columns
  (and `d_mat` rows).

- name_col:

  Student name column name in `student_df`.

## Value

A data frame with columns: `Name`, then all `<course>-t`, all
`<course>-g`, all `<course>-e`.
