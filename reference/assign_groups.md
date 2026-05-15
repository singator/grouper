# Assigns model result to the original data frame.

From the result of
[`ompr::solve_model()`](https://rdrr.io/pkg/ompr/man/solve_model.html),
this function attaches the derived groupings to the original dataframe
comprising students.

## Usage

``` r
assign_groups(
  model_result,
  assignment = c("diversity", "preference"),
  dframe,
  params_list,
  group_names
)
```

## Arguments

- model_result:

  The output solution objection.

- assignment:

  Character string indicating the type of model that this dataset is
  for. The argument is either 'preference' or 'diversity'. Partial
  matching is fine.

- dframe:

  The original dataframe used in
  [`extract_student_info()`](https://Zimmy313.github.io/grouper/reference/extract_student_info.md).

- params_list:

  The list of parameters from the YAML file, i.e. the output of
  [`extract_params_yaml()`](https://Zimmy313.github.io/grouper/reference/extract_params_yaml.md).
  This is only required for the preference-based assignment.

- group_names:

  A character string. It denotes the column name in the original
  dataframe containing the self-formed groups. Note that we need the
  string here, not the integer position, since we are going to join with
  it.

## Value

A data frame with the group assignments attached to the original group
composition dataframe.
