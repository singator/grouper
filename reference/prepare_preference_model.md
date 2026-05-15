# Prepare the preference-based assignment model

Prepare the preference-based assignment model

## Usage

``` r
prepare_preference_model(df_list, yaml_list)
```

## Arguments

- df_list:

  The output list from
  [`extract_student_info()`](https://Zimmy313.github.io/grouper/reference/extract_student_info.md)
  for `assignment = "preference"`.

- yaml_list:

  The output list from
  [`extract_params_yaml()`](https://Zimmy313.github.io/grouper/reference/extract_params_yaml.md)
  for `assignment = "preference"`.

## Value

An ompr model.
