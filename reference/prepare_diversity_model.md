# Prepare the diversity-based assignment model

Prepare the diversity-based assignment model

## Usage

``` r
prepare_diversity_model(df_list, yaml_list, w1 = 0.5, w2 = 0.5)
```

## Arguments

- df_list:

  The output list from
  [`extract_student_info()`](https://Zimmy313.github.io/grouper/reference/extract_student_info.md)
  for `assignment = "diversity"`.

- yaml_list:

  The output list from
  [`extract_params_yaml()`](https://Zimmy313.github.io/grouper/reference/extract_params_yaml.md)
  for `assignment = "diversity"`.

- w1, w2:

  Numeric values between 0 and 1. Should sum to 1. These weights
  correspond to the importance given to the diversity- and skill-based
  portions in the objective function.

## Value

An ompr model.
