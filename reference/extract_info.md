# Extract model inputs (wrapper)

Wrapper around
[`extract_student_info()`](https://Zimmy313.github.io/grouper/reference/extract_student_info.md)
and
[`extract_phd_info()`](https://Zimmy313.github.io/grouper/reference/extract_phd_info.md).

## Usage

``` r
extract_info(assignment = c("diversity", "preference", "phd"), ...)
```

## Arguments

- assignment:

  Character string indicating model type. Must be one of `"diversity"`,
  `"preference"`, or `"phd"`.

- ...:

  Additional arguments for the underlying extraction functions. See
  Details.

## Value

A model input list from
[`extract_student_info()`](https://Zimmy313.github.io/grouper/reference/extract_student_info.md)
or
[`extract_phd_info()`](https://Zimmy313.github.io/grouper/reference/extract_phd_info.md).

## Details

Explicit argument guide by assignment:

- For `assignment = "diversity"`, `extract_info()` forwards `...` to
  [`extract_student_info()`](https://Zimmy313.github.io/grouper/reference/extract_student_info.md).

  Required arguments:

  - `dframe`

  - `self_formed_groups`

  - either:

    - `d_mat`, or

    - `demographic_cols`, so Gower dissimilarity is computed internally

  Optional arguments:

  - `skills`, which can be supplied or set to `NULL`

- For `assignment = "preference"`, `extract_info()` forwards `...` to
  [`extract_student_info()`](https://Zimmy313.github.io/grouper/reference/extract_student_info.md).

  Required arguments:

  - `dframe`

  - `self_formed_groups`

  - `pref_mat`

- For `assignment = "phd"`, `extract_info()` forwards `...` to
  [`extract_phd_info()`](https://Zimmy313.github.io/grouper/reference/extract_phd_info.md).

  Required arguments:

  - `student_df`

  - `p_mat`

  - `d_mat`

  Optional arguments:

  - `e_mode`, which uses the default from
    [`extract_phd_info()`](https://Zimmy313.github.io/grouper/reference/extract_phd_info.md)

  - `C`, which uses the default from
    [`extract_phd_info()`](https://Zimmy313.github.io/grouper/reference/extract_phd_info.md)

This wrapper does not parse YAML files. YAML-based parameter extraction
remains available via
[`extract_params_yaml()`](https://Zimmy313.github.io/grouper/reference/extract_params_yaml.md).
