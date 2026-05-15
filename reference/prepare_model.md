# Initialise optimisation model (wrapper)

Initialise optimisation model (wrapper)

## Usage

``` r
prepare_model(
  df_list,
  yaml_list = NULL,
  assignment = c("diversity", "preference", "phd"),
  w1 = 0.5,
  w2 = 0.5,
  ...
)
```

## Arguments

- df_list:

  Model input list.

- yaml_list:

  Parameter list from
  [`extract_params_yaml()`](https://Zimmy313.github.io/grouper/reference/extract_params_yaml.md).
  Optional for `assignment = "diversity"` and
  `assignment = "preference"` for backward compatibility. If supplied,
  this list is used directly. Ignored for `assignment = "phd"`.

- assignment:

  Character string indicating model type. Must be one of `"diversity"`,
  `"preference"`, or `"phd"`.

- w1, w2:

  Numeric values between 0 and 1. Should sum to 1. Used only for
  `assignment = "diversity"`.

- ...:

  Additional arguments:

  - For `assignment = "diversity"` when `yaml_list` is `NULL`: supply
    `n_topics`, `R`, `nmin`, `nmax`, `rmin`, and `rmax`.

  - For `assignment = "preference"` when `yaml_list` is `NULL`: supply
    `n_topics`, `B`, `R`, `nmin`, `nmax`, `rmin`, and `rmax`.

  - For `assignment = "phd"`: passed to
    [`prepare_phd_model()`](https://Zimmy313.github.io/grouper/reference/prepare_phd_model.md).

## Value

An ompr model.
