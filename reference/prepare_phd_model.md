# Prepare the PhD workload allocation model

Builds a mixed-integer optimisation model for assigning TA, GR, and E
units across students and courses.

## Usage

``` r
prepare_phd_model(
  df_list,
  t_max_y1 = 1,
  e_max = NULL,
  ta_min = NULL,
  ta_max = NULL,
  gr_min = NULL,
  gr_max = NULL,
  e_min = NULL,
  alpha = 2,
  beta = 1,
  phi = 1,
  rho = 10,
  C = 4
)
```

## Arguments

- df_list:

  A list of model inputs, typically from
  [`extract_phd_info()`](https://Zimmy313.github.io/grouper/reference/extract_phd_info.md).
  Required elements are:

  - `Ns`: number of students

  - `Nj`: number of courses

  - `P`: preference matrix `[i, j]`

  - `d`: demand matrix `[j, r]` where `r = 1:3` for TA, GR, E

  - `s`: seniority vector (offset form; e.g. year - 2)

  - `t1`: past TA workload vector

  - `g1`: past GR workload vector

- t_max_y1:

  Maximum current-semester TA load for Year-1 students (`s == -1`)
  before slack is used.

- e_max:

  Optional upper bound on per-student E units in current semester.

- ta_min, ta_max:

  Optional lower/upper bounds on per-student TA units in current
  semester.

- gr_min, gr_max:

  Optional lower/upper bounds on per-student GR units in current
  semester.

- e_min:

  Optional lower bound on per-student E units in current semester.

- alpha:

  Objective weight on TA spread `(Tmax - Tmin)`.

- beta:

  Objective weight on TA preference term.

- phi:

  Objective weight on seniority-weighted E term.

- rho:

  Objective weight on Year-1 TA slack penalties.

- C:

  Semester workload capacity per student. The model fixes annual
  workload at `2 * C` via `T_i + G_i + e_i^(2) == 2 * C`. Default is
  `4`.

## Value

An `ompr` model object ready for
[`ompr::solve_model()`](https://rdrr.io/pkg/ompr/man/solve_model.html).

## Details

Index alignment is critical: `P[i, j]`, `d[j, ]`, `s[i]`, `t1[i]`, and
`g1[i]` must refer to the same student/course ordering.
