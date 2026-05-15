# Extract student information

Converts a dataframe with information on students to a list of
parameters. This list forms one half of the inputs to prepare_model().
The remaining model parameters can come from
[`extract_params_yaml()`](https://Zimmy313.github.io/grouper/reference/extract_params_yaml.md)
or be supplied directly to
[`prepare_model()`](https://Zimmy313.github.io/grouper/reference/prepare_model.md)
for non-YAML workflows.

## Usage

``` r
extract_student_info(
  dframe,
  assignment = c("diversity", "preference"),
  self_formed_groups,
  demographic_cols,
  skills,
  pref_mat,
  d_mat
)
```

## Arguments

- dframe:

  A dataframe with one row for each student. The columns could possibly
  contain demographic variables, an overall skill measure, and a column
  indicating self-formed groups. It is best to have an id column to
  identify each student.

- assignment:

  Character string indicating the type of model that this dataset is
  for. The argument is either 'preference' or 'diversity'. Partial
  matching is fine.

- self_formed_groups:

  An integer column that identifies the self-formed groups, submitted by
  students.

- demographic_cols:

  A set of integers indicating the columns corresponding to demographic
  information, e.g. major, year of study, gender, etc. This argument is
  only used by the diversity-based assignment.

- skills:

  A numeric measure of overall skill level (higher means more skilled).
  This argument is only used by the diversity-based assignment. This
  argument can be set to NULL. If this is done, then the model used only
  maximises the diversity.

- pref_mat:

  The preference matrix with dimensions equal to the num of groups x
  B\*T, where T is the number of topics and B is the number of
  sub-groups per topic. This argument is only used in the
  preference-based assignment. See the Details section for more
  information.

- d_mat:

  The dissimilarity matrix with number of rows equal to the number of
  students. This matrix should be symmetric, with diagonals equal to 0.
  This argument is only used in the diversity-based assignment. If it is
  not provided, the "Gower" distance from the cluster package is used.
  If this is provided, then demographic_cols is ignored.

## Value

For the diversity-based assignment model, this function returns a list
containing:

- N: number of students

- G: number of self-formed groups

- m: a (student x groups) matrix, indicating group membership for each
  student.

- d: dissimilarity matrix, NxN

- s: skills vector for each individual student (possibly NULL)

For the preference-based assignment model, this function returns a list
containing:

- N: number of students

- G: number of self-formed groups

- m: a (student x groups) matrix, indicating group membership for each
  student.

- n: a vector of length G, with the number of students in each
  self-formed group.

- p: The preference matrix from the input argument.

## Details

For the diversity-based assignment, the demographic variables are
converted into an NxN dissimilarity matrix. By default, the
dissimilarity metric used is the Gower distance
[`cluster::daisy()`](https://rdrr.io/pkg/cluster/man/daisy.html).

For the preference-based assignment, the preference matrix indicates the
preference that each group has for the project topics. For this model,
each topic has possibly B sub-groups. The number of columns of this
matrix must be B\*T. Suppose there are T=3 topics and B=2 sub-groups per
topic. Then the order of the sub-topics should be:

T1S1, T2S1, T3S1, T1S2, T2S2, and T3S2.

Note that higher values in the preference matrix reflect a greater
preference for a particular topic-subtopic combination, since the
objective function is set to be maximised.
