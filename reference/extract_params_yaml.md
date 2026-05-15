# Extract parameters from a YAML file

The remaining parameters for the models are retrieved from a YAML file,
so as not to clutter the argument list for
[`extract_student_info()`](https://Zimmy313.github.io/grouper/reference/extract_student_info.md).

## Usage

``` r
extract_params_yaml(fname, assignment = c("diversity", "preference"))
```

## Arguments

- fname:

  A YAML file containing the remaining parameters.

- assignment:

  Character string indicating the type of model that this dataset is
  for. The argument is either 'preference' or 'diversity'. Partial
  matching is fine.

## Value

For the diversity+skill-based assignment, this function returns a list
containing:

- n_topics: the number of topics

- R: the optimally desired number of repetitions per topic

- nmin: the minimum number of students per topic,

- nmax: the maximum number of students per topic,

- rmin: the minimum number of repetitions per topic,

- rmax: the maximum number of repetitions per topic.

For the preference-based assignment, this function returns a list
containing:

- n_topics: the number of topics

- R: the optimally desired number of repetitions per topic

- nmin: the minimum number of students per topic,

- nmax: the maximum number of students per topic,

- rmin: the minimum number of repetitions per topic,

- rmax: the maximum number of repetitions per topic.
