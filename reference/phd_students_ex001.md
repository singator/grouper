# PhD Student Data Example 001

An example student table to use with the PhD workload allocation model.

## Usage

``` r
phd_students_ex001
```

## Format

### `phd_students_ex001`

A data frame with 4 rows and 5 columns.

- student_id: unique student id.

- year: PhD year, encoded from 1 to 4.

- past_ta: previous-semester TA workload units.

- past_gr: previous-semester GR workload units.

- Name: student name.

In this toy dataset, `past_ta + past_gr = 4` for every student.

For a one-semester sanity-check variant, reuse this dataset with
`past_ta = 0` and `past_gr = C` for all students before extraction.

## Source

This dataset was constructed by hand.
