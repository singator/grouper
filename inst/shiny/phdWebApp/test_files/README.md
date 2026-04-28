# PhD Web App Test Files

This folder now contains one test input pair that is feasible under the app default settings:
- `C = 4`
- `e_max = 1`
- `t_max_y1 = 1`
- `alpha = 2, beta = 1, phi = 1, rho = 10`

## Default-Feasible Pair

- Current semester file: `current_semester_default.xlsx`
- Previous output file: `past_output_default.xlsx`

## Format Notes

- `current_semester_default.xlsx` has exactly two sheets: `students`, `demand`.
- `students` columns are exactly:
  `student_id, Name, year, first, second, third`
- `demand` columns are exactly:
  `course_code, TA, GR`
- `past_output_default.xlsx` includes `Name` and course-role columns ending with `-t` and `-g` (plus `-e` columns, optional for parsing).
