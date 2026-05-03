test_that("extract_phd_info returns aligned PhD inputs and computes E in rr mode", {
  x <- extract_phd_info(
    student_df = phd_students_ex001,
    p_mat = phd_prefmat_ex001,
    d_mat = phd_demand_ex001,
    e_mode = "rr",
    C = 4
  )

  expect_equal(x$Ns, nrow(phd_students_ex001))
  expect_equal(x$Nj, ncol(phd_prefmat_ex001))
  expect_equal(dim(x$P), c(4, 4))
  expect_equal(dim(x$d), c(4, 3))
  expect_equal(colnames(x$d), c("TA", "GR", "E"))
  expect_true(sum(x$d[, "E"]) >= 0)
})

test_that("extract_phd_info supports none mode and validates core schema", {
  x_none <- extract_phd_info(
    student_df = phd_students_ex001,
    p_mat = phd_prefmat_ex001,
    d_mat = phd_demand_ex001,
    e_mode = "none",
    C = 4
  )
  expect_true(all(x_none$d[, "E"] == 0))

  bad_students <- phd_students_ex001
  names(bad_students)[1] <- "student"
  expect_error(
    extract_phd_info(
      student_df = bad_students,
      p_mat = phd_prefmat_ex001,
      d_mat = phd_demand_ex001
    ),
    "first 4 columns are exactly"
  )

  expect_error(
    extract_phd_info(
      student_df = phd_students_ex001,
      p_mat = phd_prefmat_ex001[1:3, , drop = FALSE],
      d_mat = phd_demand_ex001
    ),
    "nrow\\(p_mat\\) must match"
  )
})
