test_that("extract_student_info works for diversity and preference", {
  div <- extract_student_info(
    dba_gc_ex001,
    assignment = "diversity",
    self_formed_groups = 4,
    demographic_cols = 2,
    skills = 3
  )

  expect_equal(div$N, 4)
  expect_equal(div$G, 4)
  expect_equal(dim(div$m), c(4, 4))
  expect_equal(dim(div$d), c(4, 4))
  expect_length(div$s, 4)

  pref <- extract_student_info(
    pba_gc_ex002,
    assignment = "preference",
    self_formed_groups = 2,
    pref_mat = pba_prefmat_ex002
  )

  expect_equal(pref$N, 8)
  expect_equal(pref$G, 4)
  expect_equal(dim(pref$m), c(8, 4))
  expect_equal(pref$n, c(2, 2, 2, 2))
  expect_equal(dim(pref$p), c(4, 4))
})

test_that("extract_student_info validates preference and dissimilarity inputs", {
  expect_error(
    extract_student_info(
      pba_gc_ex002,
      assignment = "preference",
      self_formed_groups = 2
    ),
    "Missing preference matrix"
  )

  bad_d <- matrix(c(0, 1, 0, 0), nrow = 2)
  expect_error(
    extract_student_info(
      dba_gc_ex001,
      assignment = "diversity",
      self_formed_groups = 4,
      skills = 3,
      d_mat = bad_d
    ),
    "not symmetric"
  )
})
