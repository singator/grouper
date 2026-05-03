test_that("extract_params_yaml parses diversity and preference parameter files", {
  d <- extract_params_yaml(
    system.file("extdata", "dba_params_ex001.yml", package = "grouper"),
    assignment = "diversity"
  )
  expect_true(all(c("n_topics", "R", "nmin", "nmax", "rmin", "rmax") %in% names(d)))
  expect_equal(nrow(d$nmin), d$n_topics)
  expect_equal(ncol(d$nmin), d$R)

  p <- extract_params_yaml(
    system.file("extdata", "pba_params_ex002.yml", package = "grouper"),
    assignment = "preference"
  )
  expect_true(all(c("n_topics", "B", "R", "nmin", "nmax", "rmin", "rmax") %in% names(p)))
  expect_equal(nrow(p$nmin), p$B * p$n_topics)
  expect_equal(ncol(p$nmin), p$R)
})
