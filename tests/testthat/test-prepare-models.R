test_that("prepare_model wrapper dispatches and validates arguments", {
  div_df <- extract_student_info(
    dba_gc_ex001,
    assignment = "diversity",
    self_formed_groups = 4,
    demographic_cols = 2,
    skills = 3
  )
  div_params <- extract_params_yaml(
    system.file("extdata", "dba_params_ex001.yml", package = "grouper"),
    assignment = "diversity"
  )

  m_div <- prepare_model(div_df, div_params, assignment = "diversity", w1 = 1, w2 = 0)
  expect_s3_class(m_div, "linear_optimization_model")

  pref_df <- extract_student_info(
    pba_gc_ex002,
    assignment = "preference",
    self_formed_groups = 2,
    pref_mat = pba_prefmat_ex002
  )
  pref_params <- extract_params_yaml(
    system.file("extdata", "pba_params_ex002.yml", package = "grouper"),
    assignment = "preference"
  )

  m_pref <- prepare_model(pref_df, pref_params, assignment = "preference")
  expect_s3_class(m_pref, "linear_optimization_model")

  phd_df <- extract_phd_info(
    student_df = phd_students_ex001,
    p_mat = phd_prefmat_ex001,
    d_mat = phd_demand_ex001,
    e_mode = "none",
    C = 4
  )

  m_phd <- prepare_model(phd_df, assignment = "phd", t_max_y1 = 1, C = 4)
  expect_s3_class(m_phd, "linear_optimization_model")

  expect_error(
    prepare_model(div_df, assignment = "diversity"),
    "yaml_list must be provided"
  )
  expect_error(
    prepare_model(pref_df, assignment = "preference"),
    "yaml_list must be provided"
  )
})

test_that("prepare_phd_model validates optional bound consistency", {
  phd_df <- extract_phd_info(
    student_df = phd_students_ex001,
    p_mat = phd_prefmat_ex001,
    d_mat = phd_demand_ex001,
    e_mode = "none",
    C = 4
  )

  expect_error(
    prepare_phd_model(phd_df, ta_min = 2, ta_max = 1),
    "ta_min cannot be greater than ta_max"
  )
  expect_error(
    prepare_phd_model(phd_df, gr_min = 2, gr_max = 1),
    "gr_min cannot be greater than gr_max"
  )
  expect_error(
    prepare_phd_model(phd_df, e_min = 2, e_max = 1),
    "e_min cannot be greater than e_max"
  )
})
