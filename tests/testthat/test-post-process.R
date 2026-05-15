test_that("assign_groups works for diversity and preference outputs", {
  skip_if_not_installed("ompr.roi")
  skip_if_not_installed("ROI.plugin.glpk")

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
  div_model <- prepare_model(div_df, div_params, assignment = "diversity", w1 = 1, w2 = 0)
  div_result <- ompr::solve_model(div_model, ompr.roi::with_ROI(solver = "glpk"))

  div_out <- assign_groups(
    model_result = div_result,
    assignment = "diversity",
    dframe = dba_gc_ex001,
    group_names = "groups"
  )
  expect_true(all(c("group", "topic", "rep") %in% names(div_out)))

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
  pref_model <- prepare_model(pref_df, pref_params, assignment = "preference")
  pref_result <- ompr::solve_model(pref_model, ompr.roi::with_ROI(solver = "glpk"))

  pref_out <- assign_groups(
    model_result = pref_result,
    assignment = "preference",
    dframe = pba_gc_ex002,
    params_list = pref_params,
    group_names = "grouping"
  )
  expect_true(all(c("topic2", "subtopic", "rep", "group", "size") %in% names(pref_out)))
})

test_that("assign_job converts PhD solver output to manual-style table", {
  skip_if_not_installed("ompr.roi")
  skip_if_not_installed("ROI.plugin.glpk")

  phd_df <- extract_phd_info(
    student_df = phd_students_ex001,
    p_mat = phd_prefmat_ex001,
    d_mat = phd_demand_ex001,
    e_mode = "none",
    C = 4
  )

  phd_model <- prepare_model(phd_df, assignment = "phd", t_max_y1 = 1, C = 4)
  phd_result <- ompr::solve_model(phd_model, ompr.roi::with_ROI(solver = "glpk"))

  job <- assign_job(
    model_result = phd_result,
    student_df = phd_students_ex001,
    course_codes = rownames(phd_demand_ex001),
    name_col = "Name"
  )

  expect_true("Name" %in% names(job))
  expect_true(any(grepl("-t$", names(job))))
  expect_true(any(grepl("-g$", names(job))))
  expect_true(any(grepl("-e$", names(job))))
  expect_equal(nrow(job), nrow(phd_students_ex001))
})
