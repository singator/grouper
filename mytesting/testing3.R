year <- 1:4
major <- c("math", "history", "dsds", "elts")
df2 <- data.frame(year = year, major=major)
df2$self_groups <- 1:NROW(df2)
df2$id <- 1:NROW(df2)

d_mat <- matrix(c(0, 1, 1, 2,
                  1, 0, 2, 1,
                  1, 2, 0, 1,
                  2, 1, 1, 0), nrow=4, byrow = TRUE)

df_list <- extract_student_info(df2,
                                skills = NULL,
                                self_formed_groups = 3,
                                d_mat=d_mat)
yaml_list <- extract_params_yaml("mytesting/input003_test.yml")
m2 <- prepare_model(df_list, yaml_list, w1=1.0, w2=0.0)
result <- solve_model(m2, with_ROI(solver="glpk", verbose=TRUE))
assigned_groups <- assign_groups(result, "diversity", df2, group_names="self_groups")
####################

id <- 1:5
python_skills <- c(1,1,1,2,3)
df3 <- data.frame(id = id, python= python_skills)
df3$self_groups <- 1:NROW(df3)

df_list <- extract_student_info(df3,
                                skills = 2,
                                self_formed_groups = 3,
                                d_mat=matrix(0, 5, 5))
yaml_list <- extract_params_yaml("mytesting/input004_test.yml")
m2 <- prepare_model(df_list, yaml_list, w1=0.0, w2=1.0)
result <- solve_model(m2, with_ROI(solver="glpk", verbose=TRUE))
assigned_groups <- assign_groups(result, "diversity", df3, group_names="self_groups")
