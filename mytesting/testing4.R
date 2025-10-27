library(grouper)
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(ROI.plugin.gurobi)
library(yaml)
library(rlang)

?extract_student_info
df1 <- read.csv("~/Downloads/grouper_test_20251027.csv")
df_list <- extract_student_info(df1, "diversity", 1, c(3,4), 5)

yaml_list <- extract_params_yaml("mdl_lamfy.yml",assignment = "diversity" )
m2 <- prepare_model(df_list, yaml_list, "diversity", w1=0.5, w2=0.5)
result <- solve_model(m2, with_ROI(solver="gurobi",
                                   TimeLimit = 60,
                                   verbose=TRUE))
assigned_groups <- assign_groups(result, "diversity", df1, group_names="ID")
