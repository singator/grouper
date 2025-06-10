library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(ROI.plugin.gurobi)
library(yaml)
library(rlang)

# comp_df: dataframe with nrow = num of students
# group_col: integer, column that contains the grouping of students
# pref_mat: G x B*T binary matrix
extract_student_info2 <- function(comp_df, group_col,
                                 pref_mat) {
  N <- NROW(comp_df)
  G <- max(comp_df[[group_col]])
  self_formed_groups_vec <- comp_df[[group_col]]

  # form student x G matrix
  m <- matrix(0, nrow = N, ncol = G)
  for (i in 1:N) {
    m[i, self_formed_groups_vec[i]] <- 1
  }
  # number of students per group
  n <- colSums(m)

  # checks on matrix dimension?

  list(N=N, G=G, m=m, n=n, p=pref_mat)
}

extract_params_yaml2 <- function(fname) {
  in_params <- yaml.load_file(fname)
  B <- in_params$B

  #' MINIMUM NUMBER OF STUDENTS PER TOPIC
  nmin <- matrix(data=in_params$nmin,
                 nrow=B*in_params$n_topics,
                 ncol=in_params$R, byrow=TRUE)

  #' MAXIMUM NUMBER OF STUDENTS PER TOPIC
  nmax <- matrix(data=in_params$nmax,
                 nrow=B*in_params$n_topics,
                 ncol=in_params$R, byrow=TRUE)

  list(n_topics = in_params$n_topics,
       B = B,
       R = in_params$R,
       nmin=nmin, nmax = nmax,
       rmin=in_params$rmin,
       rmax=in_params$rmax
       )
}

prepare_model2 <- function(df_list, yaml_list) {
  N <- df_list$N
  G <- df_list$G
  m <- df_list$m
  n <- df_list$n
  p <- df_list$p

  T <- yaml_list$n_topics
  B <- yaml_list$B
  R <- yaml_list$R
  nmin <- yaml_list$nmin
  nmax <- yaml_list$nmax
  rmin <- yaml_list$rmin
  rmax <- yaml_list$rmax

  model <- MIPModel() %>%
  #' DEFINE DECISION VARIABLES
  add_variable(x[g,t,r], g=1:G, t=1:(B*T), r=1:R, type="binary") %>%
  add_variable(a[t,r], t=1:(B*T), r=1:R, type="binary") %>%
  #' DEFINE OBJECTIVE FUNCTION
  set_objective(sum_over(x[g,t,r]*n[g]*p[g,t], g=1:G, t=1:(B*T), r=1:R), "max") %>%
  #' DEFINE CONSTRAINTS (EACH GROUP ASSIGNED A TOPIC-REP)
  add_constraint(sum_over(x[g,t,r], t=1:(B*T), r=1:R)==1, g=1:G) %>%
  #' DEFINE CONSTRAINTS (MIN NO. OF REPETITIONS PER TOPIC)
  add_constraint(a[t,r]>=x[g,t,r], g=1:G, t=1:(B*T), r=1:R) %>%
  add_constraint(a[t,r]<=sum_over(x[g,t,r], g=1:G), t=1:(B*T), r=1:R) %>%
  add_constraint(sum_over(a[t,r], r=1:R)>=rmin, t=1:T) %>%
  #' DEFINE CONSTRAINTS (BALANCED NO. OF REPETITIONS FOR SUBGROUPS)
  add_constraint(sum_over(a[t,r], r=1:R)==sum_over(a[(b*T+t),r], r=1:R), t=1:T, b=min(1,B-1):max(0,B-1)) %>%
  #' DEFINE CONSTRAINTS (MIN AND MAX NO. OF STUDENTS PER TOPIC-REPETITION)
  add_constraint(sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)>=a[t,r]*nmin[t,r], t=1:(B*T), r=1:R) %>%
  add_constraint(sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)<=a[t,r]*nmax[t,r], t=1:(B*T), r=1:R)

  model
}

assign_groups2 <- function(model_result, comp_df, params_list, group_names) {
  # for now, use "grouping", but in the package, we need to follow:
  # https://dplyr.tidyverse.org/articles/in-packages.html
  group_sizes <- comp_df %>%
    group_by(.data[[group_names]]) %>%
    summarise(size = length(id)) %>%
    ungroup
  n_topics <- params_list[["n_topics"]]
  B <- params_list[["B"]]

  topic_df <- data.frame(topic = 1:(n_topics*B),
                         topic2 = rep(1:n_topics, B),
                         subtopic=rep(1:B, each= n_topics))

  get_solution(result, x[g,t,r]) %>%
    filter(value>0) %>%
    select(t, r, g) %>%
    rename("group"="g",
           "topic"="t",
           "rep"="r") %>%
    left_join(group_sizes, by=c("group"=group_names)) %>%
    left_join(topic_df, by="topic") %>%
    select(topic2, subtopic, rep, group, size)  %>%
    group_by(topic2,subtopic) %>%
    mutate(rep=match(rep, unique(rep))) %>%
    ungroup()

    # mutate(topic = topic-(ceiling(topic/n_topics)-1)*n_topics,
    #      subtopic = ceiling(topic/n_topics)) #%>%
    # select(topic, subtopic, rep, group, size) %>%
    # group_by(topic,subtopic) %>%
    # mutate(rep=match(rep, unique(rep))) %>%
    # ungroup() %>%
    # arrange(topic, rep, subtopic, rep, group)
}


## Loading example data frames
group_comp_df1 <- readRDS("data002-composition.rds")
group_pref_mat1 <- readRDS("data002-preference.rds")

df_list <- extract_student_info2(group_comp_df1, 2, group_pref_mat1)
yaml_list <- extract_params_yaml2("mdl2_input01.yml")
mdl2_1 <- prepare_model2(df_list, yaml_list)
result <- solve_model(mdl2_1, with_ROI(solver="gurobi", verbose=TRUE))

assigned_groups <- assign_groups2(result, group_comp_df1, yaml_list, "grouping")

## Loading example data frames
## 2 topics, 2 sub-groups in each project team
group_comp_df1 <- readRDS("data003-composition.rds")
group_pref_mat1 <- readRDS("data003-preference.rds")

df_list <- extract_student_info2(group_comp_df1, 2, group_pref_mat1)
yaml_list <- extract_params_yaml2("mdl02_input02.yml")
mdl2_1 <- prepare_model2(df_list, yaml_list)
result <- solve_model(mdl2_1, with_ROI(solver="gurobi", verbose=TRUE))

assign_groups2(result, group_comp_df1, yaml_list, "grouping")

## Loading example data frames
## 2 topics, 3 sub-groups in each project team
group_comp_df1 <- readRDS("data004-composition.rds")
group_pref_mat1 <- readRDS("data004-preference.rds")
#saveRDS(data004, "data004-composition.rds")
#saveRDS(mx2, "data004-preference.rds")

df_list <- extract_student_info2(group_comp_df1, 2, group_pref_mat1)
yaml_list <- extract_params_yaml2("mdl02_input04.yml")
mdl2_4 <- prepare_model2(df_list, yaml_list)
result <- solve_model(mdl2_4, with_ROI(solver="gurobi", verbose=TRUE))

assign_groups2(result, group_comp_df1, yaml_list, "grouping")

## Try out on DSA3101-2120 (why is model infeasible?)
group_comp_df1 <- readRDS("data005-composition.rds")
group_pref_mat1 <- readRDS("data005-preference.rds")[,-c(1,6)]
df_list <- extract_student_info2(group_comp_df1, 2, group_pref_mat1)
yaml_list <- extract_params_yaml2("mdl02_input05.yml")
mdl2_5 <- prepare_model2(df_list, yaml_list)
result <- solve_model(mdl2_5, with_ROI(solver="gurobi", verbose=TRUE))

## DSA3101-2210
group_comp_df1 <- readRDS("data006-composition.rds")
group_pref_mat1 <- readRDS("data006-preference.rds")
df_list <- extract_student_info2(group_comp_df1, 2, group_pref_mat1)
yaml_list <- extract_params_yaml2("mdl02_input06.yml")
mdl2_6 <- prepare_model2(df_list, yaml_list)
result <- solve_model(mdl2_6, with_ROI(solver="gurobi", verbose=TRUE))
da3101_2210_outcome <- assign_groups2(result, group_comp_df1, yaml_list, "group_id")

## DSA3101-2220
group_comp_df1 <- readRDS("data007-composition.rds")
group_pref_mat1 <- readRDS("data007-preference.rds")
df_list <- extract_student_info2(group_comp_df1, 2, group_pref_mat1)
yaml_list <- extract_params_yaml2("mdl02_input07.yml")
mdl2_7 <- prepare_model2(df_list, yaml_list)
result <- solve_model(mdl2_7, with_ROI(solver="gurobi", verbose=TRUE))
da3101_2220_outcome <- assign_groups2(result, group_comp_df1, yaml_list, "group_id")

## DSA3101-2310
group_comp_df1 <- readRDS("data008-composition.rds")
group_pref_mat1 <- readRDS("data008-preference.rds")
df_list <- extract_student_info2(group_comp_df1, 2, group_pref_mat1)
yaml_list <- extract_params_yaml2("mdl02_input08.yml")
mdl2_8 <- prepare_model2(df_list, yaml_list)
result <- solve_model(mdl2_8, with_ROI(solver="gurobi", verbose=TRUE))
da3101_2310_outcome <- assign_groups2(result, group_comp_df1, yaml_list, "group_id")

################### UNTIL HERE OK ##############################################

