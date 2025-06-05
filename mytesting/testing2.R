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

  get_solution(result, x[g,t,r]) %>%
    filter(value>0) %>%
    select(t, r, g) %>%
    rename("group"="g",
           "topic"="t",
           "rep"="r") %>%
      left_join(group_sizes, by=c("group"=group_names)) %>%
    mutate(`topic-subtopic`=paste(topic-(ceiling(topic/n_topics)-1)*n_topics,
                                  ceiling(topic/n_topics), sep="-")) %>%
    select(`topic-subtopic`, rep, group, size) %>%
    arrange(`topic-subtopic`, rep, group) %>%
    group_by(`topic-subtopic`) %>%
    mutate(rep=match(rep, unique(rep))) %>%
    ungroup()
}


## Loading example data frames
group_comp_df1 <- readRDS("data002-composition.rds")
group_pref_mat1 <- readRDS("data002-preference.rds")

df_list <- extract_student_info2(group_comp_df1, 2, group_pref_mat1)
yaml_list <- extract_params_yaml2("mdl2_input01.yml")
mdl2_1 <- prepare_model2(df_list, yaml_list)
result <- solve_model(mdl2_1, with_ROI(solver="gurobi", verbose=TRUE))

assigned_groups <- assign_groups2(result, group_comp_df1, yaml_list, "grouping")

T <- 5
get_solution(result, x[g,t,r])  %>%
  filter(value>0) %>%
  select(t, r, g) %>%
  rename("group"="g",
         "topic"="t",
         "rep"="r") %>%
  left_join(group_sizes, by=c("group"="grouping")) %>%
  mutate(`topic-subtopic`=paste(topic-(ceiling(topic/T)-1)*T, ceiling(topic/T), sep="-")) %>% #View
  select(`topic-subtopic`, rep, group, size) %>%
  arrange(`topic-subtopic`, rep, group) %>%
  group_by(`topic-subtopic`) %>%
  mutate(rep=match(rep, unique(rep))) %>% #View
  ungroup()

group_sizes <- group_comp_df1 %>%
  group_by(grouping) %>%
  summarise(size = length(id)) %>%
  ungroup

################### UNTIL HERE OK ##############################################

##############################################################################
# first attempt, 1 - 3 repeats per topic
##############################################################################

# Prepare data, fit and solve the model
df1 <- readRDS("mytesting/df001.rds")
df_list <- extract_student_info(df1, demographic_cols = 1:3, skills = 5, self_formed_groups = 4)
yaml_list <- extract_params_yaml("mytesting/input001.yml")
m1 <- prepare_model(df_list, yaml_list)

result <- solve_model(m1, with_ROI(solver="gurobi", verbose=TRUE))
assigned_groups <- assign_groups(result, df1, "self_groups")

# compute the average skill in each group:
assigned_groups %>% group_by(topic,rep) %>%
  summarise(mean_skill = mean(skill), .groups="drop")

# compute the sum of pairwise diversity in each group:
assigned_groups %>% group_by(topic, rep) %>%
  summarise(mean_div = compute_group_diversity(id, df_list$d))

##############################################################################
# second attempt, exactly 2 repeats per topic, 3 per group
# more weight on minimising skill range
##############################################################################

df1 <- readRDS("mytesting/df001.rds")
df1$self_groups <- 1:NROW(df1)
df_list <- extract_student_info(df1, demographic_cols = 1:3, skills = 5, self_formed_groups = 4)
yaml_list <- extract_params_yaml("mytesting/input002.yml")
m2 <- prepare_model(df_list, yaml_list, w1=1.0, w2=0)

result <- solve_model(m2, with_ROI(solver="gurobi", verbose=TRUE))
assigned_groups2 <- assign_groups(result, df1, "self_groups")

# compute the average skill in each group:
assigned_groups2 %>% group_by(topic,rep) %>%
  summarise(mean_skill = mean(skill), .groups="drop")

# compute the sum of pairwise diversity in each group:
assigned_groups2 %>% group_by(topic, rep) %>%
  summarise(mean_div = compute_group_diversity(id, df_list$d), .groups="drop")

### Creating a dummy dataset

majors1 <- c("A", "A", "B", "B")
skills1 <- c(1,1,3,3)
g <- 1:4
data002 <- data.frame(id = 1:4, major= majors1, skill=skills1, groups = g)
library(groupr)
data002_list <- extract_student_info(data002, demographic_cols = 2, skills = 3,
                                     self_formed_groups = 4)
yaml002_list <- extract_params_yaml("mytesting/input003-data002.yml")
m3 <- prepare_model(data002_list, yaml_list = yaml002_list, 1.0, 0.0)
#result <- solve_model(m3, with_ROI(solver="glpk", verbose=TRUE))
result3 <- solve_model(m3, with_ROI(solver="gurobi", verbose=TRUE))
assign_groups(result3, data002, "groups")

m4 <- prepare_model(data002_list, yaml_list = yaml002_list, 0.0, 1.0)
result4 <- solve_model(m3, with_ROI(solver="gurobi", verbose=TRUE))
assign_groups(result4, data002, "groups")
