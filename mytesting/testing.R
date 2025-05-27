# check this out: https://github.com/odow/group-allocator
library(dplyr)

library(cluster)

library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(ROI.plugin.gurobi)

library(yaml)

extract_student_info <- function(df,
                                 demographic_cols,
                                 skills,
                                 self_formed_groups) {
  self_formed_groups_vec <- df[[self_formed_groups]]
  N <- NROW(df)
  G <- max(self_formed_groups_vec)

  # form student x G matrix
  m <- matrix(0, nrow = N, ncol = G)
  for (i in 1:N) {
    m[i, self_formed_groups_vec[i]] <- 1
  }

  # form dissimilarity mx
  d <- df %>%
    select(all_of(demographic_cols)) %>%
    as_tibble() %>%
    mutate(across(where(is.character), as.factor)) %>%
    daisy(metric="gower") %>%
    as.matrix()

  # extract skills
  s <- df[[skills]]

  list(N=N, G=G, m=m, d=d, s=s)
}

extract_params_yaml <- function(fname) {
  in_params <- yaml.load_file(fname)

  #' MINIMUM NUMBER OF STUDENTS PER TOPIC
  nmin <- matrix(data=in_params$nmin,
                 nrow=in_params$n_studs,
                 ncol=in_params$R, byrow=TRUE)

  #' MAXIMUM NUMBER OF STUDENTS PER TOPIC
  nmax <- matrix(data=in_params$nmax,
                 nrow=in_params$n_studs,
                 ncol=in_params$R, byrow=TRUE)

  list(T = in_params$T,
       R = in_params$R,
       nmin=nmin, nmax = nmax,
       rmin=in_params$rmin,
       rmax=in_params$rmax
       )
}

prepare_model <- function(df_list, yaml_list, w1=0.5, w2=0.5) {
  N <- df_list$N
  G <- df_list$G
  m <- df_list$m
  d <- df_list$d
  s <- df_list$s

  T <- yaml_list$T
  R <- yaml_list$R
  nmin <- yaml_list$nmin
  nmax <- yaml_list$nmax
  rmin <- yaml_list$rmin
  rmax <- yaml_list$rmax

  model <- MIPModel() %>%
  #' DEFINE DECISION VARIABLES
  add_variable(x[g,t,r], g=1:G, t=1:T, r=1:R, type="binary") %>%
  add_variable(z[i,j,t,r], i=1:(N-1), j=(i+1):N, t=1:T, r=1:R, type="binary") %>%
  add_variable(a[t,r], t=1:T, r=1:R, type="binary") %>%
  add_variable(smin, type="continuous", lb=0) %>%
  add_variable(smax, type="continuous", lb=0) %>%
  #' DEFINE OBJECTIVE FUNCTION
  set_objective(
    #' MAXIMISE DIVERSITY
    w1*sum_over(z[i,j,t,r]*d[i,j], i=1:(N-1), j=(i+1):N, t=1:T, r=1:R)+
      #' MINIMIZE SKILL VARIABILITY
      w2*(smin-smax),
                "max") %>%
  #' DEFINE CONSTRAINTS (EACH GROUP ASSIGNED A TOPIC-REP)
  add_constraint(sum_over(x[g,t,r], t=1:T, r=1:R)==1, g=1:G) %>%
  #' DEFINE CONSTRAINTS (WHETHER 2 STUDENTS IN SAME TOPIC-REP)
  add_constraint(z[i,j,t,r]<=sum_over(m[i,g]*x[g,t,r], g=1:G), i=1:(N-1), j=(i+1):N, t=1:T, r=1:R) %>%
  add_constraint(z[i,j,t,r]<=sum_over(m[j,g]*x[g,t,r], g=1:G), i=1:(N-1), j=(i+1):N, t=1:T, r=1:R) %>%
  add_constraint(z[i,j,t,r]>=sum_over(m[i,g]*x[g,t,r], g=1:G)+sum_over(m[j,g]*x[g,t,r], g=1:G)-1, i=1:(N-1), j=(i+1):N, t=1:T, r=1:R) %>%
  #' DEFINE CONSTRAINTS (MIN AND MAX NO. OF REPETITIONS PER TOPIC)
  add_constraint(a[t,r]>=x[g,t,r], g=1:G, t=1:T, r=1:R) %>%
  add_constraint(a[t,r]<=sum_over(x[g,t,r], g=1:G), t=1:T, r=1:R) %>%
  add_constraint(sum_over(a[t,r], r=1:R)>=rmin, t=1:T) %>%
  add_constraint(sum_over(a[t,r], r=1:R)<=rmax, t=1:T) %>%
  #' DEFINE CONSTRAINTS (MIN AND MAX NO. OF STUDENTS PER TOPIC-REPETITION)
  add_constraint(sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)>=a[t,r]*nmin[t,r], t=1:T, r=1:R) %>%
  add_constraint(sum_over(m[i,g]*x[g,t,r], i=1:N, g=1:G)<=a[t,r]*nmax[t,r], t=1:T, r=1:R) %>%
  #' DEFINE CONSTRAINTS (SKILL VARIABILITY)
  add_constraint(sum_over(m[i,g]*x[g,t,r]*s[i], i=1:N, g=1:G)>=smin, t=1:T, r=1:R) %>%
  add_constraint(sum_over(m[i,g]*x[g,t,r]*s[i], i=1:N, g=1:G)<=smax, t=1:T, r=1:R)

  model
}

assign_groups <- function(model_result, df, group_names) {
  get_solution(result, x[g,t,r]) %>%
    filter(value>0) %>%
    select(t, r, g) %>%
    rename("group"="g",
           "topic"="t",
           "rep"="r") %>%
    arrange(topic, rep, group) %>%
    left_join(df, by=c("group"=group_names))
}

compute_group_diversity <- function(s_ids, d_mat){
  total_div <- sum(d_mat[s_ids, s_ids])/2
  total_div / length(s_ids)
}

##############################################################################
# prepare and save data
##############################################################################
df1$id <- 1:NROW(df1)
saveRDS(df1, file="mytesting/df001.rds")

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
