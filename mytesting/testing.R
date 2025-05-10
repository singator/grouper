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
    add_variable(a[t,r], t=1:T, r=1:R, type="binary") %>%
    add_variable(smin, type="continuous", lb=0) %>%
    add_variable(smax, type="continuous", lb=0) %>%
    #' DEFINE OBJECTIVE FUNCTION
    set_objective(
      #' MAXIMISE DIVERSITY
      w1*sum_over(m[i,g]*m[j,g]*x[g,t,r]*d[i,j], i=1:(N-1), j=(i+1):N, g=1:G, t=1:T, r=1:R)+
        #' MINIMIZE SKILL VARIABILITY
        w2*(smin-smax),
                  "max") %>%
    #' DEFINE CONSTRAINTS (EACH GROUP ASSIGNED A TOPIC-REP)
    add_constraint(sum_over(x[g,t,r], t=1:T, r=1:R)==1, g=1:G) %>%
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


df1 <- readRDS("df001.rds")
df_list <- extract_student_info(df1, demographic_cols = 1:3, skills = 5, self_formed_groups = 4)
yaml_list <- extract_params_yaml("input001.yml")
m1 <- prepare_model(df_list, yaml_list)
