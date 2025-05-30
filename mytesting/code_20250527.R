################################################################################
#                                                                              #
#                            DSA4288 (Subgroups)                               #
#                                                                              #
################################################################################

# ------------------------------------------------------------------------------
# ---------- LOAD LIBRARIES ----------------------------------------------------
# ------------------------------------------------------------------------------

pacman::p_load(
  # For Data Manipulation
  readxl, tidyverse,
  # For Gower Distance,
  cluster,
  # For Integer Programming
  ompr, ompr.roi, ROI.plugin.glpk, ROI.plugin.gurobi
)

# ------------------------------------------------------------------------------
# ---------- GENERATE TEST DATA ------------------------------------------------
# ------------------------------------------------------------------------------

#' GENERATE STUDENT-GROUP MATRIX (m)
# Step 1: Generate group sizes that sum to N, with exactly G groups
N <- 100
G <- 25
generate_group_sizes <- function(N, G, min_size=1, max_size=5) {
  model <- MIPModel() %>%
    add_variable(x[g], g=1:G, type="integer", lb=min_size, ub=max_size) %>%
    set_objective(0, "min") %>%
    add_constraint(sum_over(x[g], g=1:G)==N)
  result <- solve_model(model, with_ROI(solver="gurobi", verbose=FALSE))
  solution <- get_solution(result, x[g]) %>%
    filter(value>0) %>%
    select(value) %>%
    unlist()
  solution
}
group_sizes <- generate_group_sizes(N, G)
# Step 2: Assign students to groups
student_group <- rep(1:G, times = group_sizes)
# Step 3: Create NxG matrix m
m <- matrix(0, nrow = N, ncol = G)
for (i in 1:N) {
  m[i, student_group[i]] <- 1
}

#' NUMBER OF TOPICS
T <- 5

#' NUMBER OF SUBTOPICS (ADDED FOR SUBGROUPS)
B <- 2

#' NUMBER OF REPETITIONS PER TOPIC
R <- 3

#' NUMBER OF STUDENTS
N <- nrow(m)

#' NUMBER OF GROUPS
G <- ncol(m)

#' NUMBER OF STUDENTS PER GROUP
n <- colSums(m)

#' MINIMUM NUMBER OF STUDENTS PER TOPIC
nmin <- matrix(data=3, nrow=(B*T), ncol=R, byrow=TRUE)

#' MAXIMUM NUMBER OF STUDENTS PER TOPIC
nmax <- matrix(data=5, nrow=(B*T), ncol=R, byrow=TRUE)

#' MINIMUM NUMBER OF REPETITIONS PER TOPIC
rmin <- 1

#' MAXIMUM NUMBER OF REPETITIONS PER TOPIC
rmax <- R

#' GENERATE RANDOM PREFERENCE PER GROUP
p <- matrix(data=sample(x=1:5, size=G*(B*T), replace=TRUE),
            nrow=G, byrow=TRUE)

# ------------------------------------------------------------------------------
# ---------- DEFINE MIP MODEL --------------------------------------------------
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# ---------- SOLVE MIP MODEL ---------------------------------------------------
# ------------------------------------------------------------------------------

result <- solve_model(model, with_ROI(solver="gurobi", verbose=TRUE))

#' OPTIMAL VALUE
result$objective_value

#' OPTIMAL SOLUTION
solution <- get_solution(result, x[g,t,r]) %>%
  filter(value>0) %>%
  select(t, r, g) %>%
  rename("group"="g",
         "topic"="t",
         "rep"="r") %>%
  left_join(
    data.frame(group=1:G,
               size=colSums(m))
  ) %>%
  mutate(`topic-subtopic`=paste(topic-(ceiling(topic/T)-1)*T, ceiling(topic/T), sep="-")) %>%
  select(`topic-subtopic`, rep, group, size) %>%
  arrange(`topic-subtopic`, rep, group) %>%
  group_by(`topic-subtopic`) %>%
  mutate(rep=match(rep, unique(rep))) %>%
  ungroup()

solution
solution %>%
  select(-group) %>%
  group_by(`topic-subtopic`, rep) %>%
  summarise(size=sum(size)) %>%
  ungroup()

## saving generated data
group_comp_df <- data.frame(id = 1:length(student_group), grouping=student_group)
group_preference_mat <- p
saveRDS(group_comp_df, file="data002-composition.rds")
saveRDS(group_preference_mat, file="data002-preference.rds")
