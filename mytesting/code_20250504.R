################################################################################
#                                                                              #
#                                   DSA4288                                    #
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

#' GENERATE STUDENT-GROUP MATRIX (m) and STUDENT-DEMOGRAPHIC MATRIX (df)
set.seed(123)

# Step 1: Generate group sizes that sum to 100, with exactly 30 groups
generate_group_sizes <- function(N, G, min_size = 1, max_size = 5) {
  repeat {
    sizes <- sample(min_size:max_size, G, replace = TRUE)
    diff <- sum(sizes) - N

    if (diff == 0) return(sizes)

    # Adjust the group sizes
    if (diff > 0) {
      # Need to reduce total size
      reducible <- which(sizes > min_size)
      if (length(reducible) < diff) next
      for (i in 1:diff) {
        sizes[reducible[i]] <- sizes[reducible[i]] - 1
      }
    } else {
      # Need to increase total size
      increasable <- which(sizes < max_size)
      if (length(increasable) < abs(diff)) next
      for (i in 1:abs(diff)) {
        sizes[increasable[i]] <- sizes[increasable[i]] + 1
      }
    }

    if (all(sizes >= min_size & sizes <= max_size) && sum(sizes) == N) {
      return(sizes)
    }
  }
}

N <- 30
G <- 15
group_sizes <- generate_group_sizes(N, G)

# Step 2: Assign students to groups
student_group <- rep(1:G, times = group_sizes)

# Step 3: Create NxG matrix m
m <- matrix(0, nrow = N, ncol = G)
for (i in 1:N) {
  m[i, student_group[i]] <- 1
}

# Step 4: Create demographic dataframe df
df <- data.frame(
  year = sample(1:4, N, replace = TRUE),
  major = sample(c("STEM", "Non-STEM"), N, replace = TRUE),
  gender = sample(c("Male", "Female"), N, replace = TRUE)
)

#' GENERATE STUDENT-TOPIC PREFERENCE MATRIX
d <- df %>%
  as_tibble() %>%
  mutate(across(where(is.character), as.factor)) %>%
  daisy(metric="gower") %>%
  as.matrix()
# d is dissimilarity matrix for the df generated.
# d is 30x30

#' OBJECTIVE FUNCTION WEIGHTS
w1 <- 0.5 # DIVERSITY - uses d matrix
w2 <- 0.5 # SKILL VARIABILITY - uses?

#' NUMBER OF TOPICS
T <- 5

#' NUMBER OF REPETITIONS PER TOPIC
R <- 3

#' NUMBER OF STUDENTS
N <- nrow(m)

#' NUMBER OF GROUPS
G <- ncol(m)

#' GENERATE RANDOM SKILL PER STUDENT
s <- sample(x=1:5, size=N, replace=TRUE)

#' MINIMUM NUMBER OF STUDENTS PER TOPIC
nmin <- matrix(data=3, nrow=N, ncol=R, byrow=TRUE)

#' MAXIMUM NUMBER OF STUDENTS PER TOPIC
nmax <- matrix(data=5, nrow=N, ncol=R, byrow=TRUE)

#' MINIMUM NUMBER OF REPETITIONS PER TOPIC
rmin <- 1

#' MAXIMUM NUMBER OF REPETITIONS PER TOPIC
rmax <- R

# ------------------------------------------------------------------------------
# ---------- DEFINE MIP MODEL --------------------------------------------------
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# ---------- SOLVE MIP MODEL ---------------------------------------------------
# ------------------------------------------------------------------------------

result <- solve_model(model, with_ROI(solver="gurobi", verbose=TRUE))

#' OPTIMAL VALUE
result$objective_value

#' OPTIMAL SOLUTION
get_solution(result, x[g,t,r]) %>%
  filter(value>0) %>%
  select(t, r, g) %>%
  rename("group"="g",
         "topic"="t",
         "rep"="r") %>%
  arrange(topic, rep, group) %>%
  left_join(
    data.frame(group=1:G,
               size=colSums(m))
  )
