# groupings <- c("self_groups")
# demo1 <- c("gender", "age", "yos")
# skills <- c("python")

verify_columns <- function(groupings, demographics, skills) {
  if(length(intersect(groupings, demographics)) > 0 ||
     length(intersect(groupings, skills)) > 0 ||
     length(intersect(skills, demographics)) > 0 ) {
    return("There should be no common columns between the three groups.")
  }
  if(length(skills) > 1){
    return("There should only be one column for skills.")
  }
  if(length(groupings) > 1){
    return("There should only be one column for self-formed groups.")
  }
  if(("No demographics" %in% demographics) && ("No skills" %in% skills)){
    return("At least one of demographics or skills needs to be used.")
  }
  if(("No demographics" %in% demographics) && (length(demographics) > 1)){
    return("If no demographics are selected, then no other demographic columns should be selected.")
  }
  return("Columns verified.")
}

verify_params <- function(demographics, skills, w1, w2) {
  if((length(demographics) == 1) && (demographics == "No demographics")) {
    if(w1 > 0 ){
      return("Please set w1 to be 0 since no demographics are used.")
    }
  }
  if (skills == "No skills") {
    if(w1 < 1 ){
      return("Please set w1 to be 1 since no skills are used.")
    }
  }
  return("Parameter weights specified ok.")
}
