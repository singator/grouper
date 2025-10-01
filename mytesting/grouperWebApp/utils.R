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

  return("Columns verified")
}

