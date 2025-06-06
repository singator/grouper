## TODO list

1. Corner cases for extract_student_info:
   - no demographics
   - no skills
   - no self-formed groups
2. Validate w1 and w2
3. Allow for other pairwise dissimilarity metrics
4. Two main models:
   - with demographics and skills, but no preferences (model 1)
   - with sub-groups +  topic preferences but not demographics and/or skills (model 2)


## Data frames and inputs

Remember: model 1 is the one with diversity, etc.
          model 2 is the one based on preference, with possible sub-groups
          
### For model 1:

eg1: data002-composition.rds, data002-preference.rds, mdl02_input01.yml
eg1: data003-composition.rds, data003-preference.rds, mdl02_input02.yml
