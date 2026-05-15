# Related work and grouper

## Introduction

In this vignette, we provide a brief, non-exhaustive overview of related
work to assign students to projects, in an educational setting. Finally,
we introduce our `grouper` package.

### Balancing staff workload and student preferences

One of the earlier papers found was by Anwar and Bahaj (2003). The goal
of the educator in that paper was to assign students to two types of
project: individual ones and group ones. For the individual projects,
students were asked to rank their top four projects from a list of
project briefs. Each student was then assigned to a project and a
corresponding staff member.

The assignment was carried out using two models. In the first model, the
objective was to minimise the total number of projects that each staff
managed. In the second model, the feasible solutions from the first
model were used to decide upon an assignment that maximised the number
of students obtaining their first choice projects.

Compared to the models in `grouper`, the primary difference is in the
objective function. In our case, we only work with the student
preference; there is no need to balance staff workload since the
projects are run within a single course.

For the group project, students were asked to rank their top 3 projects
*individually*. They were then allocated groups that maximised their
preferences. Compared to the model in our package, this approach does
not allow for self-formed groups, or for larger projects that involve
sub-groups.

### OptAssign

In many ways, the model described in Meyer (2009) is similar to ours.
The objective function there maximises student preference for project
topics. The model in the paper also discusses an extension whereby some
groups of students can be favoured for particular topics. This is
carried out through the inclusion of weights in the objective function.
Another additional feature adds contstraints to ensure that students
with certain skills are included in particular topics. For instance, a
project topic on “Marketing surveys” may require at least two students
with a statistics background in the group.

Overall, this model formulation is very similar to the one in our
package. Again, one difference is in the affordance for self-formed
groups, and for sub-groups in the project team.

### Students to elective courses

This paper Beroš and Meter (2015) deals with assignment of students to
elective courses in Croatia. In short, the institution offers a fixed
number of such courses. Every student must be enrolled in a
pre-determined number of courses. Each student selects and ranks a
certain number of electives. The integer programming solution that is
derived here maximises the total student satisfaction, based on these
rankings.

### Assigning students to groups based on traits

The master’s thesis from McGuirk (2020) describes a very complex integer
linear programming model. The assignment was used to assign students to
discussion groups at the University of New Hampshire chemistry
department. The complete objective function can be written as:

``` math
\begin{eqnarray}
\text{Minmise:}\; \text{preferences} + \text{sizes} + \text{single-trait} + \text{shared-trait}
\end{eqnarray}
```

The first component, preferences, incorporates the ranking that put
students put down for a group. The second component was used to control
the overall group size. Note that in our formulation, this appears as a
constraint. The third component arises as a result of the educators
experience in teaching this course. They realised that if a student is
the only one with a certain trait, e.g. gender, year-of-study, etc.,
that student would be disinclined to participate in discussions. This is
a very interesting point! The final portion of the objective function
applies a penalty if all students in the group share the same trait.

In our formulation, we have avoided using multi-objective functions for
the most part. The reason for this decision is that it is very difficult
to decide how each component should be weighted. It typically requires a
further step of post-assignment sensitivity analysis.

Another similarity with our findings is the recommendation to use Gurobi
over open source solvers.

### Student-to-project supervisor assignment

The next article we discuss is Ramotsisi et al. (2022). The formulation
of this model was the result of a thorough investigation into the
process by which students are assigned projects that are supervised by
faculty members at the University of Botswana. However, similar to Anwar
and Bahaj (2003), the objective function in this study aims to maximise
the total workload of supervisors. It does not approach the problem
solely from the angle of student preferences.

### Automated group assignments in academic setting

In the final work that we discuss, Bonfert-Taylor and Miller (2020) use
a probabilistic weighted optimisation routine to perform the group
assignment, based on survey input data. Questions in the survey pertain
to scheduling, desired attributes of team members, and indications of
whether a student may be isolated. For each question, a homogeneity
score is computed using the answers from all members of a group. This
score is summed over all questions to result in a “fitness” score for
each group. A probabilistic hill-climbing algorithm is then used to
maximise the average fitness score. Out of the papers discussed on this
page, this is the only one that does not use an integer linear
programming model.

## Introducing `grouper` package

`grouper` provides two distinct optimisation models, both of which are
are formulated as integer linear programs.

The Preference-Based Assignment (PBA) model allows educators to assign
student groups to topics primarily to maximise overall student
preferences for those topics. The topics can be viewed as project
titles. The model allows for repetitions of each project title. It is
also possible to allow each project team to be comprised of multiple
sub-groups in this formulation.

To execute the optimisation routine, an instructor needs to prepare:

1.  A group composition table listing the member students within each
    self-formed group.
2.  A preference matrix containing the preference that each self-formed
    group has for each topic.
3.  The remaining model parameters, supplied either:
    - through a YAML file (backward-compatible workflow), or
    - directly in `prepare_model(...)` as named arguments.

In both cases, extraction can be handled through the wrapper entrypoint
[`extract_info()`](https://Zimmy313.github.io/grouper/reference/extract_info.md).

The Diversity-Based Assignment (DBA) model enables educators to assign
students to groups and topics with the dual aims of maximising diversity
(based on student attributes) within groups and balancing specific skill
levels across different groups. These are formulated in a single
objective function with weights assigned to each component.

To execute the DBA optimisation routine, the instructor needs to
prepare:

1.  A group composition table containing:
    - the member students within each self-formed group,
    - the demographics that will be used to compute pairwise
      dissimilarity between students, and
    - a numeric measure of each student’s skill.
2.  The remaining model parameters, supplied either:
    - through a YAML file (backward-compatible workflow), or
    - directly in `prepare_model(...)` as named arguments.

As above, extraction can be handled through
[`extract_info()`](https://Zimmy313.github.io/grouper/reference/extract_info.md).

Refer to the remaining vignettes for more information about the model
definition and application.

In our package, we use the `ompr` framework for optimisation, which
means that you are free to use any solver. In our small examples, we
demonstrate the package using the `gplk` solver. However, for more
complicated problems, we highly recommend the `gurobi` solver. There is
a free academic license for this tool. However, the `ROI.plugin.gurobi`
and `gurobi` packages are not on CRAN. The following links will aid in
installing the `gurobi` package:

- [Installing the R package
  gurobi](https://docs.gurobi.com/projects/optimizer/en/current/reference/r/setup.html)
- [Guide to installing gurobi
  optimiser](https://cran.r-project.org/package=prioritizr/vignettes/gurobi_installation_guide.html)

The following github repository provides instructions on install the
gurobi solver:

- [Installing
  `ROI.plugin.gurobi`](https://github.com/roigrp/ROI.plugin.gurobi)

## References

Anwar, Arif A, and AS Bahaj. 2003. “Student Project Allocation Using
Integer Programming.” *IEEE Transactions on Education* 46 (3): 359–67.

Beroš, Ivo, and Joško Meter. 2015. “An Integer Programming Model for
Assigning Students to Elective Courses.” *Croatian Operational Research
Review*, 511–24.

Bonfert-Taylor, Petra, and Christopher Miller. 2020. “Improving
Automated Group Assignments in an Academic Setting.” *2020 ASEE Virtual
Annual Conference Content Access*.

McGuirk, Brendan. 2020. “Assigning Students to Groups Based on
Preference and Traits.” Master’s thesis, University of New Hampshire.

Meyer, David. 2009. “OptAssign—a Web-Based Tool for Assigning Students
to Groups.” *Computers & Education* 53 (4): 1104–19.

Ramotsisi, Johnson, Mompoloki Kgomotso, and Lone Seboni. 2022. “An
Optimization Model for the Student-to-Project Supervisor Assignment
Problem-the Case of an Engineering Department.” *Journal of
Optimization* 2022 (1): 9415210.
