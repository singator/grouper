---
title: 'grouper: An R Package for Optimal Group Assignment in Higher Education'
tags:
- R
- group assignment
- collaborative learning
date: "26 June 2025"
affiliations:
- name: National University of Singapore
  index: 1
authors:
- name: Ju Xue
  affiliation: 1
- name: Kevin Fu Yuan Lam
  orcid: "0000-0002-1535-7059"
  affiliation: 1
- name: Vik Gopal
  orcid: "0000-0002-7232-2063"
  affiliation: 1
bibliography: paper.bib
---

# Summary

Collaborative learning pedagogies are increasing in prominence in higher
education. However, the realisation of these pedagogies’ benefits depend on how
educators assign learners to groups. Because such assignments can be
time-consuming and subjective, there is a need for an algorithmic solution for
them. In this paper, we present `grouper`, an easy-to-use and free R package
that implements two flexible group allocation strategies in an automated and
objective manner. `grouper` was designed to ease the burden on educators
adopting collaborative learning pedagogies in their courses, and has been
evaluated on two undergraduate courses described here. 

# Statement of Need

Institutes of Higher Learning (IHLs) are increasingly using collaborative
learning pedagogies, which can benefit learners through deeper understanding of
course content and teamwork skills. However, the realisation of these
sought-after benefits depend on how educators assign learners to groups.

Educators have formulated various mathematical models to perform this
assignment: [@anwar2003student] and [@meyer2009optassign] developed models that
prioritised maximising students' project preferences; [@mcguirk2020assigning]
developed a model that prioritised students' preferences, group sizes and group
composition. Other models address related, but distinct, problems such as
assigning students to elective courses [@berovs2015integer] or incorporating
staff workload into student-to-project supervisor assignments
[@ramotsisi2022optimization].

As part of our research, we also conducted in-depth interviews with two
professors from contrasting disciplines.

The first professor, from the School of Computing at National University of
Singapore, allowed students to self-form sub-groups of up to three members in
his course. These sub-groups were then paired to create larger project teams of
six, in order to neutralize differences in skill levels across the final teams.

We utilised a similar procedure in a course in our department (Department of
Statistics and Data Science). However, instead of utilising skills, we
prioritised  each self-formed sub-group's preference for their roles and
topics. While self-formed sub-groups ensure familiarity between members in the
sub-group, matching sub-groups randomly possibly forces students to collaborate
with others whom they may not know well, replicating workplace scenarios.

The second professor, from the Department of Communications and New Media,
provided additional perspective on practical challenges in group allocation for
courses in the Arts and Social Sciences. For her, the main objective in group
allocation is to achieve diversity, especially for certain topics/tasks; the
ideal group should encompass varied viewpoints and expertise, drawn from
different academic specializations and personal backgrounds, e.g. year of
study. She explained that having group members from diverse backgrounds
facilitates understanding the problem from multiple angles, generally leading
to more innovative solutions.

In summary, our literature review and interviews highlight several factors that
educators could prioritise when performing group assignment in higher
education. Whichever factor is used, it is apparent that there is a need for an
algorithmic solution for the assignment. This would ease the burden on the
instructor, while providing an objective procedure for the assignment. Our
contribution is an R package `grouper` that offers two flexible group
allocation strategies.

# Functionality and Usage

`grouper` provides two distinct integer linear programming optimisation models.

The Preference-Based Assignment (PBA) model allows educators to assign student
groups to topics to maximise overall student preferences for those topics. The
topics can be viewed as project titles. The model allows for repetitions of
each project title. This formulation also allows each project team to comprise
multiple sub-groups.

To execute the optimisation routine, an instructor prepares:

1.  A group composition table listing the member students within each
    self-formed group.
2.  A preference matrix containing the preference that each self-formed
    group has for each topic.
3.  A YAML file defining the remaining parameters of the model.

The Diversity-Based Assignment (DBA) model enables educators to assign students
to groups and topics with the dual, but weighted, aims of maximising diversity
(based on student attributes) within groups and balancing specific skill levels
across different groups. 

To execute the DBA optimisation routine, the instructor prepares:

1.  A group composition table containing:
    -   the member students within each self-formed group,
    -   the demographics that will be used to compute pairwise
        dissimilarity between students, and
    -   a numeric measure of each student's skill.
2.  A YAML file defining the remaining parameters of the model.

# Application to Courses

## To Interdisciplinary Course

The final author of this paper teaches an interdisciplinary course together
with a colleague from the English Language department. Students first learn
about data science and linguistics in lectures. Then, in small groups, they
discuss chosen topics from the viewpoint of linguistics and data
science/statistics. As such, it is necessary to create groups with a good
balance of Data Science and Statistics (dsds) majors, and non-dsds majors.

The table below shows a manual breakdown of 24 students in a tutorial class,
into 5 discussion groups. The left two columns describe the result of manual
assignment. And the two right-most columns describe the result of assignment
using the DBA model in `grouper`.

|          | manual_assigned |          | grouper_assigned |          |
|----------|-----------------|----------|------------------|----------|
| group_id | dsds            | non-dsds | dsds             | non-dsds |
| 1        | 2               | 3        | 3                | 2        |
| 2        | 4               | 1        | 3                | 2        |
| 3        | 3               | 2        | 2                | 3        |
| 4        | 2               | 3        | 2                | 2        |
| 5        | 2               | 2        | 3                | 2        |

It is clear that the routine does a fairer assignment. Note that, as there are
5 such tutorial groupings, this assignment has to be carried out five times.
Here is a visualisation of the groupings:

![Diverse groupings](dba_example_hs.png)

## To Course with Sub-group Structure

In the "Statement of Need" section, we mentioned a Data Science course that
requires allocating students into project teams with 2 sub-groups: one
sub-group typically develops a user-interface; and the other works on data
analysis. The table below shows how `grouper` could have benefited us if it had
been used over the four semesters the course was taught.

Consider the first semester we ran the course (row 1): the enrolment was
62 and there were 4 topic titles to be allocated (with some permitted
repetitions). In the manual allocation carried out, the average
preference of the self-formed groups was 8.16. *If we could have used
`grouper`*, the mean preference for this same group of students would
have been 9.47 (higher is better).

| Sem | manual_pref | grouper_pref | class_size | n_topics | n_project_teams |
|-----|-------------|--------------|------------|----------|-----------------|
| 1   |  8.16       |  9.47        | 62         | 4        | 7               |
| 2   | 13.10       | 13.30        | 108        | 7        | 13              |
| 3   | 12.70       | 13.50        | 89         | 7        | 11              |
| 4   |  9.48       |  9.66        | 151        | 5        | 18              |

![Preference-based groupings](pba_example_ds.png)

It is clear that `grouper` would have improved the overall preference if we had
access to it.

# Discussion

While it seems as though the improvement proffered in both applications above
is minimal, the reader should note that manual allocation is extremely arduous.
For the data science course, manual assignment typically takes a week or more.
With `grouper`, it only takes 2 minutes! Using it allows the educator time to
focus more on course content and student guidance.

# References
