# PH125.9x: Capstone - Choose Your Own!

This repository contains my solution to the Chose Your Own! project.
The three files required for submission (Rmd, PDF, R) can be found in the
`./submission` directory.  


## Overview (copied from course materials)

For this project, you will be applying machine learning techniques that go 
beyond standard linear regression. You will have the opportunity to use a 
publicly available dataset to solve the problem of your choice. You are strongly
discouraged from using well-known datasets, particularly ones that have been 
used as examples in previous courses or are similar to them (such as the iris, 
titanic, mnist, or movielens datasets, among others) - this is your opportunity
to branch out and explore some new data! The UCI Machine Learning Repository and
Kaggle are good places to seek out a dataset. Kaggle also maintains a curated 
list of datasets that are cleaned and ready for machine learning analyses. Your 
dataset must be automatically downloaded in your code or included with your 
submission. You may not submit the same project for both the MovieLens and 
Choose Your Own project submissions.

The ability to clearly communicate the process and insights gained from an 
analysis is an important skill for data scientists. You will submit a report 
that documents your analysis and presents your findings, with supporting 
statistics and figures. The report must be written in English and uploaded as 
both a PDF document and an Rmd file. Although the exact format is up to you, the 
report should include the following at a minimum:

- an introduction/overview/executive summary section that describes the dataset 
and variables, and summarizes the goal of the project and key steps that were 
performed;
- a methods/analysis section that explains the process and techniques used, 
including data cleaning, data exploration and visualization, insights gained, 
and your modeling approaches (you must use at least two different models or 
algorithms);
- a results section that presents the modeling results and discusses the model 
performance; 
- and a conclusion section that gives a brief summary of the report, its 
potential impact, its limitations, and future work.

Your project submission will be graded both by your peers and by a staff member. 
The peer grading will give you an opportunity to check out the projects done by 
other learners. You are encouraged to give your peers thoughtful, specific 
feedback on their projects (i.e., more than just "good job" or "not enough 
detail").


## Instructions (copied from course materials)

The submission for the choose-your-own project will be three files: a report in 
the form of both a PDF document and Rmd file and the R script that performs your 
machine learning task.  

1. Your report in PDF format  
2. Your report in Rmd format  
3. A script in R format that performs a supervised machine learning task  

You must also provide access to your dataset, either 
through automatic download in your script or inclusion in a GitHub repository. 
(Remember, you are strongly discouraged from using well-known datasets, 
particularly ones that have been used as examples in previous courses or are 
similar to them. Also remember that you may not submit the same project for both 
the MovieLens and Choose Your Own project submissions.) We recommend submitting 
a link to a GitHub repository with these three files and your dataset. Your 
grade for the project will be based on your report and your script.

### Report and Script

Your report and script will be graded by your peers, based on a rubric defined 
by the course staff, as well as by the course staff. The staff grade will be 
your final grade for the project. To receive your grade, you must review and 
grade the reports of five of your fellow learners after submitting your own. 
This will give you the chance to learn from your peers. You are encouraged to 
give your peers thoughful and specific feedback on their projects.

Please pay attention to the due dates listed! The project submission is due 
before the end of the course to allow time for peer grading. Also note that you 
must grade the reports of your peers by the course close date in order to 
receive your grade.

### Honor Code

You are welcome to discuss your project with others, but all submitted work must 
be your own. Your participation in this course is governed by the terms of the 
edX Honor Code. If your report is found to violate the terms of the honor code 
(for example, if you copy a project from another learner), you will be 
unenrolled from the course and will not be eligible to receive a certificate.


## Grading Rubric (copied from course materials)

Note: after you submit your project, please check immediately after submitting 
to make sure that all files were correctly uploaded. Occasionally, there are 
file upload failures, and it's easiest to fix if these are caught early.

### Files (5 points possible)

The appropriate files are submitted in the correct formats: a report in both PDF 
and Rmd format and an R script in R format.

- 0 points: No files provided  
- 3 points: At least one file is missing and/or not in the correct format  
- 5 points: All 3 files were submitted in the requested formats  

### Report (25 points possible)

The report documents the analysis and presents the findings, along with 
supporting statistics and figures. In order to demonstrate your understanding of 
course material, please provide thorough explanation or justification for 
various steps of your project, such as why a specific train/test split (e.g. 
50/50 vs 90/10) or algorithm was used. The report must be written in English and 
uploaded. The report must include at least the following sections:

- An introduction/overview/executive summary section that describes the dataset 
and variables, and summarizes the goal of the project and key steps that were 
performed.  
- A methods/analysis section that explains the process and techniques used, 
including data cleaning, data exploration and visualization, any insights 
gained, and your modeling approach. At least two different models or algorithms 
must be used, with at least one being more advanced than simple linear 
regression for prediction problems.  
- A results section that presents the modeling results and discusses the model 
performance.  
- A conclusion section that gives a brief summary of the report, its potential 
impact, its limitations, and future work.  
  
  
- 0 points: The report is either not uploaded or contains very minimal 
information OR the report appears to violate the terms of the edX Honor Code.  
- 5 points: One or more required sections of the report are missing.  
- 10 points: The report includes all required sections, but the report is 
significantly difficult to follow or missing significant supporting detail in 
multiple sections.  
- 15 points: The report includes all required sections, but the report is 
difficult to follow or missing supporting detail in one section (or has minor 
flaws in multiple sections).  
- 20 points: The report includes all required sections and is easy to follow, 
but with minor flaws in one section.  
- 25 points: The report includes all required sections, is easy to follow with 
good supporting detail throughout, and is insightful and innovative.  


### Code (20 points)

The code in the R script should run without errors and should be well-commented 
and easy to follow. It should also use relative file paths and automatically 
install missing packages. The dataset you use should either be automatically 
downloaded by your code or provided in your GitHub repo. If your dataset is 
provided as a zip file in GitHub, your code should automatically unzip and load 
it.  

- 0 points: Code does not run and produces many errors OR code appears to 
violate the terms of the edX Honor Code.  
- 5 points: Code runs but does not produce output consistent with what is 
presented in the report OR there is overtraining (the test set is used for 
training steps).  
- 10 points: Code runs but is difficult to follow and/or may not produce output 
entirely consistent with what is presented in the report.  
- 15 points: Code runs, can be followed, is at least mostly consistent with the 
report, but is lacking (sufficient) comments and explanation OR uses absolute 
paths instead of relative paths OR does not automatically install missing 
packages OR does not provide easy access to the dataset (either via automatic 
download or inclusion in a GitHub repository).  
- 20 points: Code runs easily, is easy to follow, is consistent with the report, 
and is well-commented. All file paths are relative and missing packages are 
automatically installed with if(!require) statements.  

