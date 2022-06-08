# Survival Analysis

Lecturer: Jonas Schöley (j.schoeley@uni-rostock.de)

University of Rostock, Summer 2022

## Preparation

Please install the current versions of

- R (https://cran.r-project.org/) and
- Rstudio (https://www.rstudio.com/products/rstudio/download/).

## Course structure

- Thu Apr 14, 09-11: [**Probabilities of Survival**](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/01-probabilities_of_survival)
  - [Lecture slides](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/01-probabilities_of_survival/doc/01-probabilities_of_survival.pdf)
  - [Lecture R code](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/01-probabilities_of_survival/src/labwork.R)
  - [R code for the interactive distribution plot](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/01-probabilities_of_survival/src/interactive_exponential_distribution.R)
  - **Homework** Choose a time-to-event setting that interests you and look up a constant rate related to that setting. What is the time scale for your setting? When does the time-to-event start? When have have half of the population experienced the event given the chosen rate?
  - [Recap probability densities](https://youtu.be/hDjcxi9p0ak)
  - [Recap random variables](https://youtu.be/3v9w79NhsfI)
  - [Recap calculus](https://youtu.be/WUvTyaaNkzM)
- Thu Apr 21, 09-11: [**From Data to Distribution**](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution)
  - [Lecture slides](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/02-from_data_to_distribution/doc/02-from_data_to_distribution.pdf)
  - [Lecture R code](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution/src/labwork.R)
  - [Breastcancer data](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution/dat/breastcancer.csv)
  - [R code for the interactive likelihood demonstration](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution/src/interactive_likelihood_demonstration.R)
  - **Homework** Ensure that you have a working installation of R and RStudio. Make a new folder for this course. Associate this folder with an [RStudio project](https://support.rstudio.com/hc/en-us/articles/200526207-Using-RStudio-Projects). Write and save an R script that loads the `breastcancer` data into the R session.
- Thu Apr 28, 09-11: [**Incomplete Observations**](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution)
  - [Lecture slides](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/03-incomplete_observations/doc/03-incomplete_observations.pdf)
  - [Breastcancer data](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution/dat/breastcancer.csv)
  - [R code for the interactive likelihood demonstration](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution/src/interactive_likelihood_demonstration.R)
- Thu May 12, 09-11: [**Trial Exam**](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/04-trial_exam)
  - [Trial Exam and Solutions](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/04-trial_exam/trial_exam.pdf)
- Thu May 19, 09-11: **Exam**
- Thu June 02, 09-11: [**The Kaplan-Meier Estimator**](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/06-the_kaplan_meier_estimator)
  - [Lecture slides](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/06-the_kaplan_meier_estimator/doc/06-the_kaplan_meier_estimator.pdf)
  - [Breastcancer data](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/06-the_kaplan_meier_estimator/dat/breastcancer.csv)
  - **Homework** Using R, produce a Kaplan-Meier plot related to the topic of your eventual seminar paper ("Hausarbeit"). You don’t need to have all the data for your topic yet, but you need to find “some” related data. Be prepared to present your plot to the group. Think about study time start and end, event of interest, and censoring. You may compare multiple groups, but a KM-plot for a single group is fine as well.
