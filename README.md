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
  - [Breastcancer data]
    (https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution/dat/breastcancer.csv)
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
  - [Lecture R code](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/06-the_kaplan_meier_estimator/src/06-the_kaplan_meier_estimator.R)
  - [Breastcancer data](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/06-the_kaplan_meier_estimator/dat/breastcancer.csv)
  - Cheat sheets are short introductions to certain topics:
    - [R Language Cheat Sheet](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/06-the_kaplan_meier_estimator/doc/r_cheat_sheet.pdf)
    - [ggplot2 Cheat Sheet](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/06-the_kaplan_meier_estimator/doc/ggplot_cheat_sheet.pdf)
    - [R-Studio Cheat Sheet](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/06-the_kaplan_meier_estimator/doc/rstudio_cheat_sheet.pdf)
  - **Homework** Using R, produce a Kaplan-Meier plot related to the topic of your eventual seminar paper ("Hausarbeit"). You don’t need to have all the data for your topic yet, but you need to find “some” related data. Be prepared to present your plot to the group. Think about study time start and end, event of interest, and censoring. You may compare multiple groups, but a KM-plot for a single group is fine as well.
- Thu June 16, 09-11: [**The Logrank Test**](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/07-the_logrank_test)
- Thu June 23, 09-11: [**The Cox Proportional Hazards Regression**](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/08-the_cox_proportional_hazards_regression)
  - [Klein & Moeschberger (2003). Survival Analysis. Chapters 8.1, 8.2](https://link.springer.com/book/10.1007/b97377)
  - [Harrel (2015). Regression modeling strategies. Chapters 20, 21](https://link.springer.com/book/10.1007/978-3-319-19425-7)
- Thu June 30, 09-11: [**Effect modification**](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/09-effect_modification)

## Literature

- Survival analysis:
  - [Klein & Moeschberger (2003). Survival Analysis](https://link.springer.com/book/10.1007/b97377) is a classic textbook on survival analysis.
  - [Harrel (2015). Regression modeling strategies](https://link.springer.com/book/10.1007/978-3-319-19425-7) is a great reference for all regression related topics. It's chapters on Survival Analysis feature R code and are very approachable.
  - [Kaplan & Meier (1958). Nonparametric Estimation from Incomplete Observations](https://doi.org/10.2307/2281868). The most cited statistics paper and one of the fundamental methods of survival analysis.
  - [Cox (1958) Regression Models and Life Tables](https://www.jstor.org/stable/2985181?seq=1). The second most cited statistics paper and the other pillar of survival analysis.
- For dataviz:
  - [Hadley Wickham: ggplot2](https://ggplot2-book.org/index.html)
  - [Claus Wilke: Fundamentals of Data Visualization](https://clauswilke.com/dataviz/)
