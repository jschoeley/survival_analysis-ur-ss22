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
- Thu Apr 21, 09-11:  [**From Data to Distribution**](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution)
  - [Lecture slides](https://github.com/jschoeley/survival_analysis-ur-ss22/blob/main/02-from_data_to_distribution/doc/02-from_data_to_distribution.pdf)
  - [Lecture R code](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution/src/labwork.R)
  - [Breastcancer data](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution/dat/breastcancer.csv)
  - [R code for the interactive likelihood demonstration](https://github.com/jschoeley/survival_analysis-ur-ss22/tree/main/02-from_data_to_distribution/src/interactive_likelihood_demonstration.R)
  - **Homework** Using the breastcancer data choose a survival distribution (e.g. Exponential, Weibull, Gamma, Log-Normal) and fit it to the data via Maximum Likelihood. Use R. You can adapt the script we wrote today.