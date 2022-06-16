# Kaplan-Meier Analysis of Breast Cancer Data

# load the survival package which gives us additional functions for
# survival analysis
library(survival)

# read the breastcancer.csv data into R and give it the name "bc"
bc <- read.csv("06-the_kaplan_meier_estimator/dat/breastcancer.csv")

# if you don't know how the Surv() function works or what it does,
# type ?Surv to find help. This works with all functions
# here we create a "survival object", i.e. we tell R about our time
# and censoring variable and bind both together into a survival object
# we call "bc_surv".
bc_surv <- Surv(bc$months, event = 1-bc$censored)

# the survfit function, try ?survfit, can be used to fit a Kaplan-Meier
# estimator to the data, we stratify by the binary variable "positive"
bc_km <- survfit(bc_surv~positive, data = bc)

# if you don't want to stratify, i.e. you want a KM estimator for your
# entire sample write ~1
survfit(bc_surv~1, data = bc)

# the library broom gives the function "tidy", this function takes the
# output of many modelling functions in R, such as "survfit", and
# transforms it into a simple table, a "data frame" in R parlance.
library(broom)
bc_km_tab <- tidy(bc_km)

# let's have a look at the "tidy" Kaplan-Meier output
# you can find a description of the output here: ?tidy.survfit
# the important variables are
# time: the time point of an event
# estimate: the Kaplan-Meier estimate at this time point
# conf.high: upper end of 95% confidence interval around estimate
# conf.low: lower end of 95% confidence interval around estimate
# strata: the group the estimate refers to
bc_km_tab

# using the ggplot library we can take the Kaplan-Meier estimator
# output table and visualize it.
library(ggplot2)
breastcancer_km_plot <-
  ggplot(bc_km_tab) +
  # show confidence intervals on plot
  geom_step(aes(x = time, y = conf.low, color = strata),
            lty = 2) +
  geom_step(aes(x = time, y = conf.high, color = strata),
            lty = 2) +
  # mark censoring times on plot
  geom_point(
    aes(x = time, y = estimate, color = strata),
    shape = 3,
    data = bc_km_tab[bc_km_tab$n.censor!=0,]
  ) +
  # add kaplan meier estimate to the plot
  geom_step(aes(x = time, y = estimate, color = strata)) +
  # change the plots theme
  theme_classic() +
  # change the plots labels
  labs(y = 'Probability of Survival', x = 'Months', color = 'Blood marker')

# save plot
ggsave(path = '06-the_kaplan_meier_estimator/out/',
       filename = 'breastcancer_km_plot.png', width = 7, height = 5)

# while you can build your survival plot manually with ggplot, you may
# also make use of the "survminer" package which gives the function
# "ggsurvplot" which draws nice KM-Plots
# install.packages("survminer")
library(survminer)
ggsurvplot(bc_km, conf.int = TRUE, risk.table = TRUE)
