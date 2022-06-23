# Survival analysis of US fetal mortality
#
# H1: The risk of fetal death varies by origin of the mother.

# Load Libraries --------------------------------------------------

# load the survival package which gives us additional functions for
# survival analysis
library(survival)
# the survminer package too, gives us better functionality for
# survival analysis
library(survminer)
# functions for data transformation
library(dplyr)
# better summary tables for model objects
library(modelsummary)
# transform model output to rectangular table
library(broom)
# data visualization
library(ggplot2)

# Load and clean data ---------------------------------------------

# read the breastcancer.csv data into R and give it the name "bc"
fi <- readRDS("08-the_cox_proportional_hazards_regression/dat/fetoinfant.rds")
# check if loading worked
glimpse(fi)
summary(fi)

# select variables of interest
fi_clean <-
  fi %>%
  select(
    type,
    # here we rename the variable <new name> = <old name>
    dod = date_of_delivery_y,
    sex,
    origin = race_and_hispanic_orig_of_mother,
    gestation = gestation_at_delivery_w
  )

# check distribution of gestation at delivery
table(fi_clean$gestation)
# we remove cases which died or got delivered prior to week 24,
# i.e. we condition on still being a living fetus at week 24
# but before we do that we keep track of what we remove
fi_drop_pre24 <-
  fi_clean %>%
  filter(
    gestation < 24
  ) %>%
  count(type)
# now do the actual filter
fi_clean <-
  fi_clean %>%
  filter(
    gestation >= 24
  )

# further, we remove cases where maternal origin is unknown
fi_drop_originna <-
  fi_clean %>%
  filter(
    is.na(origin)
  ) %>%
  count(type, dod)

# assign variable labels
fi_clean <-
  fi_clean %>%
  mutate(
    # (1)  Female
    # (2)  Male
    # (-9) Missing
    sex = case_when(
      sex == 1 ~ 'Female',
      sex == 2 ~ 'Male',
      sex == -9 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    # (1) Births and infant deaths
    # (2) Fetal deaths
    type = case_when(
      type == 1 ~ 'Birth',
      type == 2 ~ 'Fetal death',
      TRUE ~ NA_character_
    ),
    # (1) Non-hispanic white
    # (2) Non-hispanic black
    # (3) Mexican
    # (4) Non-hispanic other races
    # (5) Central or South American
    # (6) Other
    origin = case_when(
      origin == 1 ~ 'Non-Hispanic White',
      origin == 2 ~ 'Non-Hispanic Black',
      origin == 3 ~ 'Mexican',
      origin == 4 ~ 'Non-Hispanic other races',
      origin == 5 ~ 'Central or South American',
      origin == 6 ~ '(Other)',
      TRUE ~ NA_character_
    )
  )

# create factor variables and choose reference level
fi_clean <-
  fi_clean %>%
  mutate(
    sex = relevel(as.factor(sex), ref = 'Female'),
    origin = relevel(as.factor(origin), ref = 'Non-Hispanic White'),
    dod_fct = relevel(as.factor(dod), ref = '1989')
  )

# generate survival time and censoring variables
fi_clean <-
  fi_clean %>%
  mutate(
    # if a fetal death occured, then set the event
    # variable to 1, otherwise set it to 0, indicating
    # a right-censored observation
    event = ifelse(type == 'Fetal death', 1, 0),
    time = gestation
  )

# Descriptive tables ----------------------------------------------

# show conception cohort sizes by sex
datasummary_crosstab(
  dod ~ sex,
  data = fi_clean
)

# show distribution of maternal origin by conception cohort
datasummary_crosstab(
  dod ~ origin,
  data = fi_clean
)

# show share of fetal deaths (events) by conception cohort
datasummary_crosstab(
  origin ~ Factor(event),
  data = fi_clean
)

datasummary(
  Factor(dod) ~ Factor(event)*Factor(sex),
  statistic = ~ Percent(denom = 'col'),
  data = fi_clean
)

datasummary_crosstab(
  dod ~ sex,
  data = fi_clean
)



# show counts and composition of maternal origin by cohort
datasummary_crosstab(
  dod ~ origin,
  data = fi_clean
)

# show counts and composition of maternal origin by cohort
datasummary_crosstab(
  dod ~ origin,
  data = fi_clean
)

# Create survival object ------------------------------------------

fi_surv <-
  Surv(time = fi_clean$time, event = fi_clean$event, type = 'right')

# Kaplan Meier and Logrank analysis -------------------------------

# the survfit function, try ?survfit, can be used to fit a Kaplan-Meier
# estimator to the data

# Kaplan-Meier estimator for survival probability of fetal death
# over weeks of gestation
fi_km_total <- survfit(fi_surv~1, data = fi_clean)

# Kaplan-Meier estimator stratified by origin
fi_km_origin <- survfit(fi_surv~origin, data = fi_clean)

# the library broom provides the function "tidy", this function takes
# the output of many modelling functions in R, such as "survfit", and
# transforms it into a simple table, a "data frame" in R parlance.
fi_km_total_tab <- tidy(fi_km_total)
fi_km_origin_tab <- tidy(fi_km_origin)

# let's have a look at the "tidy" Kaplan-Meier output
# you can find a description of the output here: ?tidy.survfit
# the important variables are
# time: the time point of an event
# estimate: the Kaplan-Meier estimate at this time point
# conf.high: upper end of 95% confidence interval around estimate
# conf.low: lower end of 95% confidence interval around estimate
# strata: the group the estimate refers to
glimpse(fi_km_tab)

# using the ggplot library we can take the Kaplan-Meier estimator
# output table and visualize it
fi_km_plot <-
  ggplot(fi_km_origin_tab) +
  # show confidence intervals on plot
  geom_step(aes(x = time, y = conf.low, color = strata),
            lty = 1, alpha = 0.3) +
  geom_step(aes(x = time, y = conf.high, color = strata),
            lty = 1, alpha = 0.3) +
  # mark censoring times on plot
  # geom_point(
  #   aes(x = time, y = estimate, color = strata),
  #   shape = 3,
  #   data = . %>% filter(n.censor > 0)
  # ) +
  # add kaplan meier estimate to the plot
  geom_step(aes(x = time, y = estimate, color = strata)) +
  # add total survival
  annotate('step', color = 'grey',
           x = fi_km_total_tab$time, y = fi_km_total_tab$estimate) +
  # change the plots theme
  theme_classic() +
  # change the plots labels
  labs(y = 'Probability of Survival', x = 'Weeks of gestation',
       color = 'Maternal origin') +
  facet_wrap(~strata) +
  guides(color = 'none')
fi_km_plot

ggsave(fi_km_plot, device = 'png', width = 8, height = 6,
       path = '08-the_cox_proportional_hazards_regression',
       filename = 'fi_km_plot.png')

# Fit Cox regression models ---------------------------------------

fi_cox_fit <- coxph(fi_surv~sex+dod+origin, data = fi_clean)
summary(fi_cox_fit)

# check the proportional hazards assumption
fi_cox_ph <- cox.zph(fi_cox_fit)
fi_cox_ph_fig <- ggcoxzph(fi_cox_ph, se = FALSE)

# produce a model table; the function modelsummary originates from
# the modelsummary package
modelsummary(
  fi_cox_fit,
  estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",
  statistic = NULL,
  exponentiate = TRUE,
  stars = TRUE,
  gof_map = c('nobs', 'aic')
  #output = './08-the_cox_proportional_hazards_regression/fi_cox.docx'
)
