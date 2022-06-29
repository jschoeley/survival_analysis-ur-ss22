# Survival analysis of US fetal mortality

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
library(gtsummary)
# transform model output to rectangular table
library(broom)
# data visualization
library(ggplot2)

# Create global objects -------------------------------------------

# I like to keep all figures in a common list, e.g. at one place;
# this list I call "fig"
fig <- list()
# ... same with tables
tab <- list()

# Load and clean data ---------------------------------------------

# read the breastcancer.csv data into R and give it the name "bc"
fi <- readRDS('09-effect_modification/dat/fetoinfant.rds')
# sample 100,000 cases so that this demonstration needs less ressources
set.seed(1987)
fi <- fi[sample(1:nrow(fi), size = 1e5),]
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

# co-variate composition of sample by event indicator
tab$cross_strata_by_event <-
  fi_clean %>%
  select(type, dod, sex, origin, gestation, event) %>%
  tbl_summary(by = "event") %>%
  add_overall() %>%
  add_p()
# save the table as document
tab$cross_strata_by_event %>% as_gt() %>%
  gt::gtsave(
    filename = 'cross_strata_by_event.rtf',
    path = './09-effect_modification/out/'
  )

# show distribution of sex by conception cohort
tab$cross_dod_sex <-
  fi_clean %>%
  tbl_cross(row = 'dod', col = 'sex', percent = 'row') %>% 
  add_p()
tab$cross_dod_sex %>% as_gt() %>%
  gt::gtsave(
    filename = 'cross_dod_sex.rtf',
    path = './09-effect_modification/out/'
  )

# show distribution of maternal origin by conception cohort
tab$cross_dod_origin <-
  fi_clean %>%
  tbl_cross(row = 'dod', col = 'origin', percent = 'row') %>% 
  add_p()
tab$cross_dod_origin %>% as_gt() %>%
  gt::gtsave(
    filename = 'cross_dod_origin.rtf',
    path = './09-effect_modification/out/'
  )

# show share of fetal deaths (events) by conception cohort
tab$cross_dod_event <-
  fi_clean %>%
  tbl_cross(row = 'dod', col = 'event', percent = 'row', digits = 2) %>% 
  add_p()
tab$cross_dod_event %>% as_gt() %>%
  gt::gtsave(
    filename = 'cross_dod_event.rtf',
    path = './09-effect_modification/out/'
  )

# Create survival object ------------------------------------------

fi_surv <-
  Surv(
    # survival or censoring time
    time = fi_clean$time,
    # event indicator (1: event, 0: censoring)
    event = fi_clean$event,
    # we only consider right censoring
    type = 'right'
  )

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
glimpse(fi_km_total_tab)

# using the ggplot library we can take the Kaplan-Meier estimator
# output table and visualize it
fig$fi_km_plot <-
  ggplot(fi_km_origin_tab) +
  # add total survival
  annotate('step', color = 'grey',
           x = fi_km_total_tab$time, y = fi_km_total_tab$estimate) +
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
  # change the plots theme
  theme_classic(base_rect_size = 0) +
  # change the plots labels
  labs(y = 'Probability of Survival', x = 'Weeks of gestation',
       color = 'Maternal origin') +
  facet_wrap(~strata) +
  guides(color = 'none')
fig$fi_km_plot

ggsave(fig$fi_km_plot, device = 'png', width = 8, height = 6,
       path = './09-effect_modification/out/',
       filename = 'fi_km_plot.png')

# Fit Cox regression models ---------------------------------------

# H1: The risk of fetal death varies by origin of the mother

# we fit a Cox Proportional hazards regression model with
# sex, year of delivery, and maternal origin as predictors
fi_h1_cox_fit <- coxph(fi_surv~sex+dod+origin, data = fi_clean)

# the summary function, if applied to the cox model fit object,
# gives us hazard ratios along with confidence intervals.
# E.g. the risk of fetal death following 24 weeks of gestation
# when the mother is of non-hispanic Black origin is elevated by
# a factor of 2.15 (95% CI 1.69, 2.73)
summary(fi_h1_cox_fit)

# check the proportional hazards assumption
fi_h1_cox_ph <- cox.zph(fi_h1_cox_fit)
fi_h1_cox_ph
# for none of the variables in the model we have significant
# evidence to reject the proportional hazards assumption
# additional we can graphically check if the effect of a variable
# varies over time, which would violate the proportional hazards
# assumption. For the PH assumption to be true, the plotted line should
# be flat around 0.
fi_h1_cox_ph_fig <- ggcoxzph(fi_h1_cox_ph, se = FALSE)
fi_h1_cox_ph_fig

# produce a model estimate table; the function tbl_regression
# originates from the modelsummary package
tab$h1_cox <- tbl_regression(fi_h1_cox_fit, exponentiate = TRUE)
tab$h1_cox %>% as_gt() %>%
  gt::gtsave(
    filename = 'h1_cox.rtf',
    path = './09-effect_modification/out/'
  )

# Fit a cox regression model with interactions --------------------

# H2: The male sex disadvantage in fetal death declined between 1989
#     and 2007

# we model an interaction between sex and the year of delivery, where
# year of delivery is modelled as a discrete (factor) variable. In
# other words, we allow the HR of sex to change by year of delivery.
# Interactions are specified via the `*` symbol.
fi_h2_cox_fit <- coxph(fi_surv~sex*dod_fct+origin, data = fi_clean)
summary(fi_h2_cox_fit)
# Let's make sense of the output. Whenever an interaction is included
# in the model, the interpretation of the participating terms changes:
# sexMale=0.80 is the hazard ratio of males to females for the baseline
# level of the dod (year of delivery) variable, i.e. in the year 1989.
# dod_fct2007 is the hazard ratio of the delivery year 1995 to 1989 for
# the baseline level of the sex variable, i.e. Females.
# sexMale:dod_fct2007 is the hazard ratio of Males in 2007 vs. the
# sex and dod baselines, i.e. females in 1989. It also tells us by
# what factor the hazard ratio of males changed from 1989 (dod baseline)
# vs. 2007. All of this can be determined by just writing down the model
# equations and some algebraic manipulations.
# Ignoring the variables not in the interaction here as they don't
# matter for the argument we have the model equation
# 
# h(t|dod2007, Male) = h(t)*
# exp(b_dod2007*dod2007)*
# exp(b_Male*Male)*
# exp(b_dod2007xMale*dod2007xMale)
#
# So the hazard ratio for males vs. females (baseline) in
# 1989 (baseline) is exp(b_Male), because
#
# HR(Male|1989) = h(t|dod2007=0, Male=1)/h(t|dod2007=0, Male=0) =
# [ h(t)*exp(0)*exp(b_Male)*exp(0) ] /
# [ h(t)*exp(0)*exp(0)*exp(0) ] =
# exp(b_Male)
#
# And the hazard ratio for males vs. females in 2007 is
# exp(b_Male)*exp(b_dod2007xMale), because
#
# HR(Male|2007) = h(t|dod2007=1, Male=1)/h(t|dod2007=1, Male=0) =
# [ h(t)*exp(b_dod2007)*exp(b_Male)*exp(b_dod2007xMale) ] /
# [ h(t)*exp(b_dod2007)*exp(0)*exp(0) ] =
# exp(b_Male)*exp(b_dod2007xMale)

# The multcomp package allows us to get confidence intervals
# around linear combinations (sums and weighted sums) of model
# parameters. This is needed to get CIs for HR(Male|2007) or
# HR(Male|2001) or HR(Male|1995)
library(multcomp)
tab$cox_h2_sex_hr_by_dod <- glht(
  fi_h2_cox_fit,
  linfct = c(
    "sexMale == 0",
    "sexMale + sexMale:dod_fct1995 == 0",
    "sexMale + sexMale:dod_fct2001 == 0",
    "sexMale + sexMale:dod_fct2007 == 0"
  )
)

# remember: we're ultimately still dealing with an interaction
# effect between sex and date of delivery, but we've converted
# the interaction effect estimate to more easily interpretable
# conditional hazard ratio estimates, which we then plot
fig$cox_h2_se_hr_by_dod <-
  exp(confint(tab$cox_h2_sex_hr_by_dod)$confint) %>%
  as_tibble() %>%
  mutate(
    year = c('1989', '1995', '2001', '2007')
  ) %>%
  ggplot() +
  geom_hline(yintercept = 1, size = 2, color = 'grey70') +
  geom_pointrange(aes(x = year, y = Estimate, ymin = lwr, ymax = upr)) +
  scale_y_log10() +
  labs(x = 'Year', y = 'Hazard ratio Male vs. Female')

ggsave(fig$cox_h2_se_hr_by_dod, device = 'png', width = 8, height = 4,
       path = './09-effect_modification/out/',
       filename = 'cox_h2_se_hr_by_dod.png')

# produce a model estimate table; the function tbl_regression
# originates from the modelsummary package
tab$h2_cox <- tbl_regression(fi_h2_cox_fit, exponentiate = TRUE)
tab$h2_cox %>% as_gt() %>%
  gt::gtsave(
    filename = 'h2_cox.rtf',
    path = './09-effect_modification/out/'
  )
