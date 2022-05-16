# Init ------------------------------------------------------------

library(tidyverse)
library(survival)

# Load data -------------------------------------------------------

bc <- read_csv(file = '02-from_data_to_distribution/dat/breastcancer.csv')

# Explore data ----------------------------------------------------

head(bc)
glimpse(bc)
summary(bc)

# Prepare data ----------------------------------------------------

bc_clean <-
  bc %>%
  mutate(
    positive = factor(
      positive, levels = c(0, 1),
      labels = c('negative', 'positive')
    ),
    event = 1 - censored
  )

# Fit Gompertz distribution via ML --------------------------------

GompertzSurvival <- function (x, a, b, log = FALSE) {
  if (log) {
    (a-a*exp(b*x))/b
  } else {
    exp((a-a*exp(b*x))/b)
  }
}

GompertzHazard <- function (x, a, b, log = FALSE) {
  if (log) {
    log(a)+b*x
  } else {
    a*exp(b*x)
  }
}

GompertzDensity <- function (x, a, b, log = FALSE) {
  log_density <-
    GompertzHazard(x, a, b, log = TRUE) +
    GompertzSurvival(x, a, b, log = TRUE)
  if (log) {
    log_density
  } else {
    exp(log_density)
  }
}

GompertzLikelihood <- function (theta, x, log = TRUE) {
  loglike <-
    sum(
      GompertzDensity(x, exp(theta['a']), theta['b'],
                      log = TRUE)
    )
  if (log) {
    loglike
  } else {
    exp(loglike)
  }
}

x <- bc_clean$months[bc_clean$positive=='positive'&!bc_clean$censored]

theta_ml_fit <- optim(
  par = c(a = log(1/mean(x)), b = -0.01),
  GompertzLikelihood,
  x = x, method = 'BFGS', control = list(fnscale = -1)
)

gompertz_survival <-
  tibble(
    x = 0:150,
    Sx = GompertzSurvival(
      x = x,
      a = exp(theta_ml_fit$par['a']),
      b = theta_ml_fit$par['b']
    ),
    fx = GompertzDensity(
      x = x,
      a = exp(theta_ml_fit$par['a']),
      b = theta_ml_fit$par['b']
    )
  )

ggplot(gompertz_survival) +
  geom_line(aes(x = x, y = Sx)) +
  annotate('point', x = x, y = 0)

# find quantile values
uniroot(
  function (x, a, b) {GompertzSurvival(x, a, b)-0.75},
  interval = c(0, 150),
  a = exp(theta_ml_fit$par['a']),
  b = theta_ml_fit$par['b']
)
