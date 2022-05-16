library(tidyverse)

breastcancer <- read_csv(file = "03-incomplete_observations/dat/breastcancer.csv")

glimpse(breastcancer)
summary(breastcancer)



filter(breastcancer, positive == 1 & censored == 0)
# is the same as
likelihood <-
  breastcancer %>%
  filter(positive == 1 & censored == 0) %>%
  mutate(
    fx = dexp(x = months, rate = 0.02),
    log_fx = log(fx)
  )

sum(likelihood$log_fx)

nrow(likelihood)/
sum(likelihood$months)
