# current incidence
indicence <- 0.02428

# probability of not catching COVID given current rates
exp(-indicence*14)

# cumulative distribution function
pexp(q = 14, rate = indicence)

# survival function
1- pexp(q = 14, rate = indicence)

rexp(1000, rate = indicence)
1/mean(rexp(1000, rate = indicence))
