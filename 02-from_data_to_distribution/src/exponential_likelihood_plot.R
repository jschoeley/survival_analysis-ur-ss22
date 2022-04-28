# Demonstrate Maximum Likelihood Estimation of an Exponential Rate

# Init ------------------------------------------------------------

library(tidyverse)
library(patchwork)

cnst <- list(
  rate_grid = seq(0.01, 0.02, 0.001),
  domain_grid = seq(0, 120, 0.1)
)

# Data ------------------------------------------------------------

# draw observations from exponential distribution
observations <- c(22, 23, 38, 42, 73, 77, 89, 115)

# generate exponential density values over x values for different rates
densities <-
  expand.grid(
    x = cnst$domain_grid,
    rate = cnst$rate_grid,
    fx = NA
  )
densities$fx <-
  sapply(cnst$rate_grid, function (rate) dexp(cnst$domain_grid, rate = rate)) |>
  c()

# calculate likelihood of data under different rates
likelihood <-
  expand.grid(
    observed_x = observations,
    rate = cnst$rate_grid
  )
likelihood$fx <-
  dexp(x = likelihood$observed_x, rate = likelihood$rate)
likelihood$log_fx <- log(likelihood$fx)

loglikelihood <-
  likelihood |>
  group_by(rate) |>
  summarise(logL = sum(log_fx))

# join likelihoods and densities
densities <-
  left_join(densities, loglikelihood, by = 'rate')

# Plot observations, densities and corresponding likelihood -------

plots <-
  map(cnst$rate_grid, ~{
    rates <- cnst$rate_grid[cnst$rate_grid <= .x]
    plot_densities <-
      densities |>
      filter(rate == rates) |>
      ggplot(aes(x = x, y = fx, color = logL, group = rate)) +
      annotate('point', shape = 21, x = observations, y = 0) +
      geom_line() +
      scale_color_viridis_c(option = 'A', limits = c(-42, -40.5)) +
      guides(color = 'none') +
      coord_cartesian(xlim = c(0, 90), ylim = c(0, 0.02)) +
      theme_classic(base_family = 'Roboto') +
      labs(y = expression(f[italic('\u03bb')](italic(x)), x = expression(italic(x))))
    
    plot_loglikelihood <-
      loglikelihood |>
      filter(rate == rates) |>
      ggplot() +
      geom_point(aes(x = rate, y = logL, color = logL)) +
      scale_color_viridis_c(option = 'A', limits = c(-42, -40.5)) +
      guides(color = 'none') +
      coord_cartesian(xlim = c(0.01, 0.02), ylim = c(-42, -40.5)) +
      theme_classic(base_family = 'Roboto') +
      labs(
        y = expression(log~L(italic('\u03bb')) == '\u03A3'[italic(i)]~log~f[italic('\u03bb')](italic(x[i]))),
        x = expression(italic('\u03bb'))
      )
    
    plot_densities +
      plot_loglikelihood
  })

imap(plots, ~{
  ggsave(
    paste0('likelihood', .y, '.png'),
    .x, path = '02-from_data_to_distribution/out/',
    width = 8, height = 3
  )  
})
