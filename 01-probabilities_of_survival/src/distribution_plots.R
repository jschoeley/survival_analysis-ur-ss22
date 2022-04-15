# Illustrate Survival Distributions and Identities
# Jonas Schöley
# 2022-04-13

# Init ------------------------------------------------------------

library(ggplot2)
library(cowplot)

# Constants -------------------------------------------------------

cnst <- within(list(), {
  rate = 0.02428
  avg = 1/rate
  quantiles = c(0.01, 0.25, 0.5, 0.75, 0.99)
  quantile_values = qexp(quantiles, rate)
  names(quantile_values) = quantiles
})

fig <- list()

# Survival function -----------------------------------------------

fig$survival_function <-
  ggplot() +
  geom_function(
    fun = pexp, args = list(rate = cnst$rate, lower.tail = FALSE)
  ) +
  annotate('point', x = cnst$quantile_values[2:4],
           y = 1-cnst$quantiles[2:4]) +
  annotate(
    'segment',
    x = cnst$quantile_values[2:4], xend = cnst$quantile_values[2:4],
    y = 0, yend = 1-cnst$quantiles[2:4],
    lty = 3
  ) +
  annotate(
    'text', x = cnst$quantile_values[2:4], y = 0,
    label = formatC(cnst$quantile_values[2:4], format = 'f', digits = 1),
    hjust = 1.1, vjust = -0.5, size = 3.5, family = 'Roboto'
  ) +
  scale_x_continuous(limits = c(0, cnst$quantile_values['0.99']),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  labs(x = 'Time x', y = 'S(x) Survival function') +
  theme_classic(base_family = 'Roboto') +
  coord_cartesian(clip = 'off')

fig$survival_function

ggsave(
  '01-probabilities_of_survival/out/survival_function.svg',
  fig$survival_function, width = 8, height = 5, units = 'in', scale = 0.8
)

# Hazard function -------------------------------------------------

fig$hazard_function <-
  ggplot() +
  geom_hline(yintercept = cnst$rate) +
  scale_x_continuous(limits = c(0, cnst$quantile_values['0.99']),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 0.1), expand = c(0,0)) +
  labs(x = 'Time x', y = 'h(x) Hazard function') +
  theme_classic(base_family = 'Roboto') +
  coord_cartesian(clip = 'off')

fig$hazard_function

ggsave(
  '01-probabilities_of_survival/out/hazard_function.svg',
  fig$hazard_function, width = 8, height = 5, units = 'in', scale = 0.8
)

# Normal density --------------------------------------------------

fig$normal_density <-
  ggplot() +
  geom_function(fun = dnorm, args = list(mean = 0, sd = 1),
                size = 1.5) +
  scale_x_continuous(limits = c(-3, 3), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = 'x', y = 'Probability density') +
  theme_classic(base_family = 'Roboto') +
  coord_cartesian(clip = 'off')

fig$normal_density

ggsave(
  '01-probabilities_of_survival/out/normal_density.svg',
  fig$normal_density, width = 8, height = 5, units = 'in', scale = 0.8
)

# Survival density ------------------------------------------------

fig$various_densities <-
  ggplot() +
  geom_function(fun = dexp, args = list(rate = cnst$rate)) +
  geom_function(fun = dlnorm, args = list(mean = 2, sd = 1), n = 1e3) +
  geom_function(fun = dweibull, args = list(shape = 5, scale = 20), n = 1e3) +
  scale_x_continuous(limits = c(0, cnst$quantile_values['0.99']), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = 'Time x', y = 'f(x) Probability density') +
  theme_classic(base_family = 'Roboto') +
  coord_cartesian(clip = 'off')

fig$various_densities

ggsave(
  '01-probabilities_of_survival/out/various_densities.svg',
  fig$various_densities, width = 8, height = 5, units = 'in', scale = 0.8
)

# Cumulative distribution function --------------------------------

fig$cumulative_distribution_function <-
  ggplot() +
  geom_function(fun = pexp, args = list(rate = cnst$rate), size = 1.5) +
  annotate('point', x = cnst$quantile_values[2:4], y = cnst$quantiles[2:4], size = 3) +
  scale_x_continuous(limits = c(0, cnst$quantile_values['0.99']), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  labs(x = 'Time x', y = 'F(x) Cumulative probability P(X\u2264x)') +
  theme_classic(base_family = 'Roboto') +
  coord_cartesian(clip = 'off')

fig$cumulative_distribution_function

ggsave(
  '01-probabilities_of_survival/out/cumulative_distribution_function.svg',
  fig$cumulative_distribution_function,
  width = 8, height = 5, units = 'in', scale = 0.8
)

# Exponential hazard ----------------------------------------------

fig$exponential_hazard <-
  ggplot() +
  geom_hline(yintercept = 0.02428) +
  annotate('text', x = 1, y = 0.025, label = 'h(x)=λ=0.02428',
           family = 'Roboto', hjust = 0, vjust = 0) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 0.1), expand = c(0,0)) +
  labs(x = 'x weeks into semester', y = 'Hazard function h(x)') +
  theme_classic(base_family = 'Roboto') +
  coord_cartesian(clip = 'off')
fig$exponential_hazard

ggsave(
  '01-probabilities_of_survival/out/exponential_hazard.svg',
  fig$exponential_hazard, width = 8, height = 5, units = 'in', scale = 0.8
)


# Exponential survival --------------------------------------------

fig$exponential_survival <-
  ggplot() +
  geom_function(
    fun = pexp, args = list(rate = cnst$rate, lower.tail = FALSE)
  ) +
  annotate('point', x = cnst$quantile_values[2:4],
           y = 1-cnst$quantiles[2:4]) +
  annotate(
    'segment',
    x = cnst$quantile_values[2:4], xend = cnst$quantile_values[2:4],
    y = 0, yend = 1-cnst$quantiles[2:4],
    lty = 3
  ) +
  annotate(
    'text', x = cnst$quantile_values[2:4], y = 0,
    label = formatC(cnst$quantile_values[2:4], format = 'f', digits = 1),
    hjust = 1.1, vjust = -0.5, size = 3.5, family = 'Roboto'
  ) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  labs(x = 'x weeks into semester', y = 'S(x) Survival function') +
  theme_classic(base_family = 'Roboto') +
  coord_cartesian(clip = 'off')

fig$exponential_survival

ggsave(
  '01-probabilities_of_survival/out/exponential_survival.svg',
  fig$exponential_survival, width = 8, height = 5, units = 'in', scale = 0.8
)
