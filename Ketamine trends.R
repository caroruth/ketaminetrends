library(tidyverse)
library(janitor)
library(patchwork)

nflis_spline <- ggplot(nflis, aes(year, per_1000)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) + # fits a generalized additive model to the data. estimates degree of smoothness and number of (cubic) basis functions  
  labs(title = NULL, x = "Year", y = "Seizures per 1,000 Seizures") +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)), breaks = c(2000:2019)) +
  scale_y_continuous(limits = c(0, 3.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text=element_text(size=14))

# using the poly function to produce orthogonal polynomials so that the variables aren't correlated
summary(lm(per_1000 ~ poly(year,3), data = nflis))

nsduh_spline_int <- ggplot(nsduh, aes(yearq, age_12_34)) +
  geom_point(aes(yearq, age_12_34_early)) +
  geom_line(aes(yearq, age_12_34_early)) +
  geom_smooth(data = nsduh, aes(yearq, age_12_34_early), method = "gam", formula = y ~ s(x, bs = "cs")) + # separate smooths for pre and post 2015 survey redesign change
  geom_point(aes(yearq, age_12_34_late)) +
  geom_line(aes(yearq, age_12_34_late)) +
  geom_smooth(data = nsduh, aes(yearq, age_12_34_late), method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(title = NULL, x = "Year", y = "Prevalence of Past-Year Use, %") +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)), limits = c(2006.0, 2019.75), breaks = seq(2006.0, 2019.75, 0.25)) +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text=element_text(size=14))

summary(lm(age_12_34 ~ poly(yearq,3), data = filter(nsduh, time < 2015))) # separate regressions for pre and post 2015 survey design change
summary(lm(age_12_34 ~ poly(yearq,3), data = filter(nsduh, time >= 2015)))

pcc_spline <-  ggplot(pcc, aes(year, rate1)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = -1, bs = "cs")) + # spline
  labs(title = NULL, x = "Year", y = "Exposures per 1,000,000 Population") +
  scale_x_continuous(expand = expansion(mult = c(0.01,0.01)), breaks = c(1991:2019)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text=element_text(size=14))

summary(lm(rate1 ~ poly(year,3), data = pcc))