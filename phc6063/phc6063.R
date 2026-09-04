
library(tidyverse)

# given values
mean.0 <- 74
mean.new <- 84
sd.yield <- 15
alpha <- 0.05

effect <- mean.new - mean.0


# power calculations
power30 <- power.t.test(n = 30, delta = effect, sd = sd.yield,
                        sig.level = alpha, type = "two.sample",
                        alternative = "two.sided")$power

power40 <- power.t.test(n = 40, delta = effect, sd = sd.yield,
                        sig.level = alpha, type = "two.sample", 
                        alternative = "two.sided")$power
power50 <- power.t.test(n = 50, delta = effect, sd = sd.yield,
                        sig.level = alpha, type = "two.sample", 
                        alternative = "two.sided")$power


power.tab <- data.frame(
Sample.Size.Per.Fertilizer = c(30, 40, 50),
  Power = round(c(power30, power40, power50), 4)
)

power.tab ## viewing results

## plot
power.tab %>% 
  ggplot(aes(x = Sample.Size.Per.Fertilizer, y = Power)) +
  geom_point(size = 2) +
  geom_line() +
  labs(
    title = "Estimated Statistical Power for Fertilizer Yield",
    x = "Number of Plots per Fertilizer",
    y = "Statistical Power") +
  ylim(.5, 1) +
  xlim(28, 52) +
  theme_light()


