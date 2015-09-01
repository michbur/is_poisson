source("is_poisson.R")

library(dplyr)
library(ggplot2)
library(reshape2)

calc_bias <- function(lambda_range = c(0.01, 0.05, 0.1, 0.2), times = 1e4)
  data.frame(do.call(rbind, lapply(lambda_range, function(real_lambda) {
    cbind(lambda = rep(real_lambda, times), t(replicate(times, {
      x <- rpois(765, real_lambda) > 0
      sqrt(c(fl = (fl(sum(x)/765) - real_lambda)^2, mean = (mean(x)- real_lambda)^2))
    })))
  })))



bias <- melt(calc_bias(), measure.vars = c("fl", "mean"))

group_by(bias, lambda, variable) %>% summarise(mean = mean(value))

dodge <- position_dodge(0.9)
ggplot(bias, aes(x = as.factor(lambda), fill = variable, colour = variable, y = value)) +
  geom_boxplot(outlier.shape = NA, colour = "black", alpha = 0.4, position = dodge) + 
  geom_point(position = dodge) +
  scale_x_discrete(expression(lambda)) +
  scale_y_continuous(latex2exp("$\\sqrt{\\left(\\lambda - \\hat{\\lambda}\\right)^2}$")) + 
  theme_bw()

