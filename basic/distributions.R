## Distributions
library(dplyr)
library(ggplot2)

data <- data.frame(x = sample(LETTERS[1:5], 1000, replace = TRUE),
                   y = rnorm(1000))

ggplot(data = data, aes(x = factor(x), y = y, fill = factor(x))) +
  geom_violin() + geom_jitter(width = 0.5)