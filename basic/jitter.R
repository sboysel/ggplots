library(scales)
library(ggplot2)

data <- data.frame(x = c(rep(0, 500), sample(0:1, 500, replace = TRUE)),
                   y = c(rep(0, 500), sample(0:1, 500, replace = TRUE)),
                   year = factor(c(rep(1999, 500), rep(2000, 500))),
                   urbr = factor(sample(c("urb","rur"), 1000, replace = TRUE)),
                   round = factor(sample(c("R3", "R4"), 1000, replace = T)))

cor(data$x, data$y)

ggplot() +
  geom_point(data = data, aes(x = x, y = y), position = "jitter")

ggplot() +
  geom_jitter(data = data, aes(x = x, y = y))

cors <- ddply(data, .(urbr), summarise,
              r = ifelse(is.na(round(cor(x, y), 2)), 0, round(cor(x, y), 2)))

cors <- data %>%
  group_by(year, urbr) %>%
  do(r = round(cor(.$x, .$y), 2))

ggplot(data = data, aes(x = x, y = y)) +
  #geom_jitter(position = position_jitter(width = 0.05,
  #                                       height = 0.05)) +
  geom_smooth() +
  #facet_grid(~ round) +
  annotate("text", label = "hello", size = 10) +
  geom_errorbar(aes(x = x, ymin = 0, ymax = 1))