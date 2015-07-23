## Scatter Plot
library(ggplot2)

# Basic
df <- data.frame(x = rnorm(1000),
                 y = rnorm(1000))

ggplot() +
  geom_point(data = df,
             mapping = aes(x = x, y = y),
             pch = 21,
             colour = "black",
             fill = "orange",
             alpha = 0.75)

# Colorby quadrant
df <- data.frame(x = rnorm(1000),
                 y = rnorm(1000))

df <- df %>%
  mutate(quadrant = ifelse(x > 0 & y > 0, 1, NA)) %>%
  mutate(quadrant = ifelse((x > 0 & y < 0) & is.na(quadrant), 2, quadrant)) %>%
  mutate(quadrant = ifelse((x < 0 & y < 0) & is.na(quadrant), 3, quadrant)) %>%
  mutate(quadrant = ifelse((x < 0 & y > 0) & is.na(quadrant), 4, quadrant))

ggplot() +
  geom_point(data = df,
             mapping = aes(x = x, y = y, fill = factor(quadrant)),
             pch = 21,
             colour = "black",
             alpha = 0.75)
