## Using geom_rect
library(ggplot2)

df <- data.frame(west = runif(10, -1, 0),
                 south = runif(10, -1, 0),
                 east = runif(10, 0, 1),
                 north = runif(10, 0, 1),
                 class = factor(1:10))

# Basic
ggplot() +
  geom_rect(data = df,
            mapping = aes(xmin = west,
                          ymin = south,
                          xmax = east,
                          ymax = north,
                          fill = class),
            alpha = 0.25)

# Advanced
ggplot(data = data.frame(x = 1:40, y = factor(rep(1:10, 4)))) +
  geom_rect(mapping = aes(xmin = x,
                          xmax = x + 1,
                          ymin = sin(0.25 * x),
                          ymax = sin(0.25 * x) + 1,
                          fill = y),
            alpha = 0.75) +
  scale_fill_brewer(palette = "Spectral")