# Description: ...
# Author: Sam Boysel <sboysel@gmail.com>
# SPDX-License-Identifier: MIT
library(ggplot2)
library(tibble)
library(lubridate)

set.seed(1234)

n <- 1e3
x <- rnorm(n)
y <- -0.1 * x^2 + runif(n)
z <- 0.5 * x
d <- tibble::tibble(
  i = sample(letters[1:2], n, replace = TRUE),
  j = sample(letters[3:4], n, replace = TRUE),
  t = lubridate::now() + runif(n) * 86400,
  x = x,
  y = y,
  z = z
)

print(d)

ggplot2::ggplot(d, aes(x=x, y=y, color=z)) +
  geom_point() +
  facet_grid(cols = vars(i)) +
  scale_color_distiller(type = "seq", palette = "GnBu") +
  labs(
    title = "A cool plot",
    subtitle = "A subtitle",
    caption = "This is where the caption goes",
    x = "Horizontal axis variable",
    y = "Vertical axis variable",
    color = "Another factor"
  ) +
  theme_classic()
  
ggplot2::ggsave(
  "plots/01_scatter.png",
  units = "in",
  width = 8,
  height = 6
)
