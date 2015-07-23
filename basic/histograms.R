## Histograms
library(dplyr)
library(ggplot2)

df <- data.frame(x = rnorm(1000),
                 y = sample(1:5, 1000, replace = TRUE),
                 z = sample(LETTERS[1:3], 1000, replace = TRUE))

# Basic
ggplot(data = df,
       mapping = aes(x = x)) +
  geom_histogram(data = df,
                 mapping = aes(y = ..density..),
                 binwidth = 0.2)

# with clearer bins
ggplot(data = df, mapping = aes(x = x)) +
  geom_histogram(data = df,
                 mapping = aes(y = ..density..),
                 binwidth = 0.2,
                 fill = "white",
                 colour = "black")

# with density curve
ggplot(data = df,
       mapping = aes(x = x)) +
  geom_histogram(data = df,
                 mapping = aes(y = ..density..),
                 binwidth = 0.2) +
  geom_density()

# with facets
ggplot(data = df,
       mapping = aes(x = x)) +
  geom_histogram(data = df,
                 mapping = aes(y = ..density..,
                               fill = z),
                 binwidth = 0.2) +
  facet_grid(~ z)

ggplot(data = df,
       mapping = aes(x = x)) +
  geom_histogram(data = df,
                 mapping = aes(y = ..density..,
                               fill = z),
                 binwidth = 0.2) +
  facet_grid(y ~ z)