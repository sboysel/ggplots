## Bubble Plot
library(ggplot2)

df <- data.frame(x = unlist(lapply(1:5, function(x) rnorm(100, mean = x^2, sd = x))),
                 y = unlist(lapply(1:5, function(x) rnorm(100, mean = x^2, sd = x))),
                 z = runif(500, 0, 5),
                 v = sample(1:5, 500, replace = TRUE))

## A bubble plot with plotmath text
# Save as Cairo .png
ggplot() +
  geom_point(data = df,
             mapping = aes(x = x, y = y, size = z, fill = factor(v)),
             pch = 21,
             alpha = 0.5,
             colour = "black") +
  labs(title = expression(paste("My super cool expression:", alpha^3)),
       x = expression(M^2),
       size = "Size",
       fill = "Type")
ggsave(filename = "bubble_test.png", type = "cairo")

