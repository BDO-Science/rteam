# Test file
### A demo for pull requests

library(tidyverse)

data <- iris %>%
  mutate(Petal_ratio = Petal.Length/Petal.Width,
         Sepal_ratio = Sepal.Length/Sepal.Width)

ggplot(data) +
  geom_point(aes(Petal_ratio, Sepal_ratio, color = Species)) +
  labs(title = "Super Cool ratios") +
  viridis::scale_color_viridis(discrete = TRUE) +
  theme_bw()
