
library(tidyverse)

tt <- tibble(
  tree_dbh = c(10, 33, 45, 100, NA_real_, NA_real_)
)

tt |>
  mutate(
    treeplot_radius = if_else(tree_dbh < 30, 8, 16)
  )


