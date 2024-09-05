

## Load Libraries
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

## Load tables
tree_init <- read_csv("data/tree-initial.csv")
treeplot  <- read_csv("data/treeplot.csv")

## Create table
agb_models <- tibble(
  lc_class = c("EF", "DD", "MDF", "CF", "MCB"),
  agb_equation = rep("AGB = a * DBH^b", 5),
  param_a = c(0.3112, 0.2137, 0.523081, 0.1277, 0.1277),
  param_b = c(2.2331, 2.2575, 2       , 2.3944, 2.3944)
)

## Add ancillary information
tree <- tree_init |>
  left_join(treeplot, by = "ONA_treeplot_id") |>
  filter(!is.na(lc_class)) |>
  left_join(agb_models, by = "lc_class") |>
  mutate(
    tree_dbh_class_num = round(tree_dbh / 5) * 5
  )


## Calc AGB
tree_agb <- tree |>
  mutate(
    treeplot_radius = if_else(tree_dbh < 30, 8, 16),
    treeplot_scale_factor = 10000 / (pi * treeplot_radius^2),
    tree_agb = param_a * tree_dbh^param_b,
    tree_ba_ha = pi * (tree_dbh/200)^2 * treeplot_scale_factor,
    tree_agb_ha = tree_agb * treeplot_scale_factor / 1000 ## AGB * scale_factor / ratio kg_to_ton
  )

## Check
summary(tree_agb$tree_agb)
summary(tree_agb$tree_agb_ha)

ggplot(tree_agb) +
  geom_point(aes(x = tree_dbh, y = tree_agb, color = lc_class))

tree_agb |>
  filter(tree_dbh < 50) |>
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_agb, color = lc_class))

