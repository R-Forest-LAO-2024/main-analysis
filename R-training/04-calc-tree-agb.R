
## Objective:
## join information from treeplot and AGB model to each tree
## Calculate AGB for each tree
## Add scale factor and basal area

## Load Libraries
library(tidyverse)

## Load tables
tree_initial <- read_csv("data/tree-initial.csv")
treeplot     <- read_csv()
plot         <- read_csv()

## Create table
agb_models <- tibble(
  lc_class = c("EF", "DD", "MDF", "CF", "MCB"),
  agb_equation = rep("AGB = a * DBH^b", 5),
  param_a = c(0.3112, 0.2137, 0.523081, 0.1277, 0.1277),
  param_b = c(2.2331, 2.2575, 2       , 2.3944, 2.3944)
)

##
## practice JOINS ######
##

## What is the matching column between "tree_init" and "treeplot"?
## use names()

## Create a data frame 'tt' by joining "tree_init" and "treeplot"
tt <- left_join(tree_init, treeplot)
tt <- left_join(tree_init, treeplot, by = "ONA_treeplot_id")

## !! EX 
## + Create 'tt2' by joining tt with 'plot'
## + Create 'tt3' by joining tt2 with 'agb_model'
## + Create tree_join from tree and reproduce all joins in one sequence of 
##    commands linked with pipe '|>'
## !!



## 
## practice DBH class ######
## 

## Option 1: case_when()
tt4 <- tree |>
  mutate(
    tree_dbh_class = case_when(
      tree_dbh < 10 ~ 0,
      tree_dbh < 20 ~ 10,
      tree_dbh < 30 ~ 20,
      tree_dbh < 40 ~ 30,
      tree_dbh < 50 ~ 40,
      tree_dbh < 60 ~ 50,
      tree_dbh < 70 ~ 60,
      tree_dbh >= 70 ~ 70,
      TRUE ~ NA_integer_
    )
  )

## !! EX
## + Create 'tt5' from 'tree' and add tree_dbh_class with 
##   mutate() and case_when() to have categories of DBH every 20 cm 
##   from 0 to 100+
## + Make a point graph from 'tt5' with tree_dbh in x-axis and tree_dbh_class 
##   in y-axis
## !!!

## Option 2: round() + if_else()

tt6 <- tree |> 
  mutate(tree_dbh_class = round(tree_dbh / 10) * 10)

ggplot(tt6) +
  geom_point(aes(x = tree_dbh, y = tree_dbh_class))

tt7 <- tree |> 
  mutate(
    tree_dbh_class = if_else(tree_dbh < 100, round(tree_dbh / 10) * 10, 100)
  )

## !! EX 
## Make point graph with 'tt7', 'tree_dbh' in x-axis and 
## 'tree_dbh_class' in y-axis
ggplot(tt7) +
  geom_point(aes(x = tree_dbh, y = tree_dbh_class))



## RECAP PREPARE TREE:
tree <- tree_init |>
  left_join(treeplot, by = "ONA_treeplot_id") |>
  filter(!is.na(lc_class)) |>
  left_join(agb_models, by = "lc_class") |>
  mutate(
    tree_dbh_class_num = if_else(tree_dbh < 100, round(tree_dbh / 5) * 5, 100)
  )



##
## ADD AGB, BA and scale factor ######
##

## Check agb_model
agb_models

## Option 1: case_when()
tt_agb <- tree |>
  mutate(
    tree_agb = case_when(
      lc_class == "EF"  ~ 0.311 * tree_dbh^2.23,
      lc_class == "MDF" ~ 0.523 * tree_dbh^2.26,  
      TRUE ~ NA_real_
    )
  )

## !! EX
## + Create 'tt_agb1' from 'tree' and add 'tree_agb' from 
##   EF, MDF, DD, CF and MCB 
## + Make a point graph from 'tt_agb1' with 'tree_dbh' in x_axis 
##   and 'tree_agb' in y-axis
## !!

## Option 2: left_join()
tt_agb3 <- tree |>
  left_join(agb_models, by = "lc_class") |>
  mutate(tree_agb = param_a * tree_dbh^param_b)


## Add scale factor and basal area
tree_agb <- tree |>
  mutate(
    treeplot_radius = if_else(tree_dbh < 30, 8, 16),
    treeplot_scale_factor = 10000 / (pi * treeplot_radius^2),
    tree_agb = param_a * tree_dbh^param_b,
    ba_ha = pi * (tree_dbh/200)^2 * treeplot_scale_factor,
    tree_agb_ha = tree_agb * treeplot_scale_factor / 1000 ## AGB * scale_factor / ratio kg_to_ton
  )

## Check
summary(tree_agb$tree_agb_ha)

ggplot(tree_agb) +
  geom_point(aes(x = tree_dbh, y = tree_agb, color = lc_class))

tree_agb |>
  filter(tree_dbh < 50) |>
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_agb, color = lc_class))

