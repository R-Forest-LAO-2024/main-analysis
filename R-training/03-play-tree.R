
## Load package
library(tidyverse)

dir.create("results", showWarnings = FALSE)

## Load data
tree <- read_csv("data/tree.csv")

## Checks
## + Check names() to get all column names
## + Check summary() for stats on numerical variables
## + Check table() for number of rows in category variable
names(tree)
summary(tree)
table(tree$filename)
table(tree$plot_no, tree$treeplot_no)

## !! EX:
## + check the number of tree per land cover class 
## + check the stats of tree_distance and tree_azimuth
## !!


## 
## Tree maps ######
##

## plot tree distance and azimuth
tree |>
  filter(plot_no == 2) |>
  ggplot(aes(x = tree_distance, y = tree_azimuth)) +
  geom_point()

## add polar coordinates
tree |>
  filter(plot_no == 2) |>
  ggplot(aes(x = tree_distance, y = tree_azimuth)) +
  geom_point() +
  coord_polar(theta = "y") +
  theme_bw()

## Add azimuth labels for checking the tree positioning is correct
tree |>
  filter(plot_no == 2) |>
  ggplot(aes(x = tree_distance, y = tree_azimuth)) +
  geom_text(aes(label = tree_azimuth, color = tree_distance)) +
  coord_polar(theta = "y") +
  theme_bw()

## Add labels of polar coordinates
tree |>
  filter(plot_no == 2) |>
  ggplot(aes(x = tree_distance, y = tree_azimuth)) +
  geom_text(aes(label = tree_azimuth, color = tree_distance)) +
  scale_x_continuous(breaks = c(0, 8, 16)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw()


## Change color based on nesting level
gg <- tree |>
  filter(plot_no == 2) |>
  ggplot(aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(color = filename)) +
  scale_x_continuous(breaks = c(0, 8, 16)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw() +
  labs(
    x = "", 
    y = "",
    color = "",
    subtitle = "tree maps for plot 2"
  )

print(gg)
ggsave(gg, filename = "results/gg-treemap-plot02.png", width = 15, height = 15, unit = "cm", dpi = 300)

## Same but with x, y instead of distance azimuth
tree |>
  filter(plot_no == 2) |>
  ggplot() +
  geom_point(aes(x = tree_x, y = tree_y, color = filename)) +
  coord_fixed() +
  theme_bw() +
  gg_treeplot_center

## Map all treeplots
tree |>
  filter(plot_no == 2) |>
  ggplot() +
  geom_point(aes(x = tree_x_treeplot, y = tree_y_treeplot, color = filename)) +
  coord_fixed() +
  theme_bw() +
  gg_treeplot_all +
  theme(legend.position = "none") +
  labs(
    x = "",
    y = ""
    )

## !! EX: 
## + Make a tree map for plots 3, 6 and 26. 
## + Save the figures in the 'result' sub-folder
## !!


##
## HOW THE COORDINATES WERE CALCULATED
##

tree_new <- tree |>
  mutate(
    tree_x_new = tree_distance * cos(pi/2 - tree_azimuth / 180 * pi),
    tree_y_new = tree_distance * sin(pi/2 - tree_azimuth / 180 * pi)
  )

## !! EX:
## + create "tree_new2", assign it "tree_new" then:
## + Use mutate() and case_when() to create: 
##     - "tree_x_treeplot_new" : if treeplot_no is A, B, C, D value is 0, 
##        otherwise values are 60, 120, 180 for E, F, G respectively 
##     - "tree_y_treeplot_new" : if treeplot_no is A, E, F, G value is 0,
##        otherwise values are 60, 120, 180 for B, C, D respectively 


##
## ABSTRACTION ######
##

## How to create treemap for all plots: map(), walk()
vec_plot_no <- sort(unique(tree$plot_no))
walk(vec_plot_no, function(x){
  
  gg <- tree |>
    filter(plot_no == x) |>
    ggplot(aes(x = tree_distance, y = tree_azimuth)) +
    geom_point(aes(color = filename)) +
    scale_x_continuous(breaks = c(0, 8, 16)) +
    scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
    coord_polar(theta = "y") +
    theme_bw() +
    labs(
      x = "", 
      y = "",
      color = "",
      subtitle = paste0("tree maps for plot ", x)
    )
  
  print(gg)
  
  ggsave(
    gg, filename = paste0("results/gg-treemap-plot", x, ".png"), 
    width = 15, height = 15, unit = "cm", dpi = 300
    )
  
})





