

## Session 03: 
## Load libraries
## Load tree table
## check summary() of numeric columns such as DBH, height, etc.
## - Check no NA in tree dbh 
## - Check no NA in dw dbh, dbase, dtop and bole_height
## Graphs
## - Make a graph of tree AGB against tree DBH
## - Make a graph of deadwood AGB against Dbase
## - Make a graph of deadwood height against Dbase 
## Make table of main species by lc cover class
## Make figures of number of trees epr DBH class


## Libraries
library(tidyverse)

## Load table
tree <- read_csv("data/tree.csv")

## Check stats of numeric columns with summary()
names(tree)
summary(tree)

## Check category columns with table()
table(tree$filename)
table(tree$lc_type)
table(tree$lc_class)
table(tree$tree_class)
table(tree$agb_equation)
table(tree$treeplot_radius)
table(tree$plot_id)
table(tree$plot_id, tree$treeplot_no)

##> Plot 10 is missing subplot ???
##> The tree plot with the largest number of trees is ???
##> 

## Graphs
ggplot(tree) +
  geom_point(aes(x = tree_dbh, )
