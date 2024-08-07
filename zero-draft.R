

library(tidyverse)
library(sf)
library(terra)
library(readxl)

path_datafile <- "data/NFI_Pilot_2024_20240510.xlsx"

## Create a category table for LC class
land_cover <- tibble(
  lc_class_no = c(11:16, 22, 51, 61, 62),
  lc_class    = c("EF", "MDF", "DD", "CF", "MCB", "B", "RV-TBC", "NF-agri", "NF-agri", "NF-agri")
) 

## Create a AGB models parameter table
agb_models <- tibble(
  lc_class = c("EF", "DD", "MDF", "CF", "MCB"),
  agb_equation = rep("AGB = a * DBH^b", 5),
  param_a = c(0.3112, 0.2137, 0.523081, 0.1277, 0.1277),
  param_b = c(2.2331, 2.2575, 2       , 2.3944, 2.3944)
)

## RS table



## Red data from the XLSX file
data_all <- readxl::read_xlsx(path_datafile, sheet = "data")
data_all

names_all <- names(data_all)
names_all

## REPRODUCE CURRENT ANALYSIS - plot data in 'Plot' tab
plot_init <- readxl::read_xlsx(path_datafile, sheet = "Plot") |>
  select(
    plot_id = "plot_info/plot_code_nmbr", 
    treeplot_id = "plot_info/sub_plot", 
    lc_type = "lc_data/lc_type",
    lc_class = "lc_class/lc_class"
    )
plot_init

plot_index <- data_all |> 
  select(
    crew_lead = "plot_info/crew_lead",
    ONA_treeplot_id = "_index",
    plot_id = "plot_info/plot_code_nmbr", 
    treeplot_no = "plot_info/sub_plot"
    ) |>
  filter(crew_lead != "QC") |>
  mutate(
    plot_id = if_else(plot_id == 135, 13, plot_id),
    treeplot_id = paste0(plot_id, treeplot_no)
    ) |>
  select(treeplot_id, treeplot_no, ONA_treeplot_id)

plot <- plot_init |>
  left_join(plot_index, by = "treeplot_id")

table(plot$plot_id)
table(plot$lc_class)

## Create 'tree' table
vec_sheets <- readxl::excel_sheets(path_datafile)
vec_tree <- vec_sheets |> str_subset(pattern = "tree_data_nest")

## PREPARE INITAL TREE TABLE BY ROW BINDING ALL SHEETS STARTING WITH "tree_data_nest"
tree_init <- map(vec_tree, function(x){
  
  ## For testing only: 
  ## x = vec_tree[2]

  tt <- readxl::read_xlsx(path_datafile, x) 
  
  ## Correct typo in nest2 column name
  err_colname <- "survey/measure/tree_data_nest2/tree_data_nest2_rep/tree_dead_nest2_cl2_tall/t_dead_height_dbh_nest2_short" 
  if (err_colname %in% names(tt)){
    err_pos <- match(err_colname, names(tt))
    names(tt)[err_pos] <- "t_dead_height_dbh_nest2_tall"
  }
  
  ## Simplify column names and harmonize ONA treeplot IDs
  tt |>
    mutate(filename = x) |>
    rename_with(str_remove, pattern = ".*/") |>
    rename_with(str_remove, pattern = "_nest[0-9]") |>
    rename(
      ONA_treeplot_id = `_parent_index`
    )
  
}) |> list_rbind()
tree_init

## Check
table(tree_init$filename)

## Simplify and harmonize
tree <- tree_init |>
  select(
    tree_no = t_nb, stem_go, stem_no = stem_nb, tree_distance = t_dist, tree_azimuth = t_az,
    tree_species_no = t_species_name, tree_species_local_name = t_species_other, tree_dbh = t_dbh,
    tree_total_height = t_height, filename, ONA_treeplot_id
    ) |>
  left_join(plot, by = "ONA_treeplot_id") |>
  filter(!is.na(lc_class)) |>
  left_join(agb_models, by = "lc_class")

table(tree$lc_class, useNA = "ifany")

## Add AGB live trees
tree_agb <- tree |>
  filter(!is.na(tree_dbh)) |>
  mutate(
    treeplot_radius = if_else(tree_dbh < 30, 8, 16),
    tree_agb = case_when(
      agb_equation == "AGB = a * DBH^b" ~ param_a * tree_dbh^param_b,
      TRUE ~ NA_real_
    ),
    
  )

summary(tree_agb$tree_dbh)
summary(tree_agb$tree_agb)

tree_agb |> 
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_agb)) +
  facet_grid(plot_id ~ treeplot_no)


