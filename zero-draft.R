

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

## RS table added directly in case_when()
## Carbon fraction
carbon_fraction <- 0.47



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

treeplot_index <- data_all |> 
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

treeplot_info <- plot_init |>
  left_join(treeplot_index, by = "treeplot_id") |>
  arrange(plot_id, treeplot_no)

table(treeplot_info$plot_id)
table(treeplot_info$plot_id, treeplot_info$treeplot_no)
table(treeplot_info$lc_class)
length(unique(treeplot_info$plot_id))


## Determine  plot LC class. Assumption: majority determines it
treeplot_lc <- treeplot_info |>
  summarise(count_treeplot = n(), .by = c(plot_id, lc_class))
treeplot_lc

plot_lc_max <- treeplot_lc |> 
  summarise(main_lc = max(count_treeplot), .by = plot_id) |>
  mutate(selected = T)
plot_lc_max

plot_info <- treeplot_lc |>
  left_join(plot_lc_max, by = c("plot_id", "count_treeplot" = "main_lc")) |>
  filter(selected) |>
  select(plot_id, lc_class_plot = lc_class)
plot_info

## Assumption 2 subplot A determine LC class - NOT USED
# plot <- treeplot |>
#   filter(treeplot_no == "A") |>
#   select(plot_id, lc_class_plot = lc_class) |>
#   distinct() |>
#   arrange(plot_id)
# plot

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
  left_join(treeplot, by = "ONA_treeplot_id") |>
  filter(!is.na(lc_class)) |>
  left_join(agb_models, by = "lc_class")

table(tree$lc_class, useNA = "ifany")

## Add AGB live trees
tree_agb <- tree |>
  filter(!is.na(tree_dbh)) |>
  mutate(
    treeplot_radius = if_else(tree_dbh < 30, 8, 16),
    treeplot_scale_factor = 10000 / (pi * treeplot_radius^2),
    tree_agb = case_when(
      agb_equation == "AGB = a * DBH^b" ~ param_a * tree_dbh^param_b,
      TRUE ~ NA_real_
    ),
    ba_ha = pi * (tree_dbh/200)^2 * treeplot_scale_factor,
    tree_agb_ha = tree_agb * treeplot_scale_factor / 1000 ## AGB * scale_factor / ratio kg_to_ton
  )

summary(tree_agb$tree_dbh)
summary(tree_agb$tree_agb)

tree_agb |> 
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_agb)) +
  facet_grid(plot_id ~ treeplot_no)

tree_agb |> 
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_agb, color = lc_class))

treeplot_carbon <- tree_agb |>
  #group_by(plot_id, treeplot_no, treeplot_id, lc_class, lc_type) |>
  summarise(
    tree_count = n(),
    tree_count_ha = sum(treeplot_scale_factor),
    treeplot_ba_ha = sum(ba_ha),
    treeplot_agb_ha = sum(tree_agb_ha), 
    .by = c(plot_id, treeplot_no, treeplot_id, lc_class, lc_type),
    )


treeplot_carbon <- tree_agb |>
  group_by(plot_id, treeplot_no, treeplot_id, lc_class, lc_type) |>
  summarise(
    tree_count = n(),
    tree_count_ha = sum(treeplot_scale_factor),
    treeplot_ba_ha = sum(ba_ha),
    treeplot_agb_ha = sum(tree_agb_ha), 
    .groups = "drop",
  ) |>
  mutate(
    treeplot_bgb_ha = case_when(
      lc_class == "CF" & treeplot_agb_ha <  50 ~ 0.46,
      lc_class == "CF" & treeplot_agb_ha <= 150 ~ 0.32,
      lc_class == "CF" & treeplot_agb_ha >  150 ~ 0.23,
      lc_class != "CF" & treeplot_agb_ha <  125 ~ 0.2,
      lc_class != "CF" & treeplot_agb_ha >= 125 ~ 0.24,
      TRUE ~ NA_real_
    ),
    treeplot_carbon_agb = treeplot_agb_ha * carbon_fraction,
    treeplot_carbon_live = (treeplot_agb_ha + treeplot_bgb_ha) * carbon_fraction,
    treeplot_carbon_all =  (treeplot_agb_ha + treeplot_bgb_ha) * carbon_fraction
  )
table(treeplot_carbon$plot_id)

## Checks
treeplot_carbon |>
  ggplot() +
  geom_col(aes(x = treeplot_id, y = treeplot_ba_ha, fill = lc_class))

treeplot_carbon |>
  ggplot() +
  geom_col(aes(x = treeplot_id, y = treeplot_agb_ha, fill = lc_class))

## Plot level 
plot_carbon <- treeplot_carbon |>
  left_join(plot_info, by = "plot_id") |>
  filter(lc_class == lc_class_plot) |>
  summarise(
    count_treeplot = n(),
    plot_carbon_agb = mean(treeplot_carbon_agb),
    plot_carbon_agb_sd = sd(treeplot_carbon_agb),
    .by = c(plot_id, lc_class_plot)
  )
plot_carbon
