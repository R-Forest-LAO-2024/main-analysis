

library(tidyverse)
library(sf)
library(terra)
library(readxl)
library(tictoc)

tic()
path_datafile <- "data-source/NFI_Pilot_2024_20240510.xlsx"

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

## make a list of circle geom_path objects for all treeplots
# treeplot_circles <-list(
#   gg_circle(center = c(0, 0), radius = 8, n = 100),
#   gg_circle(center = c(0, 0), radius = 16, n = 100)
# )
# 
# plot_circles <- list(
#   ## A
#   gg_circle(center = c(0, 0), radius = 8, n = 100),
#   gg_circle(center = c(0, 0), radius = 16, n = 100),
#   ## B
#   gg_circle(center = c(0, 60), radius = 8, n = 100),
#   gg_circle(center = c(0, 60), radius = 16, n = 100),
#   ## C
#   gg_circle(center = c(0, 120), radius = 8, n = 100),
#   gg_circle(center = c(0, 120), radius = 16, n = 100),
#   ## D
#   gg_circle(center = c(0, 180), radius = 8, n = 100),
#   gg_circle(center = c(0, 180), radius = 16, n = 100),
#   ## E
#   gg_circle(center = c(60, 0), radius = 8, n = 100),
#   gg_circle(center = c(60, 0), radius = 16, n = 100),
#   ## F
#   gg_circle(center = c(120, 0), radius = 8, n = 100),
#   gg_circle(center = c(120, 0), radius = 16, n = 100),
#   ## G
#   gg_circle(center = c(180, 0), radius = 8, n = 100),
#   gg_circle(center = c(180, 0), radius = 16, n = 100)
# )
  




## Red data from the XLSX file
data_all <- readxl::read_xlsx(path_datafile, sheet = "data")
data_all

names_all <- names(data_all)
names_all

## REPRODUCE CURRENT ANALYSIS - plot data in 'Plot' tab
treeplot_lc <- readxl::read_xlsx(path_datafile, sheet = "Plot") |>
  select(
    #plot_no = "plot_info/plot_code_nmbr", 
    treeplot_id = "plot_info/sub_plot", 
    lc_type = "lc_data/lc_type",
    lc_class = "lc_class/lc_class"
    ) |>
  mutate(
    treeplot_id = case_when(
      str_length(treeplot_id) == 2 ~ paste0("0", treeplot_id),
      str_length(treeplot_id) == 3 ~ treeplot_id,
      TRUE ~ NA_character_
    )
  )
treeplot_lc

treeplot_index <- data_all |> 
  select(
    crew_lead = "plot_info/crew_lead",
    ONA_treeplot_id = "_index",
    plot_no = "plot_info/plot_code_nmbr", 
    treeplot_no = "plot_info/sub_plot",
    treeplot_gps_lat = "plot_GPS/_GPS_latitude",
    treeplot_gps_lon = "plot_GPS/_GPS_longitude",
    ) |>
  filter(crew_lead != "QC") |>
  mutate(
    plot_no = if_else(plot_no == 135, 13, plot_no),
    plot_id = case_when(
      plot_no < 10 ~ paste0("0", plot_no),
      plot_no < 100 ~ as.character(plot_no),
      TRUE ~ NA_character_
    ),
    treeplot_id = paste0(plot_id, treeplot_no)
    ) |>
  select(plot_id, plot_no, treeplot_id, treeplot_no, ONA_treeplot_id, treeplot_gps_lat, treeplot_gps_lon)

treeplot_info <- treeplot_index |>
  inner_join(treeplot_lc, by = "treeplot_id") |>
  arrange(plot_no, treeplot_no)

table(treeplot_info$plot_id)
table(treeplot_info$plot_id, treeplot_info$treeplot_no)
table(treeplot_info$lc_class)
length(unique(treeplot_info$plot_id))

sf_treeplot <- treeplot_info |> st_as_sf(coords = c("treeplot_gps_lon", "treeplot_gps_lat"), crs = 4326)

sf_treeplot |>
  filter(plot_no == 11) |>
  ggplot() +
  geom_sf(aes(color = treeplot_no)) +
  theme_bw()

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
summary(tree_init)
table(tree_init$filename)
table(tree_init$t_livedead)
table(tree_init$t_deadcl)

## Simplify and harmonize
tree <- tree_init |>
  filter(t_livedead == 1) |>
  select(
    ONA_treeplot_id, tree_no = t_nb, tree_stem_no = stem_nb, tree_distance = t_dist, tree_azimuth = t_az,
    tree_species_no = t_species_name, tree_species_local_name = t_species_other, tree_dbh = t_dbh, filename
    ) |>
  left_join(treeplot_info, by = "ONA_treeplot_id") |>
  filter(!is.na(lc_class), treeplot_no != "H") |>
  left_join(agb_models, by = "lc_class") |>
  left_join(plot_info, by = "plot_id") |>
  mutate(
    tree_x = tree_distance * cos(pi/2 - tree_azimuth / 180 * pi),
    tree_y = tree_distance * sin(pi/2 - tree_azimuth / 180 * pi),
    tree_x_treeplot = tree_x + case_when(
      treeplot_no %in% c("A", "B", "C", "D") ~ 0,
      treeplot_no == "E" ~ 60,
      treeplot_no == "F" ~ 120,
      treeplot_no == "G" ~ 180,
      TRUE ~ NA_integer_
    ),
    tree_y_treeplot = tree_y + case_when(
      treeplot_no %in% c("A", "E", "F", "G") ~ 0,
      treeplot_no == "B" ~ 60,
      treeplot_no == "C" ~ 120,
      treeplot_no == "D" ~ 180,
      TRUE ~ NA_integer_
    ),
    tree_dbh_class_num = round(tree_dbh / 5) * 5
  )
tree

# tree |>
#   #filter(plot_no == 2) |>
#   ggplot() +
#   geom_point(aes(x = tree_x, y = tree_y)) +
#   treeplot_circles +
#   coord_fixed() +
#   theme_bw()

tree |>
  ggplot() +
  geom_point(aes(x = tree_x_treeplot, y = tree_y_treeplot, color = plot_id)) +
  #plot_circles +
  coord_fixed() +
  theme_bw()

## For all plots
length(unique(tree$plot_id))
walk(sort(unique(tree$plot_id)), function(x){
  
  gg <- tree |>
    filter(plot_id == x) |>
    ggplot() +
    geom_point(aes(x = tree_x_treeplot, y = tree_y_treeplot, color = treeplot_no)) +
    #plot_circles +
    coord_fixed() +
    theme_bw() +
    labs(subtitle = paste0("Plot no: ", x))
  
  ggsave(gg, filename = paste0("results/plot_map", "plot_id", ".png"), width = 12, height = 12, units = "cm", dpi = 300)
  
  print(gg)
  
})
  
tt <- tree |> filter(plot_no == 11)
table(tt$treeplot_no)

tree |>
  filter(plot_no == 2) |>
  ggplot(aes(x = tree_distance, y = tree_azimuth)) +
  geom_text(aes(label = tree_azimuth, color = tree_distance)) +
  scale_x_continuous(breaks = c(0, 8, 16)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw()

## Dead wood table separately
dw <- tree_init |>
  filter(t_livedead == 2, t_deadcl != 3) |>
  mutate(
    dw_class = case_when(
      t_livedead == 2 & t_deadcl == 1 ~ "dead class 1", 
      t_livedead == 2 & t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ "dead class 2 short",
      t_livedead == 2 & t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ "dead class 2 tall",
      t_livedead == 2 & t_deadcl == 3 ~ "dead class 3",
      TRUE ~ NA_character_
    ),
    dw_bole_height = case_when(
      t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ t_dead_height_short,
      t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ t_dead_height_tall,
      TRUE ~ NA_real_
    ),
    dw_dbh = case_when(
      t_livedead == 2 & t_deadcl == 1 ~ t_dbh,
      t_livedead == 2 & t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ t_dead_DBH_short,
      t_livedead == 2 & t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ t_dead_DBH_tall,
      TRUE ~ NA_real_
    ),
    dw_dbase = case_when(
      t_livedead == 2 & t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ t_dead_DB_short,
      t_livedead == 2 & t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ t_dead_DB_tall,
      TRUE ~ NA_real_
    ),
    dw_dtop = case_when(
      t_livedead == 2 & t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ t_dead_DT_short,
      t_livedead == 2 & t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ dw_dbase - (dw_bole_height * ((dw_dbase - dw_dbh) / 130 * 100)),
      TRUE ~ NA_real_
    )
  ) |>
  select(ONA_treeplot_id, dw_no = t_nb, dw_stem_no = stem_nb, dw_class, dw_dbh, dw_dbase, dw_dtop, dw_bole_height) |>
  left_join(treeplot_info, by = "ONA_treeplot_id") |>
  filter(!is.na(lc_class)) |>
  left_join(plot_info, by = "plot_id")

## Stump table separately
stump <- tree_init |>
  filter(t_deadcl == 3) |>
  mutate(stump_diameter = mean(c(diameter1, diameter2), na.rm = TRUE)) |>
  select(
    ONA_treeplot_id, stump_no = t_nb, stump_diameter1 = diameter1, stump_diameter2 = diameter2, 
    stump_diameter, stump_height = height_st
    ) |>
  left_join(treeplot_info, by = "ONA_treeplot_id") |>
  filter(!is.na(lc_class)) |>
  left_join(plot_info, by = "plot_id")


## Checks
table(tree$lc_class, useNA = "ifany")
table(tree$lc_class_plot, useNA = "ifany")
table(tree$lc_class, tree$lc_class_plot, useNA = "ifany")



## Add AGB live trees
tree_agb <- tree |>
  mutate(
    treeplot_radius = if_else(tree_dbh < 30, 8, 16),
    treeplot_scale_factor = 10000 / (pi * treeplot_radius^2),
    tree_agb = param_a * tree_dbh^param_b,
    ba_ha = pi * (tree_dbh/200)^2 * treeplot_scale_factor,
    tree_agb_ha = tree_agb * treeplot_scale_factor / 1000 ## AGB * scale_factor / ratio kg_to_ton
  )

dw_agb <- dw |>
  mutate(
    treeplot_radius = if_else(dw_dbh < 30, 8, 16),
    treeplot_scale_factor = 10000 / (pi * treeplot_radius^2),
    dw_agb = case_when(
      dw_class == "dead class 1"       ~ 0.6 * exp(-1.499 + (2.148 * log(dw_dbh)) + (0.207 * (log(dw_dbh))^2) - (0.0281 * (log(dw_dbh))^3)),
      dw_class == "dead class 2 short" ~ pi * dw_bole_height * 100 / 12 * (dw_dbase^2 + dw_dbase * dw_dtop + dw_dtop^2) * 0.6 * 0.001, ## Convert H to cm then wd in g.cm-3 with WD = 0.6 then g to kg with 0.001 
      dw_class == "dead class 2 tall"  ~ pi * dw_bole_height * 100 / 12 * (dw_dbase^2 + dw_dbase * dw_dtop + dw_dtop^2) * 0.6 * 0.001,
      TRUE ~ NA_real_
    ),
    ba_ha = pi * (dw_dbh/200)^2 * treeplot_scale_factor,
    dw_agb_ha = dw_agb * treeplot_scale_factor / 1000 ## AGB * scale_factor / ratio kg_to_ton
  )

stump_agb <- stump |>
  mutate(
    treeplot_radius = if_else(stump_diameter < 30, 8, 16),
    treeplot_scale_factor = 10000 / (pi * treeplot_radius^2),
    stump_agb = (stump_diameter / 2)^2 * pi * stump_height * 0.57 * 0.001,
    stump_agb_ha = stump_agb * treeplot_scale_factor / 1000 ## AGB * scale_factor / ratio kg_to_ton
  )
## In XLSX stump scale factor based on dbh which is empty and defaults to 8 m. Here basing on stump_diameter.

summary(tree_agb$tree_dbh)
summary(tree_agb$tree_agb)

tree_agb |> 
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_agb, color = lc_class)) +
  theme_bw()

dw_agb |> 
  ggplot() +
  geom_point(aes(x = dw_dbh, y = dw_agb, color = dw_class))

tree_agb |> 
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_agb, color = lc_class)) +
  facet_wrap(~plot_id, ncol = 4) +
  theme(legend.position = "bottom") +
  labs(color = "")

tree_agb |> 
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_agb, color = lc_class))


# treeplot_carbon <- tree_agb |>
#   summarise(
#     tree_count = n(),
#     tree_count_ha = sum(treeplot_scale_factor),
#     treeplot_ba_ha = sum(ba_ha),
#     treeplot_agb_ha = sum(tree_agb_ha), 
#     .by = c(plot_id, treeplot_no, treeplot_id, lc_class, lc_type),
#     )


## Calc treeplot level carbon
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
    treeplot_carbon_ag = treeplot_agb_ha * carbon_fraction,
    treeplot_carbon_live = (treeplot_agb_ha + treeplot_bgb_ha) * carbon_fraction,
    #treeplot_carbon_all =  (treeplot_agb_ha + treeplot_bgb_ha) * carbon_fraction
  )
table(treeplot_carbon$plot_id)

## Checks
treeplot_carbon |>
  ggplot() +
  geom_col(aes(x = treeplot_id, y = treeplot_ba_ha, fill = lc_class))

treeplot_carbon |>
  ggplot() +
  geom_col(aes(x = treeplot_id, y = treeplot_agb_ha, fill = lc_class))

## Calculate plot level carbon: 
## While at plot level we average treeplot carbon,
## This is more of a sum of all trees in all treeplots converted to per ha values.
plot_carbon <- treeplot_carbon |>
  left_join(plot_info, by = "plot_id") |>
  filter(lc_class == lc_class_plot) |>
  summarise(
    count_treeplot = n(),
    plot_carbon_ag = mean(treeplot_carbon_ag),
    plot_carbon_ag_sd = sd(treeplot_carbon_ag),
    #plot_carbon_live = mean(treeplot_carbon_live),
    #plot_carbon_live_sd = sd(treeplot_carbon_live),
    #plot_carbon_all = mean(treeplot_carbon_all),
    #plot_carbon_all_sd = sd(treeplot_carbon_all),
    .by = c(plot_id, lc_class_plot)
  ) |> 
  mutate(
    plot_carbon_ag_se = if_else(count_treeplot == 1, NA_real_, plot_carbon_ag_sd / sqrt(count_treeplot)),
    plot_carbon_ag_me = plot_carbon_ag_se * qt(0.975, count_treeplot-1),
    plot_carbon_ag_cilower = plot_carbon_ag - plot_carbon_ag_me,
    plot_carbon_ag_ciupper = plot_carbon_ag + plot_carbon_ag_me
  )
plot_carbon

plot_carbon |>
  ggplot(aes(x = plot_id)) +
  geom_col(aes(y = plot_carbon_ag, fill = lc_class_plot)) +
  geom_errorbar(aes(ymin = plot_carbon_ag_cilower, ymax = plot_carbon_ag_ciupper))



## Forest level carbon
forest_carbon <- plot_carbon |>
  summarise(
    count_plot = n(),
    carbon_ag = mean(plot_carbon_ag),
    carbon_ag_sd = sd(plot_carbon_ag_sd),
    .by = lc_class_plot
  ) |>
  mutate(
    carbon_ag_se = if_else(count_plot == 1, NA_real_, carbon_ag_sd / sqrt(count_plot)),
    carbon_ag_me = carbon_ag_se * qt(0.975, count_plot-1),
    carbon_ag_cilower = carbon_ag - carbon_ag_me,
    carbon_ag_ciupper = carbon_ag + carbon_ag_me
  )


forest_carbon |>
  ggplot(aes(x = lc_class_plot)) +
  geom_col(aes(y = carbon_ag, fill = lc_class_plot)) +
  geom_errorbar(aes(ymin = carbon_ag_cilower, ymax = carbon_ag_ciupper))

write_csv(tree_agb, "data/tree.csv")

toc()
