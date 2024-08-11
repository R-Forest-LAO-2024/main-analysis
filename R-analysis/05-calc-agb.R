

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