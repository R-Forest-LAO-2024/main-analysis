
tree <- tree_init |>
  filter(t_livedead == 1) |>
  select(
    ONA_treeplot_id, tree_no = t_nb, tree_stem_no = stem_nb, tree_distance = t_dist, tree_azimuth = t_az,
    tree_species_no = t_species_name, tree_species_local_name = t_species_other, tree_dbh = t_dbh, filename
  ) |>
  left_join(treeplot, by = "ONA_treeplot_id") |>
  #filter(!is.na(lc_class), treeplot_no != "H") |>
  left_join(agb_models, by = "lc_class") |>
  left_join(plot, by = "plot_no") |>
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
