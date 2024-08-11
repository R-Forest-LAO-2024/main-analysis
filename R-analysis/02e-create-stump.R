
## Stump table separately
stump <- tree_init |>
  filter(t_deadcl == 3) |>
  mutate(stump_diameter = mean(c(diameter1, diameter2), na.rm = TRUE)) |>
  select(
    ONA_treeplot_id, stump_no = t_nb, stump_distance = t_dist, stump_azimuth = t_az, stump_diameter1 = diameter1, stump_diameter2 = diameter2, 
    stump_diameter, stump_height = height_st
  ) |>
  left_join(treeplot, by = "ONA_treeplot_id") |>
  filter(!is.na(lc_class)) |>
  left_join(plot, by = "plot_no")
