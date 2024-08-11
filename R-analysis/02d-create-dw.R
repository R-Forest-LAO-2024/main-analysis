
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
  select(
    ONA_treeplot_id, dw_no = t_nb, dw_stem_no = stem_nb, dw_class, dw_distance = t_dist, dw_azimuth = t_az,
    dw_dbh, dw_dbase, dw_dtop, dw_bole_height) |>
  left_join(treeplot, by = "ONA_treeplot_id") |>
  filter(!is.na(lc_class)) |>
  left_join(plot, by = "plot_no")
