

## Calc treeplot AGB from deadwood and stumps
tmp_treeplot_dw <- dw_agb |>
  group_by(treeplot_id) |>
  summarise(
    dw_count = n(),
    dw_count_ha = sum(treeplot_scale_factor),
    treeplot_dw_agb_ha = sum(dw_agb_ha), 
    .groups = "drop",
  )

tmp_treeplot_stump <- stump_agb |>
  group_by(treeplot_id) |>
  summarise(
    stump_count = n(),
    stump_count_ha = sum(treeplot_scale_factor),
    treeplot_stump_agb_ha = sum(stump_agb_ha), 
    .groups = "drop",
  )

## Calc treeplot level carbon
treeplot_carbon <- tree_agb |>
  filter(!is.na(treeplot_id)) |>
  group_by(treeplot_id) |>
  summarise(
    tree_count = n(),
    tree_count_ha = sum(treeplot_scale_factor),
    treeplot_ba_ha = sum(ba_ha),
    treeplot_agb_ha = sum(tree_agb_ha), 
    .groups = "drop",
  ) |>
  left_join(tmp_treeplot_dw, by = "treeplot_id", ) |>
  left_join(tmp_treeplot_stump, by = "treeplot_id") |>
  inner_join(treeplot, by = "treeplot_id") |>
  mutate(
    treeplot_rs = case_when(
      lc_class == "CF" & treeplot_agb_ha <  50 ~ 0.46,
      lc_class == "CF" & treeplot_agb_ha <= 150 ~ 0.32,
      lc_class == "CF" & treeplot_agb_ha >  150 ~ 0.23,
      lc_class != "CF" & treeplot_agb_ha <  125 ~ 0.2,
      lc_class != "CF" & treeplot_agb_ha >= 125 ~ 0.24,
      TRUE ~ NA_real_
    ),
    treeplot_bgb_ha = treeplot_agb_ha * treeplot_rs,
    treeplot_carbon_ag = treeplot_agb_ha * carbon_fraction,
    treeplot_carbon_live = (treeplot_agb_ha + treeplot_bgb_ha) * carbon_fraction
  ) |> 
  rowwise() |>
  mutate(
    treeplot_carbon_all =  sum(treeplot_agb_ha, treeplot_bgb_ha, treeplot_dw_agb_ha, treeplot_stump_agb_ha, na.rm = TRUE) * carbon_fraction
  ) |>
  ungroup()


rm(list = str_subset(ls(), "tmp"))
