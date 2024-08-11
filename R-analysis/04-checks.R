

tt <- tree_agb |> 
  filter(plot_no == 4) |>
  summarise(
    sum_agb = sum(tree_agb_ha)
  )

dw |>
  filter(dw_class != "dead class 1") |>
  ggplot() +
  geom_point(aes(x = dw_dbase, y = dw_dbh)) +
  geom_point(aes(x = dw_dbase, y = dw_dtop), col = "darkred") +
  geom_abline(slope = 1, intercept = 0)


tt <- tree |>
  filter(is.na(treeplot_id)) |>
  pull(ONA_treeplot_id) |>
  unique()

treeplot |> filter(ONA_treeplot_id %in% tt)

table(treeplot_carbon$plot_no)
