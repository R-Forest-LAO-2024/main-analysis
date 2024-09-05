

tree |>
  filter(plot_no == 2) |>
  mutate(tree_dbh_lim = if_else(tree_dbh < 30, "small", " big")) |>
  ggplot(aes(x = tree_distance, y = tree_azimuth)) +
  geom_point(aes(color = tree_dbh_lim)) +
  scale_x_continuous(breaks = c(0, 8, 16)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw() +
  labs(
    x = "", 
    y = "",
    color = "",
    subtitle = "tree maps for plot 2"
  )


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
