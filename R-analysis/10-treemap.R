
tree |>
  filter(treeplot_id == "05A") |>
  ggplot() +
  geom_point(aes(x = tree_distance, y = tree_azimuth))

tree |>
  filter(treeplot_id == "05A") |>
  ggplot() +
  geom_point(aes(x = tree_distance, y = tree_azimuth))
  
tree |>
  filter(plot_no == 2) |>
  ggplot() +
  geom_point(aes(x = tree_x_treeplot, y = tree_y_treeplot, color = filename)) +
  coord_fixed() +
  theme_bw() +
  gg_treeplot_all +
  theme(legend.position = "none") +
  labs(
    x = "",
    y = ""
  )
