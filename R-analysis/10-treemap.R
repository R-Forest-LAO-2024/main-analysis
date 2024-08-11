
tree |>
  filter(treeplot_id == "05A") |>
  ggplot() +
  geom_point(aes(x = tree_distance, y = tree_azimuth))

tree |>
  filter(treeplot_id == "05A") |>
  ggplot() +
  geom_point(aes(x = tree_distance, y = tree_azimuth))
  
