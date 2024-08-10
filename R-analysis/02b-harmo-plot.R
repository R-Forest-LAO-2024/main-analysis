
## Plot level information is entirely calculated from treeplot.
## XL file doesn't contain plot level info.


##
## Land cover ######
##


## FOR LC, need decision on how LC is assigned at plot level
## ATM seems majority rule. 
## In practice it should be treeplot A determine the LC and even when not accessible, field crew should at least record LC

## For majority rule:
## + count how many treeplots per LC and plot ID
## + 

lc_count <- treeplot |> summarise(count_treeplot = n(), .by = c(plot_no, lc_class))

lc_max <- lc_count |> 
  summarise(main_lc = max(count_treeplot), .by = plot_no) |>
  mutate(selected = T)

plot_lc <- lc_count |>
  left_join(lc_max, by = c("plot_no", "count_treeplot" = "main_lc")) |>
  filter(selected) |>
  select(plot_no, lc_class_plot = lc_class)
plot_lc




##
## Plot coordinates ######
##

## Here we use treeplot A and if missing the average x from B, C, D and average y from E, F, G
vec_A <- treeplot |> filter(treeplot_no == "A") |> pull(plot_no)

plot_A_coords <- treeplot |>
  filter(treeplot_no == "A") |>
  select(plot_no, treeplot_gps_lon, treeplot_gps_lat) 

plot_missA_coords <- treeplot |>
  filter(!(plot_no %in% vec_A))

## If all BCD or EFG missing using average distance

