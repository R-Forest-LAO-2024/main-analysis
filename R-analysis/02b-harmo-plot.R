
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

tmp_plot_lc <- lc_count |>
  left_join(lc_max, by = c("plot_no", "count_treeplot" = "main_lc")) |>
  filter(selected) |>
  select(plot_no, lc_class_plot = lc_class)
tmp_plot_lc


##
## Plot coordinates ######
##

## Convert treeplot to spatial and add meter based CRS
tmp_coords <- treeplot |> 
  mutate(
    treeplot_lat = treeplot_gps_lat,
    treeplot_lon = treeplot_gps_lon
    ) |>
  st_as_sf(coords = c("treeplot_lon", "treeplot_lat"), crs = 4326) |>
  st_transform(crs = "ESRI:54017")

sf_tmp_treeplot <- tmp_coords |>
  mutate(
    treeplot_gps_x = st_coordinates(tmp_coords)[,1],
    treeplot_gps_y = st_coordinates(tmp_coords)[,2]
    )

## Here we use treeplot A and if missing the average x from B, C, D and average y from E, F, G
vec_A <- treeplot |> filter(treeplot_no == "A") |> pull(plot_no)

tmp_plot_A_coords <- sf_tmp_treeplot |>
  as_tibble() |>
  filter(treeplot_no == "A") |>
  select(
    plot_no, plot_gps_lon = treeplot_gps_lon, plot_gps_lat = treeplot_gps_lat, 
    plot_gps_x = treeplot_gps_x, plot_gps_y = treeplot_gps_y
    ) 

tmp_missA_coords <- sf_tmp_treeplot |>
  as_tibble() |>
  filter(!(plot_no %in% vec_A)) |>
  mutate(
    treeplot_vt = if_else(treeplot_no %in% c("B", "C", "D"), treeplot_gps_x, NA_real_),
    treeplot_hz = if_else(treeplot_no %in% c("E", "F", "G"), treeplot_gps_y, NA_real_)
    )

tmp_missA_ids <- unique(tmp_missA_coords$plot_no)

tmp_missA_coords2 <- map(tmp_missA_ids, function(x){
  
  tt <- tmp_missA_coords |> filter(plot_no == x)
  
  coords <- tt |>
    summarise(
      x = mean(treeplot_vt, na.rm = T),
      y = mean(treeplot_hz, na.rm = T),
      .by = plot_no
    )
  
  ## If none of A, B, C, get x from E coord - 60 meters, otherwise leave as NA
  if(is.na(coords$x) & "E" %in% tt$treeplot_no){
    x <- tt |> filter(tt$treeplot_no == "E") |> pull(treeplot_gps_x) - 60
    coords$x <- x
  } 
  
  if(is.na(coords$y) & "B" %in% tt$treeplot_no){
    y <- tt |> filter(tt$treeplot_no == "B") |> pull(treeplot_gps_y) - 60
    coords$y <- y
  }
  
  coords
  
}) |> list_rbind()
tmp_missA_coords2
  
sf_tmp_missA <- tmp_missA_coords2 |> 
  mutate(xx = x, yy = y) |>
  st_as_sf(coords = c("xx", "yy"), crs = "ESRI:54017") |>
  st_transform(crs = 4326)

tmp_plot_missA_coords <- sf_tmp_missA |>
  mutate(
    plot_gps_lon = st_coordinates(sf_tmp_missA)[1],
    plot_gps_lat = st_coordinates(sf_tmp_missA)[2]
    ) |>
  as_tibble() |>
  select(plot_no, plot_gps_lon, plot_gps_lat, plot_gps_x = x, plot_gps_y = y)
  
## Combine data
plot <- bind_rows(tmp_plot_A_coords, tmp_plot_missA_coords) |>
  mutate(plot_coord_status = if_else(plot_no %in% vec_A, "measured", "calculated")) |>
  inner_join(tmp_plot_lc, by = "plot_no")
plot 


rm(list = str_subset(ls(), "tmp"))
rm(list = str_subset(ls(), "vec"))
rm(list = str_subset(ls(), "lc_"))

# sf_plot <- plot_A_coords |> st_as_sf(coords = c("plot_gps"))


# remotes::install_github('r-tmap/tmap')
# library(tmap)
# 
# tmap_mode("view")
# tm_basemap("Esri.WorldImagery") +
# tm_shape(sf_treeplot) +
#   tm_dots("lc_class")
