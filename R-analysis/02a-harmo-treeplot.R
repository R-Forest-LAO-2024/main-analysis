

treeplot <- treeplot_init |>
  select(
    crew_lead = "plot_info/crew_lead",
    ONA_treeplot_id = "_index",
    plot_no = "plot_info/plot_code_nmbr", 
    treeplot_no = "plot_info/sub_plot",
    treeplot_gps_lat = "plot_GPS/_GPS_latitude",
    treeplot_gps_lon = "plot_GPS/_GPS_longitude",
  ) |>
  filter(crew_lead != "QC") |>
  mutate(
    #plot_no = if_else(plot_no == 135, 13, plot_no),
    plot_id = case_when(
      plot_no < 10 ~ paste0("0", plot_no),
      plot_no < 100 ~ as.character(plot_no),
      TRUE ~ NA_character_
    ),
    treeplot_id = paste0(plot_id, treeplot_no)
  ) |>
  select(plot_id, plot_no, treeplot_id, treeplot_no, ONA_treeplot_id, treeplot_gps_lat, treeplot_gps_lon) |>
  inner_join(treeplot_lc, by = c("plot_no", "treeplot_id")) |>
  arrange(plot_no, treeplot_no)



  