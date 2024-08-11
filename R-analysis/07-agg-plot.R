
plot_carbon <- treeplot_carbon |>
  left_join(plot, by = "plot_no") |>
  filter(lc_class == lc_class_plot) |>
  summarise(
    count_treeplot = n(),
    plot_carbon_ag = mean(treeplot_carbon_ag),
    plot_carbon_ag_sd = sd(treeplot_carbon_ag),
    #plot_carbon_live = mean(treeplot_carbon_live),
    #plot_carbon_live_sd = sd(treeplot_carbon_live),
    #plot_carbon_all = mean(treeplot_carbon_all),
    #plot_carbon_all_sd = sd(treeplot_carbon_all),
    .by = c(plot_id, plot_no, lc_class_plot)
  ) |> 
  mutate(
    plot_carbon_ag_se = if_else(is.na(plot_carbon_ag_sd), NA_real_, plot_carbon_ag_sd / sqrt(count_treeplot)),
    plot_carbon_ag_me = if_else(is.na(plot_carbon_ag_sd), NA_real_, plot_carbon_ag_se * qt(0.975, count_treeplot-1)),
    plot_carbon_ag_cilower = plot_carbon_ag - plot_carbon_ag_me,
    plot_carbon_ag_ciupper = plot_carbon_ag + plot_carbon_ag_me
  )
plot_carbon

plot_carbon |>
  ggplot(aes(x = plot_id)) +
  geom_col(aes(y = plot_carbon_ag, fill = lc_class_plot)) +
  geom_errorbar(aes(ymin = plot_carbon_ag_cilower, ymax = plot_carbon_ag_ciupper))
