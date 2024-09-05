
forest_carbon <- plot_carbon |>
  summarise(
    count_plot = n(),
    carbon_ag = mean(plot_carbon_ag),
    carbon_ag_sd = sd(plot_carbon_ag),
    carbon_live = mean(plot_carbon_live),
    carbon_live_sd = sd(plot_carbon_live), 
    carbon_all = mean(plot_carbon_all),
    carbon_all_sd = sd(plot_carbon_all),
    .by = lc_class_plot
  ) |>
  mutate(
    carbon_ag_se = carbon_ag_sd / sqrt(count_plot),
    carbon_ag_me = carbon_ag_se * qt(0.975, count_plot-1),
    carbon_ag_cilower = carbon_ag - carbon_ag_me,
    carbon_ag_ciupper = carbon_ag + carbon_ag_me
  )
print(forest_carbon)

gg <- forest_carbon |>
  ggplot(aes(x = lc_class_plot)) +
  geom_col(aes(y = carbon_all), fill = "darkblue") +
  geom_col(aes(y = carbon_live), fill = "darkred") +
  geom_col(aes(y = carbon_ag)) +
  theme_bw() +
  labs(
    x = "Forest type",
    y = "Carbon (tC/ha)"
    )
print(gg)

gg <- forest_carbon |>
  ggplot(aes(x = lc_class_plot)) +
  geom_col(aes(y = carbon_ag, fill = lc_class_plot)) +
  geom_errorbar(aes(ymin = carbon_ag_cilower, ymax = carbon_ag_ciupper), width = 0.4) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    x = "Forest type code",
    y = "Aboveground carbon (tC/ha)"
  )
print(gg)
