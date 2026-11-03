library(dplyr)
library(ggplot2)
library(readr)

mortality_data <- final_data |>
  select(iso, year, maternal_mortality) |>
  filter(year <= 2017) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(
    diff_matmon = maternal_mortality -
      maternal_mortality[year == 2000][1]
  ) |>
  ungroup()

increased_countries <- mortality_data |>
  filter(year == 2017, !is.na(diff_matmon),
         diff_matmon > 0) |>
  pull(iso)

print(increased_countries)
cat("Number of countries:", length(increased_countries), "\n")

plot_data <- mortality_data |>
  filter(iso %in% increased_countries)

maternal_plot <- ggplot(
  plot_data,
  aes(x = year, y = maternal_mortality,
      color = iso, group = iso)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Maternal Mortality Trends, 2000–2017",
    subtitle = "Countries with increased maternal mortality from 2000 to 2017",
    x = "Year",
    y = "Maternal mortality ratio",
    color = "Country (ISO)"
  ) +
  scale_x_continuous(breaks = seq(2000, 2017, 2)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(maternal_plot)

dir.create("reports", showWarnings = FALSE)

ggsave(
  filename = "reports/maternal_mortality_trends.png",
  plot = maternal_plot,
  width = 11,
  height = 7,
  dpi = 300
)
