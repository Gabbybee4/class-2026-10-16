library(ggplot2)

parrish_annual_weather <- read.csv("parrish_annual_weather.csv")
rainfall <- subset(
  parrish_annual_weather,
  measure == "Annual precipitation (mm)" & period == "Historical"
)
average_rainfall <- mean(rainfall$value)

weather_plot <- ggplot(rainfall, aes(x = year, y = value)) +
  geom_line(color = "#57C7E3", linewidth = 1.4) +
  geom_point(color = "#57C7E3", size = 3.5) +
  geom_hline(
    yintercept = average_rainfall,
    color = "#FF9F68",
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  geom_text(
    aes(label = sprintf("%.0f mm", value)),
    vjust = -0.6,
    color = "#EAF2F8",
    size = 4
  ) +
  annotate(
    "label",
    x = max(rainfall$year),
    y = average_rainfall,
    label = sprintf("5-year average: %.0f mm (%.1f in)", average_rainfall, average_rainfall / 25.4),
    hjust = 1,
    vjust = -0.5,
    color = "#101B2D",
    fill = "#FF9F68",
    size = 3.5
  ) +
  scale_x_continuous(breaks = rainfall$year) +
  scale_y_continuous(
    name = "Rainfall per year (mm)",
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "Parrish, Florida",
    subtitle = "Annual rainfall compared with the 2021–2025 average",
    x = "Year",
    caption = "The dashed line marks the five-year average. Historical data: Open-Meteo."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#101B2D", color = NA),
    panel.background = element_rect(fill = "#101B2D", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#30415A"),
    text = element_text(color = "#EAF2F8"),
    axis.text = element_text(color = "#B9C7D8"),
    plot.title = element_text(size = 22, face = "bold", color = "white"),
    plot.subtitle = element_text(color = "#B9C7D8"),
    plot.caption = element_text(color = "#8193AA")
  )

ggsave("parrish-weather.png", weather_plot, width = 10, height = 6, dpi = 300)
