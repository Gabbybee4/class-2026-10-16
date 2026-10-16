library(ggplot2)

last_complete_year <- as.integer(format(Sys.Date(), "%Y")) - 1L
first_year <- last_complete_year - 4L
start_date <- sprintf("%d-01-01", first_year)
end_date <- sprintf("%d-12-31", last_complete_year)

locations <- data.frame(
  city = c("Parrish, Florida", "Buffalo, New York"),
  latitude = c(27.5692, 42.8864),
  longitude = c(-82.4259, -78.8784),
  timezone = c("America/New_York", "America/New_York")
)

download_monthly_weather <- function(city, latitude, longitude, timezone) {
  weather_url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=", latitude,
    "&longitude=", longitude,
    "&start_date=", start_date,
    "&end_date=", end_date,
    "&daily=temperature_2m_mean,precipitation_sum",
    "&timezone=", URLencode(timezone),
    "&format=csv"
  )

  daily_weather <- read.csv(weather_url, skip = 2, check.names = FALSE)
  required_columns <- c(
    "time",
    "temperature_2m_mean (°C)",
    "precipitation_sum (mm)"
  )
  if (!all(required_columns %in% names(daily_weather))) {
    stop("Weather data for ", city, " did not contain the expected columns.")
  }

  daily_weather$date <- as.Date(daily_weather$time)
  daily_weather$month <- as.Date(format(daily_weather$date, "%Y-%m-01"))
  monthly_temperature <- aggregate(
    daily_weather[["temperature_2m_mean (°C)"]],
    by = list(month = daily_weather$month),
    FUN = mean,
    na.rm = TRUE
  )
  monthly_precipitation <- aggregate(
    daily_weather[["precipitation_sum (mm)"]],
    by = list(month = daily_weather$month),
    FUN = sum,
    na.rm = TRUE
  )

  monthly_weather <- merge(
    monthly_temperature,
    monthly_precipitation,
    by = "month",
    suffixes = c("_temperature", "_precipitation")
  )
  names(monthly_weather)[2:3] <- c("mean_temperature_c", "total_precipitation_mm")
  monthly_weather$city <- city
  monthly_weather
}

monthly_weather <- do.call(
  rbind,
  lapply(seq_len(nrow(locations)), \(i) {
    download_monthly_weather(
      locations$city[i],
      locations$latitude[i],
      locations$longitude[i],
      locations$timezone[i]
    )
  })
)
write.csv(monthly_weather, "parrish-buffalo-weather-monthly.csv", row.names = FALSE)

temperature_trend <- data.frame(
  month = monthly_weather$month,
  city = monthly_weather$city,
  measure = "Monthly mean temperature (°C)",
  value = monthly_weather$mean_temperature_c
)
rainfall_trend <- data.frame(
  month = monthly_weather$month,
  city = monthly_weather$city,
  measure = "Monthly total rainfall (mm)",
  value = monthly_weather$total_precipitation_mm
)
weather_trends <- rbind(temperature_trend, rainfall_trend)

weather_plot <- ggplot(
  weather_trends,
  aes(x = month, y = value, color = city, group = city)
) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ measure, scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = c("Parrish, Florida" = "#FF9F68", "Buffalo, New York" = "#57C7E3")
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title = "Weather trends: Parrish and Buffalo",
    subtitle = paste("Monthly summaries from", first_year, "through", last_complete_year),
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Rainfall is the monthly total; temperature is the monthly mean. Historical estimates: Open-Meteo Archive."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#101B2D", color = NA),
    panel.background = element_rect(fill = "#101B2D", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#30415A"),
    text = element_text(color = "#EAF2F8"),
    axis.text = element_text(color = "#B9C7D8"),
    strip.text = element_text(color = "#EAF2F8", face = "bold"),
    plot.title = element_text(size = 22, face = "bold", color = "white"),
    plot.subtitle = element_text(color = "#B9C7D8"),
    plot.caption = element_text(color = "#8193AA"),
    legend.position = "top"
  )

ggsave(
  "parrish-buffalo-weather-trends.png",
  weather_plot,
  width = 11,
  height = 8,
  dpi = 300
)
