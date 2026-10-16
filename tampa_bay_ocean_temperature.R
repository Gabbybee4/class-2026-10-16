library(ggplot2)
library(jsonlite)

station <- "8726384"
start_date <- Sys.Date() - 3652
end_date <- Sys.Date() - 1
years <- seq.int(
  as.integer(format(start_date, "%Y")),
  as.integer(format(end_date, "%Y"))
)

fetch_year <- function(year) {
  chunk_start <- max(start_date, as.Date(sprintf("%d-01-01", year)))
  chunk_end <- min(end_date, as.Date(sprintf("%d-12-31", year)))
  request_url <- paste0(
    "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter",
    "?begin_date=", format(chunk_start, "%Y%m%d"),
    "&end_date=", format(chunk_end, "%Y%m%d"),
    "&station=", station,
    "&product=water_temperature&interval=h",
    "&units=metric&time_zone=gmt&format=json",
    "&application=tampa_bay_temperature_trend"
  )

  response <- jsonlite::fromJSON(request_url)
  if (!is.null(response$error)) {
    stop("NOAA API error: ", response$error$message)
  }
  if (is.null(response$data) || nrow(response$data) == 0) {
    stop("NOAA returned no water-temperature data for ", year, ".")
  }

  data.frame(
    time = as.POSIXct(response$data$t, format = "%Y-%m-%d %H:%M", tz = "UTC"),
    temperature_c = as.numeric(response$data$v)
  )
}

hourly_temperature <- do.call(rbind, lapply(years, fetch_year))
hourly_temperature <- subset(
  hourly_temperature,
  !is.na(time) & !is.na(temperature_c) &
    as.Date(time, tz = "UTC") >= start_date &
    as.Date(time, tz = "UTC") <= end_date
)

hourly_temperature$month <- as.Date(
  format(hourly_temperature$time, "%Y-%m-01", tz = "UTC")
)
monthly_temperature <- aggregate(
  temperature_c ~ month,
  data = hourly_temperature,
  FUN = mean
)
all_months <- data.frame(
  month = seq(
    as.Date(format(start_date, "%Y-%m-01")),
    as.Date(format(end_date, "%Y-%m-01")),
    by = "month"
  )
)
monthly_temperature <- merge(all_months, monthly_temperature, by = "month", all.x = TRUE)
monthly_temperature$rolling_12_month_mean <- as.numeric(
  stats::filter(monthly_temperature$temperature_c, rep(1 / 12, 12), sides = 1)
)
write.csv(monthly_temperature, "tampa-bay-ocean-temperature.csv", row.names = FALSE)

temperature_plot <- ggplot(monthly_temperature, aes(x = month)) +
  geom_line(
    aes(y = temperature_c),
    color = "#57C7E3",
    alpha = 0.65,
    linewidth = 0.7
  ) +
  geom_line(
    aes(y = rolling_12_month_mean),
    color = "#FF9F68",
    linewidth = 1.3,
    na.rm = TRUE
  ) +
  labs(
    title = "Tampa Bay water temperature",
    subtitle = paste(
      format(start_date, "%b %Y"), "to", format(end_date, "%b %Y"),
      "· monthly means from hourly NOAA observations"
    ),
    x = NULL,
    y = "Water temperature (°C)",
    caption = paste(
      "Blue: monthly mean · Orange: trailing 12-month mean",
      "NOAA CO-OPS Port Manatee station (8726384)"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#101B2D", color = NA),
    panel.background = element_rect(fill = "#101B2D", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#30415A"),
    text = element_text(color = "#EAF2F8"),
    axis.text = element_text(color = "#B9C7D8"),
    axis.title.y = element_text(color = "#72D4EC"),
    plot.title = element_text(size = 22, face = "bold", color = "white"),
    plot.subtitle = element_text(color = "#B9C7D8"),
    plot.caption = element_text(color = "#8193AA")
  )

ggsave("tampa-bay-ocean-temperature.png", temperature_plot, width = 10, height = 6, dpi = 300)
