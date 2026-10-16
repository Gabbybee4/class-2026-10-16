latitude <- 27.5692
longitude <- -82.4259
timezone <- "America/New_York"

last_complete_year <- as.integer(format(Sys.Date(), "%Y")) - 1L
historical_years <- seq.int(last_complete_year - 4L, last_complete_year)
projection_years <- seq.int(last_complete_year + 1L, last_complete_year + 5L)

weather_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive",
  "?latitude=", latitude,
  "&longitude=", longitude,
  "&start_date=", historical_years[1], "-01-01",
  "&end_date=", last_complete_year, "-12-31",
  "&hourly=temperature_2m,precipitation",
  "&timezone=", URLencode(timezone),
  "&format=csv"
)

# The CSV response starts with two metadata lines before the hourly data.
parrish_weather <- read.csv(weather_url, skip = 2, check.names = FALSE)
parrish_weather$time <- as.POSIXct(
  parrish_weather$time,
  format = "%Y-%m-%dT%H:%M",
  tz = timezone
)

parrish_weather$year <- as.integer(format(parrish_weather$time, "%Y"))
parrish_weather <- subset(
  parrish_weather,
  year %in% historical_years
)

write.csv(parrish_weather, "parrish_weather.csv", row.names = FALSE)

annual_temperature <- aggregate(
  parrish_weather[["temperature_2m (°C)"]],
  by = list(year = parrish_weather$year),
  FUN = mean,
  na.rm = TRUE
)
names(annual_temperature)[2] <- "value"
annual_temperature$measure <- "Average temperature (°C)"

annual_precipitation <- aggregate(
  parrish_weather[["precipitation (mm)"]],
  by = list(year = parrish_weather$year),
  FUN = sum,
  na.rm = TRUE
)
names(annual_precipitation)[2] <- "value"
annual_precipitation$measure <- "Annual precipitation (mm)"
average_annual_precipitation <- mean(annual_precipitation$value)
cat(
  "Average annual precipitation in Parrish over",
  min(historical_years), "to", max(historical_years), ":",
  round(average_annual_precipitation, 1), "mm (",
  round(average_annual_precipitation / 25.4, 1), "inches) per year\n"
)

historical_annual <- rbind(annual_temperature, annual_precipitation)
historical_annual$period <- "Historical"

# Extend each five-year historical trend; these are estimates, not forecasts.
temperature_model <- lm(value ~ year, data = annual_temperature)
precipitation_model <- lm(value ~ year, data = annual_precipitation)
projected_temperature <- data.frame(
  year = projection_years,
  value = predict(temperature_model, newdata = data.frame(year = projection_years)),
  measure = "Average temperature (°C)"
)
projected_precipitation <- data.frame(
  year = projection_years,
  value = predict(precipitation_model, newdata = data.frame(year = projection_years)),
  measure = "Annual precipitation (mm)"
)
projected_precipitation$value <- pmax(projected_precipitation$value, 0)
projected_annual <- rbind(projected_temperature, projected_precipitation)
projected_annual$period <- "Trend estimate"

parrish_annual_weather <- rbind(historical_annual, projected_annual)
write.csv(parrish_annual_weather, "parrish_annual_weather.csv", row.names = FALSE)
