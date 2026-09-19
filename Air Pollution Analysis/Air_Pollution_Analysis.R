
# Packages

install.packages("janitor")

library(readr)
library(dplyr)
library(ggplot2)
library(janitor)

# Import CSV file.

air_pollution <- read_csv("air pollution dataset.csv") %>%
  clean_names()

# Show rows, columns, and data types

nrow(air_pollution)
ncol(air_pollution)
str(air_pollution)

# Count missing values

colSums(is.na(air_pollution))

# Remove rows where Country or City is missing

air_pollution_clean <- air_pollution %>%
  filter(!is.na(country), !is.na(city))

# Number of countries and city records

n_distinct(air_pollution_clean$country)
nrow(air_pollution_clean)

# Mean and median overall AQI

air_pollution_clean %>%
  summarise(
    mean_aqi = mean(aqi_value),
    median_aqi = median(aqi_value)
  )

# Number of records in each AQI category

air_pollution_clean %>%
  count(aqi_category, sort = TRUE)

# Five countries with the most city records

top_countries <- air_pollution_clean %>%
  count(country, name = "city_records", sort = TRUE) %>%
  slice_head(n = 5)

top_countries

# Five countries with highest average AQI
# Only countries with 20 or more city records are included

highest_average_aqi <- air_pollution_clean %>%
  group_by(country) %>%
  summarise(
    city_records = n(),
    mean_aqi = mean(aqi_value),
    .groups = "drop"
  ) %>%
  filter(city_records >= 20) %>%
  arrange(desc(mean_aqi)) %>%
  slice_head(n = 5)

highest_average_aqi

# City with the highest AQI

air_pollution_clean %>%
  filter(aqi_value == max(aqi_value)) %>%
  select(country, city, aqi_value, aqi_category)

# Number and percentage of Moderate-or-worse records

air_pollution_clean %>%
  filter(aqi_category != "Good") %>%
  summarise(
    records = n(),
    percentage = 100 * n() / nrow(air_pollution_clean)
  )

# Correlation between pollutant AQI values

aqi_correlation <- air_pollution_clean %>%
  select(
    aqi_value,
    pm2_5_aqi_value,
    ozone_aqi_value,
    no2_aqi_value,
    co_aqi_value
  ) %>%
  cor()

round(aqi_correlation, 3)

# Scatter plot: PM2.5 AQI compared with overall AQI

ggplot(air_pollution_clean, aes(x = pm2_5_aqi_value, y = aqi_value)) +
  geom_point(alpha = 0.25, colour = "blue") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  labs(
    title = "Overall AQI and PM2.5 AQI",
    x = "PM2.5 AQI Value",
    y = "Overall AQI Value"
  ) +
  theme_minimal()
