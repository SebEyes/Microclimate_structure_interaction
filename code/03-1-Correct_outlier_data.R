source("code/00-R_package_loader.R")

# Load data
daily_data = read.csv(
    "data/microclimate_data/data_sensors/daily_stats_V3.csv"
) %>% na.omit()

str(daily_data)
summary(daily_data)


# Remove mean temperature above 35°C
daily_data = daily_data %>%
  filter(temperature_mean < 35)

# Remove temperature on the ground after July 2024 for TER-PRIBS-T15
daily_data = daily_data %>%
  filter(!(site_code == "TER-NFTB-T-15" & strata == "GRD" & date > as.Date("2024-07-01")))

# Remove temperature above 20°C for the understory of TER-EXO-T06 before May 2024
daily_data = daily_data %>%
  filter(!(site_code == "TER-PRIBS-T15" & strata == "UND" & date < as.Date("2024-05-01") & temperature_mean > 20))

# Remove temperature above 20°C for the understory of TER-EXO-T09 before May 2024
daily_data = daily_data %>%
  filter(!(site_code == "TER-EXO-T09" & strata == "UND" & date < as.Date("2024-05-01") & temperature_mean > 20))

# Remove temperature above 20°C for the ground of TER-EXO-T09 before 15 July 2024
daily_data = daily_data %>%
  filter(!(site_code == "TER-EXO-T09" & strata == "GRD" & date < as.Date("2024-07-15") & temperature_mean > 20))

# Remove the first 2 days of data for the Canopy of TER-NFPG-T33
daily_data = daily_data %>%
  filter(!(site_code == "TER-NFPG-T-33" & strata == "CAN" & date < as.Date("2024-07-01")))

# Remove all data point above 25°C for TER-EXO-T09
daily_data = daily_data %>%
  filter(!(site_code == "TER-EXO-T09" & temperature_mean > 25))

# Remove canopy data for TER-EXOT09 after 1st October 2024
daily_data = daily_data %>%
  filter(!(site_code == "TER-EXO-T09" & strata == "CAN" & date > as.Date("2024-10-01")))

# value if std is greater than 10
daily_data = daily_data %>%
  filter(temperature_std < 10)


daily_data$date = as.Date(daily_data$date, format = "%Y-%m-%d")


# Rename with correct site_code
daily_data$site_code[daily_data$site_code == "TER-EXO-T03"] <- "TER-PRIBS-T28"
daily_data$site_code[daily_data$site_code == "TER-EXO-T05"] <- "TER-PRIBS-T09"
daily_data$site_code[daily_data$site_code == "TER-EXO-T06"] <- "TER-PRIBS-T15"
daily_data$site_code[daily_data$site_code == "TER-EXO-T08"] <- "TER-PRIBS-T06"

str(daily_data)

write.csv(
    daily_data,
    file = paste0("data/microclimate_data/data_sensors/daily_stats_corrected/daily_stats_V3_corrected.csv"),
    row.names = FALSE
)


# Split response variables temperature and VPD X mean and std
for (col in c("temperature_mean", "vpd_kPA_mean", "temperature_std", "vpd_kPA_std")) {
  # Compile to wide format
  daily_data_response_var = daily_data %>%
    select(date, site_code, strata, col) %>%
    pivot_wider(
      names_from = strata,
      values_from = col
    )

  # Generate sequences from the start to the edn of the daily time series
    daily_data_full = data.frame(
      date = seq(
          from = min(daily_data$date),
          to = max(daily_data$date),
          by = "day"
      )
  )

  # Merge existing correct data with full sequence
  daily_data_full = merge(
      daily_data_full,
      daily_data_response_var,
      by = "date",
      all.x = TRUE
  ) %>% arrange(date, site_code)

  # Add NA data for missing dates for each site_code
  daily_data_response_var = daily_data_full %>%
    group_by(site_code) %>%
    complete(date = seq(min(daily_data_full$date), max(daily_data_full$date), by = "day")) %>%
    ungroup() %>%
    arrange(site_code, date)

  # Save corrected data
  dataset_name = paste0("daily_stats_corrected_", col, ".csv")

  write.csv(
      daily_data_response_var,
      file = paste0("data/microclimate_data/data_sensors/daily_stats_corrected/", dataset_name),
      row.names = FALSE
  )
}
