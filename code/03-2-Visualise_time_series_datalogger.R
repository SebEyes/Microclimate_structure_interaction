source("code/00-R_package_loader.R")

# Load data
data_folder = "data/microclimate_data/data_sensors/daily_stats_corrected"
list.files(data_folder)

# Design a grpah for each file in the folder
file_list = list.files(data_folder, full.names = TRUE)
file_list = file_list[str_detect(file_list, "daily_stats_V3_corrected.csv", negate = T)]

for (file in file_list) {
    # Read the data
    daily_data = read.csv(file)

    # Melt the data to long format
    daily_data = melt(
        daily_data,
        id.vars = c("date", "site_code"),
        variable.name = "strata",
        value.name = "value"
    )
    # Convert date to Date type
    daily_data$date = as.Date(daily_data$date, format = "%Y-%m-%d")
    
    # Create a plot for each file
    variable_name = str_replace(basename(file), ".csv", "")
    daily_plot = ggplot(
        daily_data,
        aes(
            x = date,
            y = value,
            color = strata
        )
    ) + facet_wrap(.~ site_code) + geom_line() + labs(title = variable_name)
    
    # Save the plot
    plot_file_name = paste0("docs/Time_series_data_loggers/", variable_name, "_time_series.png")
    ggsave(
        filename = plot_file_name,
        plot = daily_plot,
        width = 12,
        height = 8
    )
}

# Check and quantify differences in values
# Temperature

temp = read.csv(
    file_list[3]
)
# Melt the data to long format
temp = melt(
    temp,
    id.vars = c("date", "site_code"),
    variable.name = "strata",
    value.name = "value"
)
# Convert date to Date type
temp$date = as.Date(temp$date, format = "%Y-%m-%d")

# Filter from March 2023 to September 2024
temp_2 = temp %>% filter(
    date > as.Date("2024-03-01") & date < as.Date("2024-09-30")
)
temp_2 %>% group_by(strata) %>% summarise(base::mean(value, na.rm = T))

# Filter from October 2024 to February 2025

temp_3 = temp %>% filter(
    date > as.Date("2024-10-01") & date < as.Date("2025-02-28")
)
temp_3 %>% group_by(strata) %>% summarise(base::mean(value, na.rm = T))