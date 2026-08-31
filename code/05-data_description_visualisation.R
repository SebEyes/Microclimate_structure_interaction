source("code/00-R_package_loader.R")

# Load data
dataset_modelling = read.csv(
    "data/microclimate_data/data_modelling.csv"
)
dataset_modelling$date = ymd(dataset_modelling$date)
str(dataset_modelling)

# Description recording
dataset_description = select(
    dataset_modelling,
    "site_code",
    "date",
    "GRD_DTR",
    "UND_DTR",
    "CAN_DTR"
)
str(dataset_description)

dataset_description <- dataset_description %>%
    group_by(site_code) %>%
    summarise(
        nb_day_GRD_records = sum(!is.na(GRD_DTR)),
        nb_day_UND_records = sum(!is.na(UND_DTR)),
        nb_day_CAN_records = sum(!is.na(CAN_DTR))
    )
print(dataset_description)
summary(dataset_description)

write.table(
    dataset_description,
    "docs/supplementary/log_records.csv",
    row.names = F,
    sep = ";"
)

# Description mean values
dataset_description = select(
    dataset_modelling,
    "site_code",
    "date",
    "GHI",
    "GRD_temperature_mean",
    "UND_temperature_mean",
    "CAN_temperature_mean",
    "GRD_vpd_kPA_mean",
    "UND_vpd_kPA_mean",
    "CAN_vpd_kPA_mean"
)
str(dataset_description)
summary(dataset_description)
mean_daily_GHI = dataset_description %>% group_by(date) %>% summarise(mean(GHI))
summary(mean_daily_GHI)
# Graph per variables
## Temperature mean
dataset_plot_temp = select(
    dataset_modelling,
    c(
        "site_code",
        "date",
        "GRD_temperature_mean",
        "UND_temperature_mean",
        "CAN_temperature_mean"
    )
)
str(dataset_plot_temp)

dataset_plot_temp = melt(
    dataset_plot_temp,
    id_vars = c("date", "site_code"),
    measure.vars = c("GRD_temperature_mean","UND_temperature_mean", "CAN_temperature_mean")
)

dataset_plot_temp$date_simple = paste(
    year(dataset_plot_temp$date),
    month(dataset_plot_temp$date),
    sep = "-"
)
## Reordering dataset_plot_temp$date_simple
dataset_plot_temp$date_simple <- factor(dataset_plot_temp$date_simple,
  levels = c(
    "2023-8", "2023-9", "2023-10", "2023-11", "2023-12", "2024-1",
    "2024-2", "2024-3", "2024-4", "2024-5", "2024-6", "2024-7", "2024-8",
    "2024-9", "2024-10", "2024-11", "2024-12", "2025-1", "2025-2",
    "2025-3", "2025-4", "2025-5", "2025-6"
  )
)

## Recoding dataset_plot_temp$variable into dataset_plot_temp$strata
dataset_plot_temp$strata <- as.character(dataset_plot_temp$variable)
dataset_plot_temp$strata[dataset_plot_temp$variable == "GRD_temperature_mean"] <- "GRD"
dataset_plot_temp$strata[dataset_plot_temp$variable == "UND_temperature_mean"] <- "UND"
dataset_plot_temp$strata[dataset_plot_temp$variable == "CAN_temperature_mean"] <- "CAN"

## Reordering dataset_plot_temp$strata
dataset_plot_temp$strata <- factor(dataset_plot_temp$strata,
  levels = c("GRD", "UND", "CAN")
)
ggplot(
    dataset_plot_temp, 
    aes(
        x = date_simple,
        y = value,
        fill = strata
    )
) +
geom_boxplot() +
labs(
    x = "Date (Year-Month)",
    y = "Mean temperature (°C)",
    fill = "Strata"
) +
theme_minimal() +
scale_fill_manual(
    values = c(
        "#615346",
        "#E08931",
        "#31E047"
    )
)+
theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 15, angle = 45)
)

ggsave(
    filename = "docs/Figures/Time_series_temp_V2.jpg",
    width = 12,
    height = 8
)

## VPD mean
dataset_plot_VPD = select(
    dataset_modelling,
    c(
        "site_code",
        "date",
        "GRD_vpd_kPA_mean",
        "UND_vpd_kPA_mean",
        "CAN_vpd_kPA_mean"
    )
)
str(dataset_plot_VPD)

dataset_plot_VPD = melt(
    dataset_plot_VPD,
    id_vars = c("date", "site_code"),
    measure.vars = c("GRD_vpd_kPA_mean","UND_vpd_kPA_mean", "CAN_vpd_kPA_mean")
)

dataset_plot_VPD$date_simple = paste(
    year(dataset_plot_VPD$date),
    month(dataset_plot_VPD$date),
    sep = "-"
)

dataset_plot_VPD$date_simple <- factor(dataset_plot_VPD$date_simple,
  levels = c(
    "2023-8", "2023-9", "2023-10", "2023-11", "2023-12", "2024-1",
    "2024-2", "2024-3", "2024-4", "2024-5", "2024-6", "2024-7", "2024-8",
    "2024-9", "2024-10", "2024-11", "2024-12", "2025-1", "2025-2",
    "2025-3", "2025-4", "2025-5", "2025-6"
  )
)

## Recoding dataset_plot_VPD$variable into dataset_plot_VPD$strata
dataset_plot_VPD$strata <- as.character(dataset_plot_VPD$variable)
dataset_plot_VPD$strata[dataset_plot_VPD$variable == "GRD_vpd_kPA_mean"] <- "GRD"
dataset_plot_VPD$strata[dataset_plot_VPD$variable == "UND_vpd_kPA_mean"] <- "UND"
dataset_plot_VPD$strata[dataset_plot_VPD$variable == "CAN_vpd_kPA_mean"] <- "CAN"

dataset_plot_VPD$strata <- factor(dataset_plot_VPD$strata,
  levels = c("GRD", "UND", "CAN")
)

ggplot(
    dataset_plot_VPD, 
    aes(
        x = date_simple,
        y = value,
        fill = strata
    )
) +
geom_boxplot() +
labs(
    x = "Date (Year-Month)",
    y = "Mean VPD (kPA)",
    fill = "Strata"
) +
theme_minimal() +
scale_fill_manual(
    values = c(
        "#615346",
        "#E08931",
        "#31E047"
    )
)+
theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 15, angle = 45)
)

ggsave(
    filename = "docs/Figures/Time_series_VPD_V2.jpg",
    width = 12,
    height = 8
)


## Total energy

dataset_plot_GHI = select(
    dataset_modelling,
    c(
        "site_code",
        "date",
        "GHI"
    )
)
str(dataset_plot_GHI)
dataset_plot_GHI = dataset_plot_GHI %>% group_by(site_code, date) %>% summarise(total_energy = sum(GHI))


dataset_plot_GHI$date_simple = paste(
    year(dataset_plot_GHI$date),
    month(dataset_plot_GHI$date),
    sep = "-"
)

dataset_plot_GHI$date_simple <- factor(dataset_plot_GHI$date_simple,
  levels = c(
    "2023-8", "2023-9", "2023-10", "2023-11", "2023-12", "2024-1",
    "2024-2", "2024-3", "2024-4", "2024-5", "2024-6", "2024-7", "2024-8",
    "2024-9", "2024-10", "2024-11", "2024-12", "2025-1", "2025-2",
    "2025-3", "2025-4", "2025-5", "2025-6"
  )
)


ggplot(
    dataset_plot_GHI, 
    aes(
        x = date_simple,
        y = total_energy
    )
) +
geom_boxplot(fill = "#E0D726") +
labs(
    x = "Date (Year-Month)",
    y = "Total daily solar radiation (Wh.m-2)"
) +
theme_minimal() +
theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 15, angle = 45)
)

ggsave(
    filename = "docs/Figures/Time_series_solar_radiation.jpg",
    width = 12,
    height = 8
)
