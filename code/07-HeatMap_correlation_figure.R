source("code/00-R_package_loader.R")

#---------------------------------
# 1. Data Loading
#---------------------------------
results_correlation = read.csv(
    "data/significant_interactions.csv",
    sep = ";"
)
head(results_correlation)

#---------------------------------
# 2. Data Selection
#---------------------------------

data_heatmap = select(
    results_correlation,
    c(
        "term",
        "estimate",
        "response"
    )
)

# Keep only interaction terms
data_heatmap = data_heatmap %>% filter(
  str_detect(term, "energy_tot:")
)

head(data_heatmap)


#---------------------------------
# 3. Data Renaming and ordering
#---------------------------------

## Recoding data_heatmap$response into data_heatmap$`Microclimatic variable`
data_heatmap$`Microclimatic variable` <- data_heatmap$response |>
  fct_recode(
    "Canopy DTR" = "CAN_DTR",
    "Canopy DVPDR" = "CAN_DVPDR",
    "Canopy mean temperature" = "CAN_temperature_mean",
    "Canopy mean VPD" = "CAN_vpd_kPA_mean",
    "Ground DTR" = "GRD_DTR",
    "Ground DVPDR" = "GRD_DVPDR",
    "Ground mean temperature" = "GRD_temperature_mean",
    "Ground mean VPD" = "GRD_vpd_kPA_mean",
    "Understory DTR" = "UND_DTR",
    "Understory DVPDR" = "UND_DVPDR",
    "Understory mean temperature" = "UND_temperature_mean",
    "Understory mean VPD" = "UND_vpd_kPA_mean"
  )


## Recoding data_heatmap$term into data_heatmap$Metric
data_heatmap$Metric <- data_heatmap$term |>
  fct_recode(
    "Northness" = "energy_tot:northness",
    "Eastness" = "energy_tot:eastness",
    "Canopy openness" = "energy_tot:canopy_openess",
    "Elevation" = "energy_tot:Elevation",
    "ENL0D" = "energy_tot:ENL0D",
    "FHD" = "energy_tot:foliage_height_diversity",
    "Slope" = "energy_tot:Slope_30",
    "SSCI" = "energy_tot:SSCI"
  )

## Reordering data_heatmap$Metric
data_heatmap$Metric <- data_heatmap$Metric |>
  fct_relevel(
    "Elevation", "Slope", "Northness", "Eastness", "Canopy openness", "ENL0D",
    "FHD", "SSCI"
  )

## Reordering data_heatmap$`Microclimatic variable`
data_heatmap$`Microclimatic variable` <- data_heatmap$`Microclimatic variable` |>
  fct_relevel(
    "Canopy mean temperature", "Canopy DTR", "Canopy mean VPD",
    "Canopy DVPDR", "Understory mean temperature", "Understory DTR",
    "Understory mean VPD", "Understory DVPDR", "Ground mean temperature",
    "Ground DTR", "Ground mean VPD", "Ground DVPDR"
  )

#---------------------------------
# 4. Create Heatmap
#---------------------------------
data_heatmap_plot <- data_heatmap |>
    mutate(
        estimate = ifelse(is.na(estimate), "NS", round(estimate, 3)),
        text_color = ifelse(
            as.numeric(estimate) > 0.3 | as.numeric(estimate) < -0.3,
            "white",
            "black"
        )
    ) |>
    ggplot(
        aes(
            x = Metric,
            y = fct_rev(`Microclimatic variable`),
            fill = estimate
        )
    ) +
    geom_tile(color = "black") +
    geom_text(
        aes(label = estimate, color = text_color),
        size = 3
    ) +
    scale_color_identity() +
    scale_fill_gradient2(
        low = "red",
        mid = "white",
        high = "blue",
        name = "Estimate",
        guide = "none"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y =  element_blank()
    )

print(data_heatmap_plot)
ggsave(
    "docs/Figures/Heatmap_correlation.png",
    plot = data_heatmap_plot,
    width = 10,
    height = 8,
    dpi = 300,
    units = "in"
)
