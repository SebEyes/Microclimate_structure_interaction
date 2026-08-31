source("code/00-R_package_loader.R")

#---------------------------------
# 1. Data preparation
#---------------------------------
# Load data
dataset_modelling = read.csv(
    "data/microclimate_data/data_modelling.csv"
)
str(dataset_modelling)
summary(dataset_modelling)

# Compute northness/eastness from raw aspect
# Convert degrees to radians
dataset_modelling$Aspect_30 <- dataset_modelling$Aspect_30 * pi / 180

# Calculate northness and eastness
dataset_modelling$northness <- cos(dataset_modelling$Aspect_30)
dataset_modelling$eastness  <- sin(dataset_modelling$Aspect_30)

# Sum GHI per day = total incoming energy per day

total_energy = dataset_modelling %>% 
    group_by(site_code, date) %>% 
    summarise(energy_tot = sum(GHI)) %>% 
    ungroup() %>%
    mutate(energy_tot = as.numeric(scale(energy_tot))) %>% #Standardised total energy
    unique()


dataset_modelling = merge(
    dataset_modelling,
    total_energy,
    by = c("site_code", "date")
)

#---------------------------------
# 2. Modelling
#---------------------------------
# Variables to loop over
response_vars <- c("GRD_DTR", "GRD_DVPDR",
                   "UND_DTR", "UND_DVPDR",
                   "CAN_DTR", "CAN_DVPDR",
                   "GRD_temperature_mean",
                    "GRD_vpd_kPA_mean",
                    "UND_temperature_mean",
                    "UND_vpd_kPA_mean",
                    "CAN_temperature_mean",
                    "CAN_vpd_kPA_mean"
)


independant_vars = c(
    "canopy_openess",
    "northness",
    "eastness",
    "ENL0D",
    "SSCI",
    "Elevation",
    "Slope_30",
    "foliage_height_diversity"
)

# Normalise independant variables
dataset_modelling <- dataset_modelling %>%
    dplyr::select(-"GHI") %>%
    mutate(across(all_of(c(independant_vars)), ~ scale(.)[,1])) %>%
    unique()

# Data selection
data_selected <- dataset_modelling %>%
    dplyr::select(
        all_of(c(response_vars, independant_vars)),
        energy_tot,
        site_code,
        date
    ) %>%
    mutate(
        site_code = factor(site_code),
        date = as.Date(date),
        time_days = as.numeric(date)
    ) %>%
    filter(
        complete.cases(.)
    ) %>%
    arrange(site_code, date)

# Ensure one observation per site and day
data_selected <- data_selected %>%
    distinct(site_code, date, .keep_all = TRUE)


# Initialise results storage
results_list <- list()
model_fit = data.frame(
    model = NA_character_,
    R2_marginal = NA_real_,
    R2_conditional = NA_real_
)

for (resp in response_vars) {

    # Model formula
    formula <- as.formula(sprintf(
        "%s ~ energy_tot * (canopy_openess + northness + eastness +
        ENL0D + SSCI + foliage_height_diversity + Elevation + Slope_30)",
        resp
    ))

    # Fit model with continuous-time AR(1) temporal correlation
    mod <- nlme::lme(
        fixed = formula,
        random = ~ 1 | site_code,
        correlation = corCAR1(
        form = ~ time_days | site_code
        ),
        data = data_selected,
        method = "ML",
        na.action = na.omit,
        control = nlmeControl(returnObject = TRUE)
    )

    # Tidy summary
    tidy_mod <- broom.mixed::tidy(mod, effects = "fixed")

    # # Add index for graphs
    tidy_mod$variable_idx = seq_len(nrow(tidy_mod))

    # Filter significant coefficients
    sig_mod <- tidy_mod %>% filter(p.value < 0.05)

    # Add response variable
    sig_mod$response <- resp
    results_list[[resp]] <- sig_mod

    # Check performance (R2)
    model_fit = rbind(
        na.omit(model_fit),
        data.frame(
            model = resp,
            R2_marginal = performance::model_performance(mod)$R2_marginal,
            R2_conditional = performance::model_performance(mod)$R2_conditional
        )
    )
}


# Adress multicolinearity (VIF)
vif(mod)


# Model fit
model_fit
write.table(
    model_fit,
    "docs/supplementary/model_fit.csv",
    sep = ";",
    row.names = F
)

# Combine results
signif_table <- bind_rows(results_list)

significant_interaction = signif_table %>%
    filter(variable_idx > 11)
significant_interaction$variable_idx = significant_interaction$variable_idx - 10

# Print table
print(signif_table)

# Save table
write.csv(signif_table, "docs/microclimate_structure_LMM_significant_results.csv", row.names = FALSE)


#---------------------------------
# 3. Model validation
#---------------------------------

formulas_autocor = list()
formulas_RI = list()
formulas_simple = list()
formulas_GAM = list()


for (resp in response_vars) {
    # LMM with autocorrelation
    formula_autocor <- as.formula(sprintf(
        "%s ~ energy_tot * (canopy_openess + northness + eastness +
        ENL0D + SSCI + foliage_height_diversity + Elevation + Slope_30)",
        resp
    ))

    # LMM
    formula_RI <- as.formula(sprintf(
        "%s ~ energy_tot*(canopy_openess + northness + eastness + ENL0D + SSCI + Elevation + Slope_30 + foliage_height_diversity) + (1|site_code)",
        resp
    ))

    # LM
    formula_simple <- as.formula(sprintf(
        "%s ~ energy_tot*(canopy_openess + northness + eastness + ENL0D + SSCI + Elevation + Slope_30 + foliage_height_diversity)",
        resp
    ))

    # GAM
    formula_GAM = as.formula(sprintf(
        "%s ~ te(energy_tot, canopy_openess) + te(energy_tot,northness) + te(energy_tot,eastness) + te(energy_tot, ENL0D) + te(energy_tot, SSCI) + te(energy_tot, Elevation) + te(energy_tot, Slope_30) + te(energy_tot, foliage_height_diversity)",
        resp
    ))
    formulas_autocor = append(formulas_autocor, formula_autocor)
    formulas_simple = append(formulas_simple, formula_simple)
    formulas_RI = append(formulas_RI, formula_RI)
    formulas_GAM = append(formulas_GAM, formula_GAM)
}

# List model
model_idx = data.frame(
    response_vars,
    idx = 1:length(response_vars)
)

# Function to fit the three models and extract AIC and BIC
compare_models <- function(model_number) {

    message(
        sprintf(
            "Computing model %i", model_number
        )
    )
  
  # Fit models
    m_autocor <- nlme::lme(
        fixed = formulas_autocor[[model_number]],
        random = ~ 1 | site_code,
        correlation = corCAR1(
        form = ~ time_days | site_code
        ),
        data = data_selected,
        method = "ML",
        na.action = na.omit,
        control = nlmeControl(returnObject = TRUE)
    )

  m_RI <- lmer( # RI
    formulas_RI[[model_number]],
    data = data_selected,
    REML = FALSE
  )
  
  m_simple <- lm( # No random effects
    formulas_simple[[model_number]],
    data = data_selected
  )
  
  m_GAM <- gam( # GAM
    formulas_GAM[[model_number]],
    data = data_selected,
    method = "ML"
  )
  
  # Compare models using performance::compare_performance()
  comparison <- performance::compare_performance(
    m_autocor,
    m_simple,
    m_RI,
    m_GAM,
    metrics = "all",
    rank = FALSE,
    verbose = FALSE
  )
  
  # Convert to a data frame and retain the required columns
  comparison <- as.data.frame(comparison)
  
  comparison %>%
    mutate(
      model_number = model_number,
      .before = 1
    )
}

# Apply the function to all model numbers
model_comparison <- map_dfr(seq_along(model_idx$idx), compare_models)


model_comparison = merge(
    model_idx,
    model_comparison,
    by.x = "idx",
    by.y = "model_number"
) %>% select(
    -idx
) %>% rename(
    "Response variable" = "response_vars"
)

# Display the results
model_comparison

# Save results
write.table(
    model_comparison,
    "docs/supplementary/models_comparison.csv",
    sep = ";",
    row.names = F
)

model_comparison = melt(
    model_comparison,
    id.vars = c("Model")
)

model_comparison = model_comparison %>% filter(variable %in% c("AIC", "BIC"))

## Recoding model_comparison$Model
model_comparison$Model <- model_comparison$Model |>
  fct_recode(
    "LM with autocorelation" = "lme",
    "GAM" = "gam",
    "LM" = "lm",
    "LMM" = "lmerModLmerTest"
  )

model_comparison$value = as.numeric(model_comparison$value)

plot_comp = model_comparison %>% tidyplot(
    x = Model,
    y = value
) %>% add_boxplot() %>%
add_data_points_beeswarm() %>%
adjust_x_axis(rotate_labels = T) %>%
add_test_asterisks(method = "wilcox_test") %>%
split_plot(variable)

save_plot(plot_comp, "docs/supplementary/Reviews V1/model_info_criteria.jpg")