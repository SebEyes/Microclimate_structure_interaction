#---------------------------------
# 0. Package Loading
#---------------------------------
rm(list = ls())

pacman::p_load(
    tidyverse,
    reshape2,
    tidyplots,
    mgcv,
    nlme,
    lme4,
    lmerTest,
    broom.mixed,
    stringr,
    visreg,
    car, # Test VIF
    emmeans
)

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
# 2. Modeling
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

stratum_patterns = c(
    canopy = "CAN_",
    understory = "UND_",
    ground = "GRD_"
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

#---------------------------------
# 4. Convert data to long format
#---------------------------------

# Data selection
all_vars = c(response_vars, independant_vars, "energy_tot", "site_code", "date")

data_long <- dataset_modelling %>%
    dplyr::select(
        all_of(all_vars)
    ) %>%
    pivot_longer(
        cols = all_of(response_vars),
        names_to = "response_name",
        values_to = "response"
    ) %>%
    mutate(
    stratum = case_when(
      str_detect(response_name, stratum_patterns["canopy"]) ~ "Canopy",
      str_detect(response_name, stratum_patterns["understory"]) ~ "Understory",
      str_detect(response_name, stratum_patterns["ground"]) ~ "Ground",
      TRUE ~ NA_character_
    ),
    # Remove the stratum suffix to identify the response type
    response_type = response_name %>%
      str_remove("CAN_") %>%
      str_remove("UND_") %>%
      str_remove("GRD_"),
    
    site_code = factor(site_code),
    stratum = factor(
      stratum,
      levels = c("Ground", "Understory", "Canopy")
    ),
    date = as.Date(date),
    time_days = as.numeric(date)
  ) %>%
  filter(
    !is.na(stratum),
    complete.cases(.)
  ) %>%
  arrange(site_code, stratum, date)


#---------------------------------
# 5. Combined models
#---------------------------------
# Fit one combined model for each type of microclimatic response.
#
# The interaction
#
# stratum * energy_tot * predictor
#
# formally tests whether the energy-by-predictor relationship differs
# among vertical strata.

response_types <- unique(data_long$response_type)

results_list <- list()
model_fit <- list()
models_combined <- list()


for (resp_type in response_types) {
  
  message("Fitting model for: ", resp_type)
  
  dat_resp <- data_long %>%
    filter(response_type == resp_type)
  

  formula_combined <- response ~
    stratum * energy_tot *
    (
      canopy_openess +
      northness +
      eastness +
      ENL0D +
      SSCI +
      foliage_height_diversity +
      Elevation +
      Slope_30
    )
  
  
  mod <- nlme::lme(
    fixed = formula_combined,
    random = ~ 1 | site_code,
    # The temporal correlation is modelled separately within each
    # site × stratum combination.
    correlation = nlme::corCAR1(
      form = ~ time_days | site_code / stratum
    ),
    
    data = dat_resp,
    method = "ML",
    na.action = na.omit,
    control = nlme::lmeControl(
      returnObject = TRUE,
      opt = "optim"
    )
  )
  
  
  models_combined[[resp_type]] <- mod
  
  
  # Fixed-effect results

  tidy_mod <- broom.mixed::tidy(
    mod,
    effects = "fixed"
  ) %>%
    mutate(
      response_type = resp_type
    )
  
  results_list[[resp_type]] <- tidy_mod
  
  
  # Model performance

  model_fit[[resp_type]] <- data.frame(
    response_type = resp_type,
    R2_marginal = performance::model_performance(mod)$R2_marginal,
    R2_conditional = performance::model_performance(mod)$R2_conditional
  )
}


# Combine results

combined_results <- bind_rows(results_list)

# Filter relevant interaction terms
combined_results = combined_results[str_detect(combined_results$term, ":energy_tot:"),]

model_fit <- bind_rows(model_fit)

print(combined_results)
print(model_fit)


# Save results

write.csv(
  combined_results,
  "docs/Publications/07-Forest microclimate/Submission Ecological Informatics/Reviews V1/Supplementary reviewed/combined_model_results.csv",
  row.names = FALSE
)

write.table(
  model_fit,
  "docs/Publications/07-Forest microclimate/Submission Ecological Informatics/Reviews V1/Supplementary reviewed/combined_model_fit.csv",
  sep = ";",
  row.names = FALSE
)

#---------------------------------
# 6. Formal tests of cross-stratum differences
#---------------------------------
# ANOVA tables
# The three-way interaction terms test whether the effect of energy
# and a structural predictor differs among strata.

anova_results <- list()

for (resp_type in response_types) {
  
  mod <- models_combined[[resp_type]]
  
  anova_results[[resp_type]] <- as.data.frame(
    anova(mod)
  ) %>%
    rownames_to_column("term") %>%
    mutate(
      response_type = resp_type
    )
}

anova_results <- bind_rows(anova_results)

# Filter relevant interaction terms
anova_results = anova_results[str_detect(anova_results$term, ":energy_tot:"),]
anova_results_sign = anova_results[anova_results$'p-value' < 0.05,]

print(anova_results)

write.csv(
  anova_results,
  "data/microclimate_data/results/combined_model_anova.csv",
  row.names = FALSE
)