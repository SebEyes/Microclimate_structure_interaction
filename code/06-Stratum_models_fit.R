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

# Compute totla energy
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
response_vars <- c(
    "GRD_DTR", "GRD_DVPDR",
    "UND_DTR", "UND_DVPDR",
    "CAN_DTR", "CAN_DVPDR",
    "GRD_temperature_mean",
    "GRD_vpd_kPA_mean",
    "UND_temperature_mean",
    "UND_vpd_kPA_mean",
    "CAN_temperature_mean",
    "CAN_vpd_kPA_mean"
)

structural_predictors = c(
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
    mutate(across(all_of(c(structural_predictors)), ~ scale(.)[,1])) %>%
    unique()

# Data selection
data_selected <- dataset_modelling %>%
    dplyr::select(
        all_of(c(response_vars, structural_predictors, "energy_tot")),
        site_code,
        date
    ) %>%
    mutate(
        site_code = factor(site_code),
        date = as.Date(date),
        time_days = as.numeric(date)
    )  %>%
    arrange(site_code, date)

# Ensure one observation per site and day
data_selected <- data_selected %>%
    distinct(site_code, date, .keep_all = TRUE)

## Response distribution
response_distribution <- data_selected %>%
  select(all_of(response_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "response",
    values_to = "value"
  ) %>%
  group_by(response) %>%
  summarise(
    n = sum(!is.na(value)),
    minimum = min(value, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    skewness = e1071::skewness(value, na.rm = TRUE, type = 2),
    proportion_zero =
      mean(value == 0, na.rm = TRUE),
    .groups = "drop"
  )

response_distribution

response_density_plot <- data_selected %>%
  select(all_of(response_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "response",
    values_to = "value"
  ) %>%
  ggplot(aes(value)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "grey75",
    colour = "white"
  ) +
  geom_density(colour = "blue", linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ response, scales = "free") +
  theme_bw() +
  labs(
    x = "Response value",
    y = "Density"
  )

response_density_plot

ggsave(
  "docs/diagnostics/response_distributions.png",
  response_density_plot,
  width = 14,
  height = 10,
  dpi = 300
)


fit_microclimate_model <- function(resp, data) {

    message("Fitting model for: ", resp)

    if(str_detect(resp, "temperature")){

        message("Using Gaussian family ")

        formula_lme <- as.formula(
            paste0(
            resp,
            " ~ energy_tot * (",
            paste(structural_predictors, collapse = " + "),
            ")"
            )
        )

        data$time_days <- as.numeric(difftime(data$date, min(data$date), units = "days"))

        # Extract data used in model
        model_data <- na.omit(data[, unique(c(
            all.vars(formula_lme),
            "site_code",
            "time_days"
        ))])

        # Gaussian LMM with continuous-time CAR(1)
        mod_ar1 <- nlme::lme(
                fixed = formula_lme,
                random = ~ 1 | site_code,
                correlation = corCAR1(
                form = ~ time_days | site_code
                ),
                data = data,
                method = "ML",
                na.action = na.omit,
                control = nlmeControl(returnObject = TRUE)
            )
        
        # Gaussian model without temporal correlation
        mod_ind <- nlme::lme(
                fixed = formula_lme,
                random = ~ 1 | site_code,
                data = data,
                method = "ML",
                na.action = na.omit,
                control = nlmeControl(returnObject = TRUE)
            )



    }else{
        message("Using Gamma family ")

        ## Correct for strictly positive response value
        data_gamma <- select(
            data,
            resp, "energy_tot", "date",
            structural_predictors,
            "site_code"
        ) %>% na.omit()

        data_gamma[,1][data_gamma[,1] == 0] = data_gamma[,1][data_gamma[,1] == 0] + 0.0001

        # Time_days as numeric variable representinf actual elapsed time
        data_gamma$time_days <- glmmTMB::numFactor(
            as.numeric(difftime(data_gamma$date, min(data_gamma$date), units = "days"))
        )

        # Gamma GLMM with continuous-distance decay
        formula_gamma_OU <- as.formula(
            paste0(
            resp,
            " ~ energy_tot * (",
            paste(structural_predictors, collapse = " + "),
            ")",
            "+ (1 | site_code) + ou(time_days + 0 | site_code)"
            )
        )

        mod_ar1 <- glmmTMB(
            formula = formula_gamma_OU,
            family = Gamma(link = "log"),
            data = data_gamma,
            na.action = na.omit
        )
    
        # Gamma GLMM without continuous-distance decay
        formula_gamma_ind <- as.formula(
            paste0(
            resp,
            " ~ energy_tot * (",
            paste(structural_predictors, collapse = " + "),
            ")"
            )
        )

        mod_ind <- glmmTMB(
            formula = formula_gamma_ind,
            family = Gamma(link = "log"),
            data = data_gamma,
            na.action = na.omit
        )

        # Extract data used in model
        model_data <- na.omit(data_gamma[, unique(c(
            all.vars(formula_gamma_OU),
            "site_code",
            "time_days"
        ))])

    }

  # Fixed-effect table
  fixed_tab <- broom.mixed::tidy(
    mod_ar1,
    effects = "fixed",
    conf.int = TRUE
  ) %>%
    mutate(response = resp)

  # Identify solar-energy × predictor interactions
  interaction_tab <- fixed_tab %>%
    filter(grepl("^energy_tot:", term) | grepl(":", term)) %>%
    mutate(
      p_adjusted_BH = NA_real_,
      significant_BH = NA
    )

  # Model fit statistics
  r2_values <- performance::model_performance(mod_ar1)

  fit_tab <- tibble(
    response = resp,
    n = nobs(mod_ar1),
    AIC_CAR1 = AIC(mod_ar1),
    BIC_CAR1 = BIC(mod_ar1),
    logLik_CAR1 = as.numeric(logLik(mod_ar1)),
    R2_marginal = if (!is.null(r2_values)) r2_values$R2_marginal else NA_real_,
    R2_conditional = if (!is.null(r2_values)) r2_values$R2_conditional else NA_real_,
    convergence = mod_ar1$convergence
  )

  # Temporal-correlation comparison
  temporal_comparison <- if (!is.null(mod_ind)) {
    tibble(
      response = resp,
      AIC_independent = AIC(mod_ind),
      AIC_CAR1 = AIC(mod_ar1),
      delta_AIC_CAR1 = AIC(mod_ind) - AIC(mod_ar1),
      likelihood_ratio_p = tryCatch(
        anova(mod_ind, mod_ar1)$`p-value`[2],
        error = function(e) NA_real_
      ),
      estimated_AR1 = tryCatch(
        coef(mod_ar1$modelStruct$corStruct, unconstrained = FALSE),
        error = function(e) NA_real_
      )
    )
  } else {
    tibble(
      response = resp,
      AIC_independent = NA_real_,
      AIC_CAR1 = AIC(mod_ar1),
      delta_AIC_AR1 = NA_real_,
      likelihood_ratio_p = NA_real_,
      estimated_AR1 = NA_real_
    )
  }


  model_data$fitted <- fitted(mod_ar1, level = 0)
  model_data$residual <- residuals(mod_ar1, type = "response")
  model_data$response_value <- model_data[[resp]]

  model_results = list(
    response = resp,
    model = mod_ar1,
    model_independent = mod_ind,
    fixed_effects = fixed_tab,
    interactions = interaction_tab,
    fit = fit_tab,
    temporal_comparison = temporal_comparison,
    model_data = model_data
  )
  model_results
}

model_results <- setNames(
  lapply(response_vars, fit_microclimate_model, data = data_selected),
  response_vars
)

## Save models to RDS file
saveRDS(
    model_results,
    file = "data/stratum_models.rds"
)

## Apply multiplicity correction to interaction terms

all_interactions <- purrr::map_dfr(
  model_results,
  ~ .x$interactions
)

all_interactions <- all_interactions %>%
  mutate(
    p_adjusted_BH = p.adjust(p.value, method = "BH"),
    significant_BH = p_adjusted_BH < 0.05
  )

all_fixed_effects = purrr::map_dfr(
  model_results,
  ~ .x$fixed_effects
)

all_fixed_effects <- all_fixed_effects %>%
  mutate(
    p_adjusted_BH = p.adjust(p.value, method = "BH"),
    significant_BH = p_adjusted_BH < 0.05
  )

write.table(
  all_fixed_effects,
  "data/models_estimates.csv"
)


for (resp in names(model_results)) {

  model_results[[resp]]$interactions <-
    all_interactions %>%
    filter(response == resp)

  model_results[[resp]]$fixed_effects <-
    model_results[[resp]]$fixed_effects %>%
    left_join(
      all_interactions %>%
        select(
          response,
          term,
          p_adjusted_BH,
          significant_BH
        ),
      by = c("response", "term")
    )
}

sig_mod_BH <- all_interactions %>%
  filter(significant_BH)

write.table(
    sig_mod_BH,
    file = "data/significant_interactions.csv",
    sep = ";",
    row.names = F
)


all_fits <- purrr::map_dfr(
  model_results,
  ~ .x$fit
)

write.table(
    all_fits,
    file = "data/stratum_models_fits.csv",
    sep = ";",
    row.names = F
)


#---------------------------------
# 3. Diagnostics
#---------------------------------

## VIF analysis
model_selected = model_results$GRD_temperature_mean$model

vif_results = vif(model_selected) %>% as.data.frame()
vif_results$variable = row.names(vif_results)
vif_results = vif_results %>% dplyr::rename(
  "VIF value" = "."
)

write.table(
  vif_results,
  "docs/supplementary/VIF.csv",
  sep = ";",
  row.names = F
)

# Diagnostic plot
plot_standard_diagnostics <- function(result) {

  dat <- result$model_data
  resp <- result$response

  p1 <- ggplot(dat, aes(x = fitted, y = residual)) +
    geom_point(alpha = 0.25, size = 1) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_smooth(method = "loess", se = FALSE, colour = "red") +
    labs(
      title = paste(resp, "— residuals versus fitted values"),
      x = "Fitted values",
      y = "Normalized residuals"
    ) +
    theme_bw()

  p2 <- ggplot(dat, aes(sample = residual)) +
    stat_qq(alpha = 0.25) +
    stat_qq_line(colour = "red") +
    labs(
      title = paste(resp, "— normal Q-Q plot"),
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    theme_bw()

  p3 <- ggplot(dat, aes(x = fitted, y = abs(residual))) +
    geom_point(alpha = 0.25, size = 1) +
    geom_smooth(method = "loess", se = FALSE, colour = "red") +
    labs(
      title = paste(resp, "— scale-location plot"),
      x = "Fitted values",
      y = "Absolute normalized residuals"
    ) +
    theme_bw()

  p4 <- ggplot(dat, aes(x = time_days, y = residual, group = site_code)) +
    geom_line(alpha = 0.20) +
    geom_point(alpha = 0.15, size = 0.5) +
    geom_hline(yintercept = 0, linetype = 2) +
    facet_wrap(~ site_code, scales = "free_x") +
    labs(
      title = paste(resp, "— residuals through time"),
      x = "Time since beginning of monitoring",
      y = "Normalized residuals"
    ) +
    theme_bw()

  (p1 | p2) / (p3 | p4)
}

diagnostic_plots <- lapply(
  model_results,
  plot_standard_diagnostics
)


for (resp in names(diagnostic_plots)) {
  ggsave(
    filename = file.path(
      "docs/diagnostics",
      paste0(resp, "_standard_diagnostics.png")
    ),
    plot = diagnostic_plots[[resp]],
    width = 14,
    height = 10,
    dpi = 300
  )
}
