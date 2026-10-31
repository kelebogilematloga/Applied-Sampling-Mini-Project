library(survey)
library(dplyr)
library(ggplot2)
library(xtable)
setwd("C:/Users/keleb/Desktop/2026 MS/Applied Sampling")

#Population-structure

strata_info <- data.frame(
  Stratum = 1:4,
  "Settlement type" = c(
    "Urban suburbs", "Townships",
    "Rural villages", "Informal settlements"
  ),
  "Population size Nh" = rep(2500L, 4)
)

print(
  xtable(
    strata_info,
    caption = "Finite-population structure by settlement type.",
    label = "tab:pop_struct",
    align = c("l", "c", "p{4.5cm}", "r")
  ),
  type = "latex",
  comment = FALSE,
  caption.placement = "bottom",
  booktabs = TRUE,
  include.rownames = FALSE,
  table.placement = "!ht"
)

#sample-size-calculation

N_h <- rep(2500,4) 
mu_h <- c(18, 30, 26, 14) 
sd_h <- c(8, 12, 11, 7) 
H <- length(N_h) 
N <- sum(N_h) 
W_h <- N_h / N 
planning_mean <- sum(W_h * mu_h) 
confidence_level <- 0.95 
z_value <- qnorm(1 - (1 - confidence_level) / 2) 
relative_precision <- 0.05 
target_half_width <- relative_precision * planning_mean 
nonresponse_rate <- 0.10 

stratified_variance <- function(n_total, N_h, sd_h) { 
  H <- length(N_h) 
  if (n_total %% H != 0) return(Inf) 
  n_h <- rep(n_total / H, H) 
  if (any(n_h <= 0) || any(n_h > N_h)) return(Inf) 
  W_h <- N_h / sum(N_h) 
  sum(W_h^2 * (1 - n_h / N_h) * sd_h^2 / n_h) 
} 

candidate_n <- seq(H, min(N_h) * H, by = H) 
candidate_half_width <- vapply( 
  candidate_n, 
  function(n) z_value * sqrt(stratified_variance(n, N_h, sd_h)), 
  numeric(1) 
) 
net_n <- candidate_n[which(candidate_half_width <= target_half_width)[1]] 
n_h_net <- rep(net_n / H, H) 

gross_n_raw <- ceiling(net_n / (1 - nonresponse_rate)) 
gross_n <- ceiling(gross_n_raw / H) * H 
n_h_gross <- rep(gross_n / H, H) 

sample_size_summary <- data.frame( 
  Quantity = c( 
    "Planning mean (hours)", "Target half-width (hours)", 
    "Required net sample size", "Net sample per stratum", 
    "Anticipated non-response rate", "Gross sample size", 
    "Gross sample per stratum" 
  ), 
  Value = c( 
    planning_mean, target_half_width, net_n, net_n / H, 
    nonresponse_rate, gross_n, gross_n / H 
  ) 
) 

print(
  xtable(
    sample_size_summary,
    caption = "Sample-size calculation using the planning assumptions.",
    label = "tab:sample_size",
    digits = 2,
    align = c("l", "p{6.5cm}", "r")
  ),
  type = "latex",
  comment = FALSE,
  caption.placement = "bottom",
  booktabs = TRUE,
  include.rownames = FALSE,
  table.placement = "!ht"
)


#Precision-sample-size-plot

#Continuous range of relative precision values
precision_grid <- seq(0.00, 0.10, length.out = 200)

#Required net sample size for each precision level
required_net_n <- vapply(precision_grid, function(p) {
  
  target_width <- p * planning_mean
  
  eligible_n <- candidate_n[
    candidate_half_width <= target_width
  ]
  
  if (length(eligible_n) == 0) {
    return(NA_real_)
  }
  
  min(eligible_n)
  
}, numeric(1))

#Prepare plotting data
precision_plot_data <- data.frame(
  Relative_Precision = precision_grid * 100,
  Required_Sample_Size = required_net_n
)

#Plot the relationship
ggplot(
  precision_plot_data,
  aes(x = Relative_Precision, y = Required_Sample_Size)
) +
  geom_line(linewidth = 0.8) +
  geom_vline(
    xintercept = relative_precision * 100,
    colour = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    x = "Relative precision (%)",
    y = "Required net sample size"
  ) +
  scale_x_continuous(
    breaks = seq(1, 10, by = 1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(2, 4, 2, 4)
  )

#Sensitivity-analysis

planning_mean <- 22

mean_fractions <- c(0.25, 0.50, 0.75, 1.00)
relative_margins <- seq(0.02, 0.10, by = 0.005)

sensitivity_data <- expand.grid(
  fraction = mean_fractions,
  relative_margin = relative_margins
) %>%
  mutate(
    assumed_mean = fraction * planning_mean,
    target_d = relative_margin * assumed_mean
  )

sensitivity_data$required_n <- vapply(
  seq_len(nrow(sensitivity_data)),
  function(j) {
    
    d <- sensitivity_data$target_d[j]
    
    ok <- candidate_n[
      vapply(
        candidate_n,
        function(n) {
          z_value * sqrt(
            stratified_variance(n, N_h, sd_h)
          ) <= d
        },
        logical(1)
      )
    ]
    
    if (length(ok) == 0) NA_real_ else min(ok)
  },
  numeric(1)
)

sensitivity_data$scenario <- factor(
  paste0(sensitivity_data$fraction * 100, "% of baseline"),
  levels = paste0(mean_fractions * 100, "% of baseline")
)

ggplot(
  sensitivity_data,
  aes(
    x = relative_margin * 100,
    y = required_n,
    colour = scenario
  )
) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 1.3, na.rm = TRUE) +
  labs(
    x = "Confidence-interval half-width (% of assumed mean)",
    y = "Required net sample size",
    colour = "Assumed mean"
  ) +
  scale_x_continuous(
    breaks = seq(2, 10, by = 1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = margin(2, 4, 2, 4)
  )

#Generating population
set.seed(2554365)


# Number of strata 
H <- 4 

# Population sizes for each stratum 
N_h <- c(2500, 2500, 2500, 2500) 

# Settlement types 
strata_info <- data.frame( 
  stratum = 1:4, 
  settlement_type = c( 
    "Urban suburbs", 
    "Townships", 
    "Rural villages", 
    "Informal settlements" 
  ), 
  Nh = N_h 
) 

# Mean monthly outage hours for each stratum 
mu_h <- c(16, 32, 25, 12) 

# Standard deviations for each stratum 
sd_h <- c(sqrt(32), sqrt(80), sqrt(69.4), sqrt(24)) 

population <- bind_rows(lapply(seq_len(H), function(h) { 
  shape_h <- mu_h[h]^2 / sd_h[h]^2 
  scale_h <- sd_h[h]^2 / mu_h[h] 
  Nh <- N_h[h] 
  
  data.frame( 
    household_id = sprintf("HH%05d", sum(N_h[seq_len(h - 1)]) + seq_len(Nh)), 
    stratum = h, 
    settlement_type = strata_info$settlement_type[h], 
    grid_zone = paste0("Z", h, "_", sample(1:25, Nh, replace = TRUE)), 
    household_size = pmax(1L, rpois(Nh, lambda = 3) + 1L), 
    backup_power = rbinom( 
      Nh, size = 1, 
      prob = c(0.45, 0.20, 0.12, 0.08)[h] 
    ), 
    outage_hours = rgamma(Nh, shape = shape_h, scale = scale_h) 
  ) 
})) 

stopifnot( 
  nrow(population) == 10000, 
  !anyDuplicated(population$household_id), 
  all(population$outage_hours >= 0), 
  all(table(population$stratum) == 2500) 
) 

true_mean <- mean(population$outage_hours) 
true_total <- sum(population$outage_hours) 

population_summary <- population %>% 
  group_by(stratum, settlement_type) %>% 
  summarise( 
    Nh = n(), 
    finite_mean = mean(outage_hours), 
    finite_sd = sd(outage_hours), 
    finite_variance = var(outage_hours), 
    .groups = "drop" 
  ) 

knitr::kable( 
  population_summary, digits = 3, 
  caption = "Generated finite-population characteristics by stratum." 
) 

cat( 
  "True finite-population mean:", round(true_mean, 3), "hours\n\n", 
  "True finite-population total:", round(true_total, 2), "household-hours\n" 
) 

write.csv(population, "population.csv", row.names = FALSE) 


#Drawing-realised-sample
set.seed(2554365) 

selected_sample <- population %>% 
  group_by(stratum) %>% 
  slice_sample(n = gross_n / H, replace = FALSE) %>% 
  ungroup() %>% 
  group_by(stratum) %>% 
  mutate( 
    Nh = nrow(population) / H, 
    nh_gross = n(), 
    inclusion_probability = nh_gross / Nh, 
    base_weight = 1 / inclusion_probability, 
    respondent = rbinom(n(), size = 1, prob = 1 - nonresponse_rate) 
  ) %>% 
  ungroup() 

response_counts <- selected_sample %>% 
  group_by(stratum, settlement_type) %>% 
  summarise( 
    gross_selected = n(), 
    respondents = sum(respondent), 
    response_rate = mean(respondent), 
    .groups = "drop" 
  ) 

selected_sample <- selected_sample %>% 
  group_by(stratum) %>% 
  mutate( 
    responding_n = sum(respondent), 
    final_weight = ifelse( 
      respondent == 1, 
      (nrow(population) / H) / responding_n, 
      NA_real_ 
    ), 
    outage_hours_observed = ifelse( 
      respondent == 1, outage_hours, NA_real_ 
    ) 
  ) %>% 
  ungroup() 

knitr::kable( 
  response_counts, digits = 3, 
  caption = "Gross sample and realised response by stratum." 
) 

write.csv(selected_sample, "sample.csv", row.names = FALSE) 


#Survey-design-object
respondents <- selected_sample %>%
  filter(respondent == 1, !is.na(outage_hours_observed))

strat_design <- svydesign(
  ids = ~1,
  strata = ~stratum,
  weights = ~final_weight,
  fpc = ~Nh,
  data = respondents,
  nest = TRUE
)

#Stratified-estimation

strat_mean <- svymean(~outage_hours_observed, strat_design, deff = TRUE)
strat_estimate <- as.numeric(coef(strat_mean)[1])
strat_se <- as.numeric(SE(strat_mean)[1])
strat_variance <- strat_se^2
strat_ci <- as.numeric(confint(strat_mean)[1, ])
strat_cv <- 100 * strat_se / abs(strat_estimate)
strat_deff <- tryCatch(
  as.numeric(deff(strat_mean)[1]),
  error = function(e) NA_real_
)

strat_results_vertical <- data.frame(
  Metric = c(
    "Sampling Design",
    "Point Estimate (hours)",
    "Estimated Variance",
    "Standard Error",
    "95% CI Lower Bound",
    "95% CI Upper Bound",
    "Coefficient of Variation (%)",
    "Design Effect (DEFF)",
    "CI Half-Width (hours)"
  ),
  Value = c(
    "Stratified SRSWOR",
    round(strat_estimate, 4),
    round(strat_variance, 4),
    round(strat_se, 4),
    round(strat_ci[1], 4),
    round(strat_ci[2], 4),
    round(strat_cv, 4),
    round(strat_deff, 4),
    round((strat_ci[2] - strat_ci[1]) / 2, 4)
  )
)

# Print using xtable with bottom caption placement
print(
  xtable(
    strat_results_vertical, 
    caption = "Design-based estimates for the realised stratified sample.", 
    label = "tab:strat_est",
    align = c("l", "p{5.5cm}", "r")
  ),
  type = "latex",
  comment = FALSE,
  caption.placement = "bottom",
  booktabs = TRUE,
  include.rownames = FALSE,
  table.placement = "!ht"
)


#Srs-benchmark

set.seed(2554365)

strat_results <- data.frame(
  Design = "Stratified SRSWOR",
  Estimate = strat_estimate,
  Estimated_variance = strat_variance,
  Standard_error = strat_se,
  CI_lower = strat_ci[1],
  CI_upper = strat_ci[2],
  CV_percent = strat_cv,
  Design_effect = strat_deff,
  CI_half_width = (strat_ci[2] - strat_ci[1]) / 2
)

srs_sample <- population %>%
  slice_sample(n = gross_n, replace = FALSE) %>%
  mutate(
    Nh_total = N,
    inclusion_probability = gross_n / N,
    base_weight = 1 / inclusion_probability,
    respondent = rbinom(n(), size = 1, prob = 1 - nonresponse_rate)
  )

srs_respondents <- srs_sample %>%
  filter(respondent == 1) %>%
  mutate(
    final_weight = N / n(),
    outage_hours_observed = outage_hours
  )

srs_design <- svydesign(
  ids = ~1,
  weights = ~final_weight,
  fpc = ~Nh_total,
  data = srs_respondents
)

srs_mean <- svymean(~outage_hours_observed, srs_design, deff = TRUE)
srs_estimate <- as.numeric(coef(srs_mean)[1])
srs_se <- as.numeric(SE(srs_mean)[1])
srs_ci <- as.numeric(confint(srs_mean)[1, ])
srs_deff <- tryCatch(
  as.numeric(deff(srs_mean)[1]),
  error = function(e) NA_real_
)

srs_results <- data.frame(
  Design = "SRSWOR benchmark",
  Estimate = srs_estimate,
  Estimated_variance = srs_se^2,
  Standard_error = srs_se,
  CI_lower = srs_ci[1],
  CI_upper = srs_ci[2],
  CV_percent = 100 * srs_se / abs(srs_estimate),
  Design_effect = srs_deff,
  CI_half_width = (srs_ci[2] - srs_ci[1]) / 2
)

naive_strat_mean <- mean(respondents$outage_hours_observed)
naive_srs_mean <- mean(srs_respondents$outage_hours_observed)

comparison_realised <- bind_rows(strat_results, srs_results) %>%
  mutate(
    True_finite_population_mean = true_mean,
    Difference_from_truth = Estimate - true_mean,
    Unweighted_respondent_mean = c(naive_strat_mean, naive_srs_mean)
  )

# Transpose the horizontal comparison table into a 
#vertical 3-column format for good presentation
strat_vals <- unlist(comparison_realised[1, -1])
srs_vals <- unlist(comparison_realised[2, -1])

vertical_comparison <- data.frame(
  Metric = c(
    "Point Estimate (hours)",
    "Estimated Variance",
    "Standard Error",
    "95% CI Lower Bound",
    "95% CI Upper Bound",
    "Coefficient of Variation (%)",
    "Design Effect (DEFF)",
    "CI Half-Width (hours)",
    "True Population Mean (hours)",
    "Difference from Truth",
    "Unweighted Respondent Mean"
  ),
  "Stratified SRSWOR" = as.numeric(strat_vals),
  "SRSWOR benchmark" = as.numeric(srs_vals)
)

print(
  xtable(
    vertical_comparison,
    caption = "Realised-sample comparison with the true finite-population mean.",
    label = "tab:srs_benchmark",
    digits = 4,
    align = c("l", "p{5.5cm}", "r", "r")
  ),
  type = "latex",
  comment = FALSE,
  caption.placement = "bottom",
  booktabs = TRUE,
  include.rownames = FALSE,
  table.placement = "!ht"
)

#Weights-summary

weight_summary <- selected_sample %>%
  group_by(stratum, settlement_type) %>%
  summarise(
    Population_size = first(Nh),
    Gross_sample = n(),
    Respondents = sum(respondent),
    Inclusion_probability = first(inclusion_probability),
    Base_weight = first(base_weight),
    Final_weight_for_respondents = first(final_weight[respondent == 1]),
    .groups = "drop"
  )

# Transpose the table so metrics are rows and settlement strata are columns
#for presentation
metrics_mat <- as.matrix(weight_summary[, c(
  "Population_size", "Gross_sample", "Respondents", 
  "Inclusion_probability", "Base_weight", "Final_weight_for_respondents"
)])
t_mat <- t(metrics_mat)
colnames(t_mat) <- weight_summary$settlement_type

weights_vertical <- data.frame(
  Metric = c(
    "Population Size",
    "Gross Sample",
    "Respondents",
    "Inclusion Probability",
    "Base Weight",
    "Final Weight"
  ),
  check.names = FALSE
)
weights_vertical <- cbind(weights_vertical, as.data.frame(t_mat))

print(
  xtable(
    weights_vertical,
    caption = "Inclusion probabilities and sampling weights by stratum.",
    label = "tab:weights_summary",
    digits = 4,
    align = c("l", "p{4.2cm}", "r", "r", "r", "r")
  ),
  type = "latex",
  comment = FALSE,
  caption.placement = "bottom",
  booktabs = TRUE,
  include.rownames = FALSE,
  table.placement = "!ht"
)

#Precision-check

precision_check <- data.frame(
  Design = c("Stratified SRSWOR", "SRSWOR benchmark"),
  Estimated_half_width = c(
    strat_results$CI_half_width,
    srs_results$CI_half_width
  ),
  Required_half_width = target_half_width
) %>%
  mutate(
    Meets_target = Estimated_half_width <= Required_half_width
  )

print(
  xtable(
    precision_check,
    caption = "Assessment of the required confidence-interval half-width.",
    label = "tab:precision_check",
    digits = 4,
    align = c("l", "p{4.5cm}", "r", "r", "c")
  ),
  type = "latex",
  comment = FALSE,
  caption.placement = "bottom",
  booktabs = TRUE,
  include.rownames = FALSE,
  table.placement = "H"
)


#simulation-functions
one_stratified_rep <- function(pop, seed) {
  set.seed(seed)
  
  ss <- pop %>%
    group_by(stratum) %>%
    slice_sample(n = gross_n / H, replace = FALSE) %>%
    ungroup() %>%
    mutate(respondent = rbinom(n(), 1, 1 - nonresponse_rate))
  
  ss_resp <- ss %>%
    filter(respondent == 1) %>%
    group_by(stratum) %>%
    mutate(
      Nh = N_h[first(stratum)],
      final_weight = Nh / n()
    ) %>%
    ungroup()
  
  if (n_distinct(ss_resp$stratum) != H) return(NULL)
  
  des <- svydesign(
    ids = ~1, strata = ~stratum, weights = ~final_weight,
    fpc = ~Nh, data = ss_resp, nest = TRUE
  )
  
  est <- svymean(~outage_hours, des)
  ci <- as.numeric(confint(est)[1, ])
  
  data.frame(
    estimate = as.numeric(coef(est)[1]),
    se = as.numeric(SE(est)[1]),
    lower = ci[1],
    upper = ci[2]
  )
}

one_srs_rep <- function(pop, seed) {
  set.seed(seed)
  
  ss <- pop %>%
    slice_sample(n = gross_n, replace = FALSE) %>%
    mutate(respondent = rbinom(n(), 1, 1 - nonresponse_rate)) %>%
    filter(respondent == 1)
  
  if (nrow(ss) == 0) return(NULL)
  
  ss <- ss %>%
    mutate(
      N_total = N,
      final_weight = N / nrow(ss)
    )
  
  des <- svydesign(
    ids = ~1, weights = ~final_weight,
    fpc = ~N_total, data = ss
  )
  
  est <- svymean(~outage_hours, des)
  ci <- as.numeric(confint(est)[1, ])
  
  data.frame(
    estimate = as.numeric(coef(est)[1]),
    se = as.numeric(SE(est)[1]),
    lower = ci[1],
    upper = ci[2]
  )
}

#Running-simulation
R <- 1000
simulation_seeds <- 50000 + seq_len(R)

strat_sim <- bind_rows(lapply(
  simulation_seeds,
  function(s) {
    ans <- one_stratified_rep(population, s)
    if (is.null(ans)) {
      repeat {
        s <- s + 100000
        ans <- one_stratified_rep(population, s)
        if (!is.null(ans)) break
      }
    }
    ans
  }
))

srs_sim <- bind_rows(lapply(
  simulation_seeds,
  function(s) {
    ans <- one_srs_rep(population, s + 200000)
    if (is.null(ans)) {
      repeat {
        s <- s + 100000
        ans <- one_srs_rep(population, s + 200000)
        if (!is.null(ans)) break
      }
    }
    ans
  }
))

stopifnot(nrow(strat_sim) == R, nrow(srs_sim) == R)

#Simulation-summary

summarise_performance <- function(results, design_name, truth) {
  bias <- mean(results$estimate - truth)
  coverage <- mean(results$lower <= truth & results$upper >= truth)
  
  data.frame(
    Design = design_name,
    Bias = bias,
    Relative_bias_percent = 100 * bias / truth,
    Empirical_SE = sd(results$estimate),
    Average_estimated_SE = mean(results$se),
    RMSE = sqrt(mean((results$estimate - truth)^2)),
    Coverage_percent = 100 * coverage,
    Coverage_MCSE_percent = 100 * sqrt(coverage * (1 - coverage) / nrow(results)),
    Average_CI_width = mean(results$upper - results$lower)
  )
}

simulation_summary <- bind_rows(
  summarise_performance(strat_sim, "Stratified SRSWOR", true_mean),
  summarise_performance(srs_sim, "SRSWOR benchmark", true_mean)
)

empirical_deff <- var(strat_sim$estimate) / var(srs_sim$estimate)
simulation_summary$Empirical_DEFF_relative_to_SRS <- c(empirical_deff, 1)

# Transpose into a vertical 3-column format (Metric, Stratified, SRSWOR)
strat_vals <- unlist(simulation_summary[1, -1])
srs_vals <- unlist(simulation_summary[2, -1])

vertical_sim_summary <- data.frame(
  Metric = c(
    "Bias",
    "Relative Bias (%)",
    "Empirical SE",
    "Average Estimated SE",
    "RMSE",
    "Coverage (%)",
    "Coverage MCSE (%)",
    "Average CI Width",
    "Empirical DEFF (rel. to SRS)"
  ),
  "Stratified SRSWOR" = as.numeric(strat_vals),
  "SRSWOR benchmark" = as.numeric(srs_vals),
  check.names = FALSE
)

print(
  xtable(
    vertical_sim_summary,
    caption = "Monte Carlo performance measures based on 1,000 repetitions.",
    label = "tab:sim_summary",
    digits = 4,
    align = c("l", "p{5.5cm}", "r", "r")
  ),
  type = "latex",
  comment = FALSE,
  caption.placement = "bottom",
  booktabs = TRUE,
  include.rownames = FALSE,
  table.placement = "H"
)

#simulation-estimate-distributions
simulation_plot_data <- bind_rows(
  strat_sim %>% mutate(Design = "Stratified SRSWOR"),
  srs_sim %>% mutate(Design = "SRSWOR benchmark")
)

ggplot(simulation_plot_data, aes(x = estimate, fill = Design)) +
  geom_density(alpha = 0.35) +
  geom_vline(xintercept = true_mean, linetype = "dashed", linewidth = 0.8) +
  labs(
    x = "Estimated mean monthly outage duration (hours)",
    y = "Density",
    fill = "Sampling design"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold")
  )