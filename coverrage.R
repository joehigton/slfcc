# install.packages(c("tidyverse", "ks"))

library(tidyverse)
library(ks)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(1234)

## ---
## Step 0: Set Simulation Parameters
## ---

N_POPULATION <- 10000    # Size of the entire target population
N_ANNOTATOR  <- 500     # Size of the annotator sample we draw each time
N_TRIALS     <- 500     # Number of trials to run for each R-squared x k level
N_BOOT       <- 500     # Bootstrap reps for CIs

R_SQUARED_LEVELS <- seq(0.1, 0.95, by = 0.05)  # Levels of b(Z) quality to test
K_LEVELS         <- c(0, 0.5, 1, 2)            # Overlap stress: sampling ∝ exp(k * Z1)

# ---
# Step 1: Define the "Universe" and Ground Truth
# ---

# Create a population with two observable characteristics, Z1 and Z2
population <- tibble(
  id = 1:N_POPULATION,
  Z1 = rnorm(N_POPULATION, 0, 1),
  Z2 = rnorm(N_POPULATION, 0, 1)
)

# Define the "true" underlying perspective function b(Z)
true_b_function <- function(z1, z2) {
  0.6 * z1 + 0.8 * sin(z2 * pi) + 0.2 * z1 * z2
}

# Define the true probability of a positive label, given the perspective score
true_outcome_prob <- function(b) {
  # Logistic function
  1 / (1 + exp(-(0.5 * b)))
}

# Calculate the true b(Z) and true outcome probabilities for the entire population
population <- population %>%
  mutate(
    b_true   = true_b_function(Z1, Z2),
    prob_true = true_outcome_prob(b_true)
  )

# This is the ground truth parameter we want our estimator to recover
TRUE_POPULATION_MEAN <- mean(population$prob_true)

# ---
# Step 2: Helper Functions
# ---

# Generate a "predicted" b(Z) vector with a specific R^2 relative to the true b(Z)
generate_predicted_b <- function(b_true, target_r_squared) {
  if (target_r_squared >= 1) return(b_true)
  if (target_r_squared <= 0) return(rnorm(length(b_true), mean(b_true), sd(b_true)))
  error_variance <- var(b_true) * (1 - target_r_squared) / target_r_squared
  noise <- rnorm(length(b_true), 0, sqrt(error_variance))
  b_true + noise
}

# Draw a biased sample of annotators from the population
# Bias is introduced by making people with high Z1 more likely to be sampled via exp(k * Z1)
sample_annotators <- function(data, k_strength) {
  sampling_prob <- exp(k_strength * data$Z1)
  sampling_prob <- sampling_prob / sum(sampling_prob)
  sampled_indices <- sample(data$id, size = N_ANNOTATOR, replace = FALSE, prob = sampling_prob)
  data %>% filter(id %in% sampled_indices)
}

# Calculate importance weights via KDE density ratio on b_pred
calculate_importance_weights <- function(b_pred_annotator, b_pred_population) {
  kde_pop <- ks::kde(b_pred_population)      # density under target population (gamma)
  kde_ann <- ks::kde(b_pred_annotator)       # density under annotator sample (alpha)
  p_gamma <- predict(kde_pop, x = b_pred_annotator)
  p_alpha <- predict(kde_ann, x = b_pred_annotator)
  weights <- p_gamma / p_alpha
  # Clamp weights for stability (keep as in original approach)
  pmin(weights, 10)
}

# Bootstrap CI for (weighted or unweighted) mean
# values: numeric vector of Y
# weights: NULL for unweighted mean, or numeric vector of weights (same length as values)
# Returns: list(estimate=..., se=..., ci_low=..., ci_high=...)
bootstrap_mean_ci <- function(values, weights = NULL, B = N_BOOT, conf = 0.95) {
  n <- length(values)
  stopifnot(length(values) == length(weights) | is.null(weights))
  idx_mat <- matrix(sample.int(n, size = n * B, replace = TRUE), nrow = B)
  boot_est <- numeric(B)
  if (is.null(weights)) {
    # Unweighted
    for (b in 1:B) {
      boot_est[b] <- mean(values[idx_mat[b, ]])
    }
    point <- mean(values)
  } else {
    # Weighted (use resampled rows with their existing weights)
    for (b in 1:B) {
      v  <- values[idx_mat[b, ]]
      w  <- weights[idx_mat[b, ]]
      boot_est[b] <- sum(w * v) / sum(w)
    }
    point <- sum(weights * values) / sum(weights)
  }
  alpha <- (1 - conf) / 2
  ci <- quantile(boot_est, probs = c(alpha, 1 - alpha), names = FALSE)
  list(
    estimate = point,
    se       = sd(boot_est),
    ci_low   = ci[1],
    ci_high  = ci[2]
  )
}

# ---
# Step 3: The Main Simulation Loop
# ---

simulation_results <- list()

for (k_strength in K_LEVELS) {
  for (r_sq in R_SQUARED_LEVELS) {
    message(sprintf("Running trials for k = %s, R^2 = %.2f", k_strength, r_sq))
    
    # For each R^2, generate the corresponding "predicted" b(Z) for the whole population
    population$b_predicted <- generate_predicted_b(population$b_true, r_sq)
    
    trial_results <- vector("list", N_TRIALS)
    
    for (i in 1:N_TRIALS) {
      # 1. Get a biased sample of annotators according to k_strength
      annotator_sample <- sample_annotators(population, k_strength)
      
      # 2. Simulate their labelling process (Bernoulli draws using true prob)
      annotator_sample$Y_label <- rbinom(n = nrow(annotator_sample), size = 1, prob = annotator_sample$prob_true)
      
      # 3. Calculate importance weights based on the predicted b(Z)
      weights <- calculate_importance_weights(
        b_pred_annotator  = annotator_sample$b_predicted,
        b_pred_population = population$b_predicted
      )
      
      # 4a. Weighted estimator with bootstrap CI
      boot_w <- bootstrap_mean_ci(values = annotator_sample$Y_label, weights = weights, B = N_BOOT, conf = 0.95)
      covered_w <- (boot_w$ci_low <= TRUE_POPULATION_MEAN && boot_w$ci_high >= TRUE_POPULATION_MEAN)
      
      # 4b. Unweighted baseline with bootstrap CI
      boot_u <- bootstrap_mean_ci(values = annotator_sample$Y_label, weights = NULL, B = N_BOOT, conf = 0.95)
      covered_u <- (boot_u$ci_low <= TRUE_POPULATION_MEAN && boot_u$ci_high >= TRUE_POPULATION_MEAN)
      
      # 5. Store trial results for both methods
      trial_results[[i]] <- bind_rows(
        tibble(
          trial       = i,
          method      = "Weighted",
          k_strength  = k_strength,
          r_squared   = r_sq,
          estimate    = boot_w$estimate,
          bias        = boot_w$estimate - TRUE_POPULATION_MEAN,
          covered     = covered_w,
          conf_low    = boot_w$ci_low,
          conf_high   = boot_w$ci_high,
          TRUE_POPULATION_MEAN = TRUE_POPULATION_MEAN
        ),
        tibble(
          trial       = i,
          method      = "Unweighted",
          k_strength  = k_strength,
          r_squared   = r_sq,
          estimate    = boot_u$estimate,
          bias        = boot_u$estimate - TRUE_POPULATION_MEAN,
          covered     = covered_u,
          conf_low    = boot_u$ci_low,
          conf_high   = boot_u$ci_high,
          TRUE_POPULATION_MEAN = TRUE_POPULATION_MEAN
        )
      )
    }
    simulation_results[[paste0("k=", k_strength, "_R2=", r_sq)]] <- bind_rows(trial_results)
  }
}

final_results <- bind_rows(simulation_results)

# ---
# Step 4: Aggregate and Plot the Results
# ---

# Summary statistics by method, k, R^2
summary_stats <- final_results %>%
  group_by(method, k_strength, r_squared) %>%
  summarise(
    coverage  = mean(covered),
    mean_bias = mean(bias),
    rmse      = sqrt(mean((estimate - TRUE_POPULATION_MEAN)^2)),
    .groups = "drop"
  )

# Plot: Coverage vs R^2, faceted by k_strength, colored by method
plot_coverage <- ggplot(summary_stats, aes(x = r_squared, y = coverage, colour = method)) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(breaks = R_SQUARED_LEVELS) +
  labs(
    title = "Coverage of 95% CI vs. Quality of b(Z)",
    subtitle = "Faceted by sampling shift strength k in selection ∝ exp(k·Z1)",
    x = "R-squared of learned b(Z)",
    y = "Coverage rate"
  ) +
  facet_wrap(~ k_strength, ncol = 2, labeller = label_bquote(k==.(k_strength))) +
  theme_minimal()

print(plot_coverage)

ggsave("sim_bz_cov.png",width=6,height=6)

# Prepare long form for bias & RMSE
error_long <- summary_stats %>%
  select(method, k_strength, r_squared, mean_bias, rmse) %>%
  pivot_longer(cols = c(mean_bias, rmse), names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, mean_bias = "Bias", rmse = "RMSE"))

# Plot: Error metrics vs R^2, faceted by k_strength, colored by method, linetype by metric
plot_error <- ggplot(error_long, aes(x = r_squared, y = value, colour = method, linetype = metric)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = R_SQUARED_LEVELS) +
  labs(
    title = "Error vs. Quality of b(Z)",
    subtitle = "Faceted by sampling shift strength k in selection ∝ exp(k·Z1)",
    x = "R-squared of learned b(Z)",
    y = "Error"
  ) +
  facet_wrap(~ k_strength, ncol = 2, labeller = label_bquote(k==.(k_strength))) +
  theme_minimal()

print(plot_error)
ggsave("sim_bz_error_bias.png",width=6,height=6)
