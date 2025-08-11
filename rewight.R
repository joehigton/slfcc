# ---- Packages ----
library(tidyverse)
library(haven)
library(janitor)
library(rsample)

set.seed(12)

# ---- User settings ----
DATA_PATH <- "prolific_data.dta"

# Oversampling strength for men in the biased annotator pool:
# odds multiplier = exp(k). e.g., k=0 -> no bias, k=1 -> ~2.7x odds
K_OVERSAMPLE <- 1.0

# How many annotators to keep in the biased pool (from the eval split)?
# Set to a proportion of eval annotators to ensure bias is created
N_ANNOTATORS_BIASED_PROP <- 0.6  # Use 60% of eval annotators

# Minimum labels per image required in the biased pool to keep the image in evaluation
MIN_LABELS_PER_IMAGE <- 3

# Bootstrap reps for clustered CIs (clustered by annotator)
N_BOOT <- 300

# ---- 1) Load & prep data ----
df_raw <- read_dta(DATA_PATH) %>%
  janitor::clean_names() %>% filter(grepl("Night",var_label))

df <- df_raw %>%
  transmute(
    response_id = as.character(response_id),
    img         = as.character(img),
    value       = as.numeric(value),
    gender      = factor(gender),
    race        = factor(race),
    income      = factor(income, ordered = is.ordered(income)),
    educ        = factor(educ,  ordered = is.ordered(educ)),
    party       = factor(party)
  ) %>%
  drop_na(response_id, img, value, gender, race, income, educ, party)

# sanity: assume each annotator's Z is constant
annotator_demo <- df %>%
  group_by(response_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(response_id, gender, race, income, educ, party)

# ---- 2) Pseudo-population target (truth) ----
# Treat the full, observed composition as the target. Compute per-image means.
pop_img_truth <- df %>%
  group_by(img) %>%
  summarise(mu_pop = mean(value), .groups = "drop")

# ---- 3) Split annotators to avoid leakage (train_b vs eval) ----
annotator_split <- initial_split(annotator_demo, prop = 0.7, strata = gender)
annotators_train_b <- training(annotator_split)
annotators_eval    <- testing(annotator_split)

df_train_b <- df %>% semi_join(annotators_train_b, by = "response_id")
df_eval    <- df %>% semi_join(annotators_eval,    by = "response_id")

# ---- 4) Learn b-hat(Z) on training annotators only ----
# Simple linear model: value ~ demographics (no image features)
# (You can switch to glm or an ordered model if value is bounded/ordinal.)
# b_model <- lm(
#   value ~ gender + race + income + educ + party,
#   data = df_train_b
# )
# Create a per-annotator \hat{b}(Z): the model's prediction from demographics only
# annotators_all <- annotator_demo %>%
#   mutate(b_hat = predict(b_model, newdata = annotator_demo)) %>%
#   mutate(b_hat = as.numeric(scale(b_hat)))  # scale for stable KDE


library(lme4)
b_model <- lmer(value ~ educ + income + race + party + gender + (1 | img),
                data = df_train_b)
# Step 2: Extract demographic effects only (not image effects)
annotators_all <- annotator_demo %>%
  mutate(b_hat = predict(b_model, newdata = annotator_demo, re.form = NA))  # re.form=NA excludes random effects


# Attach \hat{b}(Z) to every label row
df <- df %>%
  left_join(annotators_all %>% select(response_id, b_hat), by = "response_id")

df_eval <- df_eval %>%
  left_join(annotators_all %>% select(response_id, b_hat), by = "response_id")

# ---- 5) Construct a biased annotator pool (oversample men) ----
eval_annotators <- annotators_eval %>%
  left_join(annotators_all %>% select(response_id, b_hat), by = "response_id")

# Sampling weights ∝ exp(k * 1[gender == "male"])
is_male <- eval_annotators$gender == levels(eval_annotators$gender)[which(levels(eval_annotators$gender) %in% c("male","Male","M","man","Men"))[1]]
is_male[is.na(is_male)] <- FALSE
w_eval <- exp(K_OVERSAMPLE * as.numeric(is_male))
w_eval <- w_eval / sum(w_eval)

# Calculate N_ANNOTATORS_BIASED to ensure we create bias
N_ANNOTATORS_BIASED <- floor(N_ANNOTATORS_BIASED_PROP * nrow(eval_annotators))
cat("Total eval annotators:", nrow(eval_annotators), "\n")
cat("Biased pool size:", N_ANNOTATORS_BIASED, "\n")

n_pick <- min(N_ANNOTATORS_BIASED, nrow(eval_annotators))
biased_ids <- sample(eval_annotators$response_id, size = n_pick, replace = FALSE, prob = w_eval)

annotators_biased <- eval_annotators %>% filter(response_id %in% biased_ids)
df_biased <- df_eval %>% semi_join(annotators_biased, by = "response_id")

# Check the bias we created
cat("Original eval male proportion:", mean(is_male), "\n")
is_male_biased <- annotators_biased$gender == levels(annotators_biased$gender)[which(levels(annotators_biased$gender) %in% c("male","Male","M","man","Men"))[1]]
is_male_biased[is.na(is_male_biased)] <- FALSE
cat("Biased pool male proportion:", mean(is_male_biased), "\n")

# ---- Ensure enough labels per image in the biased pool ----
imgs_keep <- df_biased %>%
  count(img, name = "n_labels") %>%
  filter(n_labels >= MIN_LABELS_PER_IMAGE) %>%
  pull(img)

df_biased <- df_biased %>% filter(img %in% imgs_keep)

# ---- 6) Density-ratio weights w(b) = p_gamma(b)/p_alpha(b) using 1D KDE on b_hat ----
# gamma: all annotators (population composition), alpha: biased annotator pool
b_pop   <- annotators_all$b_hat
b_alpha <- annotators_biased$b_hat

# KDE via base::density (1D)
dens_pop   <- density(b_pop,   n = 2048)
dens_alpha <- density(b_alpha, n = 2048)

# helper: interpolate density at points
interp_density <- function(dens, x) approx(x = dens$x, y = dens$y, xout = x, rule = 2)$y

p_gamma <- interp_density(dens_pop,   b_alpha)
p_alpha <- interp_density(dens_alpha, b_alpha)

w_annotator <- p_gamma / p_alpha
# attach stable, truncated weights to annotators in biased pool
w_annotator <- pmin(w_annotator, 10)

annotators_biased <- annotators_biased %>%
  mutate(w = w_annotator)

# bring weights to label rows (constant per annotator)
df_biased <- df_biased %>%
  left_join(annotators_biased %>% select(response_id, w), by = "response_id")

# ---- 7) Per-image estimates: unweighted vs reweighted ----
# Unweighted per image
est_unw_img <- df_biased %>%
  group_by(img) %>%
  summarise(mu_hat_unw = mean(value), .groups = "drop")

# Weighted per image (weights constant within annotator)
est_w_img <- df_biased %>%
  group_by(img) %>%
  summarise(mu_hat_w = sum(w * value) / sum(w), .groups = "drop")

# Merge with truth
eval_table <- pop_img_truth %>%
  semi_join(est_unw_img, by = "img") %>%
  left_join(est_unw_img, by = "img") %>%
  left_join(est_w_img,   by = "img")

# ---- 8) Clustered bootstrap CIs (cluster = response_id) ----
cluster_boot_img <- function(dat_img, B = N_BOOT, weighted = FALSE) {
  # dat_img: rows for a single image; columns: value, response_id, w (if weighted)
  ids <- unique(dat_img$response_id)
  R <- length(ids)
  boot_stat <- numeric(B)
  for (b in seq_len(B)) {
    # resample annotators with replacement, include all their labels
    boot_ids <- sample(ids, size = R, replace = TRUE)
    
    # Create lookup table that preserves multiplicities
    boot_lookup <- tibble(
      response_id = boot_ids,
      boot_index = seq_along(boot_ids)
    )
    
    # Join to get all labels, preserving multiplicities
    boot_dat <- boot_lookup %>%
      left_join(dat_img, by = "response_id", relationship = "many-to-many")
    
    if (!weighted) {
      boot_stat[b] <- mean(boot_dat$value)
    } else {
      boot_stat[b] <- sum(boot_dat$w * boot_dat$value) / sum(boot_dat$w)
    }
  }
  c(
    est = if (!weighted) mean(dat_img$value) else sum(dat_img$w * dat_img$value) / sum(dat_img$w),
    lo  = quantile(boot_stat, 0.025, names = FALSE),
    hi  = quantile(boot_stat, 0.975, names = FALSE)
  )
}

# Modified bootstrap section - only create CI columns
per_img_ci <- df_biased %>%
  group_split(img) %>%
  map_dfr(function(d) {
    img_id <- unique(d$img)
    u <- cluster_boot_img(d, B = N_BOOT, weighted = FALSE)
    w <- cluster_boot_img(d, B = N_BOOT, weighted = TRUE)
    tibble(
      img = img_id,
      # Don't include mu_hat estimates here since they're already in eval_table
      ci_lo_unw = u["lo"], ci_hi_unw = u["hi"],
      ci_lo_w   = w["lo"], ci_hi_w   = w["hi"]
    )
  })

# Now the join won't create duplicate columns
eval_table <- eval_table %>%
  left_join(per_img_ci, by = "img")

# And your original code will work
results_img <- eval_table %>%
  mutate(
    bias_unw = mu_hat_unw - mu_pop,
    bias_w   = mu_hat_w   - mu_pop,
    covered_unw = (ci_lo_unw <= mu_pop & mu_pop <= ci_hi_unw),
    covered_w   = (ci_lo_w   <= mu_pop & mu_pop <= ci_hi_w)
  )

summary_metrics <- tibble(
  method   = c("Unweighted", "Reweighted"),
  bias     = c(mean(results_img$bias_unw, na.rm = TRUE),
               mean(results_img$bias_w,   na.rm = TRUE)),
  rmse     = c(sqrt(mean((results_img$bias_unw)^2, na.rm = TRUE)),
               sqrt(mean((results_img$bias_w)^2,   na.rm = TRUE))),
  coverage = c(mean(results_img$covered_unw, na.rm = TRUE),
               mean(results_img$covered_w,   na.rm = TRUE))
)

print(summary_metrics)

# ---- 10) Quick diagnostics & plots ----
# Effective sample size under weights (approx, by annotator)
ess <- with(annotators_biased, (sum(w)^2) / sum(w^2))
cat(sprintf("\nApprox. effective number of annotators under weights: %.1f (of %d)\n",
            ess, nrow(annotators_biased)))

# Compare b-hat distributions (population vs biased pool)
ggplot(
  bind_rows(
    tibble(source = "Population (all annotators)", b_hat = annotators_all$b_hat),
    tibble(source = "Biased pool (eval)",          b_hat = annotators_biased$b_hat)
  ),
  aes(x = b_hat, colour = source, fill = source)
) +
  geom_density(alpha = 0.15) +
  labs(title = "b-hat distribution: population vs biased annotator pool",
       x = "b-hat (scaled)", y = "Density") +
  theme_minimal()

# Per-image scatter: estimate vs truth
results_img %>%
  select(img, mu_pop, mu_hat_unw, mu_hat_w) %>%
  pivot_longer(cols = starts_with("mu_hat"),
               names_to = "method", values_to = "estimate") %>%
  mutate(method = recode(method,
                         mu_hat_unw = "Unweighted",
                         mu_hat_w = "Reweighted")) %>%
  ggplot(aes(x = mu_pop, y = estimate, colour = method)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7) +
  labs(title = "Per-image estimates vs pseudo-population truth",
       x = "Truth (full data mean)", y = "Estimator") +
  theme_minimal()
