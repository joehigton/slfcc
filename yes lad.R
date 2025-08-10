# Install packages if you haven't already
# install.packages(c("tidyverse", "lme4", "performance"))

library(tidyverse)
library(lme4)
library(performance)

# mm models ####

## ---
## Step 1: Load the Data
## ---
message("Step 1: Loading the evaluation data from Python...")

eval_data <- read_csv("model_data_with_bz_scores.csv")

## ---
## Step 2: (SKIPPED) No PCA This Time
## ---
# message("Step 2: SKIPPING PCA...")


## ---
## Step 3: Fit the Mixed-Effects Model with All b(Z) Dimensions
## ---
message("Step 3: Fitting the mixed-effects model with all 32 b(Z) dimensions...")

# Programmatically create the formula for the lmer model.
# This is cleaner than writing out "bz_1 + bz_2 + ... + bz_32".

# First, get the names of all the b(Z) columns
bz_col_names <- names(eval_data %>% select(starts_with("bz_")))

# Then, paste them together with "+" signs
fixed_effects_formula <- paste(bz_col_names, collapse = " + ")

# Finally, construct the full model formula
model_formula_str <- paste("value ~", fixed_effects_formula, "+ (1 | image_file)")
model_formula <- as.formula(model_formula_str)

# Now, fit the model using our dynamically created formula
lmer_model_full <- lmer(model_formula, data = eval_data)

summary(lmer_model_full)


## ---
## Step 4: Calculate and Interpret the R-squared
## ---
message("Step 4: Calculating the final R-squared...")

# Use the performance package to get both marginal and conditional R-squared
r2_results <- r2(lmer_model_full)

message("\n--- FINAL RESULTS (ALL b(Z) DIMENSIONS) ---")
print(r2_results)

message(paste0(
  "\n\n--- INTERPRETATION ---",
  "\nThe Marginal R-squared (R2m) of ", round(r2_results$R2_marginal, 3), 
  " represents the proportion of variance in the ratings explained by the FULL 32-dimensional b(Z) perspective space.",
  "\n\nThis is the key number. Compare it to the ~1-2% variance explained by your original ANOVA on the raw demographic groups."
))


# original for comp ####
# Install packages if you haven't already
# install.packages(c("tidyverse", "lme4", "performance"))

library(tidyverse)
library(lme4)
library(performance)

## ---
## Step 1: Load the Data
## ---
message("Step 1: Loading the evaluation data...")

# This CSV contains all the columns we need
eval_data <- read_csv("model_data_with_bz_scores.csv")


## ---
## Step 2: Fit the Mixed-Effects Model with Raw Demographics
## ---
message("Step 2: Fitting the model with only raw demographic variables...")

# We fit the model to predict the rating ('value') using the five original
# demographic categories as fixed effects.
# We still control for the image file as a random effect to isolate the
# variance explained by the annotator characteristics.

lmer_model_demographics <- lmer(
  value ~ gender + race + educ + income + party + (1 | image_file),
  data = eval_data
)

summary(lmer_model_demographics)


## ---
## Step 3: Calculate and Interpret the R-squared
## ---
message("Step 3: Calculating the baseline R-squared...")

r2_demographics <- r2(lmer_model_demographics)

message("\n--- FINAL RESULTS (RAW DEMOGRAPHICS) ---")
print(r2_demographics)

message(paste0(
  "\n\n--- INTERPRETATION FOR DIRECT COMPARISON ---",
  "\nThe Marginal R-squared (R2m) for the raw demographics is ", round(r2_demographics$R2_marginal, 3), ".",
  "\n\nCompare this directly to the R-squared you got from using all 32 b(Z) dimensions (which was 0.193).",
  "\nThis comparison quantifies exactly how much more explanatory power is gained by moving from simple demographic groups to the nuanced, latent perspective space learned by the two-tower model."
))