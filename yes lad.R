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
  

# a nova kmeans ####

# install.packages(c("tidyverse", "lme4", "effectsize"))

library(tidyverse)
library(lme4)
library(effectsize) # For eta_squared()

## ---
## Step 1: Load Annotator and b(Z) Data
## ---
message("Step 1: Loading annotator b(Z) scores...")

# Load the CSV file that contains the unique annotators and their b(Z) scores
annotator_bz <- read_csv("annotator_bz_scores.csv")

# Select just the b(Z) score columns for clustering
bz_cols <- annotator_bz %>% select(starts_with("bz_"))


## ---
## Step 2: Cluster Annotators into "Perspective Groups"
## ---
message("Step 2: Clustering annotators with k-means...")

# We use k-means to find natural groupings in the 32-dimensional perspective space.
# Choosing the number of clusters 'k' is a key step. For this analysis, let's
# pick a reasonable number like k=8 to see if these 8 latent groups are more
# informative than the original demographic categories.

set.seed(123) # for reproducibility
k <- 8
kmeans_results <- kmeans(bz_cols, centers = k, nstart = 25)

# Add the resulting cluster ID as a new "perspective_group" column
annotator_bz$perspective_group <- as.factor(kmeans_results$cluster)

# View the size of each new group
print(table(annotator_bz$perspective_group))


## ---
## Step 3: Merge Perspective Groups into Full Dataset
## ---
message("Step 3: Merging new perspective groups into the main dataset...")

# Load the full dataset that has one row per rating
full_data <- read_csv("model_data_with_bz_scores.csv")

# Merge the new perspective group assignments into the full data
# We only need the ID and the new group from our clustered data
data_with_groups <- full_data %>%
  left_join(annotator_bz %>% select(ResponseId, perspective_group), by = "ResponseId")


## ---
## Step 4: Run ANOVA on the New Perspective Groups
## ---
message("Step 4: Running ANOVA on all groups for comparison...")

# Here we re-use the exact ANOVA function you provided
analyze_demographic_numeric <- function(data, demographic_col) {
  # Fit model
  model <- lmer(value ~ as.factor(get(demographic_col)) + (1|image_file), data = data)
  
  # Calculate partial eta-squared
  eta_sq <- effectsize::eta_squared(model, partial = TRUE)
  
  return(tibble(
    demographic = demographic_col,
    effect_size = eta_sq$Eta2_partial[1]
  ))
}

# List all the grouping variables we want to test: the original ones PLUS our new one
groups_to_test <- c("income", "educ", "gender", "race", "party", "perspective_group")

# Run the ANOVA function for every group
comparison_results <- map_dfr(
  groups_to_test,
  ~analyze_demographic_numeric(data_with_groups, .x)
)


## ---
## Step 5: Compare the Results
## ---
message("\n--- FINAL COMPARISON ---")

# Arrange the results to see which grouping variable explains the most variance
final_comparison <- comparison_results %>%
  mutate(
    effect_size_percent = effect_size * 100
  ) %>%
  arrange(desc(effect_size))

print(final_comparison)

message(paste0(
  "\n\n--- INTERPRETATION ---",
  "\nThe table above directly compares the variance explained (Partial Eta-squared) by the original demographic groups versus the new, learned 'perspective_group'.",
  "\n\nIf the effect size for 'perspective_group' is the largest, you have strong evidence that clustering annotators based on their learned b(Z) scores creates more meaningful and powerful groupings than relying on observed demographics alone."
))