import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.stats.outliers_influence import variance_inflation_factor
import warnings
warnings.filterwarnings('ignore')

print("TensorFlow version:", tf.__version__)

## ---
## Step 1: Load and Merge Data
## ---
print("Step 1: Loading and preparing data...")

# Load the image embeddings
image_embeddings = pd.read_csv("image_embeddings.csv")
#print(f"Image embeddings columns: {list(image_embeddings.columns)}")
print(f"Image embeddings shape: {image_embeddings.shape}")

# Load your full, original annotation data
# Note: You'll need to install pyreadstat for .dta files: pip install pyreadstat
try:
    full_annotations = pd.read_stata("prolific_data.dta")
except ImportError:
    print("Please install pyreadstat to read .dta files: pip install pyreadstat")
    # Alternative: convert to CSV in R first
    # full_annotations = pd.read_csv("prolific_data.csv")

# Filter for Nighttime data (keeping your change)
full_annotations = full_annotations[full_annotations['varLabel'].str.contains('Nighttime', na=False)]

# Create image_file column and merge
full_annotations['image_file'] = full_annotations['img'] + '.jpg'
model_data = full_annotations.merge(image_embeddings, on='image_file', how='inner')

print(f"Data shape after merging: {model_data.shape}")

# Convert categorical columns using Label Encoding
demographic_cols = ['gender', 'race', 'educ', 'income', 'party']
label_encoders = {}

for col in demographic_cols:
    if col in model_data.columns:
        le = LabelEncoder()
        # Handle missing values - check if column is categorical first
        if pd.api.types.is_categorical_dtype(model_data[col]):
            # Add 'Unknown' to categories if not already present
            if 'Unknown' not in model_data[col].cat.categories:
                model_data[col] = model_data[col].cat.add_categories(['Unknown'])
            model_data[col] = model_data[col].fillna('Unknown')
        else:
            # For non-categorical columns, simple fillna works
            model_data[col] = model_data[col].fillna('Unknown')
        
        # Convert to string to ensure consistent handling
        model_data[col] = model_data[col].astype(str)
        model_data[f'{col}_encoded'] = le.fit_transform(model_data[col]) + 1  # +1 for embedding (0 reserved for padding)
        label_encoders[col] = le
        print(f"{col}: {len(le.classes_)} unique values")

# ---
# Step 2: Define the Two-Tower Model Architecture
# ---
print("\nStep 2: Defining the Two-Tower Keras model...")

# Define dimensions
embedding_dim = 32  # Size of learned b(Z) perspective score

# Get the actual feature columns from image embeddings (excluding image_file)
feature_cols = [col for col in image_embeddings.columns if col.startswith('emb_')]
item_embedding_dim = len(feature_cols)

print(f"Feature columns found: {feature_cols[:5]}...")  # Show first 5
print(f"Item embedding dimension: {item_embedding_dim}")

if item_embedding_dim == 0:
    print("ERROR: No embedding columns found in image_embeddings.csv")
    print("Available columns:", list(image_embeddings.columns))
    print("Please check your image embeddings file format")
    exit(1)

# Update the item_embedding_dim to match available columns after merge
# We'll verify this again after the merge

# Define input layers
input_item = layers.Input(shape=(item_embedding_dim,), name='input_item_embedding')

# Annotator inputs
input_gender = layers.Input(shape=(1,), name='input_gender')
input_race = layers.Input(shape=(1,), name='input_race') 
input_educ = layers.Input(shape=(1,), name='input_educ')
input_income = layers.Input(shape=(1,), name='input_income')
input_party = layers.Input(shape=(1,), name='input_party')

## --- Build the Item Tower ---
item_tower = layers.Dense(embedding_dim, name='item_representation')(input_item)

## --- Build the Annotator Tower (The b(Z) Learner) ---
# Create embedding layers for each demographic feature
vocab_sizes = {col: model_data[f'{col}_encoded'].max() + 1 for col in demographic_cols if col in model_data.columns}

embedding_gender = layers.Embedding(vocab_sizes['gender'], 8, name='embedding_gender')(input_gender)
embedding_gender = layers.Flatten()(embedding_gender)

embedding_race = layers.Embedding(vocab_sizes['race'], 8, name='embedding_race')(input_race)
embedding_race = layers.Flatten()(embedding_race)

embedding_educ = layers.Embedding(vocab_sizes['educ'], 8, name='embedding_educ')(input_educ)
embedding_educ = layers.Flatten()(embedding_educ)

embedding_income = layers.Embedding(vocab_sizes['income'], 8, name='embedding_income')(input_income)
embedding_income = layers.Flatten()(embedding_income)

embedding_party = layers.Embedding(vocab_sizes['party'], 8, name='embedding_party')(input_party)
embedding_party = layers.Flatten()(embedding_party)

# Concatenate all annotator embeddings
annotator_features = layers.Concatenate()([
    embedding_gender, embedding_race, embedding_educ, 
    embedding_income, embedding_party
])

# Final perspective score b(Z) layers
perspective_score_b_z = layers.Dense(64, activation='relu')(annotator_features)
perspective_score_b_z = layers.Dense(embedding_dim, name='perspective_score_b_z')(perspective_score_b_z)

## --- Combine the Towers ---
# Dot product prediction
prediction = layers.Dot(axes=1, name='dot_product_prediction')([item_tower, perspective_score_b_z])

# Create and compile model
two_tower_model = keras.Model(
    inputs=[input_item, input_gender, input_race, input_educ, input_income, input_party],
    outputs=prediction
)

two_tower_model.compile(
    optimizer='adam',
    loss='mse',
    metrics=['mae']
)

print(two_tower_model.summary())

# ---
# Step 3: Prepare Data and Train the Model
# ---
print("\nStep 3: Preparing data and training the model...")

# Prepare item features (image embeddings)
item_features = model_data[feature_cols].astype(float).values

# Prepare annotator features
annotator_features_dict = {}
for col in demographic_cols:
    if col in model_data.columns:
        annotator_features_dict[f'input_{col}'] = model_data[f'{col}_encoded'].values.reshape(-1, 1)

# Target variable (using 'value' as per your change)
target_ratings = model_data['value'].values

print(f"Target ratings shape: {target_ratings.shape}")
print(f"Target ratings stats: min={target_ratings.min():.3f}, max={target_ratings.max():.3f}, mean={target_ratings.mean():.3f}")

# Check for NaN values in target
if np.isnan(target_ratings).any():
    print("WARNING: NaN values found in target ratings")
    nan_mask = np.isnan(target_ratings)
    print(f"Removing {nan_mask.sum()} rows with NaN targets")
    
    # Remove NaN targets and corresponding features
    valid_mask = ~nan_mask
    target_ratings = target_ratings[valid_mask]
    item_features = item_features[valid_mask]
    
    # Update annotator features
    for key in annotator_features_dict:
        annotator_features_dict[key] = annotator_features_dict[key][valid_mask]

# Prepare input dictionary
input_dict = {
    'input_item_embedding': item_features,
    **annotator_features_dict
}

print(f"Target shape: {target_ratings.shape}")
print(f"Item features shape: {item_features.shape}")

# Verify we have valid data
assert item_features.shape[1] > 0, "No item features found!"
assert len(target_ratings) > 0, "No target ratings found!"
assert not np.isnan(target_ratings).any(), "NaN values in target ratings!"
assert not np.isnan(item_features).any(), "NaN values in item features!"

# Split data
train_indices, val_indices = train_test_split(
    range(len(target_ratings)), 
    test_size=0.2, 
    random_state=42
)

# Prepare training and validation data
train_inputs = {key: value[train_indices] for key, value in input_dict.items()}
val_inputs = {key: value[val_indices] for key, value in input_dict.items()}

train_targets = target_ratings[train_indices]
val_targets = target_ratings[val_indices]

# Train the model
print("Starting training...")
history = two_tower_model.fit(
    x=train_inputs,
    y=train_targets,
    validation_data=(val_inputs, val_targets),
    epochs=20,
    batch_size=32,
    verbose=1
)

# Plot training history
plt.figure(figsize=(12, 4))
plt.subplot(1, 2, 1)
plt.plot(history.history['loss'], label='Training Loss')
plt.plot(history.history['val_loss'], label='Validation Loss')
plt.title('Model Loss')
plt.xlabel('Epoch')
plt.ylabel('Loss')
plt.legend()

plt.subplot(1, 2, 2)
plt.plot(history.history['mae'], label='Training MAE')
plt.plot(history.history['val_mae'], label='Validation MAE')
plt.title('Model MAE')
plt.xlabel('Epoch')
plt.ylabel('MAE')
plt.legend()
plt.tight_layout()
plt.show()

# ---
# Step 4: Extract b(Z) and Evaluate Explanatory Power
# ---
print("\nStep 4: Extracting b(Z) and evaluating its explanatory power...")

# Create b(Z) extractor model
annotator_inputs = [input_gender, input_race, input_educ, input_income, input_party]
b_z_extractor_model = keras.Model(
    inputs=annotator_inputs,
    outputs=perspective_score_b_z
)

# Get unique annotators
unique_annotators = model_data.drop_duplicates(subset=['ResponseId'])

# Prepare inputs for unique annotators
unique_annotator_inputs = {}
for col in demographic_cols:
    if col in model_data.columns:
        unique_annotator_inputs[f'input_{col}'] = unique_annotators[f'{col}_encoded'].values.reshape(-1, 1)

# Extract b(Z) scores
b_z_scores = b_z_extractor_model.predict(unique_annotator_inputs)

# Create DataFrame with b(Z) scores
b_z_df = pd.DataFrame(b_z_scores, columns=[f'bz_{i+1}' for i in range(b_z_scores.shape[1])])
b_z_df['ResponseId'] = unique_annotators['ResponseId'].values

# Merge back with full data
final_eval_data = model_data.merge(b_z_df, on='ResponseId', how='left')

print(f"b(Z) scores shape: {b_z_scores.shape}")
print(f"Final evaluation data shape: {final_eval_data.shape}")

# ---
# Step 5: Save b(Z) scores and model outputs for R
# ---
print("\nStep 5: Saving b(Z) scores and model outputs for R...")

# 1) Save per-annotator b(Z) scores (32-dim by default)
bz_cols = [f'bz_{i+1}' for i in range(b_z_scores.shape[1])]
annotator_bz_out = unique_annotators[['ResponseId', 'gender', 'race', 'educ', 'income', 'party']].copy()
annotator_bz_out[bz_cols] = b_z_scores
annotator_bz_out.to_csv('annotator_bz_scores.csv', index=False)
print(f"- annotator_bz_scores.csv  [{annotator_bz_out.shape[0]} rows, {annotator_bz_out.shape[1]} cols]")

# 2) Save per-row predictions only for the rows actually used in training/eval
print("Generating predictions for training/eval rows only...")

yhat = two_tower_model.predict(input_dict, batch_size=256, verbose=1).reshape(-1)

# Recreate the valid mask used earlier when NaNs were dropped
valid_mask = ~np.isnan(model_data['value'].values)

pred_cols = ['ResponseId', 'image_file', 'img', 'value']
keep_cols = [c for c in pred_cols if c in model_data.columns]
pred_df = model_data.loc[valid_mask, keep_cols].copy()

# yhat here is from your filtered input_dict (2382 rows)
pred_df['pred'] = yhat
pred_df.to_csv('predictions_train_only.csv', index=False)
print(f"- predictions_train_only.csv [{pred_df.shape[0]} rows, {pred_df.shape[1]} cols]")

# 3) Save merged data with b(Z) attached (handy if you want long-form joins in R)
final_eval_data = model_data.merge(
    annotator_bz_out[['ResponseId'] + bz_cols],
    on='ResponseId', how='left'
)
final_eval_data.to_csv('model_data_with_bz_scores.csv', index=False)
print(f"- model_data_with_bz_scores.csv [{final_eval_data.shape[0]} rows, {final_eval_data.shape[1]} cols]")

# 4) Save training history (CSV) and plot (PNG)
hist_df = pd.DataFrame(history.history)
hist_df.to_csv('training_history.csv', index=False)
print("- training_history.csv")

# If you switched plt.show() to savefig earlier, you might already have this; otherwise:
try:
    plt.figure(figsize=(12, 4))
    plt.subplot(1, 2, 1); plt.plot(history.history['loss'], label='Training Loss'); plt.plot(history.history['val_loss'], label='Validation Loss')
    plt.title('Model Loss'); plt.xlabel('Epoch'); plt.ylabel('Loss'); plt.legend()
    plt.subplot(1, 2, 2); plt.plot(history.history['mae'], label='Training MAE'); plt.plot(history.history['val_mae'], label='Validation MAE')
    plt.title('Model MAE'); plt.xlabel('Epoch'); plt.ylabel('MAE'); plt.legend()
    plt.tight_layout()
    plt.savefig("training_history.png", dpi=150, bbox_inches="tight"); plt.close()
    print("- training_history.png")
except Exception as e:
    print(f"(Skipping plot save: {e})")

# 5) (Optional) Save the Keras model and the b(Z) extractor
two_tower_model.save('two_tower_model.keras')
b_z_extractor_model.save('bz_extractor.keras')
print("- two_tower_model.keras")
print("- bz_extractor.keras")

print("\nDone. You can load CSVs into R and run your stats there.")
