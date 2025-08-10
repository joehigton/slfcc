import os
import numpy as np
import pandas as pd
import tensorflow as tf
from tensorflow.keras.preprocessing import image
from tensorflow.keras.applications.resnet50 import ResNet50, preprocess_input

print("TensorFlow version:", tf.__version__)

## ---
## Step 1: Load the Pre-trained Model
## ---

# Load the ResNet50 model, excluding its final classification layer.
# This gives us a powerful feature extractor.
print("Loading pre-trained ResNet50 model...")
base_model = ResNet50(weights='imagenet', include_top=False, pooling='avg')

# The `pooling='avg'` argument automatically flattens the output for us,
# which simplifies the process.

# base_model.summary() # Optional: view the model architecture

## ---
## Step 2: Define Image Processing Function
## ---

def get_embedding(img_path, model):
    """
    Loads an image, prepares it for the model, and returns its embedding.
    """
    # Load the image, ensuring it's the right size for the model (224x224)
    img = image.load_img(img_path, target_size=(224, 224))
    
    # Convert the image to a numpy array
    x = image.img_to_array(img)
    
    # Add a batch dimension (the model expects a batch of images)
    x = np.expand_dims(x, axis=0)
    
    # Pre-process the image pixels in the same way ResNet50 was trained
    x = preprocess_input(x)
    
    # Get the embedding by predicting with the model
    embedding = model.predict(x, verbose=0)
    
    return embedding.flatten()

## ---
## Step 3: Extract and Save Embeddings
## ---

# --- IMPORTANT: Set this to your image directory ---
image_directory = 'images/downloaded_images/' 

print(f"Processing images from: {image_directory}")

# List all image files in the directory
try:
    image_files = [f for f in os.listdir(image_directory) if f.lower().endswith(('.png', '.jpg', '.jpeg'))]
except FileNotFoundError:
    print(f"Error: Directory not found at '{image_directory}'. Please update the path.")
    image_files = []

embeddings_list = []
if image_files:
    for filename in image_files:
        full_path = os.path.join(image_directory, filename)
        
        # Get the embedding for the current image
        embedding = get_embedding(full_path, base_model)
        
        # Store the filename and its embedding
        embeddings_list.append({'image_file': filename, 'embedding': embedding})

    # Convert the list of results into a pandas DataFrame
    results_df = pd.DataFrame(embeddings_list)
    
    # Expand the embedding array into separate columns for the CSV
    embedding_cols = pd.DataFrame(results_df['embedding'].tolist(), index=results_df.index)
    embedding_cols.columns = [f'emb_{i}' for i in range(embedding_cols.shape[1])]
    
    # Combine the filename with the new embedding columns
    final_df = pd.concat([results_df['image_file'], embedding_cols], axis=1)

    # Save the final results to a CSV file
    output_path = 'image_embeddings.csv'
    final_df.to_csv(output_path, index=False)
    
    print(f"\nSuccessfully extracted embeddings for {len(final_df)} images.")
    print(f"Results saved to '{output_path}'")
    print("\nPreview of the final data:")
    print(final_df.head())

else:
    print("No image files found to process.")