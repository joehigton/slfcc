#!/bin/bash

# Download images from CSV file
CSV_FILE="images_with_coordinates.csv"
DOWNLOAD_DIR="downloaded_images"

# Create download directory if it doesn't exist
mkdir -p "$DOWNLOAD_DIR"

# Skip header and process each line
tail -n +2 "$CSV_FILE" | while IFS=',' read -r col1 col2 col3 col4 col5 url rest; do
    # Remove quotes if present
    url=$(echo "$url" | sed 's/^"//;s/"$//')
    
    # Skip empty URLs
    if [ -z "$url" ]; then
        continue
    fi
    
    # Extract filename from URL or generate one
    filename=$(basename "$url" | sed 's/[?&].*$//')
    if [ -z "$filename" ] || [ "$filename" = "/" ]; then
        filename="image_$(date +%s%N).jpg"
    fi
    
    # Download the image
    echo "Downloading: $url"
    curl -L -o "$DOWNLOAD_DIR/$filename" "$url"
    
    # Small delay to be respectful
    sleep 0.1
done

echo "Download complete! Images saved to $DOWNLOAD_DIR/"
