# Load required libraries
library(jsonlite)
library(data.table)
library(parallel)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Function to clean JSON string
clean_json <- function(json_str) {
  json_str <- gsub('NaN', 'null', json_str)
  json_str <- iconv(json_str, "UTF-8", "UTF-8", sub="")
  json_str <- gsub("\u0080\u0093", "-", json_str)
  return(json_str)
}

# Start timing
start_time <- Sys.time()

# Optimize parallel setup for Apple Silicon
num_cores <- detectCores(logical = FALSE)  # Use physical cores for M1/M2
mc.cores <- min(num_cores * 2, detectCores())  # Leverage efficiency cores
cat(sprintf("\nStarting processing with %d cores (Apple Silicon optimized)\n", mc.cores))

# Read the JSON file
json_data <- readLines("classified_data_final_w_worker_hash.json")
total_lines <- length(json_data)
cat(sprintf("\nTotal lines to process: %d\n", total_lines))

# Create smaller chunks for better load balancing on M1/M2
chunk_size <- ceiling(total_lines / (mc.cores * 4))  # More chunks than cores for better distribution
chunk_indices <- split(seq_len(total_lines), ceiling(seq_len(total_lines) / chunk_size))

# Process function for each chunk
process_chunk <- function(indices) {
  require(jsonlite)
  require(data.table)
  
  result_list <- vector("list", length(indices))
  worker_id <- Sys.getpid()
  chunk_start_time <- Sys.time()
  last_progress_time <- chunk_start_time
  
  for (i in seq_along(indices)) {
    current_time <- Sys.time()
    # Update progress every 2 seconds instead of every N lines
    if (difftime(current_time, last_progress_time, units="secs") >= 2) {
      cat(sprintf("PID %d: Processed %d/%d lines (%.1f%%) - %.1f lines/sec\n", 
                  worker_id, i, length(indices), 
                  i/length(indices) * 100,
                  i/as.numeric(difftime(current_time, chunk_start_time, units="secs"))))
      last_progress_time <- current_time
    }
    
    tryCatch({
      json_line <- json_data[indices[i]]
      clean_line <- clean_json(json_line)
      parsed_data <- fromJSON(clean_line)
      
      # Extract base comment data
      base_data <- data.table(
        comment_id = parsed_data$comment_id,
        comment = parsed_data$comment,
        source = parsed_data$source,
        perspective_score = parsed_data$perspective_score
      )
      
      # Convert ratings to data.table and add annotator_id
      ratings_dt <- as.data.table(parsed_data$ratings)
      ratings_dt[, annotator_id := .I]
      ratings_dt[, line_number := indices[i]]  # Add original line number for reference
      
      # Replicate base data
      base_repeated <- base_data[rep(1, nrow(ratings_dt))]
      
      # Combine base data with ratings
      result_list[[i]] <- cbind(base_repeated, ratings_dt)
      
    }, error = function(e) {
      cat(sprintf("PID %d - Error processing line %d: %s\n", 
                  worker_id, indices[i], e$message))
      return(NULL)
    })
  }
  
  chunk_end_time <- Sys.time()
  chunk_duration <- difftime(chunk_end_time, chunk_start_time, units="secs")
  cat(sprintf("PID %d: Completed %d lines in %.1f seconds (%.1f lines/sec)\n", 
              worker_id, length(indices), 
              as.numeric(chunk_duration),
              length(indices)/as.numeric(chunk_duration)))
  
  # Combine results
  result_list <- result_list[!sapply(result_list, is.null)]
  if (length(result_list) > 0) {
    return(rbindlist(result_list, fill = TRUE))
  } else {
    return(data.table())
  }
}

# Process in parallel using mclapply
results <- rbindlist(
  mclapply(chunk_indices, 
           process_chunk, 
           mc.cores = mc.cores,
           mc.preschedule = FALSE),  # Dynamic scheduling for better load balancing
  fill = TRUE
)

# Calculate processing time
end_time <- Sys.time()
processing_time <- difftime(end_time, start_time, units = "mins")

# Write output
cat("\nWriting results to CSV...\n")
fwrite(results, "parsed_comments_data.csv")

# Print final statistics
cat("\nFinal Statistics:\n")
cat(sprintf("Processing time: %.2f minutes\n", as.numeric(processing_time)))
cat(sprintf("Total input lines: %d\n", total_lines))
cat(sprintf("Total output rows: %d\n", nrow(results)))
cat(sprintf("Total columns: %d\n", ncol(results)))
cat(sprintf("Average processing speed: %.1f lines per second\n", 
            total_lines/as.numeric(processing_time)/60))
cat(sprintf("Average processing speed per core: %.1f lines per second\n",
            total_lines/as.numeric(processing_time)/60/mc.cores))
cat("\nColumn names:\n")
print(colnames(results))
cat(sprintf("\nMemory used (MB): %.1f\n", memory.size()))

# Show sample of data
cat("\nFirst few rows:\n")
print(head(results))