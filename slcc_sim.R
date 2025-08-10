library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # Set random seed for reproducibility
  set.seed(42)
  
  # Simulation parameters
  n_items <- 1000  # Number of items to classify
  n_sims <- 100    # Number of simulations per scenario
  
  # Function to generate synthetic data
  generate_data <- function(n_items, alpha_1, gamma_1,group_difference) {
    # True feature that influences classification
    x <- rnorm(n_items)
    
    max_diff <- plogis(group_difference) - plogis(-group_difference)
    
    prob_group1 <- plogis(x/max_diff + group_difference)
    prob_group2 <- plogis(x/max_diff - group_difference)
    
    print(range(prob_group1))
    print(range(prob_group2))
    # Generate annotator labels
    n_annotators <- 5
    labels <- matrix(0, nrow = n_items, ncol = n_annotators)
    
    # Assign annotators to groups based on alpha
    annotator_groups <- rbinom(n_annotators, 1, alpha_1)
    
    # Generate labels
    for(i in 1:n_items) {
      for(j in 1:n_annotators) {
        prob <- if(annotator_groups[j] == 1) prob_group1[i] else prob_group2[i]
        labels[i,j] <- rbinom(1, 1, prob)
      }
    }
    
    # Calculate population-level ground truth
    true_probs <- gamma_1 * prob_group1 + (1 - gamma_1) * prob_group2
    
    list(
      x = x,
      labels = labels,
      true_probs = true_probs,
      annotator_groups = annotator_groups
    ) 
  }
  
  # Function to evaluate predictions
  evaluate_predictions <- function(pred_probs, true_probs) {
    # Bias
    bias <- mean(pred_probs - true_probs)
    
    # RMSE
    rmse <- sqrt(mean((pred_probs - true_probs)^2))
    
    # Expected Calibration Error
    n_bins <- 10
    bins <- cut(pred_probs, breaks = seq(0, 1, length.out = n_bins + 1))
    ece <- tapply(pred_probs - true_probs, bins, function(x) mean(abs(x)))
    ece <- mean(ece, na.rm = TRUE)
    
    c(bias = bias, rmse = rmse, ece = ece)
  }
  
  # Run simulations
  
  alpha_values <- seq(0.5, 0.95, by = 0.1)
  group_differences <- c(0.72, 1.31, 1.99)  # Small, Medium, Large differences
  gamma_1 <- 0.5  # True population proportion for group 1
  
  results <- data.frame()
  
  for(diff in group_differences) {
    for(alpha_1 in alpha_values) {
      for(sim in 1:n_sims) {
        # Generate data
        data <- generate_data(n_items, alpha_1, gamma_1,diff)
        
        # Unweighted predictions (majority vote)
        unweighted_preds <- rowMeans(data$labels)
        
        # Importance weighted predictions
        w1 <- gamma_1 / alpha_1
        w2 <- (1 - gamma_1) / (1 - alpha_1)
        weights <- ifelse(data$annotator_groups == 1, w1, w2)
        weighted_preds <- apply(data$labels * matrix(weights, n_items, 5, byrow = TRUE), 1, sum) / 
          sum(weights)
        
        # Evaluate
        unweighted_metrics <- evaluate_predictions(unweighted_preds, data$true_probs)
        weighted_metrics <- evaluate_predictions(weighted_preds, data$true_probs)
        
        results <- rbind(results, 
                         data.frame(alpha = alpha_1,
                                    method = "Unweighted",
                                    sim = sim,
                                    group_diff = diff,
                                    metric = names(unweighted_metrics),
                                    value = unweighted_metrics),
                         data.frame(alpha = alpha_1,
                                    method = "Weighted",
                                    sim = sim,
                                    group_diff = diff,
                                    metric = names(weighted_metrics),
                                    value = weighted_metrics))
      }
    }
  }
  
  # Plotting
  results_summary <- results %>%
    group_by(alpha, method, metric, group_diff) %>%
    summarise(mean_value = mean(value),
              se = sd(value) / sqrt(n())) %>%
    ungroup() %>%
    mutate(group_diff_label = case_when(
      group_diff == group_differences[1] ~ "5% mean disagreement",
      group_diff == group_differences[2] ~ "15% mean disagreement",
      group_diff == group_differences[3] ~ "30% mean disagreement"
    ) %>% factor(levels = c("5% mean disagreement", "15% mean disagreement", "30% mean disagreement"))) 
  
  # Modified plot function
  plot_metric <- function(data, metric_name) {
    ggplot(data %>% filter(metric == metric_name), 
           aes(x = alpha, y = mean_value, color = method)) +
      geom_line() +
      geom_point() +
      geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.02) +
      facet_wrap(~group_diff_label,nrow=3) +
      labs(x = "Population skew",
           y = metric_name,
           color = "Method") +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  # Combine plots
  p1 <- plot_metric(results_summary, "bias")
  p2 <- plot_metric(results_summary, "rmse")
  
  combined_plot <- (p1 + p2) + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  print(combined_plot)
  
  ggsave("simulations.png",width = 6, height = 9)
  
  # just unweighted 
  plot_metric <- function(data, metric_name) {
    ggplot(data %>% filter(method == "Unweighted") %>% filter(metric == metric_name), 
           aes(x = alpha, y = mean_value, color = method)) +
      geom_line() +
      geom_point() +
      geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.02) +
      facet_wrap(~group_diff_label,nrow=3, scales = "") +
      labs(x = "Population skew",
           y = metric_name,
           color = "Method") +
      theme_minimal() +
      theme(legend.position = "none")+
      scale_y_continuous(labels = scales::percent_format())+
      scale_x_continuous(labels = scales::percent_format())
  }
  
  # Combine plots
  p1 <- plot_metric(results_summary, "bias")
  p2 <- plot_metric(results_summary, "rmse")
  
  combined_plot <- (p1 + p2) + 
    plot_layout(guides = "collect") &
    theme(legend.position = "none")
  print(combined_plot)
  
  ggsave("simulations_simple.png",width = 9, height = 5)

#poster
results_summary %>% filter(grepl("15",group_diff_label), metric != "ece") %>% 
  mutate(metric = ifelse(metric == "bias","Bias","RMSE")) %>% 
  ggplot(aes(x = alpha, y = mean_value, color = method)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.02) +
  facet_wrap(~metric,nrow=1,scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")+
  labs(title = "Importance weighting faces bias-variance tradeoff",
       x = "Population skew",
       y="") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", colour = "#2C3E50"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, colour = "#7F8C8D", margin = margin(b = 20)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    legend.title = element_text(size = 10, face = "bold"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("simulations_just15.png",width = 8, height = 4)


