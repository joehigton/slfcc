library(marginaleffects)
library(lme4)
library(gridExtra)
library(tidyverse)
library(survey)
library(effectsize)  # for effect size calculations 
library(egg)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# NLPOSIT data ####

## load data ###
nlp<-read_csv('nlpositionality_toxicity_raw.csv')
dh<-read_csv('2021/Dynamically Generated Hate Dataset v2.1.csv')

# we can probably make it representative on ethnicity, religion, gender, age for just the US sample...
nlp %>% group_by(action) %>%
  summarise(n=n(),mean=mean(litw)) %>% 
  arrange(desc(n))

# let's not do that -- we can tho calculate differences / etc with the DH ones
#so we can do the regressions MEs 


# meh we may as well try it 
# https://www.pewresearch.org/2018/01/26/appendix-b-synthetic-population-dataset/

## weighting ####

# Step 1: Create population targets
# These should match your census percentages
age_tar = data.frame(age_group = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                       Freq = c(0.13,0.18,0.17,0.17,0.17,0.19))
gender_tar = data.frame(gender = c("man","woman"),
                        Freq = c(0.48,0.52))
ethnicity_tar = data.frame(ethnicity = c("white", "black, african american", 
                                         "latino/latina, hispanic","asian, asian american","Other"),
                           Freq = c(0.65,0.12,0.15,0.06,0.03))
education_tar = data.frame(education = c("pre-high school", "high school", 
                                         "college","graduate"),
                           Freq = c(0.13,0.28,0.49,0.10))



nlp_w<-nlp %>% mutate(age_group = case_when(age < 25 ~ "18-24",
                                     age < 35 ~ "25-34",
                                     age < 45 ~ "35-44",
                                     age < 55 ~ "45-54",
                                     age < 65 ~ "55-64",
                                     TRUE ~ "65+")) %>%
  mutate(ethnicity = case_when(ethnicity %in% c("white", "black, african american", 
                                                "latino/latina, hispanic","asian, asian american") ~ ethnicity,
                               TRUE ~ "Other")) %>%
  mutate(education = case_when(education %in% c("pre-high school", "high school", 
                                                "college","graduate") ~ education,
                               education %in% c("graduate school","phd") ~ "graduate",
                               TRUE ~ NA)) %>%
  mutate()

nlp_w<-nlp_w %>% filter(gender %in% c("man","woman"),
                  country_longest %in% c("United States") | country_residence %in% c("United States"),
                  !is.na(education))

# Step 2: Create survey design object
survey_design <- svydesign(ids = ~1, # For simple random sampling
                           data = nlp_w,
                           weights = NULL)

# Step 3: Calculate weights using raking
raked <- rake(survey_design,
                        sample.margins = list(
                          ~age_group,
                          ~gender,
                          ~ethnicity,
                          ~education
                        ),
                        population.margins = list(age_tar,
                                                  gender_tar,
                                                  ethnicity_tar,
                                                  education_tar))

# Extract weights and normalize to sum to sample size
nlp_w$rake_weights <- weights(raked)
nlp_w$rake_weights <- nlp_w$rake_weights * (nrow(nlp_w)/sum(nlp_w$rake_weights))

# Check distribution of new weights
summary(nlp_w$rake_weights)
hist(nlp_w$rake_weights)










#maybe we should use... the... GS ones to train and evaluate agianst nlpw? i guess so :) 
#but it doesnt really make sense as we dont have a good explanation how the nlpw ones are represewntative etc 


nlp_w %>% group_by(action) %>% summarise(mean = mean(litw)) %>% view

## prediction etc nlp ####
nlp_w <- nlp_w %>% mutate(ethnicity = case_when(ethnicity == "latino/latina, hispanic" ~ "hispanic",
                                                ethnicity == "black, african american" ~ "black",
                                                ethnicity == "asian, asian american" ~ "asian",
                                                TRUE ~ ethnicity))
### expert vs groups ####
# Function to calculate CI for correlation
cor_ci <- function(r, n, conf.level = 0.95) {
  # Fisher's Z transformation
  z <- atanh(r)
  # Standard error of z
  se <- 1/sqrt(n-3)
  # Critical value
  crit <- qnorm((1 + conf.level)/2)
  # CI for z
  ci <- c(z - crit*se, z + crit*se)
  # Transform back to correlation scale
  ci_cor <- tanh(ci)
  return(ci_cor)
}

# Modified correlation calculation with sample sizes and CIs
correlations <- bind_rows(
  # Age group correlations
  nlp_w %>%
    group_by(age_group) %>%
    summarise(
      correlation = cor(litw, dynahate, use = "complete.obs"),
      n = sum(!is.na(litw) & !is.na(dynahate))) %>% 
    mutate(
      ci_lower = map2_dbl(correlation, n, ~cor_ci(.x, .y)[1]),
      ci_upper = map2_dbl(correlation, n, ~cor_ci(.x, .y)[2])) %>%
    rename(group = age_group),
  
  # Gender correlations
  nlp_w %>%
    group_by(gender) %>%
    summarise(
      correlation = cor(litw, dynahate, use = "complete.obs"),
      n = sum(!is.na(litw) & !is.na(dynahate))) %>%
    mutate(
      ci_lower = map2_dbl(correlation, n, ~cor_ci(.x, .y)[1]),
      ci_upper = map2_dbl(correlation, n, ~cor_ci(.x, .y)[2])) %>%
    rename(group = gender),
  nlp_w %>%
    group_by(ethnicity) %>%
    summarise(
      correlation = cor(litw, dynahate, use = "complete.obs"),
      n = sum(!is.na(litw) & !is.na(dynahate))) %>%
    mutate(
      ci_lower = map2_dbl(correlation, n, ~cor_ci(.x, .y)[1]),
      ci_upper = map2_dbl(correlation, n, ~cor_ci(.x, .y)[2])) %>%
    rename(group = ethnicity),
  nlp_w %>%
    group_by(education) %>%
    summarise(
      correlation = cor(litw, dynahate, use = "complete.obs"),
      n = sum(!is.na(litw) & !is.na(dynahate))) %>%
    mutate(
      ci_lower = map2_dbl(correlation, n, ~cor_ci(.x, .y)[1]),
      ci_upper = map2_dbl(correlation, n, ~cor_ci(.x, .y)[2])) %>%
    rename(group = education),
  ) %>%
  arrange(desc(correlation))

### groups vs groups ####

# between group COR
calculate_between_group_correlations <- function(data, group_var) {
  groups <- unique(data[[group_var]])
  combinations <- combn(groups, 2, simplify = FALSE)
  
  between_correlations <- map_dfr(combinations, function(pair) {
    # Get mean ratings for each item by group
    group1_ratings <- data %>%
      filter(!!sym(group_var) == pair[1]) %>%
      group_by(action) %>%
      summarize(rating = mean(litw, na.rm = TRUE))
    
    group2_ratings <- data %>%
      filter(!!sym(group_var) == pair[2]) %>%
      group_by(action) %>%
      summarize(rating = mean(litw, na.rm = TRUE))
    
    # Join and calculate correlation
    combined <- inner_join(group1_ratings, group2_ratings, 
                           by = "action", suffix = c("_1", "_2"))
    
    tibble(
      group1 = pair[1],
      group2 = pair[2],
      correlation = cor(combined$rating_1, combined$rating_2),
      n_items = nrow(combined)
    )
  })
  
  print(paste0("Finished analyzing group correlations for ", group_var))
  return(between_correlations)
}

# Run for each demographic variable
demographic_vars <- c("age_group", "gender", "ethnicity", "education")
between_group_cors <- lapply(demographic_vars, function(var) {
  results <- calculate_between_group_correlations(nlp_w, var)
  results$demographic <- var
  return(results)
})

between_group_cors[[1]]<-between_group_cors[[1]] %>% arrange(group1,group2)

# within group cor 
calculate_within_group_correlations <- function(data, group_var, n_iterations = 100) {
  groups <- unique(data[[group_var]])
  
  within_correlations <- map_dfr(groups, function(group) {
    # Get data for this group
    group_data <- data %>%
      filter(!!sym(group_var) == group)
    
    # Run multiple iterations of split-half
    correlations <- map_dbl(1:n_iterations, function(i) {
      # Randomly split all ratings within the group
      n_rows <- nrow(group_data)
      split_indices <- sample(1:n_rows, size = floor(n_rows/2))
      
      # Calculate mean ratings for each split
      ratings1 <- group_data[split_indices, ] %>%
        group_by(action) %>%
        summarize(rating = mean(litw, na.rm = TRUE))
      
      ratings2 <- group_data[-split_indices, ] %>%
        group_by(action) %>%
        summarize(rating = mean(litw, na.rm = TRUE))
      
      # Join and calculate correlation
      combined <- inner_join(ratings1, ratings2, 
                             by = "action", suffix = c("_1", "_2"))
      combined<-combined %>%  filter(!is.na(rating_1),!is.na(rating_2))
      
      cor(combined$rating_1, combined$rating_2)
    })
    
    tibble(
      group = group,
      mean_correlation = mean(correlations,na.rm=T),
      sd_correlation = sd(correlations),
      n_iterations = n_iterations,
      n_items = n_distinct(group_data$action)
    )
  })
  
  within_correlations$demographic <- group_var
  return(within_correlations)
}

# Run for each demographic variable
within_group_cors <- lapply(demographic_vars, function(var) {
  calculate_within_group_correlations(nlp_w, var)
})

# Combine results into a single dataframe
within_group_results <- bind_rows(within_group_cors)
within_group_results %>% mutate(group = ifelse(group=="Other","Other ethnicity",group)) %>% group_by(group) %>% summarise(mean_correlation = mean(mean_correlation,na.rm=T),n_items = mean(n_items)) %>% 
  ggplot(aes(x =mean_correlation, y = reorder(group,mean_correlation)))+
  geom_point()+
  labs(x = "Correlation",y="Demographic group",title = "Correlation within groups")
ggsave("within_group_correlations_nlp.png",width=7,height=7)


#### plotting ####
library(ggplot2)
library(reshape2)

create_correlation_heatmap <- function(corr_data, title) {
  # Create matrix of correlations
  mat <- matrix(NA, 
                nrow = length(unique(c(corr_data$group1, corr_data$group2))),
                ncol = length(unique(c(corr_data$group1, corr_data$group2))))
  
  groups <- unique(c(corr_data$group1, corr_data$group2)) %>% sort
  rownames(mat) <- groups
  colnames(mat) <- groups
  
  # Fill matrix
  for(i in 1:nrow(corr_data)) {
    g1 <- which(groups == corr_data$group1[i])
    g2 <- which(groups == corr_data$group2[i])
    mat[g1, g2] <- corr_data$correlation[i]
    mat[g2, g1] <- corr_data$correlation[i]
  }
  diag(mat) <- 1
  
  # Convert to long format for ggplot
  melted_mat <- melt(mat)

  ggplot(melted_mat, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", value)), 
              color = ifelse(melted_mat$value > 0.7, "white", "black")) +  # Change text color based on background
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                         midpoint = 0.5, limit = c(0,1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title,
         x = "", y = "", fill = "Correlation") +
    coord_fixed()+
    theme(plot.background = element_rect(fill="white"))
}

# Create heatmaps
age_heatmap_nlp <- create_correlation_heatmap(between_group_cors[[1]], "Age Group")
ggsave("age_heatmap.png",width=7,height=7)
ethnicity_heatmap_nlp <- create_correlation_heatmap(between_group_cors[[3]], "Ethnicity")
ggsave("ethnicity_heatmap.png",width=7,height=7)
education_heatmap_nlp <- create_correlation_heatmap(between_group_cors[[4]], "Education")
ggsave("education_heatmap.png",width=7,height=7)

x<-ggarrange(
  age_heatmap_nlp + theme(legend.position = "none"),
  ethnicity_heatmap_nlp + theme(legend.position = "bottom"),
  education_heatmap_nlp+theme(legend.position="none"),
  ncol = 3
)
ggsave("nlp_correlations.png",x,width = 10, height = 5)

# Summary statistics
summary_stats <- lapply(between_group_cors, function(x) {
  data.frame(
    demographic = unique(x$demographic),
    mean_corr = mean(x$correlation),
    min_corr = min(x$correlation),
    max_corr = max(x$correlation),
    sd_corr = sd(x$correlation)
  )
}) %>% bind_rows()

# Print summary table
knitr::kable(summary_stats, digits = 3,
             col.names = c("Demographic", "Mean Correlation", 
                           "Min Correlation", "Max Correlation", "SD"),format = "latex")


##### correlation plot  ####
correlations %>% mutate(group = ifelse(group=="Other","Other ethnicity",group)) %>%
  ggplot(aes(x = reorder(group,correlation), y = correlation)) +
  geom_point(size = 4, colour = "#56018D", alpha = 0.8) +
  #geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
coord_flip() +
  labs(title = "Correlation between crowdsourced and expert labels",
       x = "Demographic Group",
       y = "Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.background = element_rect(fill="white"))
ggsave("expert_correlations_nlp.png",width=7,height=7)


correlations %>% 
  filter(!(group %in% c("woman", "man"))) %>% 
  mutate(group = ifelse(group=="Other","Other ethnicity",group)) %>%
  ggplot(aes(x = reorder(group, correlation), y = correlation, colour = correlation)) +
  geom_point(size = 5, alpha = 0.9) +
  scale_colour_gradient2(
    low = "#FF69B4", 
    mid = "#56018D", 
    high = "#FFFF00", 
    midpoint = 0.5,
    name = "Correlation"
  ) +
  coord_flip() +
  labs(title = "Expert labels are not neutral",
       x = "Demographic Group",
       y = "Correlation Coefficient") +
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
ggsave("expert_correlations_nlp.png",width=8,height=5.5)



# image data ####

natrep<- read_csv("prolific.csv") %>% 
  select(ResponseId,state,income,educ,employment,race,party,Zipcode,strongPartisan,gender,var,varLabel,value,image) %>% 
  filter(educ != "No formal education") %>% 
  filter(varLabel!="Average Income")

#detroit <-  read_csv("dmacs.csv")


#process a little bit 
natrep<-natrep %>% fill(image,.direction="up")

natrep<-natrep %>% 
  mutate(income = case_when(income %in% c("Less than $5,000","$5,000 to $9,999","$10,000 to $14,999",
                                          "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $29,999") ~ "<30k",
                            income %in% c("$30,000 to $34,999","$35,000 to $39,999","$40,000 to $49,999",
                                          "$50,000 to $59,999") ~ "30k - 60k",
                            income %in% c("$60,000 to $74,999","$75,000 to $99,999") ~ "60k - 100k",
                            income %in% c("$100,000 to $124,999","$125,000 to $149,999") ~ "100k - 150k",
                            income == "$150,000 or more" ~ "150k+",
                            TRUE ~ NA)) %>% 
  mutate(educ = case_when(grepl("Graduate",educ) ~ "Graduate",
                          grepl("Bachelor",educ) ~ "College",
                          grepl("Associate",educ) | grepl("no degree",educ) ~ "Some college",
                          grepl("Some education",educ) ~ "No high school",
                          TRUE ~ educ)) %>% 
  mutate(race = case_when(grepl("Alaska",race) | grepl("Native",race) |grepl("Other",race) ~"Other ethnicity",
                          grepl("Black",race) ~ "Black",
                          grepl("Asian",race)~"Asian",
                          grepl("Prefer",race)~ NA,
                          TRUE ~ race)) %>% 
  mutate(party = case_when(grepl("Other",party)~"Independent",
                           TRUE ~ party))

natrep$income =  factor(natrep$income, levels = c("<30k","30k - 60k","60k - 100k","100k - 150k","150k+")) %>% fct_rev()
natrep$educ =  factor(natrep$educ, levels = c("No high school","High school diploma or GED",
                                              "Some college","College","Graduate")) %>% fct_rev()


# between group COR
calculate_between_group_correlations <- function(data, group_var) {
  
  dfs<-list()
  data<-data%>% filter(!is.na(group_var))
  
  for (question in data$varLabel %>% unique){
    data_w <- data %>% filter(varLabel == question) 

    groups <- unique(data_w[[group_var]])
    combinations <- combn(groups, 2, simplify = FALSE)
    
    between_correlations <- map_dfr(combinations, function(pair) {
      # Get mean ratings for each item by group
      group1_ratings <- data_w %>%
        filter(!!sym(group_var) == pair[1]) %>%
        group_by(image) %>%
        summarize(rating = mean(value, na.rm = TRUE),
                  n_annotators1 = n_distinct(ResponseId))
      
      group2_ratings <- data_w %>%
        filter(!!sym(group_var) == pair[2]) %>%
        group_by(image) %>%
        summarize(rating = mean(value, na.rm = TRUE),
                  n_annotators2 = n_distinct(ResponseId))
      
      # Join and calculate correlation
      combined <- inner_join(group1_ratings, group2_ratings, 
                             by = "image", suffix = c("_1", "_2"))
      
      combined<-combined %>% filter(!is.na(rating_1))  %>% filter(!is.na(rating_2)) 
      
      correlation = cor(combined$rating_1, combined$rating_2)
      tibble(
        group1 = pair[1],
        group2 = pair[2],
        correlation = correlation,
        n_items = nrow(combined),
      )
    })
    
    between_correlations$question <- question
    
    dfs[[question]]<-between_correlations %>% filter(n_items > 30) %>% arrange(correlation)
  }
  out <- bind_rows(dfs)
  return(out)
}


# Run for each demographic variable
demographic_vars <- c("educ", "income", "race", "party","gender")
between_group_cors <- lapply(demographic_vars, function(var) {
  results <- calculate_between_group_correlations(natrep, var)
  results$demographic <- var
  return(results)
})

between_group_cors<- lapply(between_group_cors,function(x) x %>% arrange(group1,group2))


# within group cor 
calculate_within_group_correlations <- function(data, group_var, n_iterations = 100) {
  groups <- unique(data[[group_var]])
  
  dfs <- list()
  
  for (question in data$varLabel %>% unique) {
    data_w <- data %>% filter(varLabel == question)
    
    within_correlations <- map_dfr(groups, function(group) {
      # Get data for this group
      group_data <- data_w %>%
        filter(!!sym(group_var) == group)
      
      # Run multiple iterations of split-half
      correlations <- map_dbl(1:n_iterations, function(i) {
        if(nrow(group_data) < 10) {
          return(NA_real_)
        }
        # Get unique annotators for this group
        annotators <- unique(group_data$ResponseId)
        
        # Split annotators randomly into two groups
        split1 <- sample(annotators, size = floor(length(annotators)/2))
        split2 <- setdiff(annotators, split1)
        
        # Calculate mean ratings for each split
        ratings1 <- group_data %>%
          filter(ResponseId %in% split1) %>%
          group_by(image) %>%
          summarize(rating = mean(value, na.rm = TRUE))
        
        ratings2 <- group_data %>%
          filter(ResponseId %in% split2) %>%
          group_by(image) %>%
          summarize(rating = mean(value, na.rm = TRUE))
        
        # Join and calculate correlation
        combined <- inner_join(ratings1, ratings2, 
                               by = "image", suffix = c("_1", "_2"))
        
        cor(combined$rating_1, combined$rating_2)
      })
      
      tibble(
        group = group,
        mean_correlation = mean(correlations,na.rm=T),
        sd_correlation = sd(correlations,na.rm=T),
        n_iterations = n_iterations,
        n_items = n_distinct(group_data$image),
        n_annotators = n_distinct(group_data$ResponseId)  # Added this
      )
    })
    
    within_correlations$question <- question
    
    dfs[[question]] <- within_correlations %>% filter(n_items > 30) 
  }
  
  out <- bind_rows(dfs)
  return(out)
}

# Run for each demographic variable
within_group_cors <- lapply(demographic_vars, function(var) {
  calculate_within_group_correlations(natrep, var)
})

# Combine results into a single dataframe
within_group_results <- bind_rows(within_group_cors)

within_group_results %>% group_by(group) %>% summarise(mean_correlation = mean(mean_correlation),n_items = mean(n_items)) %>% 
  ggplot(aes(x =mean_correlation, y = reorder(group,mean_correlation)))+
  geom_point()+
  labs(x = "Correlation",y="Demographic group",title = "Correlation within groups")
ggsave("within_group_correlations_images.png",width=7,height=7)

## Plotting ####
create_correlation_heatmap <- function(corr_data, title) {
  # Create a single dataframe for all questions
  groups <- unique(c(corr_data$group1, corr_data$group2)) %>% sort
  
  # Split by question and create matrices
  all_melted <- corr_data %>%
    split(.$question) %>%
    lapply(function(question_data) {
      mat <- matrix(NA, 
                    nrow = length(groups),
                    ncol = length(groups))
      
      rownames(mat) <- groups
      colnames(mat) <- groups
      
      # Fill matrix
      for(i in 1:nrow(question_data)) {
        g1 <- which(groups == question_data$group1[i])
        g2 <- which(groups == question_data$group2[i])
        mat[g1, g2] <- question_data$correlation[i]
        mat[g2, g1] <- question_data$correlation[i]
      }
      diag(mat) <- 1
      
      # Convert to long format and add question
      melted <- melt(mat)
      melted$question <- unique(question_data$question)
      return(melted)
    }) %>%
    bind_rows()
  
  # Create faceted plot
  ggplot(all_melted, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", value)), 
              color = ifelse(all_melted$value > 0.7, "white", "black")) +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                         midpoint = 0.5, limit = c(0,1)) +
    facet_wrap(~question) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title,
         x = "", y = "", fill = "Correlation") +
    theme(plot.background = element_rect(fill="white",color = "white"))
}

# Usage:
hm_edu <- create_correlation_heatmap(between_group_cors[[1]] %>% filter(question!="Disorder"), "Education Correlations")
#ggsave("education_correlations.png",width=5,height=7)

hm_in <- create_correlation_heatmap(between_group_cors[[2]]%>% filter(question!="Disorder"), "")
ggsave("income_correlations.png",width=8,height=3.5)

hm_eth <- create_correlation_heatmap(between_group_cors[[3]]%>% filter(question!="Disorder"), "")
ggsave("ethnicicy_correlations.png",width=8,height=3.5)

hm_pa<-create_correlation_heatmap(between_group_cors[[4]]%>% filter(question!="Disorder"), "Party Correlations")
#ggsave("party_correlations.png",width=7,height=7)

hm_gen<- create_correlation_heatmap(between_group_cors[[5]]%>% filter(question!="Disorder"), "Gender Correlations")
#ggsave("gender_correlations.png",width=7,height=7)

x<-arrangeGrob(hm_eth+ theme(legend.position = "none"), hm_in + theme(legend.position = "bottom"),
               heights = c(1,1.2))
ggsave("correlations_image_income_race.png",x,width = 8, height = 8)

hm_in  + labs(title = "Income structures disagreement on labelling tasks")+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", colour = "#2C3E50"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, colour = "#7F8C8D", margin = margin(b = 20)),
    axis.title = element_text(size = 12, face = "bold"),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    legend.title = element_text(size = 10, face = "bold"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20),
    axis.text.x = element_text(angle = 45, hjust = 1)) 
  
ggsave("correlations_image_income.png", width = 8, height = 4)

# Run models ####
demographic_vars <- c("educ", "income", "race", "party","gender")
question_list <- unique(natrep$varLabel)

## image dta ####
out<-data.frame()
for(question in question_list){
  for(pred in demographic_vars){
    
    print(paste0("processing ",question," ", pred))
    
    data_filtered <- natrep %>% filter(varLabel == question) 
    
    model <- lmer(value ~ educ + income + race + party +gender+ (1 | image), 
                  data = data_filtered)
    
    em_formula = paste0("~",pred)
    
    emm_out <- emmeans::emmeans(model, as.formula(em_formula), type = "response", level = 0.95, rg.limit = 26000) %>% tidy %>% 
      pivot_longer(cols = pred) %>% rename(predictor = name, predictor_level = value) %>% mutate(question = question)
    
    out <- rbind(emm_out, out)
  }
}


summary(lm(value ~ educ + income + race + party +gender+ image, data = natrep %>% filter(varLabel == "Safety - Daytime Walking")))$r.squared


model1 <- lmer(value ~ educ + income + race + party +gender+ (1 | image), data = natrep %>% filter(varLabel =="Neighborhood Wealth"))
model2 <- lmer(value ~ educ + income + race + party +gender+ (1 | image), data = natrep %>% filter(varLabel =="Safety - Daytime Walking"))
model3 <- lmer(value ~ educ + income + race + party +gender+ (1 | image), data = natrep %>% filter(varLabel =="Safety - Nighttime Walking"))

r.squaredGLMM(model1)[1, "R2m"]
r.squaredGLMM(model2)[1, "R2m"]
r.squaredGLMM(model3)[1, "R2m"]



pred_variables<- demographic_vars
out<-out %>% left_join(., 
                       natrep %>% distinct(ResponseId, .keep_all = T) %>% rename(answer = value) %>% 
                         pivot_longer(cols = all_of(pred_variables)) %>% 
                         group_by(value) %>% summarise(n_respondents=n()), 
                       by = c("predictor_level" = "value"))


out_f<-out %>% filter(n_respondents>29) %>% 
  mutate(lower = estimate - qt(0.975, df) * std.error,
         upper = estimate + qt(0.975, df) * std.error) %>% 
  mutate(BY = question) %>% 
  mutate(feature = as.factor(predictor), level = predictor_level %>% as.factor) 


## nlp data ####
# Define demographic variables
demographic_vars <- c("age_group", "gender", "ethnicity", "education")

# Run models
out <- data.frame()

# Since you only have one question/outcome (litw), we don't need the question loop
for(pred in demographic_vars) {
  print(paste0("processing predictor: ", pred))
  
  # Fit model with all demographics
  model <- lmer(litw ~ age_group + gender + ethnicity + education + (1 | session_id), 
                data = nlp_w)
  
  # Create emmeans formula
  em_formula <- paste0("~", pred)
  
  # Get estimated marginal means
  emm_out <- emmeans::emmeans(model, as.formula(em_formula), type = "response", level = 0.95) %>% 
    tidy %>% 
    pivot_longer(cols = pred) %>% 
    rename(predictor = name, predictor_level = value) %>% 
    mutate(question = "litw")
  
  out <- rbind(emm_out, out)
}

# Add respondent counts
out <- out %>% 
  left_join(
    nlp_w %>% 
      distinct(session_id, .keep_all = TRUE) %>% 
      pivot_longer(cols = all_of(demographic_vars)) %>% 
      group_by(value) %>% 
      summarise(n_respondents = n()), 
    by = c("predictor_level" = "value")
  )

# Filter and add confidence intervals
nlp_f <- out %>% 
  mutate(
    lower = estimate - qt(0.975, df) * std.error,
    upper = estimate + qt(0.975, df) * std.error
  ) %>% 
  mutate(
    BY = question,
    feature = as.factor(predictor), 
    level = predictor_level %>% as.factor
  )


#Make plots ####

plot.cj_mm <- 
  function(
    x,
    group = attr(x, "by"),
    feature_headers = TRUE,
    header_fmt = "(%s)",
    size = 1.0,
    xlab = "Marginal Mean",
    ylab = "",
    legend_title = if (is.null(group)) "Feature" else group,
    legend_pos = "bottom",
    xlim = NULL,
    vline = 0,
    vline_color = "gray",
    theme = ggplot2::theme_bw(),
    ...
  ) {
    
    make_feature_headers <- function(x, fmt = "(%s)") {
      feature_levels <- rev(split(x$level, x$feature))
      for (i in seq_along(feature_levels)) {
        feature_levels[[i]] <- levels(x$level)[match(feature_levels[[i]], levels(x$level))]
        feature_levels[[i]] <- c(feature_levels[[i]], sprintf(fmt, names(feature_levels)[i]))
      }
      factor(as.character(x$level), levels = unique(unname(unlist(feature_levels))))
    }
    
    # optionally, add gaps between features
    if (isTRUE(feature_headers)) {
      x$level <- make_feature_headers(x, fmt = header_fmt)
      to_merge <- data.frame(feature = unique(x$feature), level = sprintf(header_fmt, unique(x$feature)))
      if ("BY" %in% names(x)) {
        to_merge <- do.call("rbind", lapply(unique(x[["BY"]]), function(lev) {
          to_merge[["BY"]] <- lev
          to_merge
        }))
      } else if (!is.null(group)) {
        to_merge <- do.call("rbind", lapply(unique(x[[group]]), function(lev) {
          to_merge[[group]] <- lev
          to_merge
        }))
      }
      x <- merge(x, to_merge, all = TRUE)
    }
    
    x <- droplevels(x)
    
    if (is.null(group)) {
      p <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "estimate", y = "level", colour = "feature"))
    } else {
      if (is.null(x[[group]])) {
        stop(sprintf("`group` variable '%s' not found", group))
      }
      p <- ggplot2::ggplot(data = x, ggplot2::aes_string(x = "estimate", y = "level", colour = group, group = group))
    }
    
    if (is.null(xlim)) {
      xmin <- min(x$lower, na.rm = TRUE)
      xmin <- if (xmin < 0) 1.04*xmin else .96*xmin
      xmax <- max(x$upper, na.rm = TRUE)
      xmax <- if (xmax > 0) 1.04*xmax else .96*xmax
      xlim <- c(xmin, xmax)
    }
    
    if (!is.null(vline)) {
      p <- p + ggplot2::geom_vline(xintercept = vline, colour = vline_color)
    }
    
    p <- p + ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), size = size, na.rm = TRUE) +
      ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                              size = 0.2, height = 0, na.rm = TRUE,
                              position = ggstance::position_dodgev(height = 0.75))
    if (is.null(group)) {
      p <- p + ggplot2::scale_colour_discrete(guide = ggplot2::guide_legend(title = legend_title))
    } else {
      p <- p + ggplot2::scale_colour_discrete(breaks = levels(x[[group]]),
                                              labels = levels(x[[group]]),
                                              guide = ggplot2::guide_legend(title = legend_title))
    }
    p <- p +
      ggplot2::scale_x_continuous(limits = xlim, oob = scales::rescale_none) +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylab) + 
      theme + ggplot2::theme(
        legend.position = legend_pos,
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) + 
      ggplot2::guides(colour = ggplot2::guide_legend(title = legend_title))
    return(p)
  }


mm_night <- out_f %>%   filter(grepl("Night",question)) %>% 
  plot.cj_mm + facet_wrap(~BY,scales = "free_x",drop = T)+
  theme(legend.position = "none", axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "")
mm_day <- out_f %>%   filter(grepl("Day",question)) %>% 
  plot.cj_mm + facet_wrap(~BY,scales = "free_x",drop = T)+
  theme(legend.position = "none") +
  labs(title = "")
#ggsave("mm_safety_night.png",width=5,height=7)

mm_weal<- out_f %>%   filter(grepl("Wealth",question)) %>% 
  plot.cj_mm + facet_wrap(~BY,scales = "free_x",drop = T)+
  theme(legend.position = "none", axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "")
#ggsave("mm_wealth.png",width=5,height=7)

mm_dis <- out_f %>%   filter(grepl("Disorder",question)) %>% 
  plot.cj_mm + facet_wrap(~BY,scales = "free_x",drop = T)+
  theme(legend.position = "none", axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "")
#ggsave("mm_disorder.png",width=5,height=7)

x<-arrangeGrob(mm_day,mm_night, mm_weal, ncol = 3, widths = c(1.7, 1, 1))
ggsave("mm_combined_plot.png", x, width = 8, height = 5, dpi = 300)

# Create plot
p <- nlp_f %>% 
  plot.cj_mm + 
  facet_wrap(~BY, scales = "free_x", drop = TRUE) +
  theme(legend.position = "none") +
  labs(title = "Marginal means: LITW scores by demographic groups")

# Save plot
ggsave("mm_litw.png", p, width = 7, height = 7)

# basic anova ####
# basic anova with ICC included ####
analyze_demographic_numeric <- function(data, demographic_col) {
  # Fit model
  model <- lmer(value ~ factor(get(demographic_col)) + (1|image), data = data)
  
  # Get anova result
  anova_result <- anova(model)
  
  # Calculate p-value from F statistic
  df_denominator <- nrow(data) - length(unique(data[[demographic_col]])) - 1
  f_value <- anova_result$`F value`[1]
  p_value <- pf(f_value, 
                df1 = anova_result$npar[1] - 1,
                df2 = df_denominator,
                lower.tail = FALSE)
  
  eta_sq <- effectsize::eta_squared(model)
  icc_value <- performance::icc(model)
  
  return(data.frame(
    demographic = demographic_col,
    effect_size = eta_sq$Eta2_partial[1],
    f_value = f_value,
    p_value = p_value,
    icc = icc_value$ICC_adjusted, # Add ICC to the output
    n = nrow(data)
  ))
}

ano <- list()
icc_values <- data.frame() # Create a dataframe to store ICC values

for (var in unique(natrep$varLabel)) {
  data_subset <- natrep[natrep$varLabel == var, ]
  
  demographic_effects <- map_dfr(
    c("income", "educ", "gender", "race", "party"),
    ~analyze_demographic_numeric(data_subset, .x)
  ) %>%
    mutate(p_adj = p.adjust(p_value, method = "bonferroni")) %>%
    arrange(desc(effect_size))
  
  demographic_effects$var <- var
  
  # Store the ICC value (should be the same for all demographics within a var)
  icc_values <- rbind(icc_values, data.frame(
    var = var,
    icc = demographic_effects$icc[1]
  ))
  
  ano[[var]] <- demographic_effects
}

ano <- bind_rows(ano) %>% 
  mutate(sig = case_when(p_adj < 0.05 ~ "p<0.05",
                         p_adj > 0.05 ~ "",
                         f_value < 3 ~ "",
                         f_value > 3 ~ "p<0.05") %>% factor) 


ggplot(ano, aes(x = effect_size, y = demographic))+
  geom_point(aes(color = sig), size = 3)+
  facet_wrap(~var)+
  scale_x_continuous(labels = scales::percent)+
  labs(x = "Variance explained",y = "Demographic group")+
  theme(legend.position = "none")
ggsave("images_anova.png",width=7,height=7)
  

p3 <- ggplot(ano, aes(x = effect_size, y = demographic)) +
  geom_point(aes(color = sig), size = 3) +
  facet_wrap(~var) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Variance explained by demographics", y = "Demographic group") +
  theme(legend.position = "none") +
  geom_text(data = icc_values, 
            aes(x = max(ano$effect_size) * 0.9, 
                y = "educ", 
                label = paste0("ICC: ", scales::percent(icc, accuracy = 0.1))),
            hjust = 1, vjust = -1)


## for nlp ####
analyze_demographic_numeric <- function(data, demographic_col) {
  # Fit model
  model <- lmer(litw ~ factor(get(demographic_col)) + (1|action), data = data)
  
  # Get anova result
  anova_result <- anova(model)
  
  # Calculate p-value from F statistic
  # We need denominator degrees of freedom - can get from model
  df_denominator <- nrow(data) - length(unique(data[[demographic_col]])) - 1
  f_value <- anova_result$`F value`[1]
  p_value <- pf(f_value, 
                df1 = anova_result$npar[1] - 1,
                df2 = df_denominator,
                lower.tail = FALSE)
  
  eta_sq <- effectsize::eta_squared(model)
  icc <- performance::icc(model)
  
  return(data.frame(
    demographic = demographic_col,
    effect_size = eta_sq$Eta2_partial[1],
    f_value = f_value,
    p_value = p_value,
    n = nrow(data)
  ))
}

demographic_effects <- map_dfr(
    c("gender", "ethnicity", "education", "age_group"),
    ~analyze_demographic_numeric(nlp_w, .x)
  ) %>%
    mutate(p_adj = p.adjust(p_value, method = "bonferroni")) %>%
    arrange(desc(effect_size)) %>% 
  mutate(p_adj = ifelse(is.na(p_adj),1,p_adj),
    sig=ifelse(p_adj < 0.05, "p<0.05",0))
  

ggplot(demographic_effects, aes(x = effect_size, y = demographic))+
  geom_point(aes(color = sig), size = 3)+
  scale_x_continuous(labels = scales::percent)+
  labs(x = "Variance explained",y = "Demographic group")+
  theme(legend.position = "none")
ggsave("nlp_anova.png",width=7,height=7)






