library(tidyverse)
x<- read_csv("pldl_sim_results.csv")

x_ci <- x %>% 
  pivot_longer(cols = c(bias, rmse), names_to = "metric", values_to = "value") %>% 
  group_by(alpha, delta, method, metric) %>% 
  summarise(
    mean = mean(value),
    se   = sd(value) / sqrt(n()),
    .groups = "drop"
  ) %>% 
  mutate(lo = mean - 1.96 * se,
         hi = mean + 1.96 * se) %>% 
  mutate(metric = ifelse(metric == "bias", "Bias", "RMSE"))


x_ci %>% mutate(delta = ifelse(delta == 0.72, "5% mean disagreement", "15% mean disagreement")) %>% 
  mutate(method = case_when(method == "Weighted" ~ "Naive weighting",
                            method == "PLDL-Weighted" ~ "Pool then weight",
                            TRUE ~ method)) %>%
  mutate(delta = factor(delta, levels = c("5% mean disagreement", "15% mean disagreement"))) %>% 
  ggplot(aes(alpha, mean, colour = method, group = method)) +
  # First layer: all lines except Weighted in bias panel
  geom_line(data = . %>% filter(!(metric == "Bias" & method == "Naive weighting"))) +
  # Second layer: just the Weighted line in bias panel with manual x offset
  geom_line(data = . %>% filter(metric == "Bias" & method == "Naive weighting"), 
            aes(x = alpha + 0.01, linetype = "solid")) +
  # Points: all except Weighted in bias panel
  geom_point(data = . %>% filter(!(metric == "Bias" & method == "Naive weighting"))) +
  # Points: just Weighted in bias panel with x offset
  geom_point(data = . %>% filter(metric == "Bias" & method == "Naive weighting"),
             aes(x = alpha + 0.01)) +
  # Error bars: all except Weighted in bias panel
  geom_errorbar(data = . %>% filter(!(metric == "Bias" & method == "Naive weighting")),
                aes(ymin = lo, ymax = hi), width = 0.02) +
  # Error bars: just Weighted in bias panel with x offset
  geom_errorbar(data = . %>% filter(metric == "Bias" & method == "Naive weighting"),
                aes(x = alpha + 0.01, ymin = mean - se, ymax = mean + se), width = 0.02) +
  facet_grid(metric~delta, scales = "free_y") +
  scale_linetype_manual(values = c("dashed" = "dashed")) +
  guides(linetype = "none") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Pooling then weighting pays off sometimes",
       x = "Labelling population skew",
       y="") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", colour = "#2C3E50"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, colour = "#7F8C8D", margin = margin(b = 20)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("pldlsim.png",width = 8, height = 6)










