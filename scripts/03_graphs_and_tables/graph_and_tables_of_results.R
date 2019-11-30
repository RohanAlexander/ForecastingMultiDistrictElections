#### Purpose ####
# Post-stratify the estimates.


#### Contact ####
# Author: Rohan Alexander
# Contact: rohan.alexander@utoronto.ca
# Last updated: 20 November 2019


#### Workspace setup ####
# Call the necessary packages
library(tidyverse)


#### Summarise and explain the model results ####
# Compare the model estimates with the raw data
# Read in the data
LINA_regression_data <- read_csv("outputs/data/regression_data/LinA.csv")
SmartVote_regression_data <- read_csv("outputs/data/regression_data/SmartVote.csv")
post_stratified_estimates_summary <- read_csv("outputs/data/estimates/estimates.csv")

post_stratified_estimates_summary <- 
  post_stratified_estimates_summary %>% 
  select(-forecast_alp_win, -actual_alp_win)

raw_Smartvote_estimates <- 
  SmartVote_regression_data %>% 
  group_by(division, ALP_supporter) %>%
  summarise(n = n()) %>%
  group_by(division) %>%
  mutate(prop = n/sum(n)) %>% 
  filter(ALP_supporter==1) %>% 
  rename(raw_ALP_support = prop)

raw_Smartvote_estimates$type <- "Smartvote"

raw_LinA_estimates <- 
  LINA_regression_data %>% 
  group_by(division, ALP_supporter) %>%
  summarise(n = n()) %>%
  group_by(division) %>%
  mutate(prop = n/sum(n)) %>% 
  filter(ALP_supporter==1) %>% 
  rename(raw_ALP_support = prop)

raw_LinA_estimates$type <- "LinA"

both_raw_estimates <- 
  rbind(raw_LinA_estimates, raw_Smartvote_estimates) %>% 
  select(type, division, raw_ALP_support)

post_stratified_estimates_summary <- 
  post_stratified_estimates_summary %>% 
  left_join(both_raw_estimates, by = c("division", "type"))

post_stratified_estimates_summary <- 
  post_stratified_estimates_summary %>% 
  select(type, division, lower_est_first_prefs, upper_est_first_prefs, mean_est_first_prefs, raw_ALP_support)

both <- 
  post_stratified_estimates_summary %>% 
  pivot_longer(c(raw_ALP_support, mean_est_first_prefs), 
               names_to = "source", values_to = "value") %>% 
  rename(lower = lower_est_first_prefs,
         upper = upper_est_first_prefs)
  
both <- 
  both %>% 
  rename(haha = type) %>% 
  rename(type = source) %>% 
  rename(source = haha)

both$type[both$type == "mean_est_first_prefs"] <- "Estimate"
both$type[both$type == "raw_ALP_support"] <- "Raw"

both$source[both$source == "LinA"] <- "Life in Australia"
both$source[both$source == "Smartvote"] <- "Smartvote Australia"

both %>% 
  ggplot(aes(x = forcats::fct_inorder(division))) + 
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper), 
                width = 0, 
                colour = "grey") + 
  geom_point(aes(y = value, color = type)) +
  # geom_point(aes(y = raw_ALP_support),
  #            color = "red") +
  labs(
    x = "Division",
    y = "First preference share",
    color = "Survey: ") +
  # scale_color_manual(name = "", values = c("MRP estimate" = "black", "SmartVote raw data" = "red")) + 
  theme_minimal() +
  facet_wrap(vars(source),
             nrow = 2) +
  # ylim(0,0.8) +
  scale_x_discrete(breaks = forcats::fct_inorder(both$division)[seq(1, 302, by = 4)])+
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom")
  # coord_flip()

ggsave("outputs/figures/estimates/raw_data_compared_with_model_estimates.pdf",
       width = 20, 
       height = 20, 
       units = "cm"
)


#### Compare estimates with actual groupings ####
# E.g. safe, marginal, too close to call, etc
# Read in the data
post_stratified_estimates_summary <- read_csv("outputs/data/estimates/estimates.csv") %>% 
  select(type, division, forecast_type, actual_type)



# LinA
post_stratified_estimates_summary %>% 
  filter(type == "LinA") %>% 
  janitor::tabyl(forecast_type, actual_type) %>% 
  select("forecast_type", 
         "Safe loss",
         "Fairly safe loss", 
         "Marginal loss", 
         "Too close to call",
         "Marginal win",
         "Fairly safe win",
         "Safe win") %>% 
  mutate(ordering = c(2, 6, 3, 5, 1, 7, 4)) %>% 
  arrange(ordering) %>% 
  select(-ordering) %>% 
  rename(`Estimated/Actual` = forecast_type) %>% 
  write_csv("outputs/data/estimates/LinA_outcome_by_class.csv")

# Smartvote
post_stratified_estimates_summary %>% 
  filter(type == "Smartvote") %>% 
  janitor::tabyl(forecast_type, actual_type) %>% 
  select("forecast_type", 
         "Safe loss",
         "Fairly safe loss", 
         "Marginal loss", 
         "Too close to call",
         "Marginal win",
         "Fairly safe win",
         "Safe win") %>% 
  mutate(ordering = c(1, 5, 2, 4, 6, 3)) %>% 
  arrange(ordering) %>% 
  select(-ordering) %>% 
  rename(`Estimated/Actual` = forecast_type) %>% 
  write_csv("outputs/data/estimates/Smartvote_outcome_by_class.csv")


#### Count classifications by type ####
# E.g. safe, marginal, too close to call, etc
# By Smartvote, LinA, and actual
# Read in the data
post_stratified_estimates_summary <- read_csv("outputs/data/estimates/estimates.csv") %>% 
  select(type, division, forecast_type, actual_type)

forecasts <- 
  post_stratified_estimates_summary %>% 
  select(type, division, forecast_type) %>% 
  rename(classification = forecast_type)

actual <- 
  post_stratified_estimates_summary %>% 
  filter(type == "LinA") %>% 
  select(division, actual_type) %>% 
  mutate(type = "Actual") %>% 
  rename(classification = actual_type)

both <- 
  rbind(forecasts, actual)

both <- 
  both %>% 
  count(type, classification) %>% 
  pivot_wider(id_cols = c(type, classification),
              names_from = type,
              values_from = n
  ) %>% 
  rename(Classification = classification,
         `LinA-based` = LinA,
         `SmartVote-based` = Smartvote) %>% 
  mutate(ordering = c(2, 6, 3, 5, 1, 7, 4)) %>% 
  arrange(ordering) %>% 
  select(-ordering) 

both <- replace_na(both, list(`SmartVote-based` = 0))

both$Share <- 
  c("<0.4",
    "0.4-0.44",
    "0.44-0.47",
    "0.47-0.53",
    "0.53-0.57",
    "0.57-0.6",
    ">0.6")

both <- 
  both %>% 
  select(Classification, Share, everything())

write_csv(both, "outputs/data/estimates/all_outcome_by_class.csv")



#### Make graphs of actual vs estimated results ####
# Read in the data
post_stratified_estimates_summary <- read_csv("outputs/data/estimates/estimates.csv")

# Change the data into the format that we want
# First is 2PP
part1 <- 
  post_stratified_estimates_summary %>% 
  select(type, division, actual_2PP, mean_est_2PP) %>% 
  rename(actual = actual_2PP,
         estimate = mean_est_2PP,
         source = type) %>% 
  mutate(type = "Two party preferred")

# Second is first preferences
part2 <- 
  post_stratified_estimates_summary %>% 
  select(type, division, actual_first_prefs, mean_est_first_prefs) %>% 
  rename(actual = actual_first_prefs,
         estimate = mean_est_first_prefs,
         source = type) %>% 
  mutate(type = "First preferences")

# Combine them
both <- 
  rbind(part1, part2) %>% 
  arrange(desc(type)) %>% 
  mutate(type = factor(type, levels = c("First preferences", "Two party preferred")))
# Just need to convert to factors so that the order in the facets is right

rm(part1, part2)

both$source[both$source == "LinA"] <- "Life in Australia"
both$source[both$source == "Smartvote"] <- "Smartvote Australia"

# Make the graph
ggplot(data = both, 
       aes(x = actual, y = estimate)) + 
  facet_wrap(vars(source, type), nrow = 1) +
  geom_jitter() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  xlab("Actual share") + 
  ylab("Estimated share") + 
  xlim(0, 0.8) +
  ylim(0, 0.8) +
  theme_minimal()

# Save the graph
ggsave("outputs/figures/estimates/actual_vs_estimates.pdf", 
       width = 20, 
       height = 10, 
       units = "cm")

