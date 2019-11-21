#### Purpose ####
# Post-stratify the estimates.


#### Contact ####
# Author: Rohan Alexander
# Contact: rohan.alexander@utoronto.ca
# Last updated: 18 November 2019


#### Workspace setup ####
# Call the necessary packages
library(brms)
library(lme4)
library(tidybayes)
library(tidyverse)

poststratification_data <- read_csv("outputs/data/poststratification_data/census_data.csv")
preferences_data <- read_csv("outputs/data/data_from_earlier_elections_on_2019_div_basis.csv")
results_2019_TPP <- read_csv("inputs/data/election_2019/HouseTppByDivisionDownload-24310.csv",
                             skip = 1)
results_2019_first_prefs <- read_csv("inputs/data/election_2019/HouseFirstPrefsByCandidateByVoteTypeDownload-24310.csv",
                                     skip = 1)


#### Post-stratify ####
LinA_model <- readRDS("outputs/models/LinA.rds")
SmartVote_model <- readRDS("outputs/models/SmartVote.rds")

# First LinA
# Based on https://www.monicaalexander.com/posts/2019-08-07-mrp/
post_stratified_posterior_estimates_LinA <- 
  LinA_model %>% 
  tidybayes::add_predicted_draws(newdata = poststratification_data, allow_new_levels = TRUE) %>% 
  rename(alp_predict = .prediction) %>% 
  mutate(alp_predict_prop = alp_predict*cell_proportion_of_division_total) %>% 
  group_by(division, .draw) %>% 
  summarise(alp_predict = sum(alp_predict_prop)) %>% 
  ungroup()

# Now Smartvote
# Account for no education data yet.
poststratification_data_smartvote <- 
  poststratification_data %>% 
  group_by(division, gender, age_group) %>% 
  summarise(number = sum(number),
            cell_proportion_of_division_total = sum(cell_proportion_of_division_total))

post_stratified_posterior_estimates_Smartvote <- 
  SmartVote_model %>% 
  tidybayes::add_predicted_draws(newdata = poststratification_data_smartvote, allow_new_levels = TRUE) %>% 
  rename(alp_predict = .prediction) %>% 
  mutate(alp_predict_prop = alp_predict*cell_proportion_of_division_total) %>% 
  group_by(division, .draw) %>% 
  summarise(alp_predict = sum(alp_predict_prop)) %>% 
  ungroup()

# Combine them
post_stratified_posterior_estimates_Smartvote$type <- "Smartvote"
post_stratified_posterior_estimates_LinA$type <- "LinA"

post_stratified_posterior_estimates_both <- rbind(
  post_stratified_posterior_estimates_Smartvote,
  post_stratified_posterior_estimates_LinA)

rm(post_stratified_posterior_estimates_Smartvote,
   post_stratified_posterior_estimates_LinA,
   poststratification_data,
   poststratification_data_smartvote,
   LinA_model,
   SmartVote_model
   )

# Change the name to be consistent with the 2PP data
post_stratified_posterior_estimates_both <- 
  post_stratified_posterior_estimates_both %>% 
  rename(alp_first_pref_prop = alp_predict)

post_stratified_posterior_estimates_both$year = "2019"
post_stratified_posterior_estimates_both$year <- as.integer(post_stratified_posterior_estimates_both$year)


#### Two party preferred estimates ####
preferences_data <- 
  preferences_data %>% 
  rename(alp_first_pref_prop = first_prefs_percent)

tpp_to_first_prefs_reg <- glm(ALP_2PP ~ alp_first_pref_prop + division + year, 
                              data = preferences_data, 
                              family = binomial())

post_stratified_posterior_estimates_both$estimated_2PP <- 
  predict(tpp_to_first_prefs_reg, 
          newdata = post_stratified_posterior_estimates_both,
          type = "response"
          )


#### Add actual 2019 results ####
results_2019_TPP <- 
  results_2019_TPP %>% 
  rename(actual2PP = `Australian Labor Party Percentage`,
         division = DivisionNm,
         state = StateAb) %>% 
  mutate(actual2PP = actual2PP / 100) %>% 
  select(state, division, actual2PP)

post_stratified_posterior_estimates_both <- 
  post_stratified_posterior_estimates_both %>% 
  left_join(results_2019_TPP, by = "division")

results_2019_first_prefs <- 
  results_2019_first_prefs %>% 
  rename(division = DivisionNm) %>% 
  group_by(division) %>% 
  mutate(division_total = sum(TotalVotes)) %>% 
  filter(PartyAb == "ALP") %>% 
  mutate(actual_alp_first_pref_prop = TotalVotes / division_total) %>% 
  select(division, actual_alp_first_pref_prop)

post_stratified_posterior_estimates_both <- 
  post_stratified_posterior_estimates_both %>% 
  left_join(results_2019_first_prefs, by = "division")

post_stratified_posterior_estimates_both <- 
  post_stratified_posterior_estimates_both %>% 
  rename(estimated_alp_first_pref_prop = alp_first_pref_prop) %>% 
  select(type, 
         division, 
         state, 
         estimated_alp_first_pref_prop,
         estimated_2PP,
         actual_alp_first_pref_prop,
         actual2PP
         )

# We now want summary measures - i.e. ones that aren't dependent on the draws
post_stratified_estimates_summary <- 
  post_stratified_posterior_estimates_both %>% 
  group_by(type, state, division) %>% 
  summarise(mean_est_first_prefs = mean(estimated_alp_first_pref_prop), 
            lower_est_first_prefs = quantile(estimated_alp_first_pref_prop, 0.05), 
            upper_est_first_prefs = quantile(estimated_alp_first_pref_prop, 0.95),
            # low_est_first_pres = min(estimated_alp_first_pref_prop),
            # high_est_first_pres = max(estimated_alp_first_pref_prop), 
            actual_first_prefs = mean(actual_alp_first_pref_prop),
            mean_est_2PP = mean(estimated_2PP), 
            lower_est_2PP = quantile(estimated_2PP, 0.05), 
            upper_est_2PP = quantile(estimated_2PP, 0.95),
            actual_2PP = mean(actual2PP)
  ) %>% 
  ungroup()
# 
# 
# # Add state back in
# divs_and_states <- read_csv("inputs/data/misc/number_to_name_divisions.csv") %>% 
#   select(name, state) %>% 
#   rename(division = name)
# 
# post_stratified_estimates_summary <- 
#   post_stratified_estimates_summary %>% 
#   left_join(divs_and_states, by = "division")


post_stratified_estimates_summary %>% 
  arrange(type, state, division) %>% 
  mutate(state = factor(state, levels=unique(state)),
         division = factor(division, levels=unique(division))) %>% 
  # filter(division %in% c("Adelaide", "Aston", "Ballarat", "Banks", "Barker")) %>%
  ggplot(aes(x = division)) +
  geom_linerange(aes(
    ymin = lower_est_first_prefs,
    ymax = upper_est_first_prefs
  )) +
  geom_point(aes(y = mean_est_first_prefs)) +
  geom_point(aes(y = actual_first_prefs), color = "red") +
  ggforce::facet_row(vars(state),
                     # nrow = 1,
                     scales = "free_x",
                     space = "free",
                     strip.position = 'bottom'
  ) +
  ggforce::facet_col(vars(type),
                     # nrow = 1,
                     scales = "free_x",
                     space = "free",
                     strip.position = 'bottom'
  ) +
  # remove facet spacing on x-direction
  theme(panel.spacing.x = unit(0,"line")) +
  # switch the facet strip label to outside 
  # remove background color
  theme(strip.placement = 'outside',
        strip.background.x = element_blank()) +
  labs(
    x = "Division",
    y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


post_stratified_estimates_summary %>% 
  filter(division %in% c("Adelaide", "Aston", "Ballarat", "Banks", "Barker")) %>%
  ggplot(aes(x = factor(division))) +
  geom_linerange(aes(
    ymin = lower_est_2PP,
    ymax = upper_est_2PP
  )) +
  geom_point(aes(y = mean_est_2PP)) +
  geom_point(aes(y = actual_2PP), color = "red") +
  facet_wrap(vars(type), nrow = 2)


# post_stratified_posterior_estimates %>% 
#   filter(division %in% c("Adelaide", "Aston", "Ballarat", "Banks", "Barker")) %>%
#   ggplot(aes(x = factor(division))) +
#   geom_dotplot(aes(y = estimated_2PP), binaxis = "y", stackdir = "center", binpositions="all",
#                dotsize = 0.25,
#                binwidth = 0.005) +
#   geom_point(aes(y = actual2PP), color = "red")
# 
# post_stratified_posterior_estimates %>% 
#   filter(division %in% c("Adelaide", "Aston", "Ballarat", "Banks", "Barker")) %>%
#   ggplot(aes(x=estimated_2PP, y=division)) +
#   ggridges::stat_density_ridges(quantile_lines = TRUE,
#                                 # quantiles = TRUE,
#                                 # jittered_points=TRUE, 
#                                 scale = .95,
#                                 # rel_min_height = .01,
#                                 # point_shape = "|", 
#                                 # point_size = 3, 
#                                 # size = 0.25,
#                                 # position = ggridges::position_points_jitter(height = 0)
#                                 ) +
#   geom_point(aes(x = actual2PP), color = "red") +
#   wrap

#### Add derived data and clean up ####
post_stratified_estimates_summary <- 
  post_stratified_estimates_summary %>% 
  mutate(forecast_alp_win = if_else(mean_est_2PP > 0.5, 1, 0),
         actual_alp_win = if_else(actual_2PP > 0.5, 1, 0))

post_stratified_estimates_summary <- 
  post_stratified_estimates_summary %>% 
  mutate(forecast_type = case_when(mean_est_2PP < 0.4 ~ "Safe loss",
                                   mean_est_2PP < 0.44 ~ "Fairly safe loss",
                                   mean_est_2PP < 0.47 ~ "Marginal loss",
                                   mean_est_2PP < 0.53 ~ "Too close to call",
                                   mean_est_2PP < 0.56 ~ "Marginal win",
                                   mean_est_2PP < 0.60 ~ "Fairly safe win",
                                   mean_est_2PP < 1 ~ "Safe win",
                                   TRUE ~ "OH NO"),
         actual_type = case_when(actual_2PP < 0.4 ~ "Safe loss",
                                 actual_2PP < 0.44 ~ "Fairly safe loss",
                                 actual_2PP < 0.47 ~ "Marginal loss",
                                 actual_2PP < 0.53 ~ "Too close to call",
                                 actual_2PP < 0.56 ~ "Marginal win",
                                 actual_2PP < 0.60 ~ "Fairly safe win",
                                 actual_2PP < 1 ~ "Safe win",
                                 TRUE ~ "OH NO")
  )

post_stratified_estimates_summary$forecast_type %>% unique()
post_stratified_estimates_summary$actual_type %>% unique()

write_csv(post_stratified_estimates_summary, "outputs/data/estimates/estimates.csv")

post_stratified_estimates_summary %>% 
  group_by(type, actual_alp_win) %>% 
  count(forecast_alp_win)

post_stratified_estimates_summary %>% 
  # group_by(type) %>% 
  janitor::tabyl(forecast_alp_win, actual_alp_win, type)


