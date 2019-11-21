#### Preamble ####
# Purpose: This file analyses survey results 
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 18 March 2019
# Prerequisites: Need the the survey results
# To do:
# - 
# Misc:
# -


#### Set up workspace ####
library(brms)
library(tidybayes)
library(tidyverse)
# update.packages()


#### Create example polling data ####
# Just use the sample that we created in the example MRP model ("scripts/example/MRP_model.R")
example_poll <- read_csv("outputs/data/example_poll_data.csv")


#### Train individual level model ####
individual_level_fit <- brm(formula = supports_ALP ~ 
                              (1 | is_male) + 
                              (1 | in_age_group),
                            data = example_poll, 
                            family = bernoulli(link = "logit")
                            # prior=c(set_prior("normal(0,0.2)", class='b'),
                            #         set_prior("normal(0,0.2)", class='sd', group="race.female"),
                            #         set_prior("normal(0,0.2)", class='sd', group="age.cat")),
                            )

# Maybe add the stuff from https://timmastny.rbind.io/blog/multilevel-mrp-tidybayes-brms-stan/ here
summary(individual_level_fit, waic = TRUE)
ranef(individual_level_fit)
stanplot(individual_level_fit)
plot(individual_level_fit)


#### Post-stratify individual level results ####
example_data <- read_csv("outputs/data/example_data_for_post-stratification.csv")

# Thanks to https://timmastny.rbind.io/blog/multilevel-mrp-tidybayes-brms-stan/ for the code
post_stratified_ALP_support_by_division <- individual_level_fit %>%
  add_predicted_draws(newdata = example_data, allow_new_levels=TRUE) %>%
  rename(support_ALP = .prediction) %>%
  mean_qi() %>%
  mutate(support_ALP = support_ALP * proportion) %>%
  group_by(division) %>%
  summarise(support_ALP = sum(support_ALP))


#### Train primary to 2PP model ####
# Just use the OLS model sample that we created in the example model ("scripts/analyse_first_prefs_to_2pp.R")
ALP_voting_data <- read_csv("outputs/data/ALP_voting_data.csv")

ALP_voting_data <- ALP_voting_data %>% 
  mutate(flag = if_else(Year >=2010 & DivisionNm %in% c("Melbourne", "Denison"), 1, 0)) %>% 
  filter(flag == 0) %>% 
  dplyr::select(-flag)

ALP_voting_data <- ALP_voting_data %>% 
  filter(DivisionNm != "Total")

ols_fit <- lm(TwoPartyPreferred ~ CandidateShareFirstPrefs + factor(Year) + factor(HistoricElected),
              data = ALP_voting_data)


#### Apply the primary to 2PP model to the individual level model results ####
# Just pretend its for 2016
post_stratified_ALP_support_by_division$year <- 2016

post_stratified_ALP_support_by_division <- post_stratified_ALP_support_by_division %>% 
  filter(division != "Total")

post_stratified_ALP_support_by_division <- post_stratified_ALP_support_by_division %>% 
  rename(DivisionNm = division, Year = year)

post_stratified_ALP_support_by_division <- post_stratified_ALP_support_by_division %>% 
  left_join(ALP_voting_data, by = c("DivisionNm", "Year"))
  
post_stratified_ALP_support_by_division$CandidateShareFirstPrefs <- NULL

post_stratified_ALP_support_by_division <- post_stratified_ALP_support_by_division %>% 
  rename(CandidateShareFirstPrefs = support_ALP)

predict.lm(ols_fit, post_stratified_ALP_support_by_division, se.fit = TRUE)

# These are obviously insanely high, but that's because the generated data were high
