#### Purpose ####
# Run an MRP model on the SmartVote survey results.


#### Contact ####
# Author: Rohan Alexander
# Contact: rohan.alexander@utoronto.ca
# Last updated: 29 September 2010


#### Workspace setup ####
# Call the necessary packages
library(brms)
library(lme4)
library(tidybayes)
library(tidyverse)
library("rstan") # observe startup messages
rstan_options(auto_write = TRUE)
# install.packages("rstan")

number_of_cores <- parallel::detectCores() # Used to parallelise brms

SmartVote_regression_data <- read_csv("outputs/data/regression_data/SmartVote.csv")
poststratification_data <- read_csv("outputs/data/poststratification_data/census_data.csv")
preferences_data <- readr::read_csv("outputs/data/data_from_earlier_elections_on_2019_div_basis.csv")
results_2019_TPP <- read_csv("inputs/data/election_2019/HouseTppByDivisionDownload-24310.csv",
                        skip = 1)
results_2019_first_prefs <- read_csv("inputs/data/election_2019/HouseFirstPrefsByCandidateByVoteTypeDownload-24310.csv",
                             skip = 1)


#### Individual-level model ####
# Model
SmartVote_model <- brm(ALP_supporter ~ gender + age_group + (1|division), 
                   data = SmartVote_regression_data, 
                   family = bernoulli(),
                   cores = number_of_cores,
                   file = "outputs/models/SmartVote_testre")
# 
# priors_simple <- set_prior("normal(0,1)", class = "b") + set_prior("normal(0,3)", class="Intercept")
# 
# SmartVote_model_state <- brm(ALP_supporter ~ gender + age_group + (state / division), 
#                        data = SmartVote_regression_data, 
#                        family = bernoulli(),
#                        cores = number_of_cores,
#                        prior = priors_simple,
#                        file = "outputs/models/SmartVote_test_states")
# 
# SmartVote_model_multi <- brm(first_pref ~ gender + age_group + (1|division), 
#                              data = SmartVote_regression_data, 
#                              family = categorical(link="logit"),
#                              cores = number_of_cores,
#                              # prior = priors_simple,
#                              file = "outputs/models/SmartVote_multi")
# 
# 

summary(SmartVote_model)
ranef(SmartVote_model)

broom::tidy(SmartVote_model)
broom::tidy(SmartVote_model, parameters = "^sd_", intervals = FALSE)
broom::tidy(SmartVote_model, par_type = "varying")
broom::tidy(SmartVote_model, par_type = "hierarchical", robust = TRUE)

broom::tidy(SmartVote_model, par_type = "non-varying") %>% 
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    Lower = lower,
    Upper = upper
    ) %>% 
  mutate(Term = case_when(Term == "Intercept" ~ "Intercept",
                          Term == "genderMale" ~ "Is male",
                          Term == "age_groupages30to44" ~ "Age 30-44",
                          Term == "age_groupages45to59" ~ "Age 45-59",
                          Term == "age_groupages60plus" ~ "Age 60+")
         ) %>% 
  rbind(tibble(Term = c("High school", "Grad dip.", "Post grad."),
               Estimate = c("", "", ""),
               `Std. Error` = c("", "", ""),
               Lower = c("", "", ""),
               Upper = c("", "", ""))
        ) %>% 
  write_csv("outputs/data/estimates/SmartVote_coefficients.csv")
