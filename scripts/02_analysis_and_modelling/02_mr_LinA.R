#### Purpose ####
# Run an MRP model on the LinA survey results.


#### Contact ####
# Author: Rohan Alexander
# Contact: rohan.alexander@utoronto.ca
# Last updated: 28 November 2019


#### Workspace setup ####
# Call the necessary packages
library(brms)
library(lme4)
library(tidyverse)

number_of_cores <-
  parallel::detectCores() # Used to parallelise brms

LINA_regression_data <-
  read_csv("outputs/data/regression_data/LinA.csv")
poststratification_data <-
  read_csv("outputs/data/poststratification_data/census_data.csv")
preferences_data <-
  read_csv("outputs/data/elections/data_from_earlier_elections_on_2019_div_basis.csv")
results_2019_TPP <-
  read_csv("inputs/data/election_2019/HouseTppByDivisionDownload-24310.csv",
           skip = 1)
results_2019_first_prefs <-
  read_csv(
    "inputs/data/election_2019/HouseFirstPrefsByCandidateByVoteTypeDownload-24310.csv",
    skip = 1
  )

# Add the 2016 first preferences results into the regression dataset
preferences_data <-
  preferences_data %>%
  filter(year == 2016) %>%
  select(division, first_prefs_percent)

LINA_regression_data <-
  LINA_regression_data %>%
  left_join(preferences_data, by = "division")



#### Models ####
# Just respondent demographic features
LinA_model_01 <-
  brm(
    ALP_supporter ~ gender + age_group + education,
    data = LINA_regression_data,
    family = bernoulli(),
    cores = number_of_cores,
    file = "outputs/models/LinA_01" # If running this again, either
    # need to change this file name to something new or delete
    # the saved model from the folder.
  )


LinA_model <-
  brm(
    ALP_supporter ~ gender + age_group + education + (1 | division),
    data = LINA_regression_data,
    family = bernoulli(),
    cores = number_of_cores,
    file = "outputs/models/LinA" # If running this again, either
    # need to change this file name to something new or delete
    # the saved model from the folder.
  )

brms::get_prior(
  ALP_supporter ~ gender + age_group + education + (1 + first_prefs_percent |
                                                      division),
  data = LINA_regression_data
)

priors_simple <- set_prior("normal(0,1)", class = "b") +
  set_prior("normal(0,3)", class = "Intercept") +
  set_prior("student_t(3, 0, 10)", class = "sd")

LinA_model <- brm(
  ALP_supporter ~ gender + age_group + education + (1 + first_prefs_percent |
                                                      division),
  data = LINA_regression_data,
  family = bernoulli(),
  prior = priors_simple,
  cores = number_of_cores,
  control = list(adapt_delta = 0.999),
  file = "outputs/models/LinA_divs" # If running this again, either
  # need to change this file name to something new or delete
  # the saved model from the folder.
)

LinA_model <- brm(
  ALP_supporter ~ gender + age_group + education + (1 + state |
                                                      division),
  data = LINA_regression_data,
  family = bernoulli(),
  prior = priors_simple,
  cores = number_of_cores,
  control = list(adapt_delta = 0.99,
                 iter =),
  file = "outputs/models/LinA_divs_state" # If running this again, either
  # need to change this file name to something new or delete
  # the saved model from the folder.
)


LinA_model <- brm(
  ALP_supporter ~ gender + age_group + education + (1 + state + first_prefs_percent |
                                                      division),
  data = LINA_regression_data,
  family = bernoulli(),
  prior = priors_simple,
  cores = number_of_cores,
  control = list(adapt_delta = 0.99),
  file = "outputs/models/LinA_divs_state_past" # If running this again, either
  # need to change this file name to something new or delete
  # the saved model from the folder.
)

bayesplot::mcmc_trace(LinA_model)



pairs(LinA_model)
ranef(LinA_model)
summary(LinA_model)

broom::tidy(LinA_model)
broom::tidy(LinA_model, parameters = "^sd_", intervals = FALSE)
broom::tidy(LinA_model, par_type = "varying")
broom::tidy(LinA_model, par_type = "hierarchical", robust = TRUE)

broom::tidy(LinA_model, par_type = "non-varying") %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    Lower = lower,
    Upper = upper
  ) %>%
  mutate(
    Term = case_when(
      Term == "Intercept" ~ "Intercept",
      Term == "genderMale" ~ "Is male",
      Term == "age_groupages30to44" ~ "Age 30-44",
      Term == "age_groupages45to59" ~ "Age 45-59",
      Term == "age_groupages60plus" ~ "Age 60+",
      Term == "educationgradDipDipCertIIIandIV" ~ "Grad dip.",
      Term == "educationhighSchoolorCertIorIIorLess" ~ "High school",
      Term == "educationpostgraduateDegree" ~ "Post grad."
    )
  ) %>%
  mutate(ordering = c(1, 2, 3, 4, 5, 7, 6, 8)) %>%
  arrange(ordering) %>%
  select(-ordering) %>%
  write_csv("outputs/data/estimates/LinA_coefficients.csv")
