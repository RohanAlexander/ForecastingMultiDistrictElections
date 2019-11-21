#### Preamble ####
# Purpose: This file just makes a simple MRP example, based on Kastellec, Lax, and Phillips (2016) 'Estimating State Public Opinion With Multi-Level Regression and Poststratification using R', but much more simple.
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 10 March 2019
# Prerequisites: Need the data file from the ABS - electorates_ageSingleYears_sex_longform.csv.
# To do:
# - Add an overview
# - Make it into a markdown doc
# Misc:
# -


#### Set up workspace ####
library(arm) # Need to invert the logistic
library(lme4) # Need for the model
library(tidyverse)
# update.packages()


#### Create example polling data ####
# We want to generate some sample data that we'll analyse. The advantage of this is that we have some idea of what to from the model.
# Just two variables for now (we'll add more later):
# is_male, which is 0 for females and 1 for males (IRL we'd need a third category for 'Other or prefer not to answer'); and
# in_age_group, which is one of four groups: 1, 2, 3, 4 (IRL this corresponds to something like: 18-29, 30-45, 45-60, 60+, and, again, we'd need a 'Other or prefer not to answer' category)
example_poll <-
  tibble(
    is_male = sample(c(0, 1), 2000, replace = TRUE, prob = c(0.75, 0.25)), # deliberately over-sample females
    in_age_group = sample(c(1:4), 2000, replace = TRUE)
  )

# Let's make males and older people less likely to vote for the ALP in our sample; and females and younger people more likely to vote for the ALP..
example_poll <- example_poll %>%
  mutate(
    binomial_param = case_when(
      is_male == 0 & in_age_group ==  1 ~ 0.9,
      is_male == 0 & in_age_group ==  2 ~ 0.7,
      is_male == 0 & in_age_group ==  3 ~ 0.5,
      is_male == 0 & in_age_group ==  4 ~ 0.3,
      is_male == 1 & in_age_group ==  1 ~ 0.7,
      is_male == 1 & in_age_group ==  2 ~ 0.5,
      is_male == 1 & in_age_group ==  3 ~ 0.3,
      is_male == 1 & in_age_group ==  4 ~ 0.1,
      TRUE ~ 0.5
    )
  ) %>%
  rowwise() %>%
  mutate(supports_ALP = rbinom(n = 1, size = 1, prob = binomial_param))

write_csv(example_poll, "outputs/data/example_poll_data.csv")

#### Analyse example polling data ####
# Now we'd like to see if we can get our results back - should find females less likely than males to vote ALP and that people are less likely to vote ALP as they get older.
example_model <-
  glmer(
    formula = supports_ALP ~ (1 | is_male) + (1 | in_age_group),
    data = example_poll,
    family = binomial(link = "logit") # Use this because supports_ALP is a binary
  )

summary(example_model)
ranef(example_model)
# Thankfully we find what we're looking for.


#### Post-stratify ####
# Now we'd like to see if we can use what we found in the poll to get an estimate for the seats.
# First read in some real demographic data, on a seat basis, from the ABS.
example_data <-
  read_csv("inputs/data/electorates_ageSingleYears_sex_longform.csv", skip = 8)
# The wonderful ABS allows us to download data in long format, so we only need to do some minor tidying and aggregating before we're ready to go.
names(example_data) <- c("division", "age", "sex", "number")
# Fill the rows and just keep the bits with content that we can work with
example_data <- example_data %>%
  fill(division, age) %>%
  filter(
    !division %in% c(
      "ABS data licensed under Creative Commons, see abs.gov.au/ccby",
      "Data Source: Census of Population and Housing, 2016, TableBuilder",
      "INFO",
      "Migratory - Offshore - Shipping (ACT)",
      "Migratory - Offshore - Shipping (NSW)",
      "Migratory - Offshore - Shipping (NT)",
      "Migratory - Offshore - Shipping (OT)",
      "Migratory - Offshore - Shipping (Qld)",
      "Migratory - Offshore - Shipping (SA)",
      "Migratory - Offshore - Shipping (Tas.)",
      "Migratory - Offshore - Shipping (Vic.)",
      "Migratory - Offshore - Shipping (WA)",
      "No reliance should be placed on small cells.",
      "No usual address (ACT)",
      "No usual address (NSW)",
      "No usual address (NT)",
      "No usual address (OT)",
      "No usual address (Qld)",
      "No usual address (SA)",
      "No usual address (Tas.)",
      "No usual address (Vic.)",
      "No usual address (WA)",
      "© Copyright Commonwealth of Australia, 2018, see abs.gov.au/copyright"
    )
  )

# Build the age groups that we're interested in: 18-29, 30-45, 46-60, 60+.
example_data <- example_data %>%
  mutate(age = if_else(age == "Total", "-1", age), # Going to convert to numeric, so need to change any words to a values
         age = as.integer(age)) %>% # A little dangerous because if the character wasn't an obvious integer then we'd introduce NA - fine for ABS data as it's high-quality, but need to be careful in general
  filter(!age %in% c(0:17)) %>%
  mutate(age_group = case_when( # case_when is wonderful, but you need to make sure you go from most-specific to least-specific
    age %in% c(18:29) ~ 1,
    age %in% c(30:45) ~ 2,
    age %in% c(46:59) ~ 3,
    age >= 60 ~ 4,
    TRUE ~ -1 # This is just a catch-all - if the data are not being weird then the function should never get here so we want an odd value that would alert us to that
  )) %>%
  mutate(number = as.integer(number)) %>% # The ABS adds a bit of noise for values that are small - it's not a big deal for us because we're aggregating anyway, but the numbers won't be exactly repeatable as I don't know how to impose a seed on the download.
  group_by(division, sex, age_group) %>%
  summarise(counts = sum(number)) %>%
  ungroup()
# Finally, we need to construct proportions for each sex-age_group combination, remembering that our catch-all in case_when did in fact get used, so -1 as the value for totals, but that value will include ages < 18, which isn't what our other groups include, so we need to construct our own sums.
example_data <- example_data %>%
  filter(age_group != -1) %>%
  group_by(division) %>%
  mutate(
    total_18_and_over_in_division = sum(counts),
    proportion = counts / total_18_and_over_in_division
  ) %>%
  ungroup()
# Finally, just some housekeeping for consistency
example_data <- example_data %>%
  mutate(is_male = if_else(sex == "Female", 0, 1)) %>%
  rename(in_age_group = age_group)
example_data$sex <- NULL
# (We still have 'Total' in there as a division, so there are 151 instead of 150.)

write_csv(example_data, "outputs/data/example_data_for_post-stratification.csv")


# Here we diverge from the Kastellec, Lax, and Phillips (2016) code because I couldn't really work out how theirs was working.
# Unfortunately, I couldn't get predict working for glmer. So we're just going to do some rough forecasts that won't have errors. Later, when we have a Bayesian model we'll use different functions that I know how to get proper forecasts for.
# For each is_male and in_age_group we want the relevant coefficient in the example_data.
is_male_coef <-
  tibble(
    is_male = ranef(example_model)$is_male %>% row.names() %>% as.double(),
    is_male_coef_values = ranef(example_model)$is_male[[1]]
  )
in_age_group_coef <-
  tibble(
    in_age_group = ranef(example_model)$in_age_group %>% row.names() %>% as.double(),
    in_age_group_coef_values = ranef(example_model)$in_age_group[[1]]
  )
example_data$fixed_effect <- fixef(example_model)["(Intercept)"]
example_data <- left_join(example_data, is_male_coef)
example_data <- left_join(example_data, in_age_group_coef)
rm(is_male_coef, in_age_group_coef)

# Now we can construct the estimates
example_data <- example_data %>%
  mutate(
    raw_estimate = invlogit(fixed_effect + is_male_coef_values + in_age_group_coef_values), # Sum each of the effects for each in_age_group and is_male combination for each division.
    weighted_estimate = raw_estimate * proportion # Now adjust for what share of the population each in_age_group and is_male combination comprises in each division
  ) %>% 
  dplyr::select(-c(fixed_effect, is_male_coef_values, in_age_group_coef_values, raw_estimate))

# Generate the estimates on a division basis
example_data_division_estimates <- example_data %>%
  group_by(division) %>%
  summarise(division_estimate = sum(weighted_estimate))
# So we find that the areas that skew young are more likely to vote ALP, than the areas that skew old.


#### Preferences ####
## Now we need adjust for preference flows to get a 2PP for each electorate

first_prefs <- read_csv("inputs/data/HouseFirstPrefsByCandidateByVoteTypeDownload-20499.csv",
                        skip = 1)
tpp <- read_csv("inputs/data/HouseTppByDivisionDownload-20499.csv",
                skip = 1)

both <- first_prefs %>% 
  filter(PartyAb == "ALP") %>% 
  left_join(tpp,
            by = c("StateAb", 
                   "DivisionID", 
                   "DivisionNm")) %>% 
  select("StateAb",
         "DivisionID",
         "DivisionNm",
         "TotalVotes.x",
         "TotalVotes.y",
         "Australian Labor Party Percentage") %>% 
  rename("ALP2PP" = "Australian Labor Party Percentage",
         "ALPfirstprefs" = "TotalVotes.x",
         "Totalvotes" = "TotalVotes.y") %>% 
  mutate(ALPfirst_prefs_percent = ALPfirstprefs / Totalvotes,
         ALP2PP = ALP2PP / 100)


preferences_model <-
  lm(
    formula = ALP2PP ~ ALPfirst_prefs_percent + factor(StateAb),
    data = both
  )

tidy(preferences_model)
glance(preferences_model)


d <- tidy(preferences_model) %>%
  mutate(
    low = estimate - std.error,
    high = estimate + std.error
  )

ggplot(d, aes(estimate, term, xmin = low, xmax = high, height = 0)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh()

example_data_division_estimates <- example_data_division_estimates %>% 
  rename("ALPfirst_prefs_percent" = "division_estimate")


library(modelr)
add_predictions(example_data_division_estimates, preferences_model, var = "pred")

library(broom)
both <- augment(preferences_model, both)
augment(preferences_model, newdata = example_data_division_estimates)




