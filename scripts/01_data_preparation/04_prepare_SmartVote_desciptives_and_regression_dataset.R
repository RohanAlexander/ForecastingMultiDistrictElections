#### Preamble ####
# Purpose: This file tidies up the Smart Vote data, including making summary 
# tables and preparing a dataset to feed the model.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 18 November 2019
# Prerequisites: 
# Misc:
# - Search for # BODY HERE

 
#### Set up workspace ####
library(haven) # Needed to read the dta.
library(tidyverse)

raw_data <- read_dta("/Volumes/Hansard/ForecastingMultiDistrictElections/confidential/SmartVote/dataset post stratification ltd.dta")
division_numbers <- read_csv("inputs/data/misc/number_to_name_divisions.csv") %>% 
  rename(division_name = name, division_number = number)
state_numbers <- read_csv("inputs/data/misc/number_to_name_state.csv") %>% 
  rename(state_name = state, state_number = number)


#### Dataset preparation ####
# The states and divisions don't have the usual names in the dataset, instead 
# numbers have been used. So wee need to swap the numbers to names.
# Add names to the states and divisions
raw_data <- as_tibble(raw_data)
raw_data$state <- raw_data$state %>% as.integer()

raw_data <- 
  raw_data %>% 
  left_join(state_numbers, by = c("state" = "state_number"))

raw_data$districtid <- raw_data$districtid %>% as.integer()
raw_data <- raw_data %>% 
  left_join(division_numbers, by = c("districtid" = "division_number"))
table(raw_data$division_name, raw_data$state_name)
# We need to get the data into tidy form.
# Deal with the individuals
raw_data <- 
  raw_data %>% 
  select(-recommendationid, -voterid, -timestamp, -state, -districtid, -yearofbirth)

raw_data$personID <- 1:nrow(raw_data) # Yeah, I just got rid of an id, but that was too messy.

raw_data$gender %>% unique()
table(raw_data$gender)
# Deal with gender
# raw_data$gender %>% table()
raw_data <- raw_data %>% 
  filter(gender %in% c("1", "2", "3"))
# 1 is female, 2 is male, and 3 is other.
raw_data <- raw_data %>% 
  mutate(gender = recode(gender,
                         "1" = "Female",
                         "2" = "Male",
                         "3" = "Other"))
# I agree that gender isn't binary, but the ABS data only has males and females
raw_data <- raw_data %>% 
  filter(gender %in% c("Male", "Female"))

# Deal with the parties
raw_data <- replace_na(raw_data, 
           list(otherpartyvalue = 0, 
                laborvps = 0,
                liberalvps = 0, 
                laborvps = 0, 
                nationalvps = 0,
                greensvps = 0,
                onenationvps = 0))

raw_data$otherpartyvalue %>% class()
raw_data$laborvps %>% unique()

raw_data <- raw_data %>% select(-otherpartyname)

raw_data$otherpartyvalue <- as.integer(raw_data$otherpartyvalue)
raw_data$laborvps <- as.integer(raw_data$laborvps)
raw_data$liberalvps <- as.integer(raw_data$liberalvps)
raw_data$nationalvps <- as.integer(raw_data$nationalvps)
raw_data$greensvps <- as.integer(raw_data$greensvps)
raw_data$onenationvps <- as.integer(raw_data$onenationvps)

raw_data <- raw_data %>% 
  gather(key = party, value = vote, 
         otherpartyvalue, 
         laborvps, 
         liberalvps, 
         nationalvps, 
         greensvps, 
         onenationvps) %>% 
  arrange(personID, party)

# Sometimes people just put zero or whatever for each party. They're not
# indicating a preference and need to be removed for now
# BODY HERE
# Right now we're just dropping them, but we should actually come back and say 
# they were unsure or something.
raw_data <- raw_data %>% 
  group_by(personID) %>% 
  mutate(indicated_a_preference = if_else(max(vote) == mean(vote), 0, 1)) %>% 
  ungroup()

raw_data <- raw_data %>% 
  filter(indicated_a_preference == 1) %>% 
  select(-indicated_a_preference)

# Similarly, sometimes people have equal preferences (usually labor-greens or libs-nats).
# BODY HERE
# Right, now we'll just drop them but we should probably notget rid of them, 
# but instead construct a preference. 
raw_data <- raw_data %>% 
  group_by(personID) %>%
  mutate(highest_value = if_else(max(vote) == vote, 1, 0),
         num_prefs = sum(highest_value)) %>% 
  ungroup()

raw_data <- raw_data %>% 
  filter(num_prefs == 1) %>% 
  filter(highest_value == 1) %>% 
  select(-num_prefs, -highest_value, -vote)

raw_data <- raw_data %>% 
  select(personID, state_name, division_name, gender, yofbrec, party) %>% 
  rename(state = state_name)

raw_data <- raw_data %>% 
  mutate(parties = recode(party,
                          "laborvps" = "ALP",
                          "liberalvps" = "LNP",
                          "greensvps" = "GRN",
                          "nationalvps" = "LNP",
                          "otherpartyvalue" = "Other",
                          "onenationvps" = "Other")) %>% 
  select(-party)

raw_data$parties %>% table()

# Deal with age
raw_data <- raw_data %>% 
  mutate(age = 2019 - yofbrec,
         age = age %>% as.integer(),
         age_group = case_when(age <= 29 ~ "ages18to29",
                               age <= 44 ~ "ages30to44",
                               age <= 59 ~ "ages45to59",
                               age > 59 ~ "ages60plus",
                               TRUE ~ "OH NO"))

raw_data$age_group %>% table()

# Add a binary for ALP
raw_data <- 
  raw_data %>% 
  mutate(ALP_supporter = if_else(parties == "ALP", 1, 0))

# Make the names consistent with the LinA dataset
raw_data <- 
  raw_data %>% 
  rename(first_pref = parties,
         division = division_name) %>% 
  select(-personID)

write_csv(raw_data, "outputs/data/regression_data/SmartVote.csv")


#### Construct summary statistics ####
cleaned_data <- read_csv("outputs/data/regression_data/SmartVote.csv")

# Gender
gender_surveyed <- cleaned_data %>% 
  select(gender) %>% 
  count(gender, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename(Gender = gender, Number = n, Proportion = props) %>% 
  arrange(Gender)

write_csv(gender_surveyed, "outputs/data/tables/SmartVote-gender_surveyed.csv")

# Age
age_groups_surveyed <- cleaned_data %>% 
  select(age_group) %>% 
  count(age_group, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename("Age group" = age_group, Number = n, Proportion = props)

write_csv(age_groups_surveyed, "outputs/data/tables/SmartVote-age_groups_surveyed.csv")

# Parties
party_support_by_state <- cleaned_data %>% 
  select(first_pref, state) %>% 
  count(first_pref, state, sort = TRUE) %>% 
  # mutate(props = n / sum(n)) %>% 
  spread(key = state, value = n) %>% 
  rowwise() %>% 
  mutate(All = sum(NSW, VIC, QLD, SA, WA, TAS, NT, ACT, na.rm = TRUE)) %>% 
  arrange(desc(All)) %>% 
  rename(Party = first_pref) %>% 
  replace_na(list(NSW = 0, 
                  VIC = 0, 
                  QLD = 0, 
                  SA = 0, 
                  WA = 0, 
                  TAS = 0,
                  ACT = 0,
                  NT = 0))

write_csv(party_support_by_state, "outputs/data/tables/SmartVote-summary_stats.csv")

# States
state_surveyed <- cleaned_data %>% 
  select(state) %>% 
  count(state, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename(State = state, Number = n, Proportion = props)

write_csv(state_surveyed, "outputs/data/tables/SmartVote-state_surveyed.csv")


