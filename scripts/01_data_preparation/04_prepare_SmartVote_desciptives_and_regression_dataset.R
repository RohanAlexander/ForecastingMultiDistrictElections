#### Preamble ####
# Purpose: This file tidies up the Smart Vote data, including making summary 
# tables and preparing a dataset to feed the model.
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 23 November 2019
# Prerequisites: 
# Misc:
# - Search for # BODY HERE

 
#### Set up workspace ####
library(haven) # Needed to read the dta.
library(tidyverse)
raw_data <- read_dta("/Volumes/Hansard/ForecastingMultiDistrictElections/confidential/SmartVote/Dataset smartvote for MrP 21 November Rohan paper.dta")
division_numbers <- read_csv("inputs/data/misc/number_to_name_divisions.csv") %>% 
  rename(division_name = name, division_number = number)
state_numbers <- read_csv("inputs/data/misc/number_to_name_state.csv") %>% 
  rename(state_name = state, state_number = number)


#### Dataset preparation ####
# Check the division names
divisions_SV <- raw_data$voter_district %>% unique()
divisions_correct <- division_numbers$division_name %>% unique()
setdiff(divisions_SV, divisions_correct)
setdiff(divisions_correct, divisions_SV)
# Update the division names
raw_data$voter_district[raw_data$voter_district == "Mcmahon"] <- "McMahon"
raw_data$voter_district[raw_data$voter_district == "Mcpherson"] <- "McPherson"
raw_data$voter_district[raw_data$voter_district == "Mcewen"] <- "McEwen"
# Check the division-state mix is correct
raw_data$check <- paste0(raw_data$voter_district, raw_data$voter_state)
check_SV <- raw_data$check %>% unique()
division_numbers$check <- paste0(division_numbers$division_name, division_numbers$state)
check_correct <- division_numbers$check %>% unique()
setdiff(check_SV, check_correct)
raw_data$voter_district[raw_data$voter_state == "ACT"] %>%  unique()
raw_data$check <- NULL
division_numbers$check <- NULL
raw_data <- 
  raw_data %>% 
  filter(voter_district != "")
raw_data <- 
  raw_data %>% 
  mutate(voter_district = as.character(voter_district)) %>% 
  left_join(division_numbers, by = c("voter_district" = "division_name"))
# TODO:
# BODY HERE
# We're just dropping anyone with a wrong state, given their division.
raw_data <- 
  raw_data %>% 
  mutate(state_check = if_else(state == voter_state, 1, 0)) %>% 
  filter(state_check == 1) %>% 
  select(- state_check, -state, -division_number)

# We need to get the data into tidy form.
# Deal with the individuals
raw_data <- 
  raw_data %>% 
  select(-inlineframeid)

raw_data$personID <- 1:nrow(raw_data)

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

raw_data %>% names()
# Deal with the parties
raw_data <- 
  replace_na(raw_data, 
           list(greens_propensity = 0,
                labor_propensity = 0,
                liberal_propensity = 0,
                national_propensity = 0,
                onenation_propensity = 0,
                otherpartyvps = 0))

raw_data$otherpartyvps %>% class()
raw_data$labor_propensity %>% unique()

raw_data$otherpartyvps <- as.integer(raw_data$otherpartyvps)
raw_data$labor_propensity <- as.integer(raw_data$labor_propensity)
raw_data$liberal_propensity <- as.integer(raw_data$liberal_propensity)
raw_data$national_propensity <- as.integer(raw_data$national_propensity)
raw_data$greens_propensity <- as.integer(raw_data$greens_propensity)
raw_data$onenation_propensity <- as.integer(raw_data$onenation_propensity)

raw_data <- 
  raw_data %>% 
  gather(key = party, value = vote, 
         otherpartyvps, 
         labor_propensity, 
         liberal_propensity, 
         national_propensity, 
         greens_propensity, 
         onenation_propensity) %>% 
  arrange(personID, party)

# Sometimes people just put zero or whatever for each party. They're not
# indicating a preference and need to be removed for now
# TODO:
# BODY HERE
# Right now we're just dropping them, but we should actually come back and say 
# they were unsure or something.
raw_data <- 
  raw_data %>% 
  group_by(personID) %>% 
  mutate(indicated_a_preference = if_else(max(vote) == mean(vote), 0, 1)) %>% 
  ungroup()

raw_data <- 
  raw_data %>% 
  filter(indicated_a_preference == 1) %>% 
  select(-indicated_a_preference)

# Similarly, sometimes people have equal preferences (usually labor-greens or libs-nats).
# TODO:
# BODY HERE
# Right, now we'll just drop them but we should probably not get rid of them, 
# but instead construct a preference. 
raw_data <- 
  raw_data %>% 
  group_by(personID) %>%
  mutate(highest_value = if_else(max(vote) == vote, 1, 0),
         num_prefs = sum(highest_value)) %>% 
  ungroup()

raw_data <- 
  raw_data %>% 
  filter(num_prefs == 1) %>% 
  filter(highest_value == 1) %>% 
  select(-num_prefs, -highest_value, -vote)

raw_data <- 
  raw_data %>% 
  select(personID, voter_state, voter_district, gender, age_num, education_cat, party) %>% 
  rename(state = voter_state,
         division = voter_district,
         age = age_num)

raw_data <- 
  raw_data %>% 
  mutate(parties = recode(party,
                          "labor_propensity" = "ALP",
                          "liberal_propensity" = "LNP",
                          "greens_propensity" = "GRN",
                          "national_propensity" = "LNP",
                          "otherpartyvps" = "Other",
                          "onenation_propensity" = "Other")) %>% 
  select(-party)

raw_data$parties %>% table()

# Deal with age
raw_data <- 
  raw_data %>% 
  mutate(age = age %>% as.integer(),
         age_group = case_when(age <= 29 ~ "ages18to29",
                               age <= 44 ~ "ages30to44",
                               age <= 59 ~ "ages45to59",
                               age > 59 ~ "ages60plus",
                               TRUE ~ "OH NO"))

raw_data$age_group %>% table()

raw_data <- 
  raw_data %>% 
  filter(age_group != "OH NO")

# Add a binary for ALP
raw_data <- 
  raw_data %>% 
  mutate(ALP_supporter = if_else(parties == "ALP", 1, 0))

# Make the names consistent with the LinA dataset
raw_data <- 
  raw_data %>% 
  rename(first_pref = parties) %>% 
  select(-personID)

# Fix education
# The groupings are:
  # "1" =            Postgraduate degree/diploma
  # "2" =    Bachelor degree (including honours)
  # "3" =                  Undergraduate diploma
  # "4" =                      Associate diploma
  # "5" =                    Trade qualification
  # "6" =                Non-trade qualification
  # "7" =  No qualification since leaving school
  # "9" =                                  other
# TODO:
# BODY HERE
# I don't know how to best classify the non-trade qualitifcation - is that cert 1/2?
raw_data <- 
  raw_data %>% 
  mutate(education_cat = as.integer(education_cat),
         education = 
           case_when(education_cat == 1 ~ "postgraduateDegree",
                     education_cat == 2 ~ "bachelorDegree",
                     education_cat %in% c(3:6) ~ "gradDipDipCertIIIandIV",
                     education_cat == 7 ~ "highSchoolorCertIorIIorLess",
                     TRUE ~ "OH NO")
         )

table(raw_data$education, raw_data$education_cat)

raw_data <- 
  raw_data %>% 
  filter(education != "OH NO") %>% 
  select(-education_cat)

raw_data <- 
  raw_data %>% 
  select(state, division, gender, age_group, education, first_pref, ALP_supporter) %>% 
  arrange(state, division, gender, age_group, education)

write_csv(raw_data, "outputs/data/regression_data/SmartVote.csv")


#### Construct summary statistics ####
cleaned_data <- read_csv("outputs/data/regression_data/SmartVote.csv")

# Gender
gender_surveyed <- 
  cleaned_data %>% 
  select(gender) %>% 
  count(gender, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename(Gender = gender, Number = n, Proportion = props) %>% 
  arrange(Gender)

write_csv(gender_surveyed, "outputs/data/tables/SmartVote-gender_surveyed.csv")

# Age
age_groups_surveyed <- 
  cleaned_data %>% 
  select(age_group) %>% 
  count(age_group, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename("Age group" = age_group, Number = n, Proportion = props)

write_csv(age_groups_surveyed, "outputs/data/tables/SmartVote-age_groups_surveyed.csv")

# Parties
party_support_by_state <- 
  cleaned_data %>% 
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
state_surveyed <- 
  cleaned_data %>% 
  select(state) %>% 
  count(state, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename(State = state, Number = n, Proportion = props)

write_csv(state_surveyed, "outputs/data/tables/SmartVote-state_surveyed.csv")

# Education
education_surveyed <- 
  cleaned_data %>% 
  select(education) %>% 
  count(education, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename(Education = education, 
         Number = n, 
         Proportion = props)


write_csv(education_surveyed, "outputs/data/tables/SmartVote-education_surveyed.csv")

