#### Purpose ####
# LinA is a panel survey and we have results from one point in time (just 
# before the election). We need to transform the dataset that they provided us, 
# into something that can be analysed by the model. We also need descriptive 
# stats and graphs.


#### Contact ####
# Author: Rohan Alexander
# Contact: rohan.alexander@utoronto.ca
# Last updated: 18 November 2010
# Issues (see 'BODY HERE')


#### Workspace setup ####
# Call the necessary packages
library(foreign) # Needed because they provide dta files.
library(tidyverse)

LinA2019 <- read.spss("/Volumes/Hansard/ForecastingMultiDistrictElections/confidential/LinA/2241 - Life in Australia Wave 26 - ANU weighted data file (020519).sav") %>% 
  as_tibble()

LinA2019_electorates <- haven::read_dta("/Volumes/Hansard/ForecastingMultiDistrictElections/confidential/LinA/2246 - Life in Australia Wave 28 - ANU electorates - W26 srcid.dta")
# There was some confusion around the wave needed. So originally it was Wave 28
# but our survey data is Wave 26. So need to adjust for that.
LinA2019_electorates <- 
  LinA2019_electorates %>% 
  filter(W28only == 0) %>% 
  select(-W28only) %>% 
  rename(SRCID = srcid)


#### Variable construction for descriptive stats ####
# First we need to get the electorates into the main dataset
LinA2019$SRCID <- LinA2019$SRCID %>% as.integer()
LinA2019_electorates$SRCID <- LinA2019_electorates$SRCID %>% as.integer()

LinA2019 <- 
  LinA2019 %>% 
  left_join(LinA2019_electorates, by = "SRCID")

labels <- labelled::val_labels(LinA2019$SRC_Q3)
labels <- tibble(electorate_name = labels %>% names(),
                 electorate_number = labels %>% as.integer())

write_csv(labels, "outputs/data/LinA_division_numbers_to_names.csv")

LinA2019$SRC_Q3 <- LinA2019$SRC_Q3 %>% as.integer()

# Get rid of Refused / Don't Know / NA
LinA2019 <- 
  LinA2019 %>% 
  filter(!SRC_Q3 %in% c(-99, -98)) %>% 
  filter(!is.na(SRC_Q3))


# Make sure the electorates are correct, given the state.
# Get the data that we know is right
division_numbers <- read_csv("inputs/data/misc/number_to_name_divisions.csv") %>% 
  rename(division_name = name, division_number = number)

# First get the division names back in
LinA2019 <- 
  LinA2019 %>% 
  left_join(labels, by = c("SRC_Q3" = "electorate_number"))

LinA2019_check <- 
  LinA2019 %>% 
  select(SRCID, SRC_Q3, electorate_name, p_state) %>% 
  rename(SRC_electorate_name = electorate_name)

LinA2019_check <- 
  LinA2019_check %>% 
  left_join(division_numbers, by = c("SRC_electorate_name" = "division_name")) %>% 
  rename(actual_state_of_electorate = state)

have_wrong_electorate <- 
  LinA2019_check %>% 
  mutate(states_right = if_else(actual_state_of_electorate == p_state, 1, 0)) %>% 
  filter(states_right == 0) %>% 
  select(SRCID)

LinA2019 <- 
  LinA2019 %>% 
  filter(!SRCID %in% have_wrong_electorate$SRCID)



# LinA constains a lot of variables, most of which we're not interested in. Q4 
# is basically who are you going to vote for.
LinA2019$Q4 <- LinA2019$Q4 %>% as.character()

party_support <- LinA2019 %>% 
  select(Q4, p_state) %>% 
  mutate(parties = recode(Q4,
                          "Labor" = "ALP",
                          "Liberal" = "LNP",
                          "Greens" = "GRN",
                          "Don't know" = "Unsure",
                          "Nationals" = "LNP",
                          "Liberal National Party (LNP)" = "LNP",
                          "One Nation" = "Other",
                          "Independent" = "Independent",
                          "Some other party" = "Other",
                          "No party" = "Other",
                          "Refused" = "Other",
                          "Animal Justice Party" = "Other",
                          "Christian Democratic Party" = "Other",
                          "Liberal Democrats" = "Other",
                          "Swing Voter" = "Other",
                          "Family First Party" = "Other",
                          "Pirate Party" = "Other",
                          "Australian Christians" = "Other",
                          "Australian Democrats" = "Other",
                          "Help End Marijuana Prohibition (HEMP) Party" = "Other",
                          "VOTEFLUX.ORG | Upgrade Democracy!" = "Other"
                          ))

party_support_by_state <- party_support %>% 
  mutate(parties = recode(parties,
                          "One Nation" = "Other",
                          "Palmer's United Party" = "Other",
                          "Shooters, Fishers and Farmers Party" = "Other",
                          "Katter's Australia Party" = "Other",
                          "Centre Alliance (formerly Nick Xenophon Team)" = "Other",
                          "Jacquie Lambie" = "Other",
                          "Independent" = "Other",
                          "Unsure" = "Other"
                          )) %>% 
  count(parties, p_state, sort = TRUE) %>% 
  # mutate(props = n / sum(n)) %>% 
  spread(key = p_state, value = n) %>% 
  rowwise() %>% 
  mutate(All = sum(NSW, VIC, QLD, SA, WA, TAS, NT, ACT, na.rm = TRUE)) %>% 
  arrange(desc(All)) %>% 
  rename(Party = parties) %>% 
  replace_na(list(NSW = 0, 
                  VIC = 0, 
                  QLD = 0, 
                  SA = 0, 
                  WA = 0, 
                  TAS = 0,
                  ACT = 0,
                  NT = 0))

write_csv(party_support_by_state, "outputs/data/tables/LINA_summary_stats.csv")

party_support_by_state_expanded <- party_support %>% 
  filter(parties %in% c("One Nation", 
                        "Palmer's United Party",
                        "Shooters, Fishers and Farmers Party",
                        "Katter's Australia Party",
                        "Centre Alliance (formerly Nick Xenophon Team)",
                        "Jacquie Lambie")) %>%
  count(parties, p_state, sort = TRUE) %>% 
  # mutate(props = n / sum(n)) %>% 
  spread(key = p_state, value = n) %>% 
  rowwise() %>% 
  mutate(All = sum(NSW, VIC, QLD, SA, WA, TAS, na.rm = TRUE)) %>% 
  arrange(desc(All)) %>% 
  rename(Party = parties) %>% 
  replace_na(list(NSW = 0, 
                  VIC = 0, 
                  QLD = 0, 
                  SA = 0, 
                  WA = 0, 
                  TAS = 0))

write_csv(party_support_by_state_expanded, "outputs/data/tables/LINA_summary_stats_expanded.csv")

# p_state is the state the respondent lives in
state_surveyed <- LinA2019 %>% 
  select(p_state) %>% 
  count(p_state, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename(State = p_state, Number = n, Proportion = props)

write_csv(state_surveyed, "outputs/data/tables/state_surveyed.csv")

# p_gender is gender
gender_surveyed <- LinA2019 %>% 
  select(p_gender) %>% 
  filter(p_gender %in% c("Female", "Male")) %>%
  count(p_gender, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename(Gender = p_gender, Number = n, Proportion = props)

write_csv(gender_surveyed, "outputs/data/tables/gender_surveyed.csv")

# p_age is age in single years. We'll use it to construct age-groups.
age_groups_surveyed <- LinA2019 %>% 
  select(p_age) %>% 
  mutate(p_age = p_age %>% as.integer(),
         age_group = case_when(p_age <= 29 ~ "ages18to29",
                               p_age <= 44 ~ "ages30to44",
                               p_age <= 59 ~ "ages45to59",
                               p_age > 59 ~ "ages60plus",
                               TRUE ~ "OH NO")) %>% 
  count(age_group, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename("Age group" = age_group, Number = n, Proportion = props)

write_csv(age_groups_surveyed, "outputs/data/tables/age_groups_surveyed.csv")

# p_education is basically highest level of education
education_surveyed <- LinA2019 %>% 
  select(p_education) %>% 
  mutate(education = case_when(p_education == "Secondary Education - Year 12"  ~ "highSchoolorCertIorIIorLess",
                               p_education == "Secondary Education - Years 10 and 11"  ~ "highSchoolorCertIorIIorLess",
                               p_education == "Secondary Education - Years 9 and below"  ~ "highSchoolorCertIorIIorLess",
                               p_education == "Advanced Diploma and Diploma Level" ~ "highSchoolorCertIorIIorLess",
                               p_education == "Certificate I & II Level" ~ "highSchoolorCertIorIIorLess",
                               p_education == "Certificate III & IV Level" ~ "gradDipDipCertIIIandIV",
                               p_education == "Graduate Diploma and Graduate Certificate Level" ~ "gradDipDipCertIIIandIV",
                               p_education == "Bachelor Degree Level" ~ "bachelorDegree",
                               p_education == "Postgraduate Degree Level" ~ "postgraduateDegree",
                               p_education == "Not stated / Unknown" ~ "Unknown",
                               TRUE ~ "OH NO")) %>% 
  count(education, sort = TRUE) %>% 
  mutate(props = n / sum(n)) %>%
  rename(Education = education, Number = n, Proportion = props)


write_csv(education_surveyed, "outputs/data/tables/education_surveyed.csv")


#### Regressions dataset construction ####
# We want a simple dataset for regression purposes.
# TODO: Why are we doing all of this again? All this coding has been done above?
LinA_regression_dataset <- 
  LinA2019 %>% 
  select(Q4, SRC_Q3, electorate_name, p_state, p_gender, p_education, p_age) %>% 
  # First the parties
  mutate(Q4 = as.character(Q4)) %>% 
  mutate(supports = recode(Q4,
                           "Animal Justice Party" = "Other",
                           "Australian Christians" = "Other",
                           "Australian Democrats" = "Other",
                           "Centre Alliance (formerly Nick Xenophon Team)" = "Other",
                           "Christian Democratic Party" = "Other",
                           "Don't know" = "Unsure",
                           "Family First Party" = "Other",
                           "Greens" = "GRN",
                           "Help End Marijuana Prohibition (HEMP) Party" = "Other",
                           "Independent" = "Other",
                           "Jacquie Lambie" = "Other",
                           "Katter's Australia Party" = "Other",
                           "Labor" = "ALP",
                           "Liberal Democrats" = "Other",
                           "Liberal National Party (LNP)" = "LNP",
                           "Liberal" = "LNP",
                           "Nationals" = "LNP",
                           "No party" = "Other",
                           "One Nation" = "Other",
                           "One Nation" = "Other",
                           "Palmer's United Party" = "Other",
                           "Pirate Party" = "Other",
                           "Refused" = "Other",
                           "Shooters, Fishers and Farmers Party" = "Other",
                           "Some other party" = "Other",
                           "Swing Voter" = "Other",
                           "VOTEFLUX.ORG | Upgrade Democracy!" = "Other"
  )) %>% 
  # Now education
  mutate(education = case_when(p_education == "Secondary Education - Year 12"  ~ "highSchoolorCertIorIIorLess",
                               p_education == "Secondary Education - Years 10 and 11"  ~ "highSchoolorCertIorIIorLess",
                               p_education == "Secondary Education - Years 9 and below"  ~ "highSchoolorCertIorIIorLess",
                               p_education == "Advanced Diploma and Diploma Level" ~ "highSchoolorCertIorIIorLess",
                               p_education == "Certificate I & II Level" ~ "highSchoolorCertIorIIorLess",
                               p_education == "Certificate III & IV Level" ~ "gradDipDipCertIIIandIV",
                               p_education == "Graduate Diploma and Graduate Certificate Level" ~ "gradDipDipCertIIIandIV",
                               p_education == "Bachelor Degree Level" ~ "bachelorDegree",
                               p_education == "Postgraduate Degree Level" ~ "postgraduateDegree",
                               p_education == "Not stated / Unknown" ~ "Unknown",
                               TRUE ~ "OH NO")) %>% 
  # Now age
  mutate(p_age = p_age %>% as.integer(),
         age_group = case_when(p_age <= 29 ~ "ages18to29",
                               p_age <= 44 ~ "ages30to44",
                               p_age <= 59 ~ "ages45to59",
                               p_age > 59 ~ "ages60plus",
                               TRUE ~ "OH NO")) %>% 
  select(supports, age_group, p_gender, education, everything()) %>% 
  select(-Q4, -p_age, -p_education)


# General clean ups  
LinA_regression_dataset <- 
  LinA_regression_dataset %>% 
  filter(education != "Unknown") %>% 
  rename(first_pref = supports,
         gender = p_gender,
         state = p_state,
         division = electorate_name) %>% 
  mutate(gender = as.character(gender),
         state = as.character(state)) %>% 
  select(-SRC_Q3)

table(LinA_regression_dataset$division, LinA_regression_dataset$first_pref)

# Also make a binomial of whether the voter supports the ALP to make it easier 
# to get the modeling working
LinA_regression_dataset <- 
  LinA_regression_dataset %>% 
  mutate(ALP_supporter = if_else(first_pref == "ALP", 1, 0))
  
LinA_regression_dataset$ALP_supporter <- as.character(LinA_regression_dataset$ALP_supporter)

table(LinA_regression_dataset$first_pref, LinA_regression_dataset$gender)
table(LinA_regression_dataset$first_pref, LinA_regression_dataset$age_group)
table(LinA_regression_dataset$first_pref, LinA_regression_dataset$education)
table(LinA_regression_dataset$age_group, LinA_regression_dataset$education)

# There are a few that are 'other' for gender. While I agree and understand that 
# gender is a spectrum and there are more than two genders, the ABS doesn't and 
# so on the census we only have male and female. For that reason we drop other 
# from the sample.
LinA_regression_dataset <- 
  LinA_regression_dataset %>% 
  filter(gender %in% c("Female", "Male")) %>%
  # Also get rid of respondents who don't know their highest level of education.
  filter(!education %in% c("Unknown"))


write_csv(LinA_regression_dataset, "outputs/data/regression_data/LinA.csv")


