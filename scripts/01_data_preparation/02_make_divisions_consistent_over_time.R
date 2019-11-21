#### Purpose ####
# Right, okay, so; now we know which 2019 division each of the 2016, 2013, 
# 2010, and 2007 booths belonged to we can relate them.


#### Contact ####
# Author: Rohan Alexander
# Contact: rohan.alexander@utoronto.ca
# Last updated: 5 October 2010
# Issue: We've thrown out some prepoll: 'PPVC'. Come back and fix.


#### Workspace setup ####
# Call the necessary packages
library(tidyverse)


#### Read in the data ####
# Read in the older booths to 2019 areas correspondence
correspondence <- read_csv("outputs/data/elections/booths_reduced_with_2019_division.csv")
# The first preferences booths data
all_files <- list.files(
  "inputs/data",
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE)
all_files <- tibble(file = all_files)
all_files <- all_files %>% 
  mutate(first_pref_file = str_detect(file, "HouseStateFirstPrefsByPollingPlaceDownload")) %>% 
  filter(first_pref_file == TRUE) %>% 
  separate(file, c("first", "second", "state"), sep = "-", remove=FALSE) %>% 
  mutate(state = str_remove(state, ".csv")) %>% 
  select(-first_pref_file, -second, -first)
all_files <- all_files %>% 
  separate(file, c("inputs", "data", "year", "rest"), sep = "/", remove=FALSE) %>% 
  select(-inputs, -data, -rest)
files <- all_files$file
names(files) <- all_files$year

first_prefs_by_booth <- purrr::map_df(files, readr::read_csv, skip = 1, .id = "Year")
first_prefs_by_booth$Year <- str_remove(first_prefs_by_booth$Year, "election_")
first_prefs_by_booth$Year <- as.integer(first_prefs_by_booth$Year)

# We don't have any lat/long for 2004
first_prefs_by_booth <- first_prefs_by_booth %>% 
  filter(Year != 2004)

# Read in the TPP data
all_files <- list.files(
  "inputs/data",
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE)
all_files <- tibble(file = all_files)
all_files <- all_files %>% 
  mutate(TPP_file = str_detect(file, "HouseTppByPollingPlaceDownload")) %>% 
  filter(TPP_file == TRUE) %>% 
  separate(file, c("inputs", "data", "year", "rest"), sep = "/", remove=FALSE) %>% 
  select(-inputs, -data, -rest, -TPP_file)

files <- all_files$file
names(files) <- all_files$year

two_party_preferred <- purrr::map_df(files, readr::read_csv, skip = 1, .id = "Year")
two_party_preferred$Year <- str_remove(two_party_preferred$Year, "election_")
two_party_preferred$Year <- as.integer(two_party_preferred$Year)

# We don't have any lat/long for 2004
two_party_preferred <- two_party_preferred %>% 
  filter(Year != 2004)


#### Add the 2019 division name to the booths data ####
first_prefs_by_booth_with_2019 <- first_prefs_by_booth %>% 
  left_join(correspondence, by = c("PollingPlaceID", "Year"))

first_prefs_by_booth_with_2019 %>% 
  filter(is.na(Elect_div)) %>% 
  # select(PollingPlace) %>% 
  distinct()

first_prefs_by_booth_with_2019 <- first_prefs_by_booth_with_2019 %>% 
  filter(!is.na(Elect_div))

rejigged_first_prefs <- first_prefs_by_booth_with_2019 %>% 
  group_by(Elect_div, Year, PartyAb) %>% 
  summarise(votes = sum(OrdinaryVotes)) %>% 
  ungroup()
rm(first_prefs_by_booth_with_2019)


#### Add the 2019 division name to the 2PP data ####
two_party_preferred <- two_party_preferred %>% 
  select(Year, StateAb, DivisionNm, PollingPlaceID, `Australian Labor Party Votes`, `Liberal/National Coalition Votes`, TotalVotes) %>% 
  rename(ALP2PPCount = `Australian Labor Party Votes`,
         LNP2PPCount = `Liberal/National Coalition Votes`)

two_party_preferred_with_2019 <- two_party_preferred %>% 
  left_join(correspondence, by = c("PollingPlaceID", "Year"))

two_party_preferred_with_2019 <- two_party_preferred_with_2019 %>% 
  filter(!is.na(Elect_div))

rejigged_2PP <- 
  two_party_preferred_with_2019 %>% 
  group_by(Year, Elect_div) %>% 
  summarise(ALPvotes = sum(ALP2PPCount),
            LNPvotes = sum(LNP2PPCount),
            Total = sum(TotalVotes)
            ) %>% 
  mutate(ALPTPP = ALPvotes / Total,
         LNPTPP = LNPvotes / Total) %>% 
  ungroup()
rm(two_party_preferred_with_2019)

rejigged_2PP %>% 
  mutate(check = ALPTPP + LNPTPP) %>% 
  filter(check != 1)

  
#### Finalise everything ####
rejigged_first_prefs_totals <- 
  rejigged_first_prefs %>% 
  group_by(Year, Elect_div) %>% 
  summarise(total_votes = sum(votes, na.rm = TRUE))

# rejigged_first_prefs <- rejigged_first_prefs %>% 
#   filter(PartyAb == "ALP")

rejigged_first_prefs <- 
  rejigged_first_prefs %>% 
  left_join(rejigged_first_prefs_totals, by = c("Elect_div", "Year"))

rejigged_first_prefs <- 
  rejigged_first_prefs %>% 
  mutate(first_prefs_percent = votes / total_votes)

# Need this dataset for a figure
write_csv(rejigged_first_prefs, "outputs/data/elections/first_prefs_from_earlier_elections_on_2019_div_basis.csv")

data_from_earlier_elections_on_2019_div_basis <- rejigged_2PP %>% 
  left_join(rejigged_first_prefs, by = c("Year", "Elect_div"))

data_from_earlier_elections_on_2019_div_basis <- data_from_earlier_elections_on_2019_div_basis %>% 
  rename(ALP_2PP_percent = ALPTPP,
         ALP_2PP_number = ALPvotes,
         division = Elect_div,
         LNP_2PP_percent = LNPTPP,
         LNP_2PP_number = LNPvotes,
         first_prefs_number = votes,
         year = Year)

data_from_earlier_elections_on_2019_div_basis$PartyAb %>% unique()

data_from_earlier_elections_on_2019_div_basis <- data_from_earlier_elections_on_2019_div_basis %>% 
  mutate(PartyAb = if_else(PartyAb %in% c("ALP", "LNP", "LP", "NP", "LNQ", "CLP", "GRN"), PartyAb, "Other"),
         PartyAb = if_else(PartyAb %in% c("LNP", "LP", "NP", "LNQ", "CLP"), "LNP", PartyAb))

data_from_earlier_elections_on_2019_div_basis <- data_from_earlier_elections_on_2019_div_basis %>% 
  filter(PartyAb %in% c("ALP", "LNP"))


data_from_earlier_elections_on_2019_div_basis$division[data_from_earlier_elections_on_2019_div_basis$division == "Eden-monaro"] <- "Eden-Monaro"
data_from_earlier_elections_on_2019_div_basis$division[data_from_earlier_elections_on_2019_div_basis$division == "Mcewen"] <- "McEwen"
data_from_earlier_elections_on_2019_div_basis$division[data_from_earlier_elections_on_2019_div_basis$division == "Mcmahon"] <- "McMahon"
data_from_earlier_elections_on_2019_div_basis$division[data_from_earlier_elections_on_2019_div_basis$division == "Mcpherson"] <- "McPherson"
data_from_earlier_elections_on_2019_div_basis$division[data_from_earlier_elections_on_2019_div_basis$division == "O'connor"] <- "O'Connor"

data_from_earlier_elections_on_2019_div_basis$division %>% unique()

data_from_earlier_elections_on_2019_div_basis <- 
  data_from_earlier_elections_on_2019_div_basis %>% 
  group_by(year, division, PartyAb) %>% 
  mutate(maximum = max(first_prefs_percent, na.rm = TRUE),
         maximum = if_else(maximum == first_prefs_percent, 1, 0)) %>% 
  filter(maximum == 1) %>% 
  select(-maximum) %>% 
  ungroup()
  

data_from_earlier_elections_on_2019_div_basis <- data_from_earlier_elections_on_2019_div_basis %>% 
  select(year, division, PartyAb, ALP_2PP_percent, LNP_2PP_percent, first_prefs_percent)

data_from_earlier_elections_on_2019_div_basis_just_twopp <- data_from_earlier_elections_on_2019_div_basis %>% 
  select(year, division, ALP_2PP_percent, LNP_2PP_percent) %>% 
  gather(key = party, value = tpp_percent, ALP_2PP_percent, LNP_2PP_percent) %>% 
  arrange(year, division, party) %>% 
  distinct()

data_from_earlier_elections_on_2019_div_basis_just_twopp$party[data_from_earlier_elections_on_2019_div_basis_just_twopp$party == "LNP_2PP_percent"] <- "LNP"
data_from_earlier_elections_on_2019_div_basis_just_twopp$party[data_from_earlier_elections_on_2019_div_basis_just_twopp$party == "ALP_2PP_percent"] <- "ALP"

data_from_earlier_elections_on_2019_div_basis_just_first <- data_from_earlier_elections_on_2019_div_basis %>% 
  select(year, division, PartyAb, first_prefs_percent) %>% 
  rename(party = PartyAb)

data_from_earlier_elections <- data_from_earlier_elections_on_2019_div_basis_just_first %>% 
  left_join(data_from_earlier_elections_on_2019_div_basis_just_twopp, by = c("year", "division", "party"))

write_csv(data_from_earlier_elections, "outputs/data/elections/data_from_earlier_elections.csv")



write_csv(data_from_earlier_elections_on_2019_div_basis, "outputs/data/elections/data_from_earlier_elections_on_2019_div_basis.csv")


