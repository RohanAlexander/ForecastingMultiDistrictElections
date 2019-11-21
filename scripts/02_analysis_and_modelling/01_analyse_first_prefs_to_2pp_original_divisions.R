#### Preamble ####
# Purpose: Make a model that takes first preferences to explain 2PP
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Date: 5 October 2019
# Prerequisites: Need the voting dataset on a modified division basis.
# Issues: 
# To do:


#### Workspace set-up ####
library(broom)
library(tidyverse)
# update.packages()


#### Read in and prepare the data ####
# List of the first preference files
first_pref_files <- c("inputs/data/HouseFirstPrefsByCandidateByVoteTypeDownload-12246.csv",
                      "inputs/data/HouseFirstPrefsByCandidateByVoteTypeDownload-13745.csv",
                      "inputs/data/HouseFirstPrefsByCandidateByVoteTypeDownload-15508.csv",
                      "inputs/data/HouseFirstPrefsByCandidateByVoteTypeDownload-17496.csv",
                      "inputs/data/HouseFirstPrefsByCandidateByVoteTypeDownload-20499.csv")
# Read in each CSV and combine them into one tibble
first_prefs <- map_df(first_pref_files, ~read_csv(., skip = 1, col_names = TRUE), .id = "Year")
rm(first_pref_files)
# The IDs are for each file, but we want the year, so adjust for that
first_prefs$Year <- recode(first_prefs$Year, "1" = "2004", "2" = "2007", "3" = "2010", "4" = "2013", "5" = "2016")
# The data includes information on the different types of voting, but we just want total for now
first_prefs <- first_prefs %>% 
  mutate(Year = as.integer(Year)) %>% 
  select(-OrdinaryVotes,
         -AbsentVotes,
         -ProvisionalVotes,
         -PrePollVotes,
         -PostalVotes)
# The votes are on a candidate basis, but we also want the total for the division, because we want to know percentages, not number
first_prefs <- first_prefs %>% 
  group_by(Year, DivisionID) %>% 
  mutate(SumVotesInDivision = sum(TotalVotes),
         CandidateShareFirstPrefs = TotalVotes / SumVotesInDivision) %>% 
  ungroup() %>% 
  select(-SumVotesInDivision)
# There is a slight change in the columns for 2004
first_prefs <- first_prefs %>% 
  mutate(HistoricElected = if_else(SittingMemberFl == "#" & Year == "2004", "Y", HistoricElected)) %>% 
  select(-SittingMemberFl)
first_prefs$HistoricElected[is.na(first_prefs$HistoricElected)] <- "N"

## Check for situations where the Nationals have run against the Liberals
libsvsnats <- first_prefs %>% 
  filter(PartyForGraphs == "LNP") %>% 
  group_by(Year, DivisionNm) %>% 
  summarise(n = n()) %>% 
  filter(n == 2) %>% 
  group_by(Year) %>% 
  summarise(n = n())
  arrange(desc(n))
rm(libsvsnats)



# List of the 2PP files
# Note this is just ALP vs LNP. Some seats had a different party as the other (e.g. Greens or whatever), nonetheless our focus is on ALP for now so we want this.
# Other data - two candidate preferred - is available alternatively.
tpp_files <- c("inputs/data/HouseTppByDivisionDownload-12246.csv",
               "inputs/data/HouseTppByDivisionDownload-13745.csv",
               "inputs/data/HouseTppByDivisionDownload-15508.csv",
               "inputs/data/HouseTppByDivisionDownload-17496.csv",
               "inputs/data/HouseTppByDivisionDownload-20499.csv")
# Read in each CSV and combine them into one tibble and fix the years
tpp <- map_df(tpp_files, ~read_csv(., skip = 1, col_names = TRUE), .id = "Year")
rm(tpp_files)
tpp$Year <- recode(tpp$Year, "1" = "2004", "2" = "2007", "3" = "2010", "4" = "2013", "5" = "2016")
# Rename to make it easier to deal with
tpp <- tpp %>% 
  mutate(Year = as.integer(Year)) %>% 
  rename(LNPVotes = `Liberal/National Coalition Votes`,
         LNPPercentage = `Liberal/National Coalition Percentage`,
         ALPVotes = `Australian Labor Party Votes`,
         ALPPercent = `Australian Labor Party Percentage`)
# Each division (row) has a column for the LNP, and the ALP, but we need that to be in long format
tpp <- gather(tpp, key = "Party", value = "TwoPartyPreferred", c(LNPPercentage, ALPPercent)) %>% 
  arrange(Year, StateAb, DivisionNm, Party) %>% 
  select(-LNPVotes,
         -ALPVotes,
         -TotalVotes,
         -PartyAb) %>% 
  mutate(PartyAb = str_sub(Party, start = 1, end = 3)) %>% 
  select(-Party,
         -Swing) %>% 
  filter(PartyAb == "ALP")

# Add the first-preferences into the 2PP data for the ALP
ALP_voting_data <- tpp %>% 
  left_join(first_prefs) %>% 
  mutate(TwoPartyPreferred = TwoPartyPreferred / 100) %>% 
  select(-PartyNm, -Elected)

write_csv(ALP_voting_data, "outputs/data/ALP_voting_data.csv")


# Make a graph of first preferences
first_prefs <- first_prefs %>% 
  mutate(PartyForGraphs = if_else(PartyAb %in% c("ALP", "LNP", "LP", "NP", "LNQ", "CLP", "GRN"), PartyAb, "Other"),
         PartyForGraphs = if_else(PartyForGraphs %in% c("LNP", "LP", "NP", "LNQ", "CLP"), "LNP", PartyForGraphs))

ggplot(data = first_prefs, aes(x = PartyForGraphs, y = CandidateShareFirstPrefs)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.1, alpha = 0.3) +
  scale_colour_viridis_d() + 
  labs(x = "Party", 
       y = "First preference proportion", 
       colour = "Party",
       title = "Distribution of first preferences, by election") +
  theme_classic() +
  facet_wrap(vars(Year), nrow = 1)

ggsave("outputs/figures/firstpreferencesvotes.pdf", width = 20, height = 10, units = "cm")


ggplot(data = first_prefs, aes(x = CandidateShareFirstPrefs)) + 
  geom_density(aes(fill=factor(PartyForGraphs)), alpha=0.8) + 
  facet_wrap(vars(Year), 
             # scales = "free_y",
             nrow = 3) +
  scale_fill_viridis_d() + 
  labs(x = "First preference proportion",
       y = "Number of candidates",
       fill = "Party",
       # title = "Distribution of first preferences, by election and party",
       caption = "Data source: AEC") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom")

ggsave("outputs/figures/firstpreferencesvotesdensity.pdf", width = 20, height = 30, units = "cm")




# There are two - Denison and Melbourne - that were won by an Independent and a Green, respectively.
# Dropped those for now because the regression wouldn't be happy with them. 
# This should only be a problem given the ALP focus and should fix itself once we expand the regression to account for multiple parties.
ALP_voting_data <- ALP_voting_data %>% 
  mutate(flag = if_else(Year >=2010 & DivisionNm %in% c("Melbourne", "Denison"), 1, 0)) %>% 
  filter(flag == 0) %>% 
  select(-flag)

ggplot(data = ALP_voting_data, aes(x = CandidateShareFirstPrefs, y = TwoPartyPreferred)) + 
  facet_wrap(vars(Year), nrow = 1) +
  geom_point() +
  geom_hline(yintercept = 0.5, 
             color = "grey") +
  ylim(0.2, 0.8) +
  xlim(0, 0.7) +
  labs(x = "First preference share",
       y = "Two party preferred") +
  theme_classic()

ggsave("outputs/figures/twoPP_vs_primary.pdf", width = 20, height = 10, units = "cm")



#### Analysis ####
# Start with OLS to make sure everything looks good
ols_fit <- lm(TwoPartyPreferred ~ CandidateShareFirstPrefs + factor(Year) + factor(HistoricElected),
              data = ALP_voting_data)

tidy(ols_fit)
glance(ols_fit)
augment(ols_fit)

ggplot(data = augment(ols_fit), aes(x = TwoPartyPreferred, y = .fitted)) + 
  facet_wrap(vars(factor.Year.)) +
  geom_point() +
  geom_vline(xintercept = 0.5, 
             color = "grey") +
  geom_hline(yintercept = 0.5, 
             color = "grey") +
  ylim(0.2, 0.8) +
  xlim(0.2, 0.8) +
  labs(x = "Actual 2PP",
       y = "Predicted 2PP") +
  theme_classic()

ggsave("outputs/figures/twoPP_predicted_vs_actual_OLS.pdf")

