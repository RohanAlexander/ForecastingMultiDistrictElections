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
first_prefs <- read_csv("outputs/data/elections/first_prefs_from_earlier_elections_on_2019_div_basis.csv")
twopp_to_first_prefs <- read_csv("outputs/data/elections/data_from_earlier_elections.csv")



# Make a graph of first preferences
first_prefs <- first_prefs %>% 
  mutate(PartyForGraphs = if_else(PartyAb %in% c("ALP", "LNP", "LP", "NP", "LNQ", "CLP", "GRN"), PartyAb, "Other"),
         PartyForGraphs = if_else(PartyForGraphs %in% c("LNP", "LP", "NP", "LNQ", "CLP"), "LNP", PartyForGraphs))

ggplot(data = first_prefs, aes(x = PartyForGraphs, y = first_prefs_percent)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.1, alpha = 0.3) +
  scale_colour_viridis_d() + 
  labs(x = "Party", 
       y = "First preference proportion", 
       colour = "Party",
       title = "Distribution of first preferences, by election") +
  theme_classic() +
  facet_wrap(vars(Year), nrow = 1)

# ggsave("outputs/figures/firstpreferencesvotes.pdf", width = 20, height = 10, units = "cm")


ggplot(data = first_prefs, aes(x = first_prefs_percent)) + 
  geom_density(aes(fill = forcats::fct_relevel(PartyForGraphs, "ALP", "LNP")), alpha=0.8) + 
  facet_wrap(vars(Year), 
             nrow = 3) +
  scale_fill_brewer(name = "Party", palette = "Set1") +
  labs(x = "First preference proportion",
       y = "Number of candidates",
       fill = "Party",
       # title = "Distribution of first preferences, by election and party",
       caption = "Data source: AEC") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom")

ggsave("outputs/figures/firstpreferencesvotesdensityconsistentdivision.pdf", width = 20, height = 20, units = "cm")



ggplot(data = first_prefs, aes(x = first_prefs_percent)) + 
  geom_freqpoly(aes(color = forcats::fct_relevel(PartyForGraphs, "ALP", "LNP")), 
                alpha=0.8,
                binwidth = 0.01) + 
  scale_y_log10() +
  facet_wrap(vars(Year), 
             nrow = 3) +
  scale_colour_brewer(name = "Party", palette = "Set1") +
  labs(x = "First preference proportion",
       y = "Number of candidates",
       fill = "Party",
       # title = "Distribution of first preferences, by election and party",
       caption = "Data source: AEC") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom")

ggsave("outputs/figures/firstpreferencesvotesdensityconsistentdivision_alt.pdf", width = 20, height = 10, units = "cm")





ggplot(data = twopp_to_first_prefs, aes(x = first_prefs_percent, y = tpp_percent, color = party)) + 
  facet_wrap(vars(year), nrow = 1) +
  geom_point() +
  geom_hline(yintercept = 0.5, 
             color = "grey",
             linetype = "dotted") +
  ylim(0.2, 0.8) +
  xlim(0, 0.7) +
  labs(x = "First preference share",
       y = "Two party preferred") +
  theme_minimal() +
  scale_color_brewer(name = "Party", palette = "Set1") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom")


ggsave("outputs/figures/twoPP_vs_primary_consistentdivision.pdf", width = 20, height = 10, units = "cm")



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

