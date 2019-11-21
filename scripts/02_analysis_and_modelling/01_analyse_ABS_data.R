#### Preamble ####
# Purpose: This file just makes some summary tables and graphs of the ABS data for the paper
# Author: Rohan Alexander
# Email: rohan.alexander@anu.edu.au
# Last updated: 19 March 2019
# Prerequisites: Need the data file from the ABS - electorates_ageSingleYears_sex_longform.csv.
# To do:
# - 
# Misc:
# -


#### Set up workspace ####
library(tidyverse)
# update.packages()


#### Read in the data ####
ABS_data <-
  read_csv("inputs/data/electorates_ageSingleYears_sex_longform.csv", skip = 8)
# The wonderful ABS allows us to download data in long format, so we only need to do some minor tidying and aggregating before we're ready to go.
names(ABS_data) <- c("division", "age", "sex", "number")
# Fill the rows and just keep the bits with content that we can work with
ABS_data <- ABS_data %>%
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
ABS_data <- ABS_data %>%
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
ABS_data <- ABS_data %>%
  filter(age_group != -1) %>%
  group_by(division) %>%
  mutate(
    total_18_and_over_in_division = sum(counts),
    proportion = counts / total_18_and_over_in_division
  ) %>%
  ungroup()
# Finally, just some housekeeping for consistency
ABS_data <- ABS_data %>%
  mutate(is_male = if_else(sex == "Female", 0, 1)) %>%
  rename(in_age_group = age_group)
ABS_data$sex <- NULL
# (We still have 'Total' in there as a division, so there are 151 instead of 150.)

ABS_data$in_age_group <- recode(ABS_data$in_age_group, `1` = "18-29", `2` = "30-44", `3` = "45-59", `4` = "60+")

write_csv(ABS_data, "outputs/data/ABS_data_for_post-stratification.csv")


#### Make summary tables ####
# Check I did the proportions right
ABS_data %>%
  group_by(division) %>% 
  summarise(sum  = sum(proportion))

# Get rid of total
ABS_data <- ABS_data %>%
  filter(division != "Total")

# Recode males and females
ABS_data$is_male <- recode(ABS_data$is_male, `0` = "Female", `1` = "Male")


# Plot the data
ABS_data %>% 
  ggplot(aes(x = division, y = proportion, color = is_male)) +
  facet_wrap(vars(in_age_group), ncol = 1) +
  geom_point() +
  labs(x = "Division", 
       y = "Proportion", 
       caption = "Data source: ABS (2016)",
       color = "Sex",
       title = "Age and sex sub-group proportions, by division") +
  scale_colour_viridis_d() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=90, hjust=1, vjust = 0)) +
  scale_x_discrete(breaks = ABS_data$division[seq(1, length(ABS_data$division), by = 25)])

ggsave("outputs/figures/proportions_by_subgroup_dots.pdf")


ABS_data %>% 
  ggplot(aes(x = proportion)) +
  geom_density(aes(fill=factor(in_age_group)), alpha=0.8) + 
  facet_wrap(vars(is_male)) +
  labs(caption = "Data source: ABS (2016)",
       # title = "Density of division sub-cell proportions, by age-group and sex", 
       x = "Sub-cell proportion",
       y = "Number of divisions",
       fill = "Age-group") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("outputs/figures/proportions_by_subgroup_density.pdf", width = 20, height = 10, units = "cm")



# Plot correlations between the different groups.
ABS_data %>% 
  select(division, in_age_group, proportion, is_male) %>% 
  spread(in_age_group, proportion) %>% 
  select(-division, -is_male) %>% 
  pairs()
  
ggsave("outputs/figures/proportions_by_subgroup_pairs.pdf")

# If we want to use this type of plot in the paper then use this package to make it look nice
# install.packages("GGally")
# library(GGally)


number_by_division <- ABS_data %>% 
  group_by(division) %>% 
  summarise(n = n(),
            sum = sum(counts)) %>% 
  arrange(sum)


summary_stats <- ABS_data %>% 
  group_by(in_age_group, is_male) %>% 
  summarise(n = n(),
            average = mean(counts),
            range = IQR(counts))

summary_stats <- summary_stats %>% 
  select(-n)

names(summary_stats) <- c("Age-group", "Sex", "Average", "Range")

write_csv(summary_stats, "outputs/data/summary_stats.csv")





