#### Preamble ####
# Purpose: This file prepares the post-stratification data downloaded from the ABS
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 24 September 2019
# Prerequisites: 
# Misc:


#### Set up workspace ####
library(tidyverse)

females <- read_csv("inputs/data/census/table-female.csv",
                    skip = 8,
                    col_names = FALSE)
males <- read_csv("inputs/data/census/table-male.csv",
                  skip = 8,
                  col_names = FALSE)
both <- cbind(females, males)
rm(females, males)


#### Tidy dataset ####
# First transport it - use this rather than t() so that it's still a tibble
both <- as_tibble(cbind(nms = names(both), t(both)), .name_repair = "minimal")
names(both) <- both[1,]
both <- both[2:nrow(both),]

both <- 
  both %>% 
  select(-X1, 
         -`MB by Commonwealth Electoral Divisions (UR)`,
         -`Data Source: Census of Population and Housing, 2016, TableBuilder`,
         -`INFO`,
         -`Copyright Commonwealth of Australia, 2018, see abs.gov.au/copyright`,
         -`ABS data licensed under Creative Commons, see abs.gov.au/ccby`
         ) %>% 
  rename(age_group = `AGEP Age`,
         heap = `HEAP - 1 Digit Level`,
         gender = `SEXP Sex`)

both <- rbind(both[1:20,], both[23:42,])

both <- 
  fill(both, age_group)

census_data <- gather(both, 
                      key = "division", 
                      value = "number",
                      Adelaide:Total)

rm(both)

#### Clean up ####
# Check the type before we convert to integer
census_data$check_number <- str_detect(census_data$number, "[:digit:]")
census_data$check_alpha <- str_detect(census_data$number, "[:alpha:]")
census_data$check_punct <- str_detect(census_data$number, "[:punct:]")
census_data$check_blank <- str_detect(census_data$number, "[:blank:]")
table(census_data$check_number)
table(census_data$check_alpha)
table(census_data$check_punct)
table(census_data$check_blank)

census_data$number <- as.integer(census_data$number)

census_data <- 
  census_data %>% 
  select(-check_number,
         -check_alpha,
         -check_punct,
         -check_blank)

census_data$age_group[census_data$age_group == "age18to29"] <- "ages18to29"

census_data <- 
  census_data %>% 
  select(division, gender, age_group, heap, number) %>% 
  arrange(division, gender, age_group, heap)

census_data <- 
  census_data %>% 
  filter(age_group != "Total") %>%
  filter(division != "Total")
  


#### Make proportions ####
census_data <- 
  census_data %>% 
  group_by(division) %>% 
  mutate(total = sum(number)) %>% 
  mutate(cell_proportion_of_division_total = number / total) %>% 
  ungroup() %>% 
  select(-total)

census_data <- 
  census_data %>% 
  rename(education = heap)

# census_data %>% 
#   ggplot() +
#   geom_histogram(aes(x = number)) +
#   facet_wrap(vars(gender, age_group, education))

# Make table

census_data_for_paper <- 
  census_data %>%
    group_by(gender, age_group, education) %>% 
    summarise(Average = mean(number),
              Minimum = min(number, na.rm = TRUE),
              Maximum = max(number, na.rm = TRUE)
              ) %>% 
    mutate(
      Average = as.integer(Average)
      ) %>% 
    arrange(gender, age_group, education) %>% 
    rename(
      Gender = gender,
      `Age group` = age_group,
      Education = education
    )

census_data_for_paper$`Age group`[census_data_for_paper$`Age group` == "ages18to29"] <- "18 - 29"
census_data_for_paper$`Age group`[census_data_for_paper$`Age group` == "ages30to44"] <- "30 - 44"
census_data_for_paper$`Age group`[census_data_for_paper$`Age group` == "ages45to59"] <- "45 - 60"
census_data_for_paper$`Age group`[census_data_for_paper$`Age group` == "ages60plus"] <- "60+"

census_data_for_paper$Education[census_data_for_paper$Education == "highSchoolorCertIorIIorLess"] <- "High School"
census_data_for_paper$Education[census_data_for_paper$Education == "gradDipDipCertIIIandIV"] <- "Graduate Diploma"
census_data_for_paper$Education[census_data_for_paper$Education == "bachelorDegree"] <- "Bachelor"
census_data_for_paper$Education[census_data_for_paper$Education == "postgraduateDegree"] <- "Postgraduate"

census_data_for_paper <- census_data_for_paper %>% 
  mutate(Education = forcats::fct_relevel(Education, 
                                 "High School",
                                 "Graduate Diploma",
                                 "Bachelor",
                                 "Postgraduate"))


write_csv(census_data_for_paper, "outputs/data/tables/census_data.csv")  


write_csv(census_data, "outputs/data/poststratification_data/census_data.csv")





census_data$age_group[census_data$age_group == "ages18to29"] <- "18 - 29"
census_data$age_group[census_data$age_group == "ages30to44"] <- "30 - 44"
census_data$age_group[census_data$age_group == "ages45to59"] <- "45 - 60"
census_data$age_group[census_data$age_group == "ages60plus"] <- "60+"

census_data$education[census_data$education == "highSchoolorCertIorIIorLess"] <- "High School"
census_data$education[census_data$education == "gradDipDipCertIIIandIV"] <- "Graduate Diploma"
census_data$education[census_data$education == "bachelorDegree"] <- "Bachelor"
census_data$education[census_data$education == "postgraduateDegree"] <- "Postgraduate"

census_data <- census_data %>% 
  mutate(education = forcats::fct_relevel(education, 
                                          "High School",
                                          "Graduate Diploma",
                                          "Bachelor",
                                          "Postgraduate"))

census_data %>% 
  ggplot(aes(x = cell_proportion_of_division_total)) +
  geom_freqpoly(aes(colour=factor(education)), alpha=0.8, 
                 bins = 30) + 
  facet_wrap(vars(gender, age_group), nrow = 2, ncol = 4) +
  labs(
    # caption = "Data source: ABS (2016)",
       # title = "Density of division sub-cell proportions, by age-group and sex", 
       x = "Sub-cell proportion",
       y = "Number of divisions",
       fill = "Education") +
  scale_color_brewer(name = "Education: ", palette = "Set1") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom")

ggsave("outputs/figures/proportions_by_subgroup_density_corrected_divisions.pdf", width = 20, height = 10, units = "cm")


census_data %>% 
  group_by(division) %>% 
  summarise(n = sum(number)) %>% 
  arrange(desc(n))

census_data %>% 
  group_by(division) %>% 
  summarise(n = sum(number))


