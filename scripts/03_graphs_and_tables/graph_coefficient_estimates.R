library(tidybayes)
library(tidyverse)

SmartVote_model <- readRDS("outputs/models/SmartVote_divs.rds")
LinA_model <- readRDS("outputs/models/LinA_divs.rds")



SmartVote_model %>%
  tidybayes::get_variables()

# b_Intercept
# b_genderMale
# b_age_groupages30to44
# b_age_groupages45to59
# b_age_groupages60plus


LinA_model %>%
  tidybayes::get_variables()

# "b_Intercept"
# "b_genderMale"
# "b_age_groupages30to44"
# "b_age_groupages45to59"
# "b_age_groupages60plus"
# "b_educationgradDipDipCertIIIandIV"
# "b_educationhighSchoolorCertIorIIorLess"
# "b_educationpostgraduateDegree"

SmartVote_model_coefficients <- 
  SmartVote_model %>%
  tidybayes::gather_draws(`b_.*`, regex=TRUE) %>%
  ungroup() %>%
  mutate(coefficient = stringr::str_replace_all(.variable, c("b_" = ""))) %>%
  mutate(coefficient = forcats::fct_recode(coefficient,
                                           Intercept = "Intercept", 
                                           `Is male` = "genderMale",
                                           `Age 30-44` = "age_groupages30to44",
                                           `Age 45-59` = "age_groupages45to59",
                                           `Age 60+` = "age_groupages60plus",
                                           `High school` = "educationhighSchoolorCertIorIIorLess",
                                           `Grad dip.` = "educationgradDipDipCertIIIandIV",
                                           `Post grad.` = "educationpostgraduateDegree"))
SmartVote_model_coefficients$Type <- "Smartvote Australia"

LinA_model_coefficients <- 
  LinA_model %>%
  gather_draws(`b_.*`, regex=TRUE) %>%
  ungroup() %>%
  mutate(coefficient = stringr::str_replace_all(.variable, c("b_" = ""))) %>%
  mutate(coefficient = forcats::fct_recode(coefficient,
                                           Intercept = "Intercept", 
                                           `Is male` = "genderMale",
                                           `Age 30-44` = "age_groupages30to44",
                                           `Age 45-59` = "age_groupages45to59",
                                           `Age 60+` = "age_groupages60plus",
                                           `High school` = "educationhighSchoolorCertIorIIorLess",
                                           `Grad dip.` = "educationgradDipDipCertIIIandIV",
                                           `Post grad.` = "educationpostgraduateDegree",
                                           )) 
LinA_model_coefficients$Type <- "Life in Australia"

both <- rbind(SmartVote_model_coefficients, LinA_model_coefficients)

LinA_model_coefficients$coefficient %>% unique()
both$coefficient %>% unique()

both <- 
  both %>% 
  mutate(coefficient = forcats::fct_relevel(coefficient,
                                            "Intercept",
                                            "Is male",
                                            "Age 30-44",
                                            "Age 45-59",
                                            "Age 60+",
                                            "High school",
                                            "Grad dip.",
                                            "Post grad."))
both$coefficient %>% unique()

both %>% 
  ggplot(aes(y=fct_rev(coefficient), x = .value, fill = Type)) + 
  ggridges::geom_density_ridges2(aes(height = ..density..),
                                 rel_min_height = 0.01, 
                                 stat = "density",
                                 scale=1.5) +
  xlab("Distribution of estimate") +
  ylab("Coefficient") +
  scale_fill_brewer(name = "Dataset: ", palette = "Set1") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom")


ggsave("outputs/figures/coefficientdistributions.pdf", width = 20, height = 10, units = "cm")

