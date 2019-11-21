library(arm)
library(lme4)
library(tidyverse)
###################################################################
####                         Example poll                    ######
###################################################################
example_poll <- 
  tibble(
    is_male = sample(c(0,1),
                     100,
                     replace=TRUE,
                     prob= c(0.75,0.25)
                     ),
    in_age_group =sample(c(1:4),100, replace=T)
  )

example_poll <- example_poll %>%
  mutate(
    multinom_param1 = case_when(
      is_male == 0 & in_age_group == 1 ~ .2,
      is_male == 0 & in_age_group == 2 ~ .3,
      is_male == 0 & in_age_group == 3 ~ .4,
      is_male == 0 & in_age_group == 4 ~ .5,
      is_male == 1 & in_age_group == 1 ~ .5,
      is_male == 1 & in_age_group == 2 ~ .5,
      is_male == 1 & in_age_group == 3 ~ .6,
      is_male == 1 & in_age_group == 4 ~ .4,
      TRUE ~ 0.5
    ),
    multinom_param2 = case_when(
      is_male == 0 & in_age_group == 1 ~ .3,
      is_male == 0 & in_age_group == 2 ~ .4,
      is_male == 0 & in_age_group == 3 ~ .5,
      is_male == 0 & in_age_group == 4 ~ .4,
      is_male == 1 & in_age_group == 1 ~ .2,
      is_male == 1 & in_age_group == 2 ~ .2,
      is_male == 1 & in_age_group == 3 ~ .1,
      is_male == 1 & in_age_group == 4 ~ .1,
      TRUE ~ 0.5
    ),
    multinom_param3 = case_when(
      is_male == 0 & in_age_group == 1 ~ .1,
      is_male == 0 & in_age_group == 2 ~ .2,
      is_male == 0 & in_age_group == 3 ~ .1,
      is_male == 0 & in_age_group == 4 ~ .05,
      is_male == 1 & in_age_group == 1 ~ .15,
      is_male == 1 & in_age_group == 2 ~ .3,
      is_male == 1 & in_age_group == 3 ~ .3,
      is_male == 1 & in_age_group == 4 ~ .2,
      TRUE ~ 0.5
    ),
    multinom_param4 = 1-(multinom_param1+multinom_param2+multinom_param3)
  ) %>%
  rowwise() %>% mutate(first_pref =
                         which(rmultinom(n = 1, size = 1, prob = c(multinom_param1,multinom_param2,multinom_param3,multinom_param4))==1))

table(example_poll$first_pref)

####################################################################
####                      Example prev                        ######
####################################################################

example_prev_elect <-
  expand.grid(
    year = factor(c(2004, 2007, 2010, 2013, 2016)),
    electorate = factor(1:151),
    p_first_pref_1 = -1,
    p_first_pref_2 = -1,
    p_first_pref_3 = -1,
    p_first_pref_4 = -1
  )

#Softmax function
softmax <- function(x){
  softmax_x <- exp(x)/apply(exp(x),1,sum)
  return(softmax_x)
}

electorate_effect <- cbind(rnorm(151,0,1),rnorm(151,0,1),rnorm(151,0,1),rnorm(151,0,1))
year_effect <- cbind(c(.3,.4,.5,.6,.7),c(.7,.6,.4,.4,.3),c(.1,.3,.4,.2,.1),c(.1,.3,.2,.1,.1))
for (i in 1:nrow(example_prev_elect)){
  example_prev_elect[i,c("p_first_pref_1","p_first_pref_2","p_first_pref_3","p_first_pref_4")]<-
    softmax(t(electorate_effect[as.factor(example_prev_elect$electorate[i]),]+year_effect[as.factor(example_prev_elect$year[i]),]))
  example_prev_elect$first_pref[i]<-which(rmultinom(1,1,example_prev_elect[i,c("p_first_pref_1","p_first_pref_2","p_first_pref_3","p_first_pref_4")])==1)
}

example_prev_elect$sitting=sample(c(0,1),nrow(example_prev_elect),replace=T,prob=c(.25,.75))

beta_1 <- c(1,1,.5,.5)
beta_2 <- .5
example_prev_elect$mu_d <-.2 + beta_1[example_prev_elect$first_pref]+beta_2*example_prev_elect$sitting
example_prev_elect$prob_two_pp <- invlogit(rnorm(nrow(example_prev_elect),example_prev_elect$mu_d,.5))
example_prev_elect$two_pp <- rbinom(nrow(example_prev_elect),1,example_prev_elect$prob_two_pp)

example_prev_elect %>% head()

####################################################################
####                      Create a fake population            ######
####################################################################

# Make a dataframe that is just the combination of all the different bits of the vectors.
# So the first row is males, aged 1, in electorate 1; the second row is females, aged 1, in electorate 1; etc.
census <- expand.grid(ismale = c(1,2),
                      age = c(1,2,3,4),
                      electorate = 1:151,
                      N = -1) 
# For the number in each sub-group in each electorate, grab an integer centered around 1,000.
# The 2*$*151 bit is just the length - if you add want to add more subgroups then that'll need to change.
census$N <- round(rnorm(2*4*151,1000,200))

census <- merge(census, 
                example_prev_elect[example_prev_elect$year==2016,c("electorate","sitting")],
                by = "electorate")


####################################################################
####                      Fit the model                       ######
####################################################################
library(rstan)
options(mc.cores = parallel::detectCores())
stan_data <- list(nparty=4,
                  nelect=151,
                  nyear=5,
                  pe_sitting=example_prev_elect$sitting,
                  pe_fp = example_prev_elect$first_pref,
                  pe_2pp = example_prev_elect$two_pp,
                  poll_n = nrow(example_poll),
                  level_age = 4,
                  level_male = 2,
                  poll_ismale = example_poll$is_male,
                  poll_age = example_poll$in_age_group,
                  poll_fp = example_poll$first_pref,
                  census_ismale = census$ismale,
                  census_age = census$age,
                  census_elect = census$electorate,
                  census_sitting = census$sitting, 
                  census_N = census$N)
fit <- stan(file = 'scripts/00_simulated_stan.stan', data = stan_data,control = list(adapt_delta=.95))


#### Examine model ####
summary(fit)
print(fit, pars = "alpha", probs = c(0.025, 0.5, 0.975))
pars(fit)

y_pred <- extract(fit, 'elect_est_2pp')
y_pred <- unlist(y_pred, use.names=FALSE)

fit

library(shinystan)

launch_shinystan(fit)


library(tidybayes)

results <- fit %>%
  spread_draws(census_fp[i], elect_est_mu[i], elect_est_prob[i], elect_est_2pp[i])

results %>% 
  select(i, elect_est_prob) %>% 
  mutate(first_prefs = invlogit(elect_est_prob)) %>% 
  # group_by(i) %>% 
  filter(i %in% c(1:10)) %>% 
  ggplot(aes(x = first_prefs)) +
  facet_wrap(vars(i)) + 
  geom_density()

results$i %>% unique()

  

y_pred <- extract(fit, 'elect_est_prob')
y_pred <- unlist(y_pred, use.names=TRUE)
y_pred_hmmm <- invlogit(y_pred)
mean(y_pred_hmmm)


plot(density(y_pred)), xlim=c(1,2000),
     xlab="Loss", col=grey(0, 0.8),
     main="Predicitive distribution")


fit %>%
  spread_draws(elect_est_prob[i]) %>%
  ggplot(aes(x = elect_est_prob, y = i)) +
  geom_halfeyeh()