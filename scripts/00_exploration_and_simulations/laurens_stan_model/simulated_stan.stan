// functions{
//   //Adapted from Bob's comment here https://discourse.mc-stan.org/t/find-or-which-command-in-stan/1101/4
//   int num_lt(real x, vector y) {
//     int n = 0;
//     for (i in 1:rows(y))
//       if (y[i] == x)
//          n = n + 1;
//     return n;
//   }
// 
//   int[] find_lt(real x, vector y) {
//     int result[num_lt(x, y)];
//     int n = 1;
//     for (i in 1:rows(y)) {
//       if (y[i] == x) {
//         result[n] = i;
//         n = n + 1;
//       }
//     }
//     return result;
//   }
// }

data {
  //general parameters
  int<lower=0> nparty;
  int<lower=0> nelect;
  int<lower=0> nyear;
  //Election data (pe = previous election)
  vector<lower=0>[nelect*nyear] pe_sitting;
  int<lower=0> pe_fp[nelect*nyear]; // at the moment [1,4]: i.e. who has won in each electorate
  int<lower=0> pe_2pp[nelect*nyear]; // this should be a % but at the moment is [0,1]
  //Poll data
  int<lower=0> poll_n;
  int<lower=0> level_age;
  int<lower=0> level_male;
  vector<lower=0>[poll_n] poll_ismale;
  int<lower=0> poll_age[poll_n]; 
  int<lower=0> poll_fp[poll_n];
  //Population data
  int<lower=0> census_age[level_age*level_male*nelect];
  vector<lower=0>[level_age*level_male*nelect] census_ismale;
  vector<lower=0>[level_age*level_male*nelect] census_elect;
  vector<lower=0>[level_age*level_male*nelect] census_sitting;
  vector<lower=0>[level_age*level_male*nelect] census_N;
}
parameters {
  //Parameters for prev election model
  real beta_0;
  vector[nparty] beta_1_raw;
  real<lower=0> sigma_beta_1;
  real beta_2;
  real<lower=0> sigma_2pp;
  vector[nelect*nyear] prob_2pp_raw;
  //Parameters for poll data model
  vector[nparty] delta_0;
  vector[nparty]  delta_1;
  matrix[level_age,nparty] delta_2_raw;
  real<lower=0> sigma_delta_2;
}
transformed parameters {
  //Election model tp
  vector[nelect*nyear] mu_2pp;
  vector[nelect*nyear] prob_2pp;
  vector[nparty] beta_1;
  //Poll model tp
  matrix[level_age,nparty] delta_2;
  matrix[poll_n,nparty] prob_poll;
  //Election model
  beta_1 = beta_1_raw*sigma_beta_1;
  mu_2pp = beta_0 + beta_1[pe_fp] +beta_2*pe_sitting;
  prob_2pp = mu_2pp + sigma_2pp*prob_2pp_raw;
  //Poll model
  delta_2 = delta_2_raw*sigma_delta_2;
  for(k in 1:nparty){
    prob_poll[,k] = delta_0[k] + poll_ismale*delta_1[k] + delta_2[poll_age,k];
  }
}
model {
  //Election model
  sigma_beta_1 ~ normal(0,1);
  sigma_2pp ~ normal(0,1);
  beta_1_raw ~ normal(0,1);
  beta_0 ~ normal(0,1);
  beta_2 ~ normal(0,1);
  prob_2pp_raw ~ normal(0,1);
  pe_2pp ~ bernoulli_logit(prob_2pp);
  //Poll model
  delta_0 ~ normal(0,1);
  delta_1 ~ normal(0,1);
  sigma_delta_2 ~ normal(0,1);
  for(k in 1:nparty){
     delta_2_raw[,k] ~ normal(0,1);
     poll_fp ~ categorical_logit(prob_poll[,k]);
  }
}
generated quantities{
  //MRP for the census
  matrix[level_age*level_male*nelect,nparty] census_fp_prob;
  int<lower=1,upper=4> census_fp[level_age*level_male*nelect];
  //predict 2pp
  vector[level_age*level_male*nelect] elect_est_mu;
  vector[level_age*level_male*nelect] elect_est_prob;
  vector[level_age*level_male*nelect] elect_est_2pp;
  //MRP for the census  
  for(k in 1:nparty){
    census_fp_prob[,k] = delta_0[k] + census_ismale*delta_1[k] + delta_2[census_age,k];
  }
  //poststratify for electorate
  for(i in 1:(level_age*level_male*nelect)){
     census_fp[i] = categorical_logit_rng(to_vector(census_fp_prob[i,]));
  //predict 2pp
      elect_est_mu[i] = beta_0 + beta_1[census_fp[i]] + beta_2*census_sitting[i];
      elect_est_prob[i] = normal_rng(elect_est_mu[i],sigma_2pp);
      elect_est_2pp[i] = bernoulli_logit_rng(elect_est_prob[i]);
  }
}
