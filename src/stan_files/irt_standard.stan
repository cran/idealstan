
#include /chunks/license.stan

functions {
#include /chunks/stationary_functions.stan
#include /chunks/jacobians.stan
#include /chunks/calc_rlnorm_gp.stan
}

data {
  int N;
  int N_int; // if outcome is an integer
  int N_cont; // if outcome is continuous
  int T;
  int Y_int[N_int]; // integer outcome
  real Y_cont[N_cont]; // continuous outcome

  /* Use this to set the type of IRT Model to run
  1= basic IRT 2 Pl (no inflation)
  2= basic IRT 2 PL (with inflation)
  3 = ratingscale IRT (no inflation)
  4= ratingscale IRT (with inflation)
  5 = grm IRT (no inflation)
  6= grm IRT (with inflation)
  7= ZIP IRT
  */
  int model_type;
  int LX;
  int SRX;
  int SAX;
  int<lower=1> num_legis;
  int<lower=1> num_bills;
  int ll[N];
  int bb[N];
  int time[N];
  matrix[N,LX] legis_pred;
  matrix[N,SRX] srx_pred;
  matrix[N,SAX] sax_pred;
  real diff; // difference between high and low constrained parameters
  real diff_high;
  real discrim_reg_sd;
  real discrim_abs_sd;
  real legis_sd;
  real diff_abs_sd;
  real diff_reg_sd;
  real restrict_sd;
  real ar_sd;
  int sample_stationary;
  real time_sd;
  int restrict_var;
  real restrict_var_high;
  real restrict_mean_val[2];
  int restrict_mean_ind[8];
  int restrict_mean;
  int time_proc;
  real time_ind[T]; // the actual indices/values of time points, used for Gaussian processes
  int zeroes; // whether to use traditional zero-inflation for bernoulli and poisson models
  real gp_sd_par; // residual variation in GP
  real num_diff[2]; // number of time points used to calculate GP length-scale prior
  real m_sd_par[2]; // the marginal standard deviation of the GP
  int min_length; // the minimum threshold for GP length-scale prior
}

transformed data {
	int m;                         // missing value
	real m_cont; // missing value if continuous
	int m_step; // number of ordinal categories
	int absence[N]; // need to create absence indicator
	int num_constrain_l;
	int Y_new[N];
	int num_var_free; // whether to restrict variance parameters
	int num_var_restrict;
	real num_legis_real; // used to adjust jacobian for mean restriction
	int num_ls; // extra person params for latent space
	int gp_N; // use for creating zero-length arrays if gp not used
	int gp_1; // zero-length gp-related scalars
	int gp_nT; // used to make L_tp1 go to model block if GPs are used
	int gp_oT; // used to make L_tp1 go to model block if GPs are used
	vector[2] gp_length; 
	
	// set mean of log-normal distribution for GP length-scale prior
	
	if(time_proc==4) {
	  gp_length = gp_prior_mean(time_ind,num_diff[1],min_length);
	} else {
	  gp_length = [0,0]';
	}
	
	
	//reset these values to use GP-specific parameters
	if(time_proc!=4) {
	  gp_N=0;
	  gp_1=0;
	  gp_oT=T;
	  gp_nT=0;
	  gp_length[2] = 0;
	} else {
	  gp_N=num_legis;
	  gp_1=1;
	  gp_nT=T;
	  gp_oT=0;
	}
	
	// need to assign a type of outcome to Y based on the model (discrete or continuous)
	// to do this we need to trick Stan into assigning to an integer. 
	
#include /chunks/change_outcome.stan
	
  //determine how many and which parameters to constrain
#include /chunks/create_constrained.stan

// determine whether to restrict variance or not

if(restrict_var==1) {
  num_var_restrict=num_legis;
  num_var_free=0;
} else {
  num_var_restrict=0;
  num_var_free=num_legis;
}

  num_legis_real = num_legis; // promote N to type real
  
  if(model_type==13) {
    num_ls=num_legis;
  } else {
    num_ls=0;
  }
  
}

parameters {
  vector[num_bills] sigma_abs_free;
  vector[num_legis - num_constrain_l] L_free; // first T=1 params to constrain
  vector<lower=0,upper=m_sd_par[1]>[gp_N] m_sd; // marginal standard deviation of GP
  vector<lower=0,upper=gp_sd_par>[gp_N] gp_sd; // residual GP variation in Y
  vector[num_legis] L_tp2[gp_nT]; // additional L_tp1 for GPs only
  vector[num_ls] ls_int; // extra intercepts for non-inflated latent space
  vector[num_legis] L_tp1_var[T-1]; // non-centered variance
  vector<lower=-.99,upper=.99>[num_legis-1] L_AR1_free; // AR-1 parameters for AR-1 model
  vector[num_bills] sigma_reg_free;
  vector[1] restrict_high;
  vector[LX] legis_x;
  vector[SRX] sigma_reg_x;
  vector[SAX] sigma_abs_x;
  vector[num_bills] B_int_free;
  vector[num_bills] A_int_free;
  ordered[m_step-1] steps_votes;
  ordered[m_step-1] steps_votes_grm[num_bills];
  real<lower=0> extra_sd;
  real<lower=-.9,upper=.9> ar_fix;
  vector<lower=gp_length[2]>[num_legis] time_var;
  vector<lower=0,upper=restrict_var_high>[num_legis] time_var_restrict;
  
}

transformed parameters {

  vector[num_legis] L_full;
  vector[num_legis] L_AR1;
  vector[num_legis] L_tp1[T];
  vector[1] restrict_low;

  if(T==1) {
    restrict_low = restrict_high - diff;
  } else {
    restrict_low = restrict_high - diff;
  }
  

  L_AR1 = append_row(L_AR1_free,ar_fix);
  
    //combine constrained and unconstrained parameters
#include /chunks/build_params_v2.stan

  
  if(T>1) {
    if(time_proc==3) {
      // in AR model, intercepts are constant over time
#include /chunks/l_hier_ar1_prior.stan
    } else if(time_proc==2){
      // in RW model, intercepts are used for first time period
#include /chunks/l_hier_prior.stan
    } else if(time_proc==4) {
      L_tp1 = L_tp2; // just copy over the variables, saves code if costs a bit of extra memory
                      // should be manageable memory loss
      L_full = rep_vector(0,num_legis); // not used but must be defined
    } else  {
      L_tp1[1] = L_full;
    } 
  } else {
    L_tp1[1] = L_full;
  }
}

model {
  //vectors to hold model calculations
  vector[N] pi1;
  vector[N] pi2;
  legis_x ~ normal(0,5);
  sigma_abs_x ~ normal(0,5);
  sigma_reg_x ~ normal(0,5);
  extra_sd ~ exponential(1);
  gp_sd ~ normal(0,2);
  ar_fix ~ normal(0,1);
  L_AR1_free ~ normal(0,ar_sd);

  if(model_type>2 && model_type<5) {
     for(i in 1:(m_step-2)) {
    steps_votes[i+1] - steps_votes[i] ~ normal(0,5);
  }
  } else {
    steps_votes ~ normal(0,5);
  }
    L_free ~ normal(0,legis_sd);
  
  if(time_proc==4) {
    {
    matrix[T, T] cov[gp_N]; // zero-length if not a GP model
    matrix[T, T] L_cov[gp_N];// zero-length if not a GP model
// chunk giving a GP prior to legislators/persons

for(n in 1:num_legis) {
  
  //create covariance matrices given current values of hiearchical parameters
  
  cov[n] =   cov_exp_quad(time_ind, m_sd[n], time_var[n])
      + diag_matrix(rep_vector(square(gp_sd_par),T));
  L_cov[n] = cholesky_decompose(cov[n]);

  to_vector(L_tp2[,n]) ~ multi_normal_cholesky(rep_vector(0,T), L_cov[n]); 
  
    
}
    }
  }
  
  for(t in 1:(T-1)) {
    L_tp1_var[t] ~ normal(0,1);
  }
    
  ls_int ~ normal(0,legis_sd);
  
  B_int_free ~ normal(0,diff_reg_sd);
  A_int_free ~ normal(0,diff_abs_sd);
  m_sd ~ inv_gamma(m_sd_par[2],1); // tight prior on GP marginal standard deviation ("bumps")
  
  //exog_param ~ normal(0,5);
  for(b in 1:num_bills) {
  steps_votes_grm[b] ~ normal(0,5);
  }
  

  time_var_restrict ~ exponential(1/time_sd);
  if(time_proc!=4) {
    time_var ~ exponential(1/time_sd);
  } else {
    time_var ~ lognormal(gp_length[1],num_diff[2]);
  }
  

// add correction for time-series models

if(T>1 && restrict_mean==1) {
  //additional restriction if GP fit is used
    (L_tp1[restrict_mean_ind[1],restrict_mean_ind[2]] - L_tp1[restrict_mean_ind[5],restrict_mean_ind[6]]) ~ normal(restrict_mean_val[1],.01);
    (L_tp1[restrict_mean_ind[3],restrict_mean_ind[4]] - L_tp1[restrict_mean_ind[7],restrict_mean_ind[8]]) ~ normal(restrict_mean_val[2],.01);
}
  

  //priors for legislators and bill parameters
#include /chunks/modeling_statement_v9.stan

  //all model types

#include /chunks/model_types.stan

}
generated quantities {
/*
vector[num_legis] L_tp2[gp_nT]; // equal to T if GPs are used
  row_vector[gp_nT] calc_values; // used to hold values from legis_preds
  if(time_proc==4) {
  
  for(n in 1:gp_N) {
    // generate estimated ideal points here to avoid creating all the correlation matrices
    for(t in 1:T) {
        calc_values[t] = legis_pred[t, n, ] * legis_x;
    }
    L_tp2[,n] = multi_normal_cholesky(calc_values * legis_x, L_cov[n]); 
    
  }
  */
}

