/* This file sets up the various types of IRT models that can be run in idealstan */

if(model_type==1) {
  //2 PL no inflation
      
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];

        }
      }
      
      Y_new ~ bernoulli_logit(pi1);

} else if(model_type==2) {
  //2 PL inflation
  
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
        pi2 = (sigma_abs_free[bb] + sax_pred*sigma_abs_x) .* (L_full[ll] + legis_pred*legis_x) - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];
              pi2[n] = (sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_new[n] ~ bernoulli_logit(pi1[n]);
          }
        }

} else if(model_type==3) {
  //ratingscale no inflation
  
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) {
      Y_int[n] ~ ordered_logistic(pi1[n],steps_votes);
    }
        
        

} else if(model_type==4) {
  //ratingscale inflation

      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
        pi2 = (sigma_abs_free[bb] + sax_pred*sigma_abs_x) .* (L_full[ll] + legis_pred*legis_x) - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];
              pi2[n] = (sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_int[n] ~ ordered_logistic(pi1[n],steps_votes);
          }
        }

} else if(model_type==5) {
  //grm no inflation
      
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) 
        Y_int[n] ~ ordered_logistic(pi1[n],steps_votes_grm[bb[n]]);

} else if(model_type==6) {
  //grm inflation

      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
        pi2 = (sigma_abs_free[bb] + sax_pred*sigma_abs_x) .* (L_full[ll] + legis_pred*legis_x) - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];
              pi2[n] = (sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_int[n] ~ ordered_logistic(pi1[n],steps_votes_grm[bb[n]]);
          }
        }

} else if(model_type==7) {
  //poisson no inflation
  
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) {
      Y_int[n] ~ poisson(exp(pi1[n]));
    }
        
        

} else if(model_type==8) {
  //hurdle poisson
  
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
        pi2 = (sigma_abs_free[bb] + sax_pred*sigma_abs_x) .* (L_full[ll] + legis_pred*legis_x) - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];
              pi2[n] = (sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          if(zeroes==1) {
            Y_int[n] ~ poisson(exp(pi1[n])) T[1,];
          } else {
            Y_int[n] ~ poisson(exp(pi1[n]));
          }
          
          }
        }

} else if(model_type==9) {
  //normal no inflation
  
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) {
      Y_cont[n] ~ normal(pi1[n],extra_sd);
    }
        
        

} else if(model_type==10) {
  //normal hurdle
  
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
        pi2 = (sigma_abs_free[bb] + sax_pred*sigma_abs_x) .* (L_full[ll] + legis_pred*legis_x) - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];
              pi2[n] = (sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_cont[n] ~ normal(pi1[n],extra_sd);
          }
        }

} else if(model_type==11) {
  //lognormal no inflation
  
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
      } else {
        for(n in 1:N) {

        pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];

        }
      }
      
    for(n in 1:N) {
      Y_cont[n] ~ lognormal(exp(pi1[n]),extra_sd);
    }

} else if(model_type==12) {
  //hurdle lognormal
  
      if(T==1) {
        pi1 = (sigma_reg_free[bb] + srx_pred*sigma_reg_x) .*  (L_full[ll] + legis_pred*legis_x) - B_int_free[bb];
        pi2 = (sigma_abs_free[bb] + sax_pred*sigma_abs_x) .* (L_full[ll] + legis_pred*legis_x) - 
                  A_int_free[bb] ;
      } else {
        for(n in 1:N) {
          
              pi1[n] = (sigma_reg_free[bb[n]] + srx_pred[n,]*sigma_reg_x) *  (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - B_int_free[bb[n]];
              pi2[n] = (sigma_abs_free[bb[n]] + sax_pred[n,]*sigma_abs_x) * (L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - 
                    A_int_free[bb[n]] ; 

        }
      }

      for(n in 1:N) {
        
        if(absence[n]==1) {
  	      1 ~ bernoulli_logit(pi2[n]);
        } else {
          0 ~ bernoulli_logit(pi2[n]);
          Y_cont[n] ~ lognormal(exp(pi1[n]),extra_sd);
          }
        }

} else if(model_type==13) {
  //latent space non-inflated (normal parameterization)
  if(T==1) {
    
        pi1 = ls_int[ll]  + sigma_abs_free[bb] -  sqrt(square( (L_full[ll] + legis_pred*legis_x) - (B_int_free[bb] + srx_pred*sigma_reg_x)));

  } else {
      for(n in 1:N) {

        pi1[n] = ls_int[ll[n]] + sigma_abs_free[bb[n]] -
                  sqrt(square((L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[bb[n]] + srx_pred[n,]*sigma_reg_x)));

      }
    }

  Y_new ~ bernoulli_logit(pi1);
  
} else if(model_type==14) {
  //latent space inflated (idealstan parameterization)
    if(T==1) {
        pi1 = -sqrt(square((L_full[ll] + legis_pred*legis_x) - (B_int_free[bb] + srx_pred*sigma_reg_x)));
        pi2 = -sqrt(square((L_full[ll] + legis_pred*legis_x) - (A_int_free[bb] + sax_pred*sigma_abs_x)));
    } else {
        for(n in 1:N) {
          
              pi1[n] = -sqrt(square((L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - (B_int_free[bb[n]] + srx_pred[n,]*sigma_reg_x)));
              pi2[n] = -sqrt(square((L_tp1[time[n],ll[n]] + legis_pred[n,]*legis_x) - (A_int_free[bb[n]] + sax_pred[n,]*sigma_abs_x))); 

        }
    }

    for(n in 1:N) {
        
        if(absence[n]==1) {
          // multiply by 2 to make the model work across the probability scale
  	      target += log(2) + bernoulli_logit_lpmf(1|pi2[n]);
        } else {
          target += log(2) + bernoulli_logit_lpmf(0|pi2[n]);
          target += log(2) + bernoulli_logit_lpmf(Y_new[n]|pi1[n]);
          }
    }
  
}

