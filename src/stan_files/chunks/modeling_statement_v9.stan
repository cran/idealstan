
// Version 9: only one kind of constraint  possible for discrimination
// We include diff_high as an offset as we want this parameter to be halfway past 0 

  if(T==1) {
    restrict_high ~normal(diff_high, 
                    restrict_sd);
  } else {
    restrict_high ~normal(diff_high, 
                    restrict_sd);
  }
        



  sigma_abs_free~normal(0, discrim_abs_sd);
  sigma_reg_free~normal(0, discrim_reg_sd);
  


  
  
  



