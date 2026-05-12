## ----setup, include=FALSE, cache=FALSE----------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning=FALSE,fig.align = 'center',fig.width=6, fig.height=5)

library(idealstan)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tinytable)

options(cmdstanr_warn_inits = FALSE)


## ----sim_data-----------------------------------------------------------------
#| cache: false

ord_ideal_sim <- id_sim_gen(model='ordinal_grm',inflate = T)



## ----constrain_sim------------------------------------------------------------
#| eval: false

# true_legis <- ord_ideal_sim@simul_data$true_person
# high_leg <- sort(true_legis,decreasing = TRUE,index.return=TRUE)
# low_leg <- sort(true_legis,index.return=TRUE)
# 
# ord_ideal_est <- id_estimate(idealdata=ord_ideal_sim,
#                              model_type=6,
#                              fixtype='prefix',
#                              restrict_ind_high = as.character(high_leg$ix[1]),
#                              restrict_ind_low=as.character(low_leg$ix[1]),
#                              fix_high = sort(true_legis,decreasing = TRUE)[1],
#                              fix_low = sort(true_legis,decreasing = FALSE)[1],
#                              id_refresh=500,
#                              ncores=8,
#                              nchains=2)


## ----check_true---------------------------------------------------------------
#| eval: false

# id_plot_persons(ord_ideal_est,show_true = TRUE)


## ----restrict_auto------------------------------------------------------------
#| eval: false
# ord_ideal_est <- id_estimate(idealdata=ord_ideal_sim,
#                              model_type=6,
#                              id_refresh=2000,fixtype="vb_full",
#                              ncores=8,
#                            nchains=2)


## ----rhats--------------------------------------------------------------------
#| eval: false
#| 
# id_plot_rhats(ord_ideal_est)
# id_plot_persons(ord_ideal_est,show_true = T)

