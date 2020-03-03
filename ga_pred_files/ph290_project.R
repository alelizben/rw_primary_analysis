setwd('~/Documents/ptbi/ga_prediction/')

devtools::install_github("tlverse/tmle3shift", dependencies = T)
devtools::install_github("tlverse/tmle3", dependencies = T)
devtools::install_github('tlverse/sl3', dependencies = T)

devtools::install_github('osofr/condensier', build_vignettes = FALSE)
devtools::install_github('osofr/simcausal', build_vignettes = FALSE)
library(tidyverse)
library(data.table)
library(condensier)
library(ranger)
library(sl3)
library(tmle3)
library(tmle3shift)

#dat = read.csv('sample_ga_data_binomial.csv', row.names = 1)
#trainb = read.csv('lmp_sample_data_binomial.csv', row.names = 1)
train = read.csv('sample_ga_data_binomial_2019-04-26.csv', row.names = 1)
#dat = read.csv('lmp_sample_data_gauss.csv')
#X = make_x(traing)

lrn1 <- Lrnr_mean$new()
lrn2 <- Lrnr_glm$new()
lrn3 <- Lrnr_ranger$new()
sl_lrn <- Lrnr_sl$new(
  learners = list(lrn1, lrn2, lrn3),
  metalearner = Lrnr_nnls$new()
)

# learners used for conditional density regression (e.g., propensity score)
lrn1_dens <- Lrnr_condensier$new(
  nbins = 10, bin_estimator = lrn1,
  bin_method = "dhist"
)
lrn2_dens <- Lrnr_condensier$new(
  nbins = 15, bin_estimator = lrn2,
  bin_method = "dhist"
)
lrn3_dens <- Lrnr_condensier$new(
  nbins = 20, bin_estimator = lrn3,
  bin_method = "dhist"
)
sl_lrn_dens <- Lrnr_sl$new(
  learners = list(lrn1_dens, lrn2_dens, lrn3_dens),
  metalearner = Lrnr_solnp_density$new()
)
#devtools::install_github('nhejazi/haldensify')

# hal_dens <- Lrnr_haldensify$new(
#   grid_type = "equal_mass",
#   n_bins = 5
# )

Q_learner <- sl_lrn
g_learner <- sl_lrn_dens
#g_learner <- hal_dens
learner_list <- list(Y = Q_learner, A = g_learner)

new_tmle = function(likelihd, update, tmle_task, tmle_spec, updater) {
  
  targeted_likelihood <- Targeted_Likelihood$new(likelihd, update)
  
  # define parameter
  tmle_params <- tmle_spec$make_params(tmle_task, targeted_likelihood)
  updater$tmle_params <- tmle_params
  ate <- tmle_params[[1]]
  
  # fit tmle update
  tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, list(ate), update, max_it)
  #out = c(tmle_fit$summary$tmle_est, tmle_fit$summary$se, tmle_fit$summary$lower, tmle_fit$summary$upper)
  return(tmle_fit)
  
}

get_sd  = function(tm_fit, stat1, stat2) {
  # test_sd = sqrt(var(tm_fit$estimates[[stat1]]$IC - tm_fit$estimates[[stat2]]$IC)/nrow(tm_fit$estimates[[stat2]]$IC))
  sd1 = sqrt(var(tm_fit$estimates[[1]]$IC)/length(tm_fit$estimates[[1]]$IC))
  sd2 = sqrt(var(tm_fit$estimates[[2]]$IC)/length(tm_fit$estimates[[2]]$IC))
  sd3 = sqrt(var(tm_fit$estimates[[3]]$IC)/length(tm_fit$estimates[[3]]$IC))
  
  test_sd = sqrt(var(tm_fit$estimates[[stat2]]$IC - tm_fit$estimates[[stat1]]$IC)/nrow(tm_fit$estimates[[stat2]]$IC))
  test_stat = ((tm_fit$estimates[[stat2]]$psi) - (tm_fit$estimates[[stat1]]$psi)) /test_sd
  ate = tm_fit$estimates[[stat2]]$psi - tm_fit$estimates[[stat1]]$psi
  pv = 2 * pnorm(-abs(test_stat))
  
  ## MSM ##
  var_D <- cov(tm_fit$estimates[[4]]$IC)
  n <- nrow(tm_fit$estimates[[4]]$IC)
  se <- sqrt(diag(var_D) / n)
  level <- 0.95
  beta_stat = tm_fit$estimates[[4]]$psi[2] / (se[2])
  pval_beta = 2 * pnorm(-abs(beta_stat))
  pval = 2 * pnorm(-abs(test_stat))
  
  #return(c(pval_beta, pval))
  return(c(ate, pval, test_sd, beta_stat, pval_beta, se))
}

Xcont_date = subset(train, select = c(ga_at_anc1_by_lmp, ga_anc1_by_edd, ga_at_deliv_lmp, ga_at_delivery_by_ga_anc1, 
                    ga_at_delivery_by_edd_anc1, ga_at_delivery_by_fh_anc1))

Xcont = subset(train, select = -c(m_stature, fuel, enough_food, ever_no_food, run_out_food, not_enough_food,
                              hh_smoker, alc, sex, dhc, y,
                              ga_at_anc1_by_lmp, ga_anc1_by_edd, ga_at_deliv_lmp, ga_at_delivery_by_ga_anc1, 
                                ga_at_delivery_by_edd_anc1, ga_at_delivery_by_fh_anc1))
Xcat = subset(train, select = c(m_stature, fuel, enough_food, ever_no_food, run_out_food, not_enough_food,
                              hh_smoker, alc, sex))
#current_a = current_a + runif(-0.1, 0.1, n = length(current_a))
#dat[,Wnames[i]] <- current_a
train[, names(Xcont)] <- Xcont + runif(0, 0.1, n = nrow(Xcont))
Wnames = c(names(Xcont), names(Xcont_date))
Wnames_cat = names(Xcat)

#center and scale data first if 
#pvals_beta = rep(NA, length(Wnames))
#pvals = rep(NA, length(Wnames))
run_vim_shift = function(train, Wnames) {
  
  ates = list()
  for (i in 1:(length(Wnames))) {
    dat = data.frame(train)
    node_list <- list(W = names(dat)[!(names(dat) %in% c(Wnames[i], "y", "X"))], A = Wnames[i], Y = "y")
    node_list
    
    #current_a = Xcont$fun_ht_anc1
    current_a = dat[,which(names(dat) == Wnames[i])]
    print(class(dat))
    delta_grid <- c(-sd(current_a), 0,  sd(current_a))
    #delta_grid <- c(-sd(current_a), 0,  sd(current_a))
    delta_grid
    #delta_grid <- seq(min(current_a), max(current_a), sd(current_a))
    #seq(from = min(current_a), to = max(current_a), length.out = 3)
    
    # initialize a tmle specification
    tmle_spec <- tmle_vimshift_delta(shift_grid = delta_grid,
                                     max_shifted_ratio = 3)
    tmle_fit <- tmle3(tmle_spec, dat, node_list, learner_list)
    
    ates[[i]] = get_sd(tmle_fit, 2, 3)
    
  }
  #load(file = "var_imp_vimpshift_26_apr_2019.Rdata")
  names(ates) = Wnames
  results_tab = round(do.call(rbind, ates), 6)
  #results_tab
  colnames(results_tab) <- c("ate", "pval", "test_sd", "beta_stat", "pval_beta", "se_int", "se_beta")
  results_tab
  ordered_tab = results_tab[order(abs(results_tab[,1]), decreasing = T),]
  return(ordered_tab) 
}

#save(ates, file = "var_imp_vimpshift_26_apr_2019.Rdata")


##### NOW FOR CATEGORICAL VARIABLES ###

lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glmnet <- make_learner(Lrnr_glmnet)

#lrnr_bart <- make_learner(Lrnr_bartMachine)
stack <- make_learner(Stack, lrnr_glm, lrnr_mean, lrnr_glmnet)
metalearner <- make_learner(Lrnr_nnls)
sl <- Lrnr_sl$new(learners = stack, metalearner = metalearner)
learner_list_cat <- list(Y = sl, A = sl)
#dhc and fun_ht_1

run_cat_vars = function(train, Wnames_cat) {
  reg_ate = list()
  for (i in 1:length(Wnames_cat)) {
    
    dat = data.frame(train)
    node_list <- list(W = names(dat)[!(names(dat) %in% c(Wnames_cat[i], "y", "X"))], A = Wnames_cat[i], Y = "y")
    node_list
    
    ########
    tmle_spec <- tmle_ATE(1,0)
    
    # define data
    tmle_task <- tmle_spec$make_tmle_task(dat, node_list)
    
    #this one is standard tmle (no c-tmle like approach)
    initial_likelihood = tmle_spec$make_initial_likelihood(tmle_task, learner_list_cat)
    updater <- tmle3_Update$new()
    ########
    reg_tmle = new_tmle(initial_likelihood, tmle3_Update$new(), tmle_task, tmle_spec, updater)
    estim = reg_tmle$estimates[[1]]$psi
    test_sd = sqrt(var(reg_tmle$estimates[[1]]$IC)/nrow(train))
    test_stat = estim/test_sd
    pval = 2 * pnorm(-abs(test_stat))
    reg_ate[[i]] = c(estim, pval, test_sd)
    ########
  }
  names(reg_ate) = Wnames_cat
  results_tab = round(do.call(rbind, reg_ate), 6)
  #results_tab
  colnames(results_tab) <- c("ate", "pval", "test_sd")
  results_tab
  ordered_tab = results_tab[order(abs(results_tab[,1]), decreasing = T),]
  return(ordered_tab) 
}

##combine rankings for cat vars and continuous vars
run_combined_var_imp = function(train, Wnames, Wnames_cat){
  
  conts = run_vim_shift(train, Wnames)
  cat = run_cat_vars(train, Wnames_cat)
  combined_ordered = sort(abs(c(conts[, 1], cat[, 1])), decreasing = T)
  return(names(combined_ordered))
  
}

run_combined_var_imp(train, Wnames, Wnames_cat)