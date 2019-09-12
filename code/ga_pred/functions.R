date_cleaning = function(date) {
  x = as.numeric(make_year(date))
  index1 = which(x == 1017 | x == 0217 | x == 2027 | x == 3017 | x == 2107)
  index2 = which(x == 3018 | x == 2008 | x == 2028)
  char_date = as.character(date)
  
  if (length(index1 > 0)) {
    fixed_date1 = mapply(function(x, y) sub(x, "2017", y), substr(char_date[index1], 1, 4), char_date[index1])
    char_date[index1] <- fixed_date1
  } 
  if (length(index2 > 0)) {
    fixed_date2 = mapply(function(x, y) sub(x, "2018", y), substr(char_date[index2], 1, 4), char_date[index2])
    char_date[index2] <- fixed_date2
    
  }
  
  out = as.Date(char_date)
  
  return(out)
  
}

filter_dates = function(x, year_lower, year_upper){
  
  year = format(x, "%Y")
  year_bound = (year <= year_upper & year >= year_lower)
  
  return(year_bound)
}


make_year = function(x) {
  year = format(x, "%Y")
  return(year)
}


# choose_lmp_or_us = function(data, x, y, z) {
#   ga_at_del_adj = data$temp_ga_del_us
#   
#   diff_btwn_lmp = as.numeric(abs(data$ga_at_deliv_lmp - data$temp_ga_del_us))
#   index = which(data$ga_at_us >= x & data$ga_at_us < y & diff_btwn_lmp <= z)
#   print(length(index))
#   ga_at_del_adj[index] = vars$ga_at_deliv_lmp[index]
#   
#   return(list(ga_at_del_adj, index))
# }


SL.bartMachine2 <- function(Y, X, newX, family, obsWeights, id,
                            num_trees = 50, num_burn_in = 250, verbose = F,
                            alpha = 0.95, beta = 2, k = 2, q = 0.9, nu = 3,
                            num_iterations_after_burn_in = 1000,
                            ...) {
  #.SL.require("bartMachine")
  
  ################
  ### CK changes:
  if (family$family == "binomial") {
    # Need to convert Y to a factor, otherwise bartMachine does regression.
    # And importantly, bartMachine expects the first level to be the positive
    # class, so we have to specify levels.
    Y = factor(Y, levels = c("1", "0"))
  }
  model = bartMachine::bartMachine(X, Y, num_trees = num_trees,
                                   num_burn_in = num_burn_in, verbose = verbose,
                                   alpha = alpha, beta = beta, k = k, q = q, nu = nu,
                                   num_iterations_after_burn_in = num_iterations_after_burn_in)
  # pred returns predicted responses (on the scale of the outcome)
  pred <- bartMachine:::predict.bartMachine(model, newX)
  
  fit <- list(object = model)
  class(fit) <- c("SL.bartMachine")
  
  out <- list(pred = pred, fit = fit)
  return(out)
}


# arm1 = read.csv('wide_form_deid_arm1_2018-07-03.csv')
# 
# arm2 = read.csv('wide_form_deid_arm2_2018-07-03.csv')
# 
# arm3 = read.csv('wide_form_deid_arm3_2018-07-03.csv')
# 
# arm4 = read.csv('wide_form_deid_arm4_2018-07-03.csv')

get_us_fix_date = function(x, arm) {
  x[x == "#NULL!" | x == " " ] <- NA
  #date_del = date_cleaning(as.Date(x[, 'date_delivery_mat'], "%Y-%m-%d"))
  date_del = fix_date(x[, 'date_delivery_mat'])
  #date_del2 = (x[, sprintf('date_delivery_mat.delivery_arm_%s', arm)])
  #date_anc1 = date_cleaning(as.Date(x[, 'anc1date_anc.anc1_visit_arm_%s'], "%Y-%m-%d"))
  date_anc1 = fix_date(x[, 'anc1date_anc'])
  #  date_anc12 = (x[, sprintf('anc1date_anc.anc1_visit_arm_%s', arm)])
  days_from_anc1_to_del = (date_del - date_anc1)
  wks_from_anc1_to_del = (date_del - date_anc1)/7
  
  # 
  # if (date_del < date_anc1) {
  #   
  #   print("deliver before anc1")
  # }
  # age_vars = arm2[,grep('m_age', names(arm2), value = T)]
  # apply(age_vars, 2, function(x) mean(!is.na(x)))
  # age_vars = arm4[,grep('m_age', names(arm4), value = T)]
  # apply(age_vars, 2, function(x) mean(!is.na(x)))
  # table(arm4$m_age_anc.anc1_visit_arm_4)
  age_data = subset(x, select = c(m_age_anc, m_age))
  mismatch_age = age_data[age_data$m_age_anc == age_data$m_age, ]
  
  m_age_enroll = as.numeric(as.character(x[ ,'m_age']))
  m_age_anc1 = as.numeric(x[ ,'m_age_anc'])
  #as.numeric
  
  m_stature =  as.numeric(x[ , 'ht_und150'])
  m_wt = as.numeric(x[ , 'wt_1stvisit'])
  #miscar = as.numeric(x[, sprintf("hx_rptdmiscar.anc1_visit_arm_%s", arm)])
  #ptdel = as.numeric(x[, sprintf("hx_ptdlvry.anc1_visit_arm_%s", arm)])
  #prevfsb = as.numeric(x[, sprintf("hx_prevfsb.anc1_visit_arm_%s", arm)])
  parity = as.numeric(x[, "parity_anc"])
  grav = as.numeric(x[, "gravidity_anc"])
  
  fuel = as.numeric(x[, sprintf("fuel_use")])
  
  enough_food = as.numeric(x[, sprintf("hh_enoughfood")])
  ever_no_food = as.numeric(x[, sprintf("hh_evernofood")])
  run_out_food = as.numeric(x[, sprintf("hh_runoutfood")])
  not_enough_food = as.numeric(x[, sprintf("hh_notenoughfood")])
  
  smoke = as.numeric(x[, sprintf("preg_smoke")])
  hh_smoker = as.numeric(x[, sprintf("hh_smoker")])
  
  alc = as.numeric(x[, sprintf("preg_alc")])
  
  sex = factor(x[, 'infant_sex'],
               levels = c(1,2), labels = c(0,1))
  muac = as.numeric(x[, sprintf('muac_birth')])
  chest_c = as.numeric(x[, sprintf('cc_birth')])
  weight = as.numeric(x[, sprintf('bwt_birth')])
  
  weight[!is.na(weight) & (weight/1000) < 1] <- (weight * 1000)[!is.na(weight) & (weight/1000) < 1]
  
  head_cir =  as.numeric(x[ ,sprintf('hc_birth')])
  length_b =  as.numeric(x[ ,sprintf('length_birth')])
  apgar1 = as.numeric(x[, sprintf('apgar_1mbirth')])
  apgar5 = as.numeric(x[, sprintf('apgar_5mbirth')])
  
  # us,
  frame = data.frame(m_age_enroll, m_age_anc1, m_stature, m_wt, parity, grav, fuel, enough_food, ever_no_food,
                     run_out_food, not_enough_food, smoke, hh_smoker, alc,
                     sex, weight, head_cir, length_b, apgar1, apgar5)
  
  
  ## START OTHER SCRIPT ##
  
  study_id_enroll = x[, sprintf('study_id_full')]
  study_id_anc1 = x[, sprintf('study_id_anc')]
  dhc = substr(as.character(study_id_enroll), 1,3)
  #dhc = x[, sprintf('study_id_dhc')]
  #dhc = substr(study_id, 1, 3)
  
  ## Question 1 ##
  #lmp = date_cleaning(as.Date(x[, sprintf('lmp_anc')], format = "%Y-%m-%d"))
  lmp = fix_date(x[, sprintf('lmp_anc')])
  wks_from_lmp_to_del = (date_del - lmp) / 7
  
  ga_at_anc1_by_lmp = (date_anc1 - lmp) / 7
  
  ga_at_deliv_lmp = (ga_at_anc1_by_lmp) + (days_from_anc1_to_del / 7)
  ##create new variable "GA at delivery by LMP"
  
  
  ## Question 2 ### Create GA at delivery by EDD at ANC1 ##
  
  #edd = date_cleaning(as.Date(x[, sprintf('edd_anc')], format = '%Y-%m-%d'))
  edd = fix_date(x[, sprintf('edd_anc')])
  
  ga_at_delivery_by_edd_anc1 = (280 + (date_del - edd)) / 7
  
  #Question 3
  ##GA AT DELIVERY BY GA AT ANC1 ##
  ga_anc1_recorded = (x[, sprintf('anc1ga_anc')])
  ga_anc1_by_edd = 40 - ((edd - date_anc1)/7)
  
  ga_at_delivery_by_ga_anc1 = ga_anc1_recorded + (days_from_anc1_to_del / 7)
  
  ########
  ## Question 4 ### Create GA at delivery by HC US##
  if (arm == 2 | arm == 4) {
    #us_edd = date_cleaning(as.Date(x[, sprintf('edd_us')], format = "%Y-%m-%d"))
    us_edd = fix_date(x[, sprintf('edd_us')])
    
    usga = (x[, sprintf('us_adjga')])
    #table(usga)
    new_usga = as.numeric(gsub("[^0-9\\.]", "", as.character(usga)))
    ga_at_us = (ifelse((new_usga/10) >= 4, new_usga/10, new_usga))
    
    #print(c("ga_at_us_uncleaned", table(usga)))
    #print(c("ga_at_us_clean", table(ga_at_us)))
    
    us_date =  date_cleaning(as.Date(x[, sprintf('us_date')], format = "%Y-%m-%d"))
    us_date =  fix_date(x[, sprintf('us_date')])
    
    ga_del_by_us_date = ga_at_us + (date_del - us_date)/7
    edd_by_us_date = 7*(40 - ga_at_us) + us_date
    
    ga_us_diff = date_del - us_edd
    ga_del_us = (280 + ga_us_diff) / 7
    
  } else {
    ga_del_us = rep(NA, nrow(x))
    ga_at_us = rep(NA, nrow(x))
    us_date = rep(NA, nrow(x))
  }
  
  wks_from_lmp_to_us = (us_date - lmp)/7
  
  wks_from_anc1_to_us = (us_date - date_anc1)/7
  wks_from_us_to_del = (date_del - us_date)/7
  ######## Question 5 ## GA at delivery by FH @ anc1 ### 
  
  ####
  fun_ht_anc1 = x[, sprintf('fundalht_1stvisit')]
  ## check serial fundal height
  
  ga_at_delivery_by_fh_anc1 = fun_ht_anc1 + (days_from_anc1_to_del/7)
  ga_at_delivery_by_fh_anc1
  
  ## ga weeks delivery recoded
  ga_weeks_recorded = x[, sprintf('ga_weeks')]
  ga_weeks = as.numeric(gsub("[^0-9\\.]", "", as.character(ga_weeks_recorded)))
  ga_weeks_deliv_recorded = (ifelse((ga_weeks/10) >= 4.6, ga_weeks/10, ga_weeks))
  #print(c("ga_weeks_deliv_recorded_unclean", table(ga_weeks_recorded)))
  #print(c("ga_weeks_deliv_recorded_clean", table(ga_weeks_deliv_recorded)))
  ga_weeks_deliv_recorded[ga_weeks_deliv_recorded == 9 | ga_weeks_deliv_recorded <= 6]  <- 41
  ### GA WEEKS ################
  #date_anc12, date_del2, date_anc1, date_del, lmp, days_from_anc1_to_del, 
  #ga_at_delivery_by_ga_anc1, ga_at_delivery_by_edd_anc1, ga_at_delivery_by_us,
  
  
  final = data.frame(study_id_enroll, study_id_anc1, dhc, wks_from_anc1_to_del,
                     wks_from_anc1_to_us,
                     wks_from_lmp_to_us, wks_from_lmp_to_del,
                     wks_from_us_to_del,
                     lmp, date_anc1, us_date, date_del, 
                     ga_at_anc1_by_lmp, ga_anc1_by_edd, ga_anc1_recorded,
                     ga_at_us, fun_ht_anc1,
                     edd, edd_by_us_date, us_edd, ga_del_by_us_date,
                     ga_at_deliv_lmp, ga_at_delivery_by_ga_anc1, 
                     ga_at_delivery_by_edd_anc1,
                     ga_at_delivery_by_fh_anc1, ga_del_us, ga_weeks_deliv_recorded)
  
  
  #index_bounds = filter_dates(date_anc1, "2016", "2019") & filter_dates(date_del, "2016", "2019") & filter_dates(lmp, "2016", "2019")
  
  #return(final)  
  #return(final_bounds)
  ## END OTHER SCRIPT ##
  index_bounds = filter_dates(date_anc1, "2016", "2019") & filter_dates(date_del, "2016", "2019") & filter_dates(lmp, "2016", "2019")
  #final_bounds = final[which(index_bounds == T), ]
  #
  
  #[which(index_bounds == T), ]
  frame = data.frame(final, frame)
  return(frame)
}

get_us = function(x, arm) {
  x[x == "#NULL!" | x == " " ] <- NA
  #date_del = date_cleaning(as.Date(x[, 'date_delivery_mat'], "%Y-%m-%d"))
  date_del = fix_date(x[, 'date_delivery_mat'])
  date_del_o = (x[, 'date_delivery_mat'])
  #date_del2 = (x[, sprintf('date_delivery_mat.delivery_arm_%s', arm)])
  #date_anc1 = date_cleaning(as.Date(x[, 'anc1date_anc.anc1_visit_arm_%s'], "%Y-%m-%d"))
  date_anc1 = fix_date(x[, 'anc1date_anc'])
  date_anc1_o = (x[, 'anc1date_anc'])
  #  date_anc12 = (x[, sprintf('anc1date_anc.anc1_visit_arm_%s', arm)])
  days_from_anc1_to_del = (date_del - date_anc1)
  wks_from_anc1_to_del = as.numeric((date_del - date_anc1)/7)

  # 
  # if (date_del < date_anc1) {
  #   
  #   print("deliver before anc1")
  # }
  # age_vars = arm2[,grep('m_age', names(arm2), value = T)]
  # apply(age_vars, 2, function(x) mean(!is.na(x)))
  # age_vars = arm4[,grep('m_age', names(arm4), value = T)]
  # apply(age_vars, 2, function(x) mean(!is.na(x)))
  # table(arm4$m_age_anc.anc1_visit_arm_4)
  age_data = subset(x, select = c(m_age_anc, m_age))
  mismatch_age = age_data[age_data$m_age_anc == age_data$m_age, ]
  
  m_age_enroll = as.numeric(as.character(x[ ,'m_age']))
  m_age_anc1 = as.numeric(x[ ,'m_age_anc'])
  #as.numeric
  
  m_stature =  as.numeric(x[ , 'ht_und150'])
  m_wt = as.numeric(x[ , 'wt_1stvisit'])
  #miscar = as.numeric(x[, sprintf("hx_rptdmiscar.anc1_visit_arm_%s", arm)])
  #ptdel = as.numeric(x[, sprintf("hx_ptdlvry.anc1_visit_arm_%s", arm)])
  #prevfsb = as.numeric(x[, sprintf("hx_prevfsb.anc1_visit_arm_%s", arm)])
  parity = as.numeric(x[, "parity_anc"])
  grav = as.numeric(x[, "gravidity_anc"])
  
  fuel = as.numeric(x[, sprintf("fuel_use")])
  
  enough_food = as.numeric(x[, sprintf("hh_enoughfood")])
  ever_no_food = as.numeric(x[, sprintf("hh_evernofood")])
  run_out_food = as.numeric(x[, sprintf("hh_runoutfood")])
  not_enough_food = as.numeric(x[, sprintf("hh_notenoughfood")])
  
  smoke = as.numeric(x[, sprintf("preg_smoke")])
  hh_smoker = as.numeric(x[, sprintf("hh_smoker")])
  
  alc = as.numeric(x[, sprintf("preg_alc")])
  
  sex = factor(x[, 'infant_sex'],
               levels = c(1,2), labels = c(0,1))
  muac = as.numeric(x[, sprintf('muac_birth')])
  chest_c = as.numeric(x[, sprintf('cc_birth')])
  weight = as.numeric(x[, sprintf('bwt_birth')])
  
  weight[!is.na(weight) & (weight/1000) < 1] <- (weight * 1000)[!is.na(weight) & (weight/1000) < 1]
  
  head_cir =  as.numeric(x[ ,sprintf('hc_birth')])
  length_b =  as.numeric(x[ ,sprintf('length_birth')])
  apgar1 = as.numeric(x[, sprintf('apgar_1mbirth')])
  apgar5 = as.numeric(x[, sprintf('apgar_5mbirth')])
  
  # us,
  frame = data.frame(m_age_enroll, m_age_anc1, m_stature, m_wt, parity, grav, fuel, enough_food, ever_no_food,
                     run_out_food, not_enough_food, smoke, hh_smoker, alc,
                     sex, weight, head_cir, length_b, apgar1, apgar5)
  
  
  ## START OTHER SCRIPT ##
  
  study_id_enroll = x[, sprintf('study_id_full')]
  study_id_anc1 = x[, sprintf('study_id_anc')]
  dhc = substr(as.character(study_id_enroll), 1,3)
  #dhc = x[, sprintf('study_id_dhc')]
  #dhc = substr(study_id, 1, 3)
  
  ## Question 1 ##
  #lmp = date_cleaning(as.Date(x[, sprintf('lmp_anc')], format = "%Y-%m-%d"))
  lmp = fix_date(x[, sprintf('lmp_anc')])
  lmp_o = (x[, sprintf('lmp_anc')])
  wks_from_lmp_to_del = as.numeric((date_del - lmp) / 7)
  
  ga_at_anc1_by_lmp = as.numeric((date_anc1 - lmp) / 7)
  
  ga_at_deliv_lmp = as.numeric((ga_at_anc1_by_lmp) + (days_from_anc1_to_del / 7))
  ##create new variable "GA at delivery by LMP"
  
  
  ## Question 2 ### Create GA at delivery by EDD at ANC1 ##
  
  #edd = date_cleaning(as.Date(x[, sprintf('edd_anc')], format = '%Y-%m-%d'))
  edd = fix_date(x[, sprintf('edd_anc')])
  edd_o = (x[, sprintf('edd_anc')])
  
  ga_at_delivery_by_edd_anc1 = as.numeric((280 + (date_del - edd)) / 7)
  
  #Question 3
  ##GA AT DELIVERY BY GA AT ANC1 ##
  ga_anc1_recorded = (x[, sprintf('anc1ga_anc')])
  ga_anc1_by_edd = as.numeric(40 - ((edd - date_anc1)/7))
  
  ga_at_delivery_by_ga_anc1 = as.numeric(ga_anc1_recorded + (days_from_anc1_to_del / 7))
  
  ########
  
  
  ## Question 4 ### Create GA at delivery by HC US##
  if (arm == 2 | arm == 4) {
    #us_edd = date_cleaning(as.Date(x[, sprintf('edd_us')], format = "%Y-%m-%d"))
    us_edd = fix_date(x[, sprintf('edd_us')])
    us_edd_o = (x[, sprintf('edd_us')])
    
    usga = (x[, sprintf('us_adjga')])
    #table(usga)
    new_usga = as.numeric(gsub("[^0-9\\.]", "", as.character(usga)))
    ga_at_us = (ifelse((new_usga/10) >= 4, new_usga/10, new_usga))
    
    #print(c("ga_at_us_uncleaned", table(usga)))
    #print(c("ga_at_us_clean", table(ga_at_us)))

    us_date =  fix_date(x[, sprintf('us_date')])
    us_date_o =  (x[, sprintf('us_date')])
    
    ga_del_by_us_date = as.numeric(ga_at_us + (date_del - us_date)/7)
    edd_by_us_date = as.numeric(7*(40 - ga_at_us) + us_date)
    
    ga_us_diff = date_del - us_edd
    ga_del_by_us_edd = as.numeric((280 + ga_us_diff) / 7)
    
  } else {
    ga_del_us = rep(NA, nrow(x))
    ga_at_us = rep(NA, nrow(x))
    us_date = rep(NA, nrow(x))
  }
  
  wks_from_lmp_to_us = as.numeric((us_date - lmp)/7)
  
  wks_from_anc1_to_us = as.numeric((us_date - date_anc1)/7)
  wks_from_us_to_del = as.numeric((date_del - us_date)/7)
  ######## Question 5 ## GA at delivery by FH @ anc1 ### 
  
  ####
  fun_ht_anc1 = x[, sprintf('fundalht_1stvisit')]
  
  ga_at_delivery_by_fh_anc1 = as.numeric(fun_ht_anc1 + (days_from_anc1_to_del/7))
  ga_at_delivery_by_fh_anc1
  
  ## ga weeks delivery recoded
  ga_weeks_recorded = x[, sprintf('ga_weeks')]
  ga_weeks = as.numeric(gsub("[^0-9\\.]", "", as.character(ga_weeks_recorded)))
  ga_weeks_deliv_recorded = (ifelse((ga_weeks/10) >= 4.6, ga_weeks/10, ga_weeks))
  #print(c("ga_weeks_deliv_recorded_unclean", table(ga_weeks_recorded)))
  #print(c("ga_weeks_deliv_recorded_clean", table(ga_weeks_deliv_recorded)))
  ga_weeks_deliv_recorded[ga_weeks_deliv_recorded == 9 | ga_weeks_deliv_recorded <= 6]  <- 41
  ### GA WEEKS ################
  #date_anc12, date_del2, date_anc1, date_del, lmp, days_from_anc1_to_del, 
  #ga_at_delivery_by_ga_anc1, ga_at_delivery_by_edd_anc1, ga_at_delivery_by_us,
  
  final = data.frame(study_id_enroll, study_id_anc1, dhc, wks_from_anc1_to_del,
                     wks_from_anc1_to_us,
                     wks_from_lmp_to_us, wks_from_lmp_to_del,
                     wks_from_us_to_del,
                     lmp, lmp_o, date_anc1, date_anc1_o, us_date, us_date_o, date_del, date_del_o, 
                     edd, edd_o, us_edd, us_edd_o,
                     ga_at_anc1_by_lmp, ga_anc1_by_edd, ga_anc1_recorded,
                     ga_at_us, fun_ht_anc1,
                     edd_by_us_date, 
                     ga_del_by_us_date, ga_del_by_us_edd,
                     ga_at_deliv_lmp, ga_at_delivery_by_ga_anc1, 
                     ga_at_delivery_by_edd_anc1,
                     ga_at_delivery_by_fh_anc1,  ga_weeks_deliv_recorded)
  # library(dplyr)
  # outlier = final %>% filter(ga_at_anc1_by_lmp <= 0)
  # 
  # 
  
  
  #index_bounds = filter_dates(date_anc1, "2016", "2019") & filter_dates(date_del, "2016", "2019") & filter_dates(lmp, "2016", "2019")
  
  #return(final)  
  #return(final_bounds)
  ## END OTHER SCRIPT ##
  index_bounds = filter_dates(date_anc1, "2016", "2019") & filter_dates(date_del, "2016", "2019") & filter_dates(lmp, "2016", "2019")
  #final_bounds = final[which(index_bounds == T), ]
  #
  
  #[which(index_bounds == T), ]
  frame = data.frame(final, frame)
  return(frame)
  
  
}


make_data = function(us_2, us_4, family, wk_thresh) {
  
  us_2 = us_2[!is.na(us_2$ga_del_by_us_date) | !is.na(us_2$ga_del_by_us_edd), ]
  # 
  us_4 = us_4[!is.na(us_4$ga_del_by_us_date) | !is.na(us_4$ga_del_by_us_edd), ]
  
  #us_2 = us_2[!is.na(us_2$us_adjga), ]
  
  #us_4 = us_4[!is.na(us_4$ga_del_us), ]
  
  ## m_age_anc1 may reduce completion #
  out2 = us_2[complete.cases(subset(us_2, select = -c(ga_weeks_deliv_recorded, study_id_anc1))),]
  out4 = us_4[complete.cases(subset(us_4, select = -c(ga_weeks_deliv_recorded, study_id_anc1))),]
  dim(out2)
  dim(out4)
  
  vars = rbind(out2, out4)
  #vars = vars[vars$ga_at_anc1_by_lmp > 0  | vars$ga_anc1_recorded > 0, ]
  table(vars$m_age_anc1)
  
  #vars = vars[!(vars$wks_from_anc1_to_del < 0 & vars$wks_from_lmp_to_del < 0), ]
  vars = vars[!(vars$wks_from_anc1_to_del < 0 | vars$ga_at_anc1_by_lmp < 0), ]
  vars = vars[vars$ga_anc1_by_edd > 0,  ]
  vars = vars[vars$wks_from_anc1_to_us >= -1,  ]
  dim(vars)
  
  wks22 = vars
  wks22 = wks22[wks22$ga_at_us <= wk_thresh, ]
  dim(wks22)
  train = wks22
  
  # train = train[train$dhc != 113000000 & train$dhc != 119000000 & train$dhc != 226000000 & 
  #                                     train$dhc != 334000000 & 
  #                                     #train$dhc != 331000000 & 
  #                                     train$dhc != 435000000
  #                                 & train$dhc != 437000000 & train$dhc != 539000000, ]
  # 
  
  m_age_diff = abs(train$m_age_anc1 - train$m_age_enroll)
  train = train[m_age_diff <= 3, ]
  keep = train[get_diff(train, 24),]
  dim(keep)
  #keep[keep$ga_at_deliv_lmp > 45,]
  library(tidyverse)
  #train = choose_lmp_or_us(keep, ga_lower_bd = 18, diff = 1.8)
  choose_us = function(x) {
    #%>% select(ga_del_by_us_date, ga_del_by_us_edd)

    tr = x %>% filter( (ga_del_by_us_date >= 24 & ga_del_by_us_date <= 45) | 
                         (ga_del_by_us_edd >= 24 & ga_del_by_us_edd <= 45)) 
    #new_us_del = rep(NA, nrow(tr))
    diff4 = (abs(as.numeric(tr$ga_del_by_us_date - tr$ga_del_by_us_edd)))
    summary(diff4)
    sum(tr$us_date < tr$date_anc1)
    
    tr$ga_del_by_us_date[(tr$ga_del_by_us_date < 24 | tr$ga_del_by_us_date > 45)] <- 
      tr$ga_del_by_us_edd[(tr$ga_del_by_us_date < 24 | tr$ga_del_by_us_date > 45)] 
    tr$ga_del_by_us_edd[(tr$ga_del_by_us_edd < 24 | tr$ga_del_by_us_edd > 45)] <- 
      tr$ga_del_by_us_date[(tr$ga_del_by_us_edd < 24 | tr$ga_del_by_us_edd > 45)]
    
    diff4 = (abs(as.numeric(tr$ga_del_by_us_date - tr$ga_del_by_us_edd)))
    summary(diff4)
    
    tr = tr[diff4 < 4, ]
    final_us = (tr$ga_del_by_us_date + tr$ga_del_by_us_edd)/2
    
    tr$y = as.numeric(final_us)
    return(tr)
  }
  
  train = choose_us(keep)
  # outliers = keep %>%
  #   filter(ga_del_by_us_date >= 42 | ga_at_us >= 42 | ga_del_by_us_edd >= 42 | ga_weeks_deliv_recorded >= 42 |
  #          ga_at_deliv_lmp >= 42 | ga_at_delivery_by_edd_anc1 >= 42 | ga_at_delivery_by_ga_anc1 >= 42 | 
  #            ga_del_by_us_date < wk_thresh | ga_del_by_us_edd < wk_thresh | ga_weeks_deliv_recorded < wk_thresh |
  #            ga_at_deliv_lmp < wk_thresh | ga_at_delivery_by_edd_anc1 < wk_thresh | ga_at_delivery_by_ga_anc1 < wk_thresh) %>% 
  #   select(lmp, date_anc1, ga_at_anc1_by_lmp, ga_anc1_recorded, ga_at_us, ga_del_by_us_date, ga_del_by_us_edd, ga_weeks_deliv_recorded,
  #           ga_at_deliv_lmp, ga_at_delivery_by_edd_anc1, ga_at_delivery_by_ga_anc1)
 
  # head(outliers)
  # View(outliers)  
  # summary(as.numeric(keep$ga_at_us))
  # summary(as.numeric(keep$ga_del_by_us_date))
  # summary(as.numeric(keep$ga_del_us))
  
  
  train = train[!is.na(train$y),]
  dim(train)
  train = train[train$fuel != 4, ]
  
  index_dhc = table(train$dhc)
  index_dhc = index_dhc[index_dhc >= 11]
  train = train[train$dhc %in% names(index_dhc), ]
  train$dhc = as.factor(train$dhc)
  
  if (family == "binomial") {
    train$y = as.numeric(train$y < 37)
    
  } 
  
  train = train[train$m_wt < 150, ]
  return(train)
}

# ga_at_delivery_by_ga_anc1, wks_from_us_to_del, wks_from_anc1_to_del, temp_ga,
#  wks_from_lmp_to_us, wks_from_lmp_to_del, m_age_enroll,
# ga_at_delivery_by_edd_anc1, ga_at_delivery_by_fh_anc1,
# ga_at_anc1_by_lmp, ga_anc1_recorded,

#94.38% auc, (91=97.5) wks_from_anc1_to_del removed 
#94.7 92-97.4
make_x = function(train) {
  X = subset(train, select = -c(y, edd, m_age_enroll, wks_from_lmp_to_del, wks_from_us_to_del,
                                wks_from_anc1_to_del, wks_from_lmp_to_us,
                                lmp_o, date_anc1_o, edd_o, us_date_o, date_del_o, edd_o, us_edd_o,
                                ga_weeks_deliv_recorded, smoke,
                                study_id_enroll, study_id_anc1, 
                                wks_from_anc1_to_us,
                                date_del, date_anc1, lmp, ga_at_us, edd_by_us_date, 
                                us_edd, us_date, ga_del_by_us_date, ga_del_by_us_edd))
  #ga_at_deliv_lmp
  X$fuel = ifelse(X$fuel == 1, 0, 1)
  
  
  #m <- mapply(X[, c(1:5)], FUN = as.numeric)
  #X[,1:5] <- m
  return(X)
}



get_diff = function(train, ga_lower_bd){
  # 
  diff1 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_del_by_us_date)) <= 2.8 &
             train$ga_at_deliv_lmp <= 45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  diff2 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_at_delivery_by_edd_anc1)) <= 2.8 &
             train$ga_at_deliv_lmp <=45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  diff3 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_del_by_us_edd)) <= 2.8 &
             train$ga_at_deliv_lmp <=45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  diff4 = (abs(as.numeric(train$ga_del_by_us_date - train$ga_del_by_us_edd)) <= 2.8 & 
             train$ga_del_by_us_edd <= 45 & train$ga_del_by_us_edd >= ga_lower_bd)
  
  diff5 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_at_delivery_by_edd_anc1)) <= 2.8 & 
             train$ga_at_deliv_lmp <=45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  diff6 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_at_delivery_by_ga_anc1)) <= 2.8 & 
             train$ga_at_deliv_lmp <=45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  
  diff7 = (abs(as.numeric(train$ga_del_by_us_date - train$ga_at_delivery_by_ga_anc1)) <= 2.8 & 
             train$ga_del_by_us_date <=45 & train$ga_del_by_us_date >= ga_lower_bd)
  
  diff8 = (abs(as.numeric(train$ga_del_by_us_date - train$ga_at_delivery_by_edd_anc1)) <= 2.8 & 
             train$ga_del_by_us_date <=45 & train$ga_del_by_us_date >= ga_lower_bd)
  
  diff9 = (abs(as.numeric(train$ga_del_by_us_edd - train$ga_at_delivery_by_ga_anc1)) <= 2.8 & 
             train$ga_del_by_us_edd <= 45 & train$ga_del_by_us_edd >= ga_lower_bd)
  
  return(((diff2 & diff4) | (diff2 & diff3) | (diff5 & diff6) | (diff1 & diff3) | 
            (diff1 & diff2) | (diff7 & diff8) | (diff1 & diff6) | (diff7 & diff9)))
}


