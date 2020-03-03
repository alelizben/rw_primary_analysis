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

## old functions for date cleaning
  #date_del = date_cleaning(as.Date(x[, 'date_delivery_mat'], "%Y-%m-%d"))
  #date_del = fix_date(x[, 'date_delivery_mat'])
  #date_anc1 = fix_date(x[, 'anc1date_anc'])
  #index_bounds = filter_dates(date_anc1, "2016", "2019") & filter_dates(date_del, "2016", "2019") & filter_dates(lmp, "2016", "2019")
  #final_bounds = final[which(index_bounds == T), ]


get_us = function(x, arm) {
  study_id = x[, sprintf('study_id_full_new')]
  val_ga = x$validgestatdel
  recorded = x$ga_weeks

  dhc = x$dhc
  studygrp = x$StudyGrp
  studyarm = x$StudyArm
  m_age_enroll = as.numeric(as.character(x[ ,'m_age']))
  m_age_anc1 = as.numeric(x[ ,'m_age_anc'])
  
  x$mother_age_cat = rep(NA, nrow(x))
  x$mother_age_cat[x$mother_age >= 12 & x$mother_age < 18] = "12-17" 
  x$mother_age_cat[x$mother_age >= 18 & x$mother_age <= 35] = "18-35"
  x$mother_age_cat[x$mother_age > 35] = "$>$ 35"
  #table(df$mother_age_cat)
  #as.numeric
  
  m_stature =  as.numeric(x[ , 'ht_und150'])
  m_wt = as.numeric(x[ , 'wt_1stvisit'])
  
  #date_del = fix_date(x[, 'date_delivery_mat'])
  date_del = as.Date(x[, 'date_delivery_mat'], format= "%d-%b-%y")
  date_del = as.Date(x[, 'date_delivery_mat'], format= "%m/%d/%Y")
  
  
  date_anc1 = as.Date(x[, 'anc1date_anc'], format= "%d-%b-%y")
  date_anc1 = as.Date(x[, 'anc1date_anc'], format = "%m/%d/%Y")
  
  days_from_anc1_to_del = (date_del - date_anc1)
  wks_from_anc1_to_del = (date_del - date_anc1)/7
  
  ## LMP ##
  lmp = as.Date(x[, sprintf('lmp_anc')], format= "%m/%d/%Y")
  
  wks_from_lmp_to_del = (date_del - lmp) / 7
  
  ga_at_anc1_by_lmp = (date_anc1 - lmp) / 7
  
  ga_at_deliv_lmp = (ga_at_anc1_by_lmp) + (days_from_anc1_to_del / 7)
  ## LMP ##
  edd = as.Date(x[, sprintf('edd_anc')], format= "%d-%b-%y")
  edd = as.Date(x[, sprintf('edd_anc')], format= "%m/%d/%Y")
  
  ga_at_delivery_by_edd_anc1 = (280 + (date_del - edd)) / 7
  
  #Question 3
  ##GA AT DELIVERY BY GA AT ANC1 ##
  ga_anc1_recorded = (x[, sprintf('anc1ga_anc')])
  ga_anc1_by_edd = 40 - ((edd - date_anc1)/7)
  ga_at_delivery_by_ga_anc1 = ga_anc1_recorded + (days_from_anc1_to_del / 7)
  
  
  ###mother
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
  ubud = x$ubudehe_cat
  ###mother
  
  
  ## BABY
  sex = x[, 'infant_sex']
  muac = as.numeric(x[, sprintf('muac_birth')])
  chest_c = as.numeric(x[, sprintf('cc_birth')])
  weight = as.numeric(as.character(x[, sprintf('bwt_birth')]))
  
  weight[!is.na(weight) & (weight/10) < 1] <- (weight * 1000)[!is.na(weight) & (weight/10) < 1]
  table(weight)
  head_cir =  as.numeric(x[ ,sprintf('hc_birth')])
  length_b =  as.numeric(x[ ,sprintf('length_birth')])
  apgar1 = as.numeric(x[, sprintf('apgar_1mbirth')])
  apgar5 = as.numeric(x[, sprintf('apgar_5mbirth')])
  validgestatdel = x[, 'validgestatdel']
  ## BABY
  if (arm == 2 | arm == 4) {
    #us_edd = date_cleaning(as.Date(x[, sprintf('edd_us')], format = "%Y-%m-%d"))
    us_edd = as.Date(x[, sprintf('edd_us')], format= "%d-%b-%y")
    us_edd = as.Date(x[, sprintf('edd_us')], format= "%m/%d/%Y")
    
    ga_at_us = as.numeric(as.character(x[, sprintf('us_adjga')]))
    
    us_date =  as.Date(x[, sprintf('us_date')],format= "%m/%d/%Y")
    #us_date =  as.Date(x[, sprintf('us_date')],format= "%d %b %y")
    
    #us_date[140:145]
    #x[140:145, 'us_date']
    
    ga_del_by_us_date = ga_at_us + (date_del - us_date)/7
    edd_by_us_date = 7*(40 - ga_at_us) + us_date
    
    ga_us_diff = date_del - us_edd
    ga_del_by_us_edd = (280 + ga_us_diff) / 7
    
    wks_from_lmp_to_us = (us_date - lmp)/7
    wks_from_anc1_to_us = (us_date - date_anc1)/7
    wks_from_us_to_del = (date_del - us_date)/7
    
    final = data.frame(wks_from_anc1_to_us,
                       wks_from_lmp_to_us, 
                       wks_from_us_to_del,
                       us_date,
                       ga_at_us, edd_by_us_date, us_edd, ga_del_by_us_date,
                       ga_del_by_us_edd)
    
  } else {
    wks_from_lmp_to_us = NA
    wks_from_anc1_to_us = NA
    wks_from_us_to_del = NA
    us_date = NA

    ga_at_us = rep(NA, nrow(x))
    edd_by_us_date =  rep(NA, nrow(x))
    us_edd = rep(NA, nrow(x))
    ga_del_by_us_date = rep(NA, nrow(x))
    ga_del_by_us_edd = rep(NA, nrow(x))

    
    final = data.frame(wks_from_anc1_to_us,
                       wks_from_lmp_to_us, 
                       wks_from_us_to_del,
                       us_date,
                       ga_at_us, edd_by_us_date, us_edd, ga_del_by_us_date,
                       ga_del_by_us_edd)
  }
  
  ######## Question 5 ## GA at delivery by FH @ anc1 ### 
  
  ####
  fun_ht_anc1 = as.numeric(as.character(x[, sprintf('fundalht_1stvisit')]))
  ## check serial fundal height
  
  ga_at_delivery_by_fh_anc1 = fun_ht_anc1 + (days_from_anc1_to_del/7)
  
  ## ga weeks delivery recoded
  ga_weeks_recorded = x[, sprintf('ga_weeks')]
  back_calc_dod_ga_recorded = ga_weeks_recorded - wks_from_anc1_to_del
  
  usrec = x$us_rec

  risk_factors = x %>% select(obhx_preterm, obhx_lbw, obhx_abortion, obhx_sb, obhx_28d_death)
  risk_factors[is.na(risk_factors)] <- 0
  sapply(risk_factors, table)
  frame = data.frame(study_id, studygrp, studyarm, risk_factors, dhc, m_age_enroll, m_age_anc1, m_stature, m_wt, parity, grav, fuel, 
                     enough_food, ever_no_food,
                     run_out_food, not_enough_food, smoke, hh_smoker, alc, ubud,
                     lmp, date_anc1,
                     usrec, ga_at_anc1_by_lmp, ga_anc1_by_edd, ga_anc1_recorded,
                     fun_ht_anc1, edd, 
                     wks_from_anc1_to_del, wks_from_lmp_to_del, date_del, 
                     val_ga, recorded,
                     ga_at_deliv_lmp, ga_at_delivery_by_ga_anc1, 
                     ga_at_delivery_by_edd_anc1,
                     ga_at_delivery_by_fh_anc1,
                     validgestatdel,
                     ga_weeks_recorded,
                     back_calc_dod_ga_recorded, 
                     sex, weight, head_cir, length_b, apgar1, apgar5)
  frame = data.frame(frame, final)
  frame
}

choose_us = function(x) {
  #%>% select(ga_del_by_us_date, ga_del_by_us_edd)
  
  tr = x %>% filter( (ga_del_by_us_date >= 24 & ga_del_by_us_date <= 45) | 
                       (ga_del_by_us_edd >= 24 & ga_del_by_us_edd <= 45)) 
  
  #new_us_del = rep(NA, nrow(tr))
  diff4 = (abs(as.numeric(tr$ga_del_by_us_date - tr$ga_del_by_us_edd)))
  
  # 
  tr = tr[diff4 < 6, ]
  # final_us = (tr$ga_del_by_us_date + tr$ga_del_by_us_edd)/2
  # 
  # tr$y = as.numeric(final_us)
  return(tr)
}

make_data = function(us_2, us_4, family, wk_thresh, arm) {
  
  if ( arm == 2 | arm == 4) {
    us_2 = us_2[!is.na(us_2$ga_del_by_us_date) | !is.na(us_2$ga_del_by_us_edd), ]
    # 
    us_4 = us_4[!is.na(us_4$ga_del_by_us_date) | !is.na(us_4$ga_del_by_us_edd), ]
    
  } else if (arm == 1 | arm == 3) {
    
    new = (rbind(us_2, us_4))
    new = subset(new, select = -c(wks_from_anc1_to_us, wks_from_lmp_to_us, 
                                  wks_from_us_to_del, us_date, ga_at_us, 
                                  usrec, edd_by_us_date, us_edd, ga_del_by_us_date, ga_del_by_us_edd))
    #new = new[complete.cases(new), ]
    return(new)
  }
 
  ## m_age_anc1 may reduce completion #
  #out2 = us_2[complete.cases(subset(us_2, select = -c(ga_weeks_recorded, study_id))),]
  #out4 = us_4[complete.cases(subset(us_4, select = -c(ga_weeks_recorded, study_id))),]
  out2 = us_2
  out4 = us_4
  dim(out2)
  dim(out4)
  
  vars = rbind(out2, out4)
  #vars = vars[vars$ga_at_anc1_by_lmp > 0  | vars$ga_anc1_recorded > 0, ]
  table(vars$m_age_anc1)
  table(vars$ga_at_anc1_by_lmp < 0)
  #vars = vars[!(vars$wks_from_anc1_to_del < 0 & vars$wks_from_lmp_to_del < 0), ]
  vars = vars[!(vars$wks_from_anc1_to_del < 0 | vars$ga_at_anc1_by_lmp < 0), ]
  #vars = vars[vars$ga_anc1_by_edd > 0,  ]
  #vars = vars[vars$wks_from_anc1_to_us >= -1,  ]
  dim(vars)
  
  wks22 = vars
  wks22 = wks22[wks22$ga_at_us <= wk_thresh, ]
  dim(wks22)
  train = wks22
  
  m_age_diff = abs(train$m_age_anc1 - train$m_age_enroll)
  train = train[m_age_diff <= 3, ]
  train$ga_weeks_recorded = as.numeric(train$ga_weeks_recorded)
  train$ga_del_by_us_date = as.numeric(train$ga_del_by_us_date)
  train$ga_del_by_us_edd = as.numeric(train$ga_del_by_us_edd)
  
  train$mother_age_cat = rep(NA, nrow(train))
  train$mother_age_cat[train$m_age_anc1 >= 12 & train$m_age_anc1 < 18] = "12-17" 
  train$mother_age_cat[train$m_age_anc1 >= 18 & train$m_age_anc1 <= 35] = "18-35"
  train$mother_age_cat[train$m_age_anc1 > 35] = "$>$ 35"
  
  #### choosing 
  keep = train[get_diff(train, 24),]
  dim(keep)
  #keep[keep$ga_at_deliv_lmp > 45,]
  #train = choose_lmp_or_us(keep, ga_lower_bd = 18, diff = 1.8)
  
  #choose_us
  train = keep %>% filter( (ga_del_by_us_date >= 24 & ga_del_by_us_date <= 45) | 
                       (ga_del_by_us_edd >= 24 & ga_del_by_us_edd <= 45)) 


  train = train[train$fuel != 4, ]
  
  index_dhc = table(train$dhc)
  
  ##does  not drop 1 from study group and 1 from  control
  #index_dhc = index_dhc[index_dhc >= 11]
  #train = train[train$dhc %in% names(index_dhc), ]
  train$dhc = as.factor(train$dhc)

  
  #train = train[train$m_wt < 150, ]
  return(train)
}

## for SL



make_data_SL = function(us_2, us_4, family, wk_thresh, arm) {
  
  if ( arm == 2 | arm == 4) {
    sum(!is.na(us_2$ga_del_by_us_date))

    us_2 = us_2[!is.na(us_2$ga_del_by_us_date) | !is.na(us_2$ga_del_by_us_edd), ]
    # 
    us_4 = us_4[!is.na(us_4$ga_del_by_us_date) | !is.na(us_4$ga_del_by_us_edd), ]
    
  } else if (arm == 1 | arm == 3) {
    
    new = (rbind(us_2, us_4))
    new = subset(new, select = -c(wks_from_anc1_to_us, wks_from_lmp_to_us, 
                                  wks_from_us_to_del, us_date, ga_at_us, 
                                  usrec, edd_by_us_date, us_edd, ga_del_by_us_date, ga_del_by_us_edd))
    #new = new[complete.cases(new), ]
    return(new)
  }
  
  ## m_age_anc1 may reduce completion #
  out2 = us_2[complete.cases(subset(us_2, select = -c(ga_weeks_recorded, study_id))),]
  out4 = us_4[complete.cases(subset(us_4, select = -c(ga_weeks_recorded, study_id))),]
  #out2 = us_2
  #out4 = us_4
  dim(out2)
  dim(out4)
  
  vars = rbind(out2, out4)
  #vars = vars[vars$ga_at_anc1_by_lmp > 0  | vars$ga_anc1_recorded > 0, ]
  table(vars$m_age_anc1)
  table(vars$ga_at_anc1_by_lmp < 0)
  #vars = vars[!(vars$wks_from_anc1_to_del < 0 & vars$wks_from_lmp_to_del < 0), ]
  vars = vars[!(vars$wks_from_anc1_to_del < 0 | vars$ga_at_anc1_by_lmp < 0), ]
  vars = vars[vars$ga_anc1_by_edd > 0,  ]
  #vars = vars[vars$wks_from_anc1_to_us >= -1,  ]
  dim(vars)
  
  wks22 = vars
  wks22 = wks22[wks22$ga_at_us <= wk_thresh, ]
  dim(wks22)
  train = wks22
  
  m_age_diff = abs(train$m_age_anc1 - train$m_age_enroll)
  train = train[m_age_diff <= 3, ]
  train$ga_weeks_recorded = as.numeric(train$ga_weeks_recorded)
  train$ga_del_by_us_date = as.numeric(train$ga_del_by_us_date)
  train$ga_del_by_us_edd = as.numeric(train$ga_del_by_us_edd)
  
  train$mother_age_cat = rep(NA, nrow(train))
  train$mother_age_cat[train$m_age_anc1 >= 12 & train$m_age_anc1 < 18] = "12-17" 
  train$mother_age_cat[train$m_age_anc1 >= 18 & train$m_age_anc1 <= 35] = "18-35"
  train$mother_age_cat[train$m_age_anc1 > 35] = "$>$ 35"
  
  #### choosing 
  keep = train[get_diff(train, 24),]
  dim(keep)
  #keep[keep$ga_at_deliv_lmp > 45,]
  #train = choose_lmp_or_us(keep, ga_lower_bd = 18, diff = 1.8)
  
  #choose_us
  train = keep %>% filter( (ga_del_by_us_date >= 24 & ga_del_by_us_date <= 45) | 
                             (ga_del_by_us_edd >= 24 & ga_del_by_us_edd <= 45)) 
  
  
  train = keep %>% filter( (ga_del_by_us_date >= 24 & ga_del_by_us_date <= 45)) 
  
  
  train = train[train$fuel != 4, ]
  
  index_dhc = table(train$dhc)
  
  ##does  not drop 1 from study group and 1 from  control
  #index_dhc = index_dhc[index_dhc >= 11]
  #train = train[train$dhc %in% names(index_dhc), ]
  train$dhc = as.factor(train$dhc)
  
  
  #train = train[train$m_wt < 150, ]
  return(train)
}

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


# ga_at_delivery_by_ga_anc1, wks_from_us_to_del, wks_from_anc1_to_del, temp_ga,
#  wks_from_lmp_to_us, wks_from_lmp_to_del, m_age_enroll,
# ga_at_delivery_by_edd_anc1, ga_at_delivery_by_fh_anc1,
# ga_at_anc1_by_lmp, ga_anc1_recorded,

#94.38% auc, (91=97.5) wks_from_anc1_to_del removed 
#94.7 92-97.4
make_x = function(train) {
  X = subset(train, select = -c(y, edd, m_age_enroll, wks_from_lmp_to_del, wks_from_us_to_del,
                                wks_from_anc1_to_del, wks_from_lmp_to_us, mother_age_cat,
                                #lmp_o, date_anc1_o, edd_o, us_date_o, date_del_o, edd_o, us_edd_o,
                                ga_weeks_recorded, smoke, usrec, Arm,
                                study_id,
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
  
  ##check ga_del_by_us_date consistent
  diff1 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_del_by_us_date)) <= 2.8 &
             train$ga_at_deliv_lmp <= 45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  
  diff2 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_at_delivery_by_edd_anc1)) <= 2.8 &
             train$ga_at_deliv_lmp <=45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  
  diff3 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_del_by_us_edd)) <= 2.8 &
             train$ga_at_deliv_lmp <=45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  
  
  ## check ga_del_by_us_edd consistent
  diff4 = (abs(as.numeric(train$ga_del_by_us_date - train$ga_del_by_us_edd)) <= 2.8 & 
             train$ga_del_by_us_edd <= 45 & train$ga_del_by_us_edd >= ga_lower_bd)
  
  ### check ga_by_lmp is consistent 
  diff5 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_at_delivery_by_edd_anc1)) <= 2.8 & 
             train$ga_at_deliv_lmp <=45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  
  diff6 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_at_delivery_by_ga_anc1)) <= 2.8 & 
             train$ga_at_deliv_lmp <=45 & train$ga_at_deliv_lmp >= ga_lower_bd)
  
  
  ## check ga_us_date is consistent
  diff7 = (abs(as.numeric(train$ga_del_by_us_date - train$ga_at_delivery_by_ga_anc1)) <= 2.8 & 
             train$ga_del_by_us_date <=45 & train$ga_del_by_us_date >= ga_lower_bd)
  
  diff8 = (abs(as.numeric(train$ga_del_by_us_date - train$ga_at_delivery_by_edd_anc1)) <= 2.8 & 
             train$ga_del_by_us_date <=45 & train$ga_del_by_us_date >= ga_lower_bd)
  
  ## check ga_by edd is consistent
  diff9 = (abs(as.numeric(train$ga_del_by_us_edd - train$ga_at_delivery_by_ga_anc1)) <= 2.8 & 
             train$ga_del_by_us_edd <= 45 & train$ga_del_by_us_edd >= ga_lower_bd)
  
  return(((diff2 & diff4) | (diff2 & diff3) | (diff5 & diff6) | (diff1 & diff3) | 
            (diff1 & diff2) | (diff7 & diff8) | (diff1 & diff6) | (diff7 & diff9)))
}
# yh = sort(yhat)
# thresh = yh[560]
# mean(yhat[train$y == 1] > thresh)
# mean(yhat[train$y == 0] <= thresh)
# 
# mean(train$y == 1)
get_auc = function(out, train, folds, name, type) {
  yhat = out$SL.predict
  pred = lapply(out$folds, function(x) yhat[x])
  labs = lapply(out$folds, function(x) train$y[x])
  
  get_sens = function(est, y) {
    
    yh = sort(est)
    
    get_spec = function(x, yhat, y) {

      out = mean(yhat[y == 0] <= x)
      out
      
    }
    
    thresh = yh[min(which(sapply(yh, function(x) get_spec(x, est, y)) >= .80))]
    sens = c(mean(est[y == 1] > thresh))
    sens

  }
  
  cvsens = mapply(function(x, y) get_sens(x, y), pred, labs)
  mean(cvsens)
  if(type == "sens") {
    return(cvsens)
  }

  plot_val = cvAUC(pred, labs, label.ordering = NULL, folds = NULL)
  val = ci.cvAUC_withIC(yhat, train$y, label.ordering = NULL, folds = out$folds)
  val
  p <- pnorm(-abs((val$cvAUC - 0.5)/val$se))
  out = c(val$cvAUC, val$ci)
  out = round(out, 3)
  names(out) = c("cvAUC", "lower_CI", "upper_CI")
  
  
  jpeg(sprintf('%s_fold_plot_%s_%s.jpg', folds, name, Sys.Date()), width = 1100, height = 1000)
  
  plot(plot_val$perf, col="grey82", lty=3, axes = FALSE,
       main=sprintf("%s, %s-fold CV ROC", name, folds))
  plot(plot_val$perf, col="red", avg="vertical", add=TRUE, axes = FALSE)
  legend("bottomright", inset=.05, title="", 
         cex = 2, legend = c(" ", " "),
         horiz=TRUE, bty = "n")
  #cex.lab=4, cex.axis=4, cex.main=4
  text(0.78, 0.2, sprintf('cvAUC and C.I.: %s (%s-%s)', round(val$cvAUC, 3), round(val$ci[1], 3), 
                         round(val$ci[2], 3)), cex = 2.3)
  text(0.78, 0.16, sprintf('%s%% Sens, 80%% Spec', round(sens * 100, 0)), cex = 2.3)


  abline(0, 1, lty = 2)
  #abline(v = 0.4)
  abline(v = 0.2, lty = 3)
  #abline(v = 0.2)
  abline(h = 0.8, lty = 3)
  #abline(h = 0.9)
  dev.off()
  
  #Plot CV AUC

  return(out) 
}


gaus_perf = function(out, train) {
  p1 = sqrt(mean((out$SL.predict  - train$y)^2))
  p2 = median(abs(out$SL.predict  - train$y))
  #print(summary(abs(out$SL.predict  - train$y)))
  return(c(p1, p2))
  
}
