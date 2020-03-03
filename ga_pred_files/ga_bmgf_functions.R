
get_us = function(x, arm) {
  study_id_enroll = x[, sprintf('study_id_full_new')]
  # table(arm2$SID)
  # mean(is.na(arm2$study_id_full_new))
  # dim(arm4)
  # table(df$SIDcorrect)
  
  #study_id_enroll = x[, sprintf('study_id_enroll')]
  #study_id_anc1 = x[, sprintf('study_id_anc')]
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
  #date_del = as.Date(x[, 'date_delivery_mat'], format= "%d %b %y")
  
  
  date_anc1 = as.Date(x[, 'anc1date_anc'], format= "%d-%b-%y")
  date_anc1 = as.Date(x[, 'anc1date_anc'], format = "%m/%d/%Y")
  
  days_from_anc1_to_del = (date_del - date_anc1)
  wks_from_anc1_to_del = (date_del - date_anc1)/7
  
  ## LMP ##
  lmp = as.Date(x[, sprintf('lmp_anc')], format= "%m/%d/%Y")
  #lmp = as.Date(x[, sprintf('lmp_anc')], format= "%d %b %y")
  
  wks_from_lmp_to_del = (date_del - lmp) / 7
  
  ga_at_anc1_by_lmp = (date_anc1 - lmp) / 7
  
  ga_at_deliv_lmp = (ga_at_anc1_by_lmp) + (days_from_anc1_to_del / 7)
  ## LMP ##
  #edd = as.Date(x[, sprintf('edd_anc')], format= "%d-%b-%y")
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
  ## BABY
  if (arm == 2 | arm == 4) {
    #us_edd = date_cleaning(as.Date(x[, sprintf('edd_us')], format = "%Y-%m-%d"))
    #us_edd = as.Date(x[, sprintf('edd_us')], format= "%d-%b-%y")
    us_edd = as.Date(x[, sprintf('edd_us')], format= "%m/%d/%Y")
    
    ga_at_us = as.numeric(as.character(x[, sprintf('us_adjga')]))
    
    us_date =  as.Date(x[, sprintf('us_date')],format= "%m/%d/%Y")
    
    #us_date[140:145]
    #x[140:145, 'us_date']
    
    ga_del_by_us_date = ga_at_us + (date_del - us_date)/7
    edd_by_us_date = 7*(40 - ga_at_us) + us_date
    
    ga_us_diff = date_del - us_edd
    ga_del_us_edd = (280 + ga_us_diff) / 7
    
  } else {
    ga_del_us_edd = rep(NA, nrow(x))
    ga_at_us = rep(NA, nrow(x))
    us_date = rep(NA, nrow(x))
  }
  
  
  wks_from_lmp_to_us = (us_date - lmp)/7
  wks_from_anc1_to_us = (us_date - date_anc1)/7
  wks_from_us_to_del = (date_del - us_date)/7
  ######## Question 5 ## GA at delivery by FH @ anc1 ### 
  
  ####
  fun_ht_anc1 = as.numeric(as.character(x[, sprintf('fundalht_1stvisit')]))
  ## check serial fundal height
  
  ga_at_delivery_by_fh_anc1 = fun_ht_anc1 + (days_from_anc1_to_del/7)
  
  ## ga weeks delivery recoded
  ga_weeks_recorded = x[, sprintf('ga_weeks')]
  
  # study_id_anc1,
  final = data.frame(study_id_enroll, wks_from_anc1_to_del,
                     wks_from_anc1_to_us,
                     wks_from_lmp_to_us, wks_from_lmp_to_del,
                     wks_from_us_to_del,
                     lmp, date_anc1, us_date, date_del, 
                     ga_at_anc1_by_lmp, ga_anc1_by_edd, ga_anc1_recorded,
                     ga_at_us, fun_ht_anc1,
                     edd, edd_by_us_date, us_edd, ga_del_by_us_date,
                     ga_at_deliv_lmp, ga_at_delivery_by_ga_anc1, 
                     ga_at_delivery_by_edd_anc1,
                     ga_at_delivery_by_fh_anc1, ga_del_us_edd, ga_weeks_recorded)
  
  
  frame = data.frame(m_age_enroll, m_age_anc1, m_stature, m_wt, parity, grav, fuel, 
                     enough_food, ever_no_food,
                     run_out_food, not_enough_food, smoke, hh_smoker, alc,
                     sex, weight, head_cir, length_b, apgar1, apgar5)
  frame = data.frame(final, frame)
  frame
}


make_data = function(us_2, us_4, family, wk_thresh) {
  
  us_2 = us_2[!is.na(us_2$ga_del_by_us_date) | !is.na(us_2$ga_del_us_edd), ]
  # 
  us_4 = us_4[!is.na(us_4$ga_del_by_us_date) | !is.na(us_4$ga_del_us_edd), ]
  
  #us_2 = us_2[!is.na(us_2$us_adjga), ]
  
  #us_4 = us_4[!is.na(us_4$ga_del_us), ]
  
  ## m_age_anc1 may reduce completion #
  out2 = us_2[complete.cases(subset(us_2, select = -c(ga_weeks_recorded, study_id_anc1))),]
  out4 = us_4[complete.cases(subset(us_4, select = -c(ga_weeks_recorded, study_id_anc1))),]
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

get_consort = function(df, ga_type) {
  #eligible_enroll_date
  #anc1_date = as.Date(df$anc1date_anc, format = "%m/%d/%Y")
  enroll_date = as.Date(df$enroll_date, format = "%m/%d/%Y")
  valid_enroll = enroll_date >=  "2017-05-22" & enroll_date <= "2018-10-31"
  df$valid_enroll = valid_enroll == T
  elig_enroll_date = table(df$valid_enroll, useNA = 'always')
  
  ##departed after without clinic visit
  df = df %>% filter(valid_enroll == T)
  df$enroll_but_no_anc1 = (!is.na(df$enroll_date) & is.na(df$anc1date_anc))
  departed_without_clinic = table(df$enroll_but_no_anc1, useNA = "always")
  (departed_without_clinic)
  
  ###anc1_ga_lt_24_weeks
  df$anc1ga_anc = as.numeric(as.character(df$anc1ga_anc))
  df$anc_ga_lt_24_wks = df$anc1ga_anc <= 24 & (df$enroll_but_no_anc1 == F)
  anc1_ga_lt_24 = table(df$anc_ga_lt_24_wks, useNA = 'always')
  anc1_ga_lt_24
  #anc1_ga_lt_24_valid_enroll_date = table((df$valid_anc1 == T & !is.na(df$anc_ga_lt_24_wks) &
  #                                         df$anc_ga_lt_24_wks == T), useNA = "always")
  
  out_elig_enroll = data.frame(rbind(elig_enroll_date, departed_without_clinic, anc1_ga_lt_24))
  
  ## subset ##
  elig_enroll = df %>% filter(anc_ga_lt_24_wks)
  ## subset ##
  
  ## MOTHER BABY PAIRS ##
  #deliver_within_study_w_valid_ga_2_anc
  elig_enroll$date_del = as.Date(elig_enroll$date_delivery_mat, format = "%m/%d/%Y")
  elig_enroll$lmp = as.Date(elig_enroll$lmp_anc, format = "%m/%d/%Y")
  elig_enroll$ga_at_del_by_lmp = as.numeric((elig_enroll$date_del - elig_enroll$lmp)/7)
  dates = subset(elig_enroll, select = c(anc1date_anc, date_ancfuvst_2,
                                         date_ancfuvst_2b, date_ancfuvst_2c, date_ancfuvst_2d))
  number_missing = is.na(dates)
  number_missing_dates = rowSums(number_missing)
  elig_enroll$number_anc = 5 - number_missing_dates
  
  elig_enroll$valid_ga_recorded = elig_enroll$ga_weeks >= 20 & elig_enroll$ga_weeks <= 44
  elig_enroll$valid_ga_lmp = elig_enroll$ga_at_del_by_lmp >= 20 & elig_enroll$ga_at_del_by_lmp <= 44
  ##all should be valid recorded GA
  valid_recorded_ga_del = c(table(elig_enroll$valid_ga_recorded, useNA = "always"), 0)[c(3,1,2)]
  names(valid_recorded_ga_del)[1] = FALSE
  valid_ga_lmp_del = table(elig_enroll$valid_ga_lmp, useNA = "always")
  
  delivered_before_march_31 = table(elig_enroll$date_del <= "2019-03-31", useNA = "always")
  
  elig_enroll = elig_enroll %>% filter(date_del <= "2019-03-31") 
  elig = (elig_enroll$anc1ga_anc <= 24 & elig_enroll$number_anc >= 2)
  
  anc_lt_24_and_2_anc_visits = table(elig, useNA = 'always')
  
  elig_mother_baby = elig_enroll %>% filter(elig == T) 
  
  ### NA COME FROM MOSTLY GA RECORDED
  elig_mother_baby$has_outcome = ((elig_mother_baby$valid_ga_recorded == T | elig_mother_baby$valid_ga_lmp == T))
  attained_outcome = c(table(elig_mother_baby$has_outcome, useNA = 'always'), 0)[c(3, 1, 2)]
  attained_outcome
  names(attained_outcome)[1] = "FALSE"
  out_frame = elig_mother_baby %>% filter(has_outcome == T)
  #df_16 = df %>% filter(anc1ga_anc <= 16 & number_anc >= 2)
  
  #valid_recorded_ga_del, valid_ga_lmp_del, 
  out_elig_mother_baby = data.frame(rbind(delivered_before_march_31, 
                                          anc_lt_24_and_2_anc_visits,
                                          attained_outcome))
  
  #out_elig_enroll, 
  out = rbind(out_elig_enroll, out_elig_mother_baby)
  out$total = rowSums(out)
  
  out_frame
  
  
}
