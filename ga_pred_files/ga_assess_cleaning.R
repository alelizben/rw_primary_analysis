


tr %>% filter(is.na(new_us_del)) %>% select(ga_del_by_us_date, ga_del_by_us_edd, ga_at_delivery_by_ga_anc1, ga_at_delivery_by_edd_anc1)

summary(as.numeric(tr$ga_del_by_us_date[diff4 < 2.8]))
summary(as.numeric(tr$ga_del_by_us_edd[diff4 < 2.8]))

summary(as.numeric(tr$ga_del_by_us_date[diff4 >= 2.8]))
summary(as.numeric(tr$ga_del_by_us_edd[diff4 >= 2.8]))



choose_lmp_or_us = function(train, diff, ga_lower_bd) {
  train$ga_del_by_us_date
  train$ga_at_deliv_lmp
  train$ga_at_delivery_by_ga_anc1
  train$ga_at_delivery_by_edd_anc1
  train$ga_del_us
  
  temp_ga = rep(NA, nrow(train))
  
  diff5 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_del_by_us_date)))
  
  diff6 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_del_by_us_edd)))
  
  diff1 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_at_delivery_by_edd_anc1)) <= diff)
  diff2 = (abs(as.numeric(train$ga_at_deliv_lmp - train$ga_at_delivery_by_ga_anc1)) <= diff)
  #then compare the last two components of diff1 and diff 2 to each other
  diff3 = (abs(as.numeric(train$ga_at_delivery_by_edd_anc1 - train$ga_at_delivery_by_ga_anc1)) <= diff)
  
  diff4 = (abs(as.numeric(train$ga_del_by_us_date - train$ga_del_by_us_edd)) <= diff)
  
  ##check ga_del_us_date against ga_anc1 vs ga_edd_anc1
  diff7 = (abs(as.numeric(train$ga_del_by_us_date - train$ga_at_delivery_by_ga_anc1)) <= diff & 
             train$ga_del_by_us_date <=45 & train$ga_del_by_us_date >= ga_lower_bd)
  diff8 = (abs(as.numeric(train$ga_del_by_us_date - train$ga_at_delivery_by_edd_anc1)) <= diff & 
             train$ga_del_by_us_date <=45 & train$ga_del_by_us_date >= ga_lower_bd)
  ##check ga_del_us against ga_anc1 vs ga_edd_anc1  
  diff9 = (abs(as.numeric(train$ga_del_us - train$ga_at_delivery_by_ga_anc1)) <= diff & 
             train$ga_del_by_us_edd <= 45 & train$ga_del_by_us_edd >= ga_lower_bd)
  diff10 = (abs(as.numeric(train$ga_del_us - train$ga_at_delivery_by_edd_anc1)) <= diff & 
              train$ga_del_by_us_edd <= 45 & train$ga_del_by_us_edd >= ga_lower_bd)
  ###
  
  #| lmp and anc1 | lmp and edd | anc1 and edd & lmp and us
  
  ## means ultrasound does not agree
  temp_ga[(diff1 & diff2 & diff3 & diff4 == FALSE)] <- train$ga_at_deliv_lmp[(diff1 & diff2 & diff3 & diff4 == FALSE)]
  #temp_ga[(diff1 & diff6 | diff2 & diff6) & diff4 == FALSE] <- train$ga_at_deliv_lmp[(diff1 & diff2 & diff3 & diff4 == FALSE)]
  ## this means ultrasound agrees and lmp not working..
  temp_ga[diff4 == T & !(diff1 & diff2 & diff3)] <- train$ga_del_by_us_date[diff4 == T & !(diff1 & diff2 & diff3)]
  
  
  ##If both lmp and US dates are good, we need tie-breaker, using tiffany's pic from august
  temp_ga[(diff1 & diff2 & diff3 & diff4) & train$ga_at_us >= 0 & train$ga_at_us < 9 & diff5 <= 0.7] <- 
    train$ga_at_deliv_lmp[(diff1 & diff2 & diff3 & diff4) & train$ga_at_us >= 0 & train$ga_at_us < 9 & diff5 <= 0.7]
  
  temp_ga[(diff1 & diff2 & diff3 & diff4) & train$ga_at_us >= 9 & train$ga_at_us < 16 & diff5 <= 1] <- 
    train$ga_at_deliv_lmp[(diff1 & diff2 & diff3 & diff4) & train$ga_at_us >= 9 & train$ga_at_us < 16 & diff5 <= 1]
  
  temp_ga[(diff1 & diff2 & diff3 & diff4) & train$ga_at_us >= 16 & train$ga_at_us < 22 & diff5 <= 1.5] <- 
    train$ga_at_deliv_lmp[(diff1 & diff2 & diff3 & diff4) & train$ga_at_us >= 16 & train$ga_at_us < 22 & diff5 <= 1.5]
  
  train$temp_ga = temp_ga
  
  #sum(diff4 == T & !(diff1 & diff2 & diff3))
  vote1 = (diff6 <= diff) & diff10 #ga_del_us and deliv lmp, and ga del_us gadeliv edd
  vote2 = (diff5 <= diff) & diff1 #ga del us date and ga deliv lmp, ga dliv lmp and ga edd
  vote3 = (diff5 <= diff) & diff2 #ga del us date and ga deliv lmp, ga dliv lmp and ga anc1
  
  temp_ga[is.na(temp_ga) & vote1] <- train$ga_del_us[is.na(temp_ga) & vote1]
  
  temp_ga[is.na(temp_ga) & vote2] <- train$ga_del_by_us_date[is.na(temp_ga) & vote2]
  
  temp_ga[is.na(temp_ga) & vote3] <- train$ga_del_by_us_date[is.na(temp_ga) & vote3]
  
  train$y = temp_ga
  
  return(train)
}




check_qual = function(vars, ga_lower_bd){
  
  vars$anc_1 = !((vars$us_date > vars$date_del) | (vars$lmp > vars$us_date)) & 
    (vars$lmp >= vars$date_anc1) & (vars$wks_from_anc1_to_del >= 0 & vars$wks_from_anc1_to_del <= 40) &
    (vars$wks_from_anc1_to_us >= 0 & vars$wks_from_anc1_to_us <= 40)
  
  ## check date_anc1 to date us vs date lmp to us
  vars$lmp_1 = !((vars$us_date > vars$date_del) | (vars$lmp > vars$us_date)) & 
    (vars$lmp >= vars$date_anc1) & (vars$wks_from_lmp_to_del > ga_lower_bd & vars$wks_from_lmp_to_del <= 45) & 
    (vars$wks_from_lmp_to_us > 0 & vars$wks_from_lmp_to_us <= 40)
  
  vars$anc_2 = !((vars$us_date > vars$date_del) | (vars$lmp > vars$us_date)) & 
    (vars$date_anc1 > vars$date_del) & (vars$wks_from_anc1_to_us >= 0 & vars$wks_from_anc1_to_us <= 40) & 
    (vars$ga_at_anc1_by_lmp >= 0 & vars$ga_at_anc1_by_lmp <= 40)
  
  vars$date_del_1 = !((vars$us_date > vars$date_del) | (vars$lmp > vars$us_date)) & 
    (vars$date_anc1 > vars$date_del) & (vars$wks_from_lmp_to_del >= ga_lower_bd & vars$wks_from_lmp_to_del <= 45) & 
    (vars$wks_from_lmp_to_us >= 0 & vars$wks_from_lmp_to_us <= 40) & 
    (vars$wks_from_us_to_del >= 0 & vars$wks_from_us_to_del <= 40)
  
  
  vars$lmp_del = (vars$us_date > vars$date_del) & (vars$date_anc1 > vars$date_del) & 
    (vars$wks_from_lmp_to_del > ga_lower_bd & vars$wks_from_lmp_to_del <= 45)
  
  vars$lmp_us = (vars$us_date > vars$date_del) & (vars$date_anc1 > vars$date_del) & 
    (vars$wks_from_lmp_to_us > 0 & vars$wks_from_lmp_to_us <= 40)
  
  vars$lmp_anc1 = (vars$us_date > vars$date_del) & (vars$date_anc1 > vars$date_del) & 
    (vars$ga_at_anc1_by_lmp > ga_lower_bd & vars$ga_at_anc1_by_lmp <= 40)
  
  ###############################
  
  vars$us_date_1 = (vars$us_date > vars$date_del) & 
    (vars$wks_from_lmp_to_us > 0 & vars$wks_from_lmp_to_us <= 40) & 
    (vars$ga_at_anc1_by_lmp > ga_lower_bd & vars$ga_at_anc1_by_lmp <= 40) 
  #(vars$wks_from_anc1_to_us >= 0 & vars$)
  
  vars$lmp_delivery_1 = (vars$us_date > vars$date_del) & 
    (vars$wks_from_lmp_to_del > 0 & vars$wks_from_lmp_to_del <= 45) & 
    (vars$ga_at_anc1_by_lmp >= 0 & vars$ga_at_anc1_by_lmp <=40) 
  #(vars$wks_from_anc1_to_del >= 0 & vars$wks_from_lmp_to_del <= 40)
  
  
  vars[(vars$lmp > vars$us_date),]
  
  vars$lmp3 = (vars$lmp > vars$us_date) & 
    (vars$ga_at_anc1_by_lmp > 0 & vars$ga_at_anc1_by_lmp <= 40) & 
    (vars$wks_from_lmp_to_del > 0 & vars$wks_from_lmp_to_del <= 45)
  #DONT USE LMP
  
  vars$us2 = (vars$lmp > vars$us_date) & 
    (vars$wks_from_anc1_to_us >= 0 & vars$wks_from_anc1_to_us <= 40) & 
    (vars$wks_from_us_to_del > 0 & vars$wks_from_us_to_del <= 40)
  last = data.frame(vars[,38:48])
  return(last)
}



# 
# make_data = function(arm2, arm4, family) {
#   
#   us_2 = get_us(arm2, 2)
#   #us_2$days_from_anc1_to_del > 0 & 
#   us_2 = us_2[!is.na(us_2$ga_del_us),]
#   
#   us_4 = get_us(arm4, 4)
#   #us_4$days_from_anc1_to_del > 0 & 
#   us_4 = us_4[!is.na(us_4$ga_del_us), ]
#   
#   
#   out2 = us_2[complete.cases(subset(us_2, select = -c(ga_weeks_deliv_recorded, study_id_anc1))),]
#   out4 = us_4[complete.cases(subset(us_4, select = -c(ga_weeks_deliv_recorded, study_id_anc1))),]
#   dim(out2)
#   dim(out4)
#   
#   vars = rbind(out2, out4)
#   #vars = vars[vars$ga_at_anc1_by_lmp > 0  | vars$ga_anc1_recorded > 0, ]
#   
#   vars = vars[!(vars$wks_from_anc1_to_del < 0 & vars$wks_from_lmp_to_del < 0), ]
#   dim(vars)
#   
#   wks22 = vars
#   wks22 = wks22[wks22$ga_at_us <= 22, ]
#   dim(wks22)
#   train = wks22
#   train = train[train$study_id_anc1 == train$study_id_enroll, ]
#   #train = train[train$dhc != 331000000 & train$dhc != 334000000 & train$dhc != 435000000 & train$dhc != 539000000, ]
#   train = train[train$dhc != 113000000 & train$dhc != 119000000 & train$dhc != 334000000 & train$dhc != 331000000 & 
#                   train$dhc != 435000000 & train$dhc != 437000000 & train$dhc != 539000000, ]
#   
#   train$dhc = as.factor(train$dhc)
#   m_age_diff = abs(train$m_age_anc1 - train$m_age_enroll)
#   train = train[m_age_diff <= 3, ]
#   
#   keep = train[get_diff(train),]
#   dim(keep)
#   #keep[keep$ga_at_deliv_lmp > 45,]
#   
#   train = choose_lmp_or_us(keep, 1.8)
#   train = train[!is.na(train$y),]
#   dim(train)
#   train = train[train$fuel != 4, ]
#   if (family == "binomial") {
#     train$y = as.numeric(train$y < 37)
#     
#   } 
#   
#   return(train)
# }



# get_us = function(x, arm) {
#   date_del = date_cleaning(as.Date(x[, sprintf('date_delivery_mat.delivery_arm_%s', arm)], "%Y-%m-%d"))
#   #date_del2 = (x[, sprintf('date_delivery_mat.delivery_arm_%s', arm)])
#   date_anc1 = date_cleaning(as.Date(x[, sprintf('anc1date_anc.anc1_visit_arm_%s', arm)], "%Y-%m-%d"))
#   #  date_anc12 = (x[, sprintf('anc1date_anc.anc1_visit_arm_%s', arm)])
#   days_from_anc1_to_del = (date_del - date_anc1)
#   wks_from_anc1_to_del = (date_del - date_anc1)/7
#   (arm1$ga_weeks.delivery_arm_1)
#   # 
#   # if (date_del < date_anc1) {
#   #   
#   #   print("deliver before anc1")
#   # }
#   # age_vars = arm2[,grep('m_age', names(arm2), value = T)]
#   # apply(age_vars, 2, function(x) mean(!is.na(x)))
#   # age_vars = arm4[,grep('m_age', names(arm4), value = T)]
#   # apply(age_vars, 2, function(x) mean(!is.na(x)))
#   # table(arm4$m_age_anc.anc1_visit_arm_4)
#   age_data = subset(arm4, select = c(m_age_anc.anc1_visit_arm_4, m_age.enrollment_arm_4))
#   mismatch_age = age_data[age_data$m_age_anc.anc1_visit_arm_4 == age_data$m_age.enrollment_arm_4, ]
#   
#   m_age_enroll = as.numeric(as.character(x[ ,sprintf('m_age.enrollment_arm_%s', arm)]))
#   m_age_anc1 = as.numeric(x[ ,sprintf('m_age_anc.anc1_visit_arm_%s', arm)])
#   #as.numeric
#   
#   m_stature =  as.numeric(x[ ,sprintf('ht_und150.anc1_visit_arm_%s', arm)])
#   m_wt = as.numeric(x[ ,sprintf('wt_1stvisit.anc1_visit_arm_%s', arm)])
#   #miscar = as.numeric(x[, sprintf("hx_rptdmiscar.anc1_visit_arm_%s", arm)])
#   #ptdel = as.numeric(x[, sprintf("hx_ptdlvry.anc1_visit_arm_%s", arm)])
#   #prevfsb = as.numeric(x[, sprintf("hx_prevfsb.anc1_visit_arm_%s", arm)])
#   parity = as.numeric(x[, sprintf("parity_anc.anc1_visit_arm_%s", arm)])
#   grav = as.numeric(x[, sprintf("gravidity_anc.anc1_visit_arm_%s", arm)])
#   
#   fuel = as.numeric(x[, sprintf("fuel_use.enrollment_arm_%s", arm)])
#   
#   enough_food = as.numeric(x[, sprintf("hh_enoughfood.enrollment_arm_%s", arm)])
#   ever_no_food = as.numeric(x[, sprintf("hh_evernofood.enrollment_arm_%s", arm)])
#   run_out_food = as.numeric(x[, sprintf("hh_runoutfood.enrollment_arm_%s", arm)])
#   not_enough_food = as.numeric(x[, sprintf("hh_notenoughfood.enrollment_arm_%s", arm)])
#   
#   smoke = as.numeric(x[, sprintf("preg_smoke.enrollment_arm_%s", arm)])
#   hh_smoker = as.numeric(x[, sprintf("hh_smoker.enrollment_arm_%s", arm)])
#   
#   alc = as.numeric(x[, sprintf("preg_alc.enrollment_arm_%s", arm)])
#   
#   sex = factor(x[, sprintf('infant_sex.delivery_arm_%s', arm)],
#                levels = c(1,2), labels = c(0,1))
#   muac = as.numeric(x[, sprintf('muac_birth.delivery_arm_%s', arm)])
#   chest_c = as.numeric(x[, sprintf('cc_birth.delivery_arm_%s', arm)])
#   weight = as.numeric(x[, sprintf('bwt_birth.delivery_arm_%s', arm)])
#   
#   weight[!is.na(weight) & (weight/1000) < 1] <- (weight * 1000)[!is.na(weight) & (weight/1000) < 1]
#   
#   head_cir =  as.numeric(x[ ,sprintf('hc_birth.delivery_arm_%s', arm)])
#   length_b =  as.numeric(x[ ,sprintf('length_birth.delivery_arm_%s', arm)])
#   apgar1 = as.numeric(x[, sprintf('apgar_1mbirth.delivery_arm_%s', arm)])
#   apgar5 = as.numeric(x[, sprintf('apgar_5mbirth.delivery_arm_%s', arm)])
#   
#   # us,
#   frame = data.frame(m_age_enroll, m_age_anc1, m_stature, m_wt, parity, grav, fuel, enough_food, ever_no_food,
#                      run_out_food, not_enough_food, smoke, hh_smoker, alc,
#                      sex, weight, head_cir, length_b, apgar1, apgar5)
#   
#   
#   ## START OTHER SCRIPT ##
#   
#   study_id_enroll = x[, sprintf('study_id_full.enrollment_arm_%s', arm)]
#   study_id_anc1 = x[, sprintf('study_id_anc.anc1_visit_arm_%s', arm)]
#   dhc = x[, sprintf('study_id_dhc.enrollment_arm_%s', arm)]
#   #dhc = substr(study_id, 1, 3)
#   
#   ## Question 1 ##
#   lmp = date_cleaning(as.Date(x[, sprintf('lmp_anc.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d"))
#   wks_from_lmp_to_del = (date_del - lmp) / 7
#   
#   ga_at_anc1_by_lmp = (date_anc1 - lmp) / 7
#   
#   ga_at_deliv_lmp = (ga_at_anc1_by_lmp) + (days_from_anc1_to_del / 7)
#   ##create new variable "GA at delivery by LMP"
#   
#   
#   ## Question 2 ### Create GA at delivery by EDD at ANC1 ##
#   
#   edd = date_cleaning(as.Date(x[, sprintf('edd_anc.anc1_visit_arm_%s', arm)], format = '%Y-%m-%d'))
#   
#   ga_at_delivery_by_edd_anc1 = (280 + (date_del - edd)) / 7
#   
#   #Question 3
#   ##GA AT DELIVERY BY GA AT ANC1 ##
#   ga_anc1_recorded = (x[, sprintf('anc1ga_anc.anc1_visit_arm_%s', arm)])
#   
#   ga_at_delivery_by_ga_anc1 = ga_anc1_recorded + (days_from_anc1_to_del / 7)
#   
#   ########
#   
#   
#   ## Question 4 ### Create GA at delivery by HC US##
#   if (arm == 2 | arm == 4) {
#     us_edd = date_cleaning(as.Date(x[, sprintf('edd_us.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d"))
#     
#     usga = (x[, sprintf('us_adjga.anc1_visit_arm_%s', arm)])
#     #table(usga)
#     new_usga = as.numeric(gsub("[^0-9\\.]", "", as.character(usga)))
#     ga_at_us = (ifelse((new_usga/10) >= 4, new_usga/10, new_usga))
#     
#     #print(c("ga_at_us_uncleaned", table(usga)))
#     #print(c("ga_at_us_clean", table(ga_at_us)))
#     
#     us_date =  date_cleaning(as.Date(x[, sprintf('us_date.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d"))
#     ga_del_by_us_date = ga_at_us + (date_del - us_date)/7
#     edd_by_us_date = 7*(40 - ga_at_us) + us_date
#     
#     ga_us_diff = date_del - us_edd
#     ga_del_us = (280 + ga_us_diff) / 7
#     
#   } else {
#     ga_del_us = rep(NA, nrow(x))
#     ga_at_us = rep(NA, nrow(x))
#     us_date = rep(NA, nrow(x))
#   }
#   
#   wks_from_lmp_to_us = (us_date - lmp)/7
#   
#   wks_from_anc1_to_us = (us_date - date_anc1)/7
#   wks_from_us_to_del = (date_del - us_date)/7
#   ######## Question 5 ## GA at delivery by FH @ anc1 ### 
#   
#   ####
#   fun_ht_anc1 = x[, sprintf('fundalht_1stvisit.anc1_visit_arm_%s', arm)]
#   
#   ga_at_delivery_by_fh_anc1 = fun_ht_anc1 + (days_from_anc1_to_del/7)
#   ga_at_delivery_by_fh_anc1
#   
#   ## ga weeks delivery recoded
#   ga_weeks_recorded = x[, sprintf('ga_weeks.delivery_arm_%s', arm)]
#   ga_weeks = as.numeric(gsub("[^0-9\\.]", "", as.character(ga_weeks_recorded)))
#   ga_weeks_deliv_recorded = (ifelse((ga_weeks/10) >= 4.6, ga_weeks/10, ga_weeks))
#   #print(c("ga_weeks_deliv_recorded_unclean", table(ga_weeks_recorded)))
#   #print(c("ga_weeks_deliv_recorded_clean", table(ga_weeks_deliv_recorded)))
#   ga_weeks_deliv_recorded[ga_weeks_deliv_recorded == 9 | ga_weeks_deliv_recorded <= 6]  <- 41
#   ### GA WEEKS ################
#   #date_anc12, date_del2, date_anc1, date_del, lmp, days_from_anc1_to_del, 
#   #ga_at_delivery_by_ga_anc1, ga_at_delivery_by_edd_anc1, ga_at_delivery_by_us,
#   
#   
#   final = data.frame(study_id_enroll, study_id_anc1, dhc, wks_from_anc1_to_del,
#                      wks_from_anc1_to_us,
#                      wks_from_lmp_to_us, wks_from_lmp_to_del,
#                      wks_from_us_to_del,
#                      lmp, date_anc1, us_date, date_del, 
#                      ga_at_anc1_by_lmp, ga_anc1_recorded,
#                      ga_at_us, fun_ht_anc1,
#                      edd, edd_by_us_date, us_edd, ga_del_by_us_date,
#                      ga_at_deliv_lmp, ga_at_delivery_by_ga_anc1, 
#                      ga_at_delivery_by_edd_anc1,
#                      ga_at_delivery_by_fh_anc1, ga_del_us, ga_weeks_deliv_recorded)
#   
#   
#   #index_bounds = filter_dates(date_anc1, "2016", "2019") & filter_dates(date_del, "2016", "2019") & filter_dates(lmp, "2016", "2019")
#   
#   #return(final)  
#   #return(final_bounds)
#   ## END OTHER SCRIPT ##
#   index_bounds = filter_dates(date_anc1, "2016", "2019") & filter_dates(date_del, "2016", "2019") & filter_dates(lmp, "2016", "2019")
#   #final_bounds = final[which(index_bounds == T), ]
#   #
#   
#   #[which(index_bounds == T), ]
#   frame = data.frame(final, frame)
#   return(frame)
#   
#   
# }
