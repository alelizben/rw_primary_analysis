#1. n= number of US exams completed among women in Arms 2 & 4
#2. N=number of women enrolled in Arms 2 & 4
#3. mean # of minutes spent completing the US examination using variable "minutes_us"
#4. mean GA at time of US by US biometrics using variable "us_adjga"
#5. % of all US exams done less than 22 completed weeks using variable "us_adjga"

make_us_rep = function(df, arm, day) {
  
  df[df == "#NULL!" | df == " " | df == "" ] <- NA
  #facilities = split(df, df[, sprintf('study_id_dhc.enrollment_arm_%s', arm)])
  #df$edd.asdatae = as.Date(df$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%Y-%m-%d"))
  
  #anc_index = df[, sprintf('anc1ga_anc.anc1_visit_arm_%s', arm)]
  
  #facilities = split(df, df$dhc)
  j = get_expected(df, day)
  pct_different_ids = mean(is.na(j$DifferentIDs) | j$DifferentIDs == 1)
  #j = j[!is.na(j$DifferentIDs) & j$DifferentIDs == 0, ]
  num.enroll = nrow(j)
  
  
  anc_var = sprintf('anc1ga_anc')
  #df = df[anc_index <= 24, ]
  
  #edd.asdate = as.Date(df[,sprintf('edd_anc.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d")
  #anc1_date_a1 = sapply(x, function(j) sum(!is.na(j$date_delivery_mat.delivery_arm_4))/nrow(j))
  anc1_enroll_compl_a1 = sum(j$enrollment_complete == 2, na.rm = T)/nrow(j)
  
  anc1_date_compl_a1 = mean(!is.na(j$anc1date_anc))
  anc1_ga_compl_a1 = mean(!is.na(j$anc1ga_anc))
  anc1_lmp_compl_a1 = mean(!is.na(j$lmp_anc))
  anc1_age_compl_a1 = mean(!is.na(j$m_age))
  anc1_edd_compl_a1 = mean(!is.na(j$edd_anc))
  
  edd.asdate = fix_date(j$edd_anc)
  
  head(j$edd_anc)
  head(edd.asdate)
  event = j[,grep('Event', names(j), value = T)]
  rc_event = j[,grep('redcap_event_name', names(j), value = T)]
  
  date = fix_date(j$date_delivery_mat)
  j$myr = format.Date(date, "%m-%Y")
  
  by_myr = split(j, j$myr)
  
  #tpt_birth; sb_birth; infant_sex; bwt_birth (<2500g)
  total_births_by_myr = sapply(by_myr, function(x) sum(!is.na(x$date_delivery_mat)))
  elig_by_myr = sapply(by_myr, function(x) sum(x$anc1ga_anc < 24 & !is.na(x$date_ancfuvst_2), na.rm = T))
  
  
  tpt_birth = sapply(by_myr, function(x) sum(x$tpt_birth == 2, na.rm = T))
  
  sb_birth = sapply(by_myr, function(x) sum(x$sb_birth == 1 | x$sb_birth == 2, na.rm = T))
  sex1 = sapply(by_myr, function(x) sum(x$infant_sex == 2, na.rm = T))
  sex2 = sapply(by_myr, function(x) sum(x$infant_sex2 == 2, na.rm = T))
  sex3 = sapply(by_myr, function(x) sum(x$infant_sex3 == 2, na.rm = T))
  
  bwt_birth1 = sapply(by_myr, function(x) sum(x$bwt_birth <= 2500, na.rm = T))
  bwt_birth2 = sapply(by_myr, function(x) sum(x$bwt_birth2 <= 2500, na.rm = T))
  bwt_birth3 = sapply(by_myr, function(x) sum(x$bwt_birth3 <= 2500, na.rm = T))
  
  # tpt_birth = mean(j$tpt_birth == 2, na.rm = T)
  # sb_birth = mean(j$sb_birth == 1 | j$sb_birth == 2, na.rm = T)
  # sex1 = mean(j$infant_sex == 2, na.rm = T)
  # sex2 = mean(j$infant_sex2 == 2, na.rm = T)
  # sex3 = mean(j$infant_sex3 == 2, na.rm = T)
  # 
  # bwt_birth1 = mean(j$bwt_birth <= 2500, na.rm = T)
  # 
  # bwt_birth2 = mean(j$bwt_birth2 <= 2500, na.rm = T)
  # 
  # bwt_birth3 = mean(j$bwt_birth3 <= 2500, na.rm = T)
  
  #edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%m/%d/%y"))
  ancfuvst_date = j[, grep('date_ancfuvst', names(j), value = T)]
  
  
  #lapply(ancfuvst_date, function(x) (mean(!is.na(x))))
  
  #datefile_fuvst = j[, grep('datefile_fuvst', names(j), value = T)]
  
  ancfuvst_ga = j[, grep('ga_anc_fuvst', names(j), value = T)]
  
  completion_anc_fuvst = sapply(ancfuvst_date, function(x) (mean(!is.na(x))))
  
  ancfuvst_funht = j[, grep('fundal_ht_fuvst', names(j), value = T)] 
  
  
  malaria_dx = j[,grep('malaria_dx', names(j), value = T)]
  
  hb_fuvst = j[ , grep('hb_fuvst', names(j), value = T)]
  hiv_fuvst = j[, grep("hivtstdone_fuvst", names(j), value = T)]
  muac_fuvst = j[, grep('muac_fuvst', names(j), value = T)]
  hivstat = j[,grep('prev_known_hivstat_fuvst', names(j), value = T)]
  referral = j[, grep('referral_indication_us', names(j), value = T)]
  
  actual2 = sum(!is.na(j$date_ancfuvst_2) & !is.na(j[,anc_var]) & j[,anc_var] <= 24)
  
  actual3 = sum(!is.na(j$date_ancfuvst_2b) & !is.na(j[,anc_var]) & j[,anc_var] <= 24)
  actual4 = sum(!is.na(j$date_ancfuvst_2c) & !is.na(j[,anc_var]) & j[,anc_var] <= 24)
  
  pct_redcap_2_is_anc1 = length(grep("anc1_visit", j$redcap_event_name.2, value = T))/num.enroll
  pct_redcap_2_is_na = mean(is.na(j$redcap_event_name.2))
  
  
  #actual4_a2 = sapply(x, function(j) sum(!is.na(j$anc1_file_vstdate.anc_followup_visit_arm_1d)))
  
  # expected2_a1 = sapply(x, function(j) sum(!is.na(j$expected2) & j$expected2 < day))
  # expected3_a1 = sapply(x, function(j) sum(!is.na(j$expected3) & j$expected3 < day))
  # expected4_a1 = sapply(x, function(j) sum(!is.na(j$expected4) & j$expected4 < day))
  # 
  expected2_a1 = sum(j$expected2 == T & j[,anc_var] <= 24, na.rm = T)
  
  expected3_a1 = sum(j$expected3 == T & j[,anc_var] <= 24, na.rm = T)
  
  expected4_a1 = sum(j$expected4 == T & j[,anc_var] <= 24, na.rm = T)
  
  
  anc1_edd_expect_arm1 = sum(edd.asdate < day,  na.rm = T)
  
  #delivered_a1 = lapply(x, function(j) sum(j$del_ancreg.anc_followup_visit_arm_1 == 1, na.rm = T))
  delivered = sum(!is.na(j$date_delivery_mat))
  
  
  #ancpnc1 = lapply(x, function(j) sum(j$ancpnc_file_followup_visits_complete.anc_followup_visit_arm_1 == 2, na.rm = T))
  
  expected_pnc_1 = sum((edd.asdate + 56) < day,  na.rm = T)
  date_pnc = sum(!is.na(j$date_pnc4))
  
  ###### HERE
  #edd.asdate_all = as.Date(df$edd_anc.anc1_visit_arm_1, format = "%Y-%m-%d")
  #pnc_ids = df$study_id_full.enrollment_arm_1[(!is.na(edd.asdate_all) & edd.asdate_all + 56 < day)]
  ####### HERE
  
  num_lt_24_wks = sum(j$anc1ga_anc < 24, na.rm = T)
  
  
  pct_lt_24_wks = num_lt_24_wks / sum(!is.na(j$anc1ga_anc))
  
  
  #num_complete_anc2_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_1 < 24 & !is.na(j$anc_rf_fuvst.anc_followup_visit_arm_1), na.rm = T))
  num_complete_anc2_lt_24_wks = sum(j$anc1ga_anc < 24 & !is.na(j$date_ancfuvst_2), na.rm = T)
  
  # delivered_a1 
  # 
  pct_complete_anc2_lt_24_wks = num_complete_anc2_lt_24_wks / num_lt_24_wks
  # 
  num_expected_anc2_lt_24_wks = sum(j$expected2 == T & j$anc1ga_anc < 24, na.rm = T)
  # 
  # 
  
  num_complete_anc2_lt_24_wks_del = sum(j$anc1ga_anc < 24 & !is.na(j$date_ancfuvst_2)
                                        & !is.na(j$date_delivery_mat), na.rm = T)
  
  
  num_expected_anc2_lt_24_wks_del = sum(j$expected2 == T & (edd.asdate < day) & j$anc1ga_anc < 24, na.rm = T)
  
  # out = rbind(num.enroll, pct_different_ids, pct_redcap_2_is_anc1, pct_redcap_2_is_na, anc1_enroll_compl_a1, anc1_date_compl_a1, 
  #             anc1_ga_compl_a1, anc1_lmp_compl_a1, anc1_age_compl_a1, anc1_edd_compl_a1,
  #             actual2, actual3, actual4,  
  #             expected2_a1, expected3_a1, expected4_a1, delivered_a1, anc1_edd_expect_arm1, date_pnc,
  #             expected_pnc_1,
  #             num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
  #             num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
  # 
  #out = rbind(total_births_by_myr, elig_by_myr, tpt_birth, sb_birth, sex1, sex2, sex3, bwt_birth1, bwt_birth2, bwt_birth3)
  if (arm == 2 | arm == 4) {
    anc_date = fix_date(j$anc1date_anc)
    us_received = (j$us_rec == 1)
    rec = sum(j$us_rec == 1, na.rm = T)    
    us_rec = sprintf("%s/%s = %s", rec, sum(!is.na(j$us_rec)), round(mean(j$us_rec == 1, na.rm = T), 3))
    
    date_us = fix_date(j$us_date)
    
    anc1_date_equals_us_date = sprintf("%s/%s = %s", sum(anc_date == date_us, na.rm = T), 
                                   sum(!is.na(date_us) & !is.na(anc_date)), round(mean(anc_date == date_us, na.rm = T), 3))
    
    anc1_us_date_within_2_days = sprintf("%s/%s = %s", sum(abs(anc_date - date_us) < 3, na.rm = T), 
                                   sum(!is.na(date_us) & !is.na(anc_date)), round(mean(abs(anc_date - date_us) < 3, na.rm = T), 3))
    
    
    date_us_myr = format.Date(date_us, "%m-%Y")
    
    us_adjga = sprintf("%s (n = %s)", round(mean(j$us_adjga, na.rm = T), 3), sum(!is.na(j$us_adjga)))
    min_us = sprintf("%s (n = %s)", round(mean(j$minutes_us, na.rm = T), 3), sum(!is.na(j$minutes_us)))
    #3. mean # of minutes spent completing the US examination using variable "minutes_us"
    #4. mean GA at time of US by US biometrics using variable "us_adjga"
    #5. % of all US exams done less than 22 completed weeks using variable "us_adjga"
    
    #table(usga)
    library(OneR)
    new_usga = as.numeric(gsub("[^0-9\\.]", "", as.character(us_adjga)))
    
    ga_at_us = (ifelse((new_usga/10) >= 4, new_usga/10, new_usga))
    #us_lt_22wks = (ga_at_us < 22) 
    us_lt_14wks = (ga_at_us < 14) 
    #ga_us_sum = (table(bin(ga_at_us, 7)))
    
    usga_lt_9wks = sum(ga_at_us < 9, na.rm = T) 
    usga_9_16wks = sum(ga_at_us >= 9 & ga_at_us < 16, na.rm = T) 
    usga_16_22wks = sum(ga_at_us >= 16 & ga_at_us < 22, na.rm = T) 
    usga_22_28wks = sum(ga_at_us >= 22 & ga_at_us < 28, na.rm = T) 
    usga_28_40wks = sum(ga_at_us >= 28 & ga_at_us < 40, na.rm = T) 
    
    sum(!is.na(ga_at_us))
    table(ga_at_us)
    
    indication1 = sprintf('%s/%s = %s', sum(j$indication_us == 1, na.rm = T), 
                          sum(!is.na(j$indication_us)),
                          round(mean(j$indication_us == 1, na.rm = T), 3))
    
    indication2 = sprintf('%s/%s = %s', sum(j$indication_us == 2, na.rm = T),
                          sum(!is.na(j$indication_us)),
                          round(mean(j$indication_us == 2, na.rm = T), 3))
    
    indication3 = sprintf('%s/%s = %s', sum(j$indication_us == 3, na.rm = T), 
                          sum(!is.na(j$indication_us)),
                          round(mean(j$indication_us == 3, na.rm = T), 3))
 
  }
  #date_us
  out1 = data.frame(num.enroll, delivered, us_rec, usga_lt_9wks, usga_9_16wks,
          usga_16_22wks,  usga_22_28wks,  usga_28_40wks, us_adjga, min_us,
          anc1_date_equals_us_date, anc1_us_date_within_2_days,
          indication1, indication2, indication3)
  
  out2 = date_us_myr

  #this = print(c(is.null(us_received), is.null(min_us), is.null(ga_at_us), is.null(us_lt_22wks)))
  #print(this)
  out3 = data.frame(us_received, min_us, ga_at_us, us_lt_14wks)
  #out = round(out, 3)
  # out = rbind(num.enroll, anc1_enroll_compl_a1, anc1_date_compl_a1, 
  #             anc1_ga_compl_a1, anc1_lmp_compl_a1, anc1_age_compl_a1, anc1_edd_compl_a1,
  #             actual2_a1, actual3_a1, actual4_a1, 
  #             expected2_a1, expected3_a1, expected4_a1, delivered_a1, anc1_edd_expect_arm1, date_pnc,
  #             expected_pnc_1,
  #             num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
  #             num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
  # 
  
  
  
  output = out1
  
  #id_arm$HC[id_arm$DHC %in% colnames(output)]
  return(output)
}



make_anc_rt = function(df, arm, day) {
  
  df[df == "#NULL!" | df == " " ] <- NA
  #facilities = split(df, df[, sprintf('study_id_dhc.enrollment_arm_%s', arm)])
  #df$edd.asdatae = as.Date(df$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%Y-%m-%d"))
  
  #anc_index = df[, sprintf('anc1ga_anc.anc1_visit_arm_%s', arm)]
  
  #facilities = split(df, df$dhc)
  j = get_expected(df, day)
  pct_different_ids = mean(is.na(j$DifferentIDs) | j$DifferentIDs == 1)
  j = j[!is.na(j$DifferentIDs) & j$DifferentIDs == 0, ]
  num.enroll = nrow(j)
  
  
  anc_var = sprintf('anc1ga_anc')
  #df = df[anc_index <= 24, ]
  
  #edd.asdate = as.Date(df[,sprintf('edd_anc.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d")
  #anc1_date_a1 = sapply(x, function(j) sum(!is.na(j$date_delivery_mat.delivery_arm_4))/nrow(j))
  anc1_enroll_compl_a1 = sum(j$enrollment_complete == 2, na.rm = T)/nrow(j)
  
  anc1_date_compl_a1 = mean(!is.na(j$anc1date_anc))
  anc1_ga_compl_a1 = mean(!is.na(j$anc1ga_anc))
  anc1_lmp_compl_a1 = mean(!is.na(j$lmp_anc))
  anc1_age_compl_a1 = mean(!is.na(j$m_age))
  anc1_edd_compl_a1 = mean(!is.na(j$edd_anc))
  
  edd.asdate = fix_date(j$edd_anc)
  
  head(j$edd_anc)
  head(edd.asdate)
  event = j[,grep('Event', names(j), value = T)]
  rc_event = j[,grep('redcap_event_name', names(j), value = T)]
  
  date = fix_date(j$date_delivery_mat)
  j$myr = format.Date(date, "%m-%Y")
  
  by_myr = split(j, j$myr)
  
  #tpt_birth; sb_birth; infant_sex; bwt_birth (<2500g)
  total_births_by_myr = sapply(by_myr, function(x) sum(!is.na(x$date_delivery_mat)))
  elig_by_myr = sapply(by_myr, function(x) sum(x$anc1ga_anc < 24 & !is.na(x$date_ancfuvst_2), na.rm = T))
  
  
  tpt_birth = sapply(by_myr, function(x) sum(x$tpt_birth == 2, na.rm = T))
  
  sb_birth = sapply(by_myr, function(x) sum(x$sb_birth == 1 | x$sb_birth == 2, na.rm = T))
  sex1 = sapply(by_myr, function(x) sum(x$infant_sex == 2, na.rm = T))
  sex2 = sapply(by_myr, function(x) sum(x$infant_sex2 == 2, na.rm = T))
  sex3 = sapply(by_myr, function(x) sum(x$infant_sex3 == 2, na.rm = T))
  
  bwt_birth1 = sapply(by_myr, function(x) sum(x$bwt_birth <= 2500, na.rm = T))
  bwt_birth2 = sapply(by_myr, function(x) sum(x$bwt_birth2 <= 2500, na.rm = T))
  bwt_birth3 = sapply(by_myr, function(x) sum(x$bwt_birth3 <= 2500, na.rm = T))
  
  # tpt_birth = mean(j$tpt_birth == 2, na.rm = T)
  # sb_birth = mean(j$sb_birth == 1 | j$sb_birth == 2, na.rm = T)
  # sex1 = mean(j$infant_sex == 2, na.rm = T)
  # sex2 = mean(j$infant_sex2 == 2, na.rm = T)
  # sex3 = mean(j$infant_sex3 == 2, na.rm = T)
  # 
  # bwt_birth1 = mean(j$bwt_birth <= 2500, na.rm = T)
  # 
  # bwt_birth2 = mean(j$bwt_birth2 <= 2500, na.rm = T)
  # 
  # bwt_birth3 = mean(j$bwt_birth3 <= 2500, na.rm = T)
  
  if (arm == 2 | arm == 4) {
    
    us_rec=  mean(j$us_rec == 1, na.rm = T)
    us_adjga = mean(j$us_adjga, na.rm = T)
    indication1 = mean(j$indication_us == 1, na.rm = T)
    
    indication2 = mean(j$indication_us == 2, na.rm = T)
    
    indication3 = mean(j$indication_us == 3, na.rm = T)
    
    
    
  }
  #edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%m/%d/%y"))
  ancfuvst_date = j[, grep('date_ancfuvst', names(j), value = T)]
  
  
  #lapply(ancfuvst_date, function(x) (mean(!is.na(x))))
  
  #datefile_fuvst = j[, grep('datefile_fuvst', names(j), value = T)]
  
  ancfuvst_ga = j[, grep('ga_anc_fuvst', names(j), value = T)]
  
  completion_anc_fuvst = sapply(ancfuvst_date, function(x) (mean(!is.na(x))))
  
  ancfuvst_funht = j[, grep('fundal_ht_fuvst', names(j), value = T)] 
  
  
  malaria_dx = j[,grep('malaria_dx', names(j), value = T)]
  
  hb_fuvst = j[ , grep('hb_fuvst', names(j), value = T)]
  hiv_fuvst = j[, grep("hivtstdone_fuvst", names(j), value = T)]
  muac_fuvst = j[, grep('muac_fuvst', names(j), value = T)]
  hivstat = j[,grep('prev_known_hivstat_fuvst', names(j), value = T)]
  referral = j[, grep('referral_indication_us', names(j), value = T)]
  
  actual2 = sum(!is.na(j$date_ancfuvst_2) & !is.na(j[,anc_var]) & j[,anc_var] <= 24)
  
  actual3 = sum(!is.na(j$date_ancfuvst_2b) & !is.na(j[,anc_var]) & j[,anc_var] <= 24)
  actual4 = sum(!is.na(j$date_ancfuvst_2c) & !is.na(j[,anc_var]) & j[,anc_var] <= 24)
  
  pct_redcap_2_is_anc1 = length(grep("anc1_visit", j$redcap_event_name.2, value = T))/num.enroll
  pct_redcap_2_is_na = mean(is.na(j$redcap_event_name.2))
  
  
  #actual4_a2 = sapply(x, function(j) sum(!is.na(j$anc1_file_vstdate.anc_followup_visit_arm_1d)))
  
  # expected2_a1 = sapply(x, function(j) sum(!is.na(j$expected2) & j$expected2 < day))
  # expected3_a1 = sapply(x, function(j) sum(!is.na(j$expected3) & j$expected3 < day))
  # expected4_a1 = sapply(x, function(j) sum(!is.na(j$expected4) & j$expected4 < day))
  # 
  expected2_a1 = sum(j$expected2 == T & j[,anc_var] <= 24, na.rm = T)
  
  expected3_a1 = sum(j$expected3 == T & j[,anc_var] <= 24, na.rm = T)
  
  expected4_a1 = sum(j$expected4 == T & j[,anc_var] <= 24, na.rm = T)
  
  
  anc1_edd_expect_arm1 = sum(edd.asdate < day,  na.rm = T)
  
  #delivered_a1 = lapply(x, function(j) sum(j$del_ancreg.anc_followup_visit_arm_1 == 1, na.rm = T))
  delivered_a1 = sum(!is.na(j$date_delivery_mat))
  
  
  #ancpnc1 = lapply(x, function(j) sum(j$ancpnc_file_followup_visits_complete.anc_followup_visit_arm_1 == 2, na.rm = T))
  
  expected_pnc_1 = sum((edd.asdate + 56) < day,  na.rm = T)
  date_pnc = sum(!is.na(j$date_pnc4))
  
  ###### HERE
  #edd.asdate_all = as.Date(df$edd_anc.anc1_visit_arm_1, format = "%Y-%m-%d")
  #pnc_ids = df$study_id_full.enrollment_arm_1[(!is.na(edd.asdate_all) & edd.asdate_all + 56 < day)]
  ####### HERE
  
  num_lt_24_wks = sum(j$anc1ga_anc < 24, na.rm = T)
  
  
  pct_lt_24_wks = num_lt_24_wks / sum(!is.na(j$anc1ga_anc))
  
  
  #num_complete_anc2_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_1 < 24 & !is.na(j$anc_rf_fuvst.anc_followup_visit_arm_1), na.rm = T))
  num_complete_anc2_lt_24_wks = sum(j$anc1ga_anc < 24 & !is.na(j$date_ancfuvst_2), na.rm = T)
  
  # delivered_a1 
  # 
  pct_complete_anc2_lt_24_wks = num_complete_anc2_lt_24_wks / num_lt_24_wks
  # 
  num_expected_anc2_lt_24_wks = sum(j$expected2 == T & j$anc1ga_anc < 24, na.rm = T)
  # 
  # 
  
  num_complete_anc2_lt_24_wks_del = sum(j$anc1ga_anc < 24 & !is.na(j$date_ancfuvst_2)
                                        & !is.na(j$date_delivery_mat), na.rm = T)
  
  
  num_expected_anc2_lt_24_wks_del = sum(j$expected2 == T & (edd.asdate < day) & j$anc1ga_anc < 24, na.rm = T)
  
  # out = rbind(num.enroll, pct_different_ids, pct_redcap_2_is_anc1, pct_redcap_2_is_na, anc1_enroll_compl_a1, anc1_date_compl_a1, 
  #             anc1_ga_compl_a1, anc1_lmp_compl_a1, anc1_age_compl_a1, anc1_edd_compl_a1,
  #             actual2, actual3, actual4,  
  #             expected2_a1, expected3_a1, expected4_a1, delivered_a1, anc1_edd_expect_arm1, date_pnc,
  #             expected_pnc_1,
  #             num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
  #             num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
  # 
  #out = rbind(total_births_by_myr, elig_by_myr, tpt_birth, sb_birth, sex1, sex2, sex3, bwt_birth1, bwt_birth2, bwt_birth3)
  out = completion_anc_fuvst
  out = round(out, 3)
  # out = rbind(num.enroll, anc1_enroll_compl_a1, anc1_date_compl_a1, 
  #             anc1_ga_compl_a1, anc1_lmp_compl_a1, anc1_age_compl_a1, anc1_edd_compl_a1,
  #             actual2_a1, actual3_a1, actual4_a1, 
  #             expected2_a1, expected3_a1, expected4_a1, delivered_a1, anc1_edd_expect_arm1, date_pnc,
  #             expected_pnc_1,
  #             num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
  #             num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
  # 
  
  
  
  output = out
  
  #id_arm$HC[id_arm$DHC %in% colnames(output)]
  return(output)
}

make_delivery_by_month = function(df, arm, day) {
  
  df[df == "#NULL!" | df == " " ] <- NA
  #facilities = split(df, df[, sprintf('study_id_dhc.enrollment_arm_%s', arm)])
  #df$edd.asdatae = as.Date(df$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%Y-%m-%d"))
  
  #anc_index = df[, sprintf('anc1ga_anc.anc1_visit_arm_%s', arm)]
  
  #facilities = split(df, df$dhc)
  j = get_expected(df, day)
  pct_different_ids = mean(is.na(j$DifferentIDs) | j$DifferentIDs == 1)
  j = j[!is.na(j$DifferentIDs) & j$DifferentIDs == 0, ]
  num.enroll = nrow(j)
  

    anc_var = sprintf('anc1ga_anc')
    #df = df[anc_index <= 24, ]
    
    #edd.asdate = as.Date(df[,sprintf('edd_anc.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d")
    #anc1_date_a1 = sapply(x, function(j) sum(!is.na(j$date_delivery_mat.delivery_arm_4))/nrow(j))
    anc1_enroll_compl_a1 = sum(j$enrollment_complete == 2, na.rm = T)/nrow(j)
    
    anc1_date_compl_a1 = mean(!is.na(j$anc1date_anc))
    anc1_ga_compl_a1 = mean(!is.na(j$anc1ga_anc))
    anc1_lmp_compl_a1 = mean(!is.na(j$lmp_anc))
    anc1_age_compl_a1 = mean(!is.na(j$m_age))
    anc1_edd_compl_a1 = mean(!is.na(j$edd_anc))
    
    edd.asdate = fix_date(j$edd_anc)
    
    head(j$edd_anc)
    head(edd.asdate)
    event = j[,grep('Event', names(j), value = T)]
    rc_event = j[,grep('redcap_event_name', names(j), value = T)]
    
    date = fix_date(j$date_delivery_mat)
    j$myr = format.Date(date, "%m-%Y")
    
    by_myr = split(j, j$myr)
    
    #tpt_birth; sb_birth; infant_sex; bwt_birth (<2500g)
    total_births_by_myr = sapply(by_myr, function(x) sum(!is.na(x$date_delivery_mat)))
    elig_by_myr = sapply(by_myr, function(x) sum(x$anc1ga_anc < 24 & !is.na(x$date_ancfuvst_2), na.rm = T))
    
    
    tpt_birth = sapply(by_myr, function(x) sum(x$tpt_birth == 2, na.rm = T))
    
    sb_birth = sapply(by_myr, function(x) sum(x$sb_birth == 1 | x$sb_birth == 2, na.rm = T))
    sex1 = sapply(by_myr, function(x) sum(x$infant_sex == 2, na.rm = T))
    sex2 = sapply(by_myr, function(x) sum(x$infant_sex2 == 2, na.rm = T))
    sex3 = sapply(by_myr, function(x) sum(x$infant_sex3 == 2, na.rm = T))
    
    bwt_birth1 = sapply(by_myr, function(x) sum(x$bwt_birth <= 2500, na.rm = T))
    bwt_birth2 = sapply(by_myr, function(x) sum(x$bwt_birth2 <= 2500, na.rm = T))
    bwt_birth3 = sapply(by_myr, function(x) sum(x$bwt_birth3 <= 2500, na.rm = T))
    
    # tpt_birth = mean(j$tpt_birth == 2, na.rm = T)
    # sb_birth = mean(j$sb_birth == 1 | j$sb_birth == 2, na.rm = T)
    # sex1 = mean(j$infant_sex == 2, na.rm = T)
    # sex2 = mean(j$infant_sex2 == 2, na.rm = T)
    # sex3 = mean(j$infant_sex3 == 2, na.rm = T)
    # 
    # bwt_birth1 = mean(j$bwt_birth <= 2500, na.rm = T)
    # 
    # bwt_birth2 = mean(j$bwt_birth2 <= 2500, na.rm = T)
    # 
    # bwt_birth3 = mean(j$bwt_birth3 <= 2500, na.rm = T)
    
    if (arm == 2 | arm == 4) {
      
      us_rec=  mean(j$us_rec == 1, na.rm = T)
      us_adjga = mean(j$us_adjga, na.rm = T)
      indication1 = mean(j$indication_us == 1, na.rm = T)
     
      indication2 = mean(j$indication_us == 2, na.rm = T)
     
      indication3 = mean(j$indication_us == 3, na.rm = T)
      
      
      
    }
    #edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%m/%d/%y"))
    ancfuvst_date = j[, grep('date_ancfuvst', names(j), value = T)]

    
    #lapply(ancfuvst_date, function(x) (mean(!is.na(x))))
    
    #datefile_fuvst = j[, grep('datefile_fuvst', names(j), value = T)]
    
    ancfuvst_ga = j[, grep('ga_anc_fuvst', names(j), value = T)]
    
    completion_anc_fuvst = sapply(ancfuvst_date, function(x) (mean(!is.na(x))))
    
    ancfuvst_funht = j[, grep('fundal_ht_fuvst', names(j), value = T)] 
    
    
    malaria_dx = j[,grep('malaria_dx', names(j), value = T)]
    
    hb_fuvst = j[ , grep('hb_fuvst', names(j), value = T)]
    hiv_fuvst = j[, grep("hivtstdone_fuvst", names(j), value = T)]
    muac_fuvst = j[, grep('muac_fuvst', names(j), value = T)]
    hivstat = j[,grep('prev_known_hivstat_fuvst', names(j), value = T)]
    referral = j[, grep('referral_indication_us', names(j), value = T)]
    
    actual2 = sum(!is.na(j$date_ancfuvst_2) & !is.na(j[,anc_var]) & j[,anc_var] <= 24)
    
    actual3 = sum(!is.na(j$date_ancfuvst_2b) & !is.na(j[,anc_var]) & j[,anc_var] <= 24)
    actual4 = sum(!is.na(j$date_ancfuvst_2c) & !is.na(j[,anc_var]) & j[,anc_var] <= 24)
    
    pct_redcap_2_is_anc1 = length(grep("anc1_visit", j$redcap_event_name.2, value = T))/num.enroll
    pct_redcap_2_is_na = mean(is.na(j$redcap_event_name.2))
    
    
    #actual4_a2 = sapply(x, function(j) sum(!is.na(j$anc1_file_vstdate.anc_followup_visit_arm_1d)))
    
    # expected2_a1 = sapply(x, function(j) sum(!is.na(j$expected2) & j$expected2 < day))
    # expected3_a1 = sapply(x, function(j) sum(!is.na(j$expected3) & j$expected3 < day))
    # expected4_a1 = sapply(x, function(j) sum(!is.na(j$expected4) & j$expected4 < day))
    # 
    expected2_a1 = sum(j$expected2 == T & j[,anc_var] <= 24, na.rm = T)
    
    expected3_a1 = sum(j$expected3 == T & j[,anc_var] <= 24, na.rm = T)
    
    expected4_a1 = sum(j$expected4 == T & j[,anc_var] <= 24, na.rm = T)
    
    
    anc1_edd_expect_arm1 = sum(edd.asdate < day,  na.rm = T)
    
    #delivered_a1 = lapply(x, function(j) sum(j$del_ancreg.anc_followup_visit_arm_1 == 1, na.rm = T))
    delivered_a1 = sum(!is.na(j$date_delivery_mat))
    
    
    #ancpnc1 = lapply(x, function(j) sum(j$ancpnc_file_followup_visits_complete.anc_followup_visit_arm_1 == 2, na.rm = T))
    
    expected_pnc_1 = sum((edd.asdate + 56) < day,  na.rm = T)
    date_pnc = sum(!is.na(j$date_pnc4))
    
    ###### HERE
    #edd.asdate_all = as.Date(df$edd_anc.anc1_visit_arm_1, format = "%Y-%m-%d")
    #pnc_ids = df$study_id_full.enrollment_arm_1[(!is.na(edd.asdate_all) & edd.asdate_all + 56 < day)]
    ####### HERE
    
    num_lt_24_wks = sum(j$anc1ga_anc < 24, na.rm = T)
    
    
    pct_lt_24_wks = num_lt_24_wks / sum(!is.na(j$anc1ga_anc))
    
    
    #num_complete_anc2_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_1 < 24 & !is.na(j$anc_rf_fuvst.anc_followup_visit_arm_1), na.rm = T))
    num_complete_anc2_lt_24_wks = sum(j$anc1ga_anc < 24 & !is.na(j$date_ancfuvst_2), na.rm = T)
    
    # delivered_a1 
    # 
    pct_complete_anc2_lt_24_wks = num_complete_anc2_lt_24_wks / num_lt_24_wks
    # 
    num_expected_anc2_lt_24_wks = sum(j$expected2 == T & j$anc1ga_anc < 24, na.rm = T)
    # 
    # 
    
    num_complete_anc2_lt_24_wks_del = sum(j$anc1ga_anc < 24 & !is.na(j$date_ancfuvst_2)
                                          & !is.na(j$date_delivery_mat), na.rm = T)
    
    
    num_expected_anc2_lt_24_wks_del = sum(j$expected2 == T & (edd.asdate < day) & j$anc1ga_anc < 24, na.rm = T)
    
    # out = rbind(num.enroll, pct_different_ids, pct_redcap_2_is_anc1, pct_redcap_2_is_na, anc1_enroll_compl_a1, anc1_date_compl_a1, 
    #             anc1_ga_compl_a1, anc1_lmp_compl_a1, anc1_age_compl_a1, anc1_edd_compl_a1,
    #             actual2, actual3, actual4,  
    #             expected2_a1, expected3_a1, expected4_a1, delivered_a1, anc1_edd_expect_arm1, date_pnc,
    #             expected_pnc_1,
    #             num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
    #             num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
    # 
    out = rbind(total_births_by_myr, elig_by_myr, tpt_birth, sb_birth, sex1, sex2, sex3, bwt_birth1, bwt_birth2, bwt_birth3)
    #out = completion_anc_fuvst
    #out = round(out, 3)
    # out = rbind(num.enroll, anc1_enroll_compl_a1, anc1_date_compl_a1, 
    #             anc1_ga_compl_a1, anc1_lmp_compl_a1, anc1_age_compl_a1, anc1_edd_compl_a1,
    #             actual2_a1, actual3_a1, actual4_a1, 
    #             expected2_a1, expected3_a1, expected4_a1, delivered_a1, anc1_edd_expect_arm1, date_pnc,
    #             expected_pnc_1,
    #             num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
    #             num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
    # 
    
    

  output = out
  
  #id_arm$HC[id_arm$DHC %in% colnames(output)]
  return(output)
}

make_del_rates = function(dat) {
  
  dat$tpt_rt = sprintf("%s/%s = %s", dat$tpt_birth, dat$total_births_by_myr, 
                       round(dat$tpt_birth/ dat$total_births_by_myr, 3))
  dat$stb_rt = sprintf("%s/%s = %s", dat$sb_birth, dat$total_births_by_myr, 
                       round(dat$sb_birth/ dat$total_births_by_myr, 3))
  dat$lt_2500g_rt = sprintf("%s/%s = %s", dat$bwt_birth1, dat$total_births_by_myr, 
                            round(dat$bwt_birth1 / dat$total_births_by_myr, 3))
  dat$newbr_girls_rt = sprintf("%s/%s = %s", dat$sex1, dat$total_births_by_myr, 
                               round(dat$sex1 / dat$total_births_by_myr, 3))
  return(dat)
}
# 
# out1 = lapply(arm1, function(g) make_us_rep(g, 1, "2018-11-22"))
# col_names = colnames(out1[[2]])
# out1 = sapply(arm1, function(x) make_delivery_by_month(x, 1, "2018-11-22"))
# out2 = lapply(arm2, function(g) make_delivery_by_month(g, 2, "2018-11-22"))
# out3 = sapply(arm3, function(g) make_delivery_by_month(g, 3, "2018-11-22"))
# out4 = lapply(arm4, function(g) make_delivery_by_month(g, 4, "2018-11-22"))
# 
# col_names = colnames(out4$'334')[-c(8)]
# 
# new1 = lapply(out1, function(f) f[ ,colnames(f) %in% col_names])
# ind1 = new1[sapply(new1, ncol) == 9]
# new2 = lapply(out2, function(f) f[ ,colnames(f) %in% col_names])
# ind2 = new2[sapply(new2, ncol) == 9]
# new3 = lapply(out3, function(f) f[ ,colnames(f) %in% col_names])
# ind3 = new3[sapply(new3, ncol) == 9]
# new4 = lapply(out4, function(f) f[ ,colnames(f) %in% col_names])
# ind4 = new4[sapply(new4, ncol) == 9]
# 
# fin1 = data.frame(t(Reduce('+', ind1)))
# fin2 = data.frame(t(Reduce('+', ind2)))
# fin3 = data.frame(t(Reduce('+', ind3)))
# fin4 = data.frame(t(Reduce('+', ind4)))
# 
# fin1 = make_del_rates(fin1)
# fin2 = make_del_rates(fin2)
# fin3 = make_del_rates(fin3)
# fin4 = make_del_rates(fin4)
# 
# final_out = list(fin1, fin2, fin3, fin4)
# 
# #final_out = list((out1), (out2), (out3), (out4))
# library(openxlsx)
# names(final_out) = paste("Arm", 1:4)
# write.xlsx(final_out, sprintf("delivery_report_by_month_%s.xlsx", Sys.Date()), row.names = T)
# 
# 
