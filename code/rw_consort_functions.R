get_n = function(j) {
  j = as.numeric(j)
  mu = mean(j, na.rm = T)
  med = median(j, na.rm = T)
  n = sum(!is.na(j))
  out = sprintf("mean = %s (n = %s)", round(mu, 3), n)
  return(out)
}



get_elig = function(df, type) {
  #df = df %>% filter(!is.na(m_age) & m_age > 15)
  #df = df %>% filter(!is.na(m_age_anc) & m_age_anc > 15)
  df = df %>% filter(is.na(m_age) |  m_age > 15)
  df = df %>% filter(is.na(m_age_anc) | m_age_anc > 15)
  summary(df$m_age_anc)
  
  enroll_date = as.Date(df$enroll_date, format = "%m/%d/%Y")
  anc1date = as.Date(df$anc1date_anc, format = "%m/%d/%Y")
  
  #enroll_date >=  "2017-05-22" & 
  #anc1date >=  "2017-05-22" & 
  #| (anc1date <= "2018-10-31")
  valid_enroll = (enroll_date >=  "2017-05-22") & (enroll_date <= "2018-10-31") 
  valid_anc = (anc1date >=  "2017-05-22") & (anc1date <= "2018-10-31")
  #df$valid_enroll = valid_enroll == T
  table(valid_enroll, useNA = 'always')
  table(valid_anc, useNA = "always")
  # good up to hear
  dates = subset(df, select = c(anc1date_anc, date_ancfuvst_2,
                                date_ancfuvst_2b, date_ancfuvst_2c, date_ancfuvst_2d))
  number_missing = is.na(dates)
  number_missing_dates = rowSums(number_missing)
  
  df$number_anc = 5 - number_missing_dates
  
  df$anc1ga_anc = as.numeric(as.character(df$anc1ga_anc))
  ##departed after without clinic visit
  
  if (type == "valid_enroll") {
    
    df = df %>% filter(valid_enroll | valid_anc)
    return(df)
    
  } else if (type == "valid_anc_ga") {
    
    df = df %>% filter(valid_anc & !is.na(anc1ga_anc))
    return(df)
  } else if (type == "valid_anc_date") {
    
    df = df %>% filter(valid_anc)
    return(df)
  }
  ## is possible for one of these to be not valid date
  df = df %>% filter(valid_enroll | valid_anc)
  
  #df$enroll_but_no_anc1 = (!is.na(df$enroll_date) & is.na(df$anc1date_anc))
  
  ###anc1_ga_lt_24_weeks
  
  #df$anc_ga_lt_24_wks = df$anc1ga_anc <= 24 & (df$enroll_but_no_anc1 == F)
  df$anc_ga_lt_24_wks = df$anc1ga_anc <= 24 
  summary(df$anc_ga_lt_24_wks)
  ## subset ##
  elig_enroll = df %>% filter(anc_ga_lt_24_wks == TRUE)
  elig_enroll = elig_enroll %>% filter(NumberANCs >= 2)
  dim(elig_enroll)
  
  # t = df %>% filter(eligible == 1)
  # table(t$NumberANCs >=4 )
  # mean(t$NumberANCs)
  # mean(t$anc1ga_anc < 16, na.rm = T)
  # mean(t$NumberANCs >= 4)
  # 
  
  if (type == "elig_enroll") {
    return(elig_enroll)
  }
  
  # subset ##
  
  ## MOTHER BABY PAIRS ##
  #deliver_within_study_w_valid_ga_2_anc
  elig_enroll$date_del = as.Date(elig_enroll$date_delivery_mat, format = "%m/%d/%Y")
  elig_enroll$lmp = as.Date(elig_enroll$lmp_anc, format = "%m/%d/%Y")
  elig_enroll$ga_at_del_by_lmp = as.numeric((elig_enroll$date_del - elig_enroll$lmp)/7)
  
  elig_enroll$valid_ga_recorded = elig_enroll$ga_weeks >= 20 & elig_enroll$ga_weeks <= 44
  elig_enroll$valid_ga_lmp = elig_enroll$ga_at_del_by_lmp >= 20 & elig_enroll$ga_at_del_by_lmp <= 44
  ##all should be valid recorded GA
  
  ##elig for primary analysis
  elig_enroll = elig_enroll %>% filter(date_del <= "2019-03-31") 
  elig_enroll$elig = (elig_enroll$anc1ga_anc <= 24 & elig_enroll$NumberANCs >= 2)
  
  elig_mother_baby = elig_enroll %>% filter(elig == T) 
  
  if (type == "analysis") {
    
    return(elig_mother_baby)
    
  }
  ## NA COME FROM MOSTLY GA RECORDED
  ltfu = ((!is.na(elig_mother_baby$valid_ga_lmp) & elig_mother_baby$valid_ga_lmp == T | !is.na(elig_mother_baby$valid_ga_recorded) & elig_mother_baby$valid_ga_recorded == T ))
  ltfu = ( elig_mother_baby$valid_ga_lmp == T | elig_mother_baby$valid_ga_recorded == T )
  has_outcome = elig_mother_baby %>% filter(ltfu == T)
  table(ltfu, useNA = "always")
}

#df1 = get_elig(arm1, "elig_anc")
#Proportion of women who attended at least 4 ANC visits
#Proportion of women who attended ANC 1 before 16 completed weeks GA
#Mean number of ANC visits attended per woman, in number of visits
#Mean GA at first ANC visit, in weeks
get_anc_covg = function(x){

  n_anc1ga = sum(!is.na(x$anc1ga_anc)) 
  n_anc = sum(!is.na(x$NumberANCs))
  n_anc1date = sum(!is.na(x$anc1date_anc))
  
  ga_lt_16 = c(table(x$anc1ga_anc < 16, useNA = "always"), n_anc1ga)
  mean_ga_anc1 = c(mean(x$anc1ga_anc, na.rm = T), sd(x$anc1ga_anc, na.rm = T), sum(is.na(x$anc1ga_anc)), n_anc1ga)
  mean_num_anc = c(mean(x$NumberANCs), sd(x$NumberANCs), sum(is.na(x$NumberANCs)), n_anc)
  gt_eq_4_anc = c(table(x$NumberANCs >= 4, useNA = "always"), n_anc)
  #gt_2_anc = c(table(x$NumberANCs >= 2, useNA = "always"), n_anc1date)
  returned_2_anc_date = c(table(!is.na(x$date_ancfuvst_2), useNA = "always"), n_anc1date)
  
  out = data.frame(rbind(ga_lt_16, gt_eq_4_anc, mean_num_anc, mean_ga_anc1))
  names(out)[4] = "total (non-na)"
  out$pct = out$TRUE./out$`total (non-na)`
  out = round(out, 3)
  
  out
}


get_consort = function(df, ga_type) {
  #eligible_enroll_date
  #anc1_date = as.Date(df$anc1date_anc, format = "%m/%d/%Y")
  enroll_date = as.Date(df$enroll_date, format = "%m/%d/%Y")
  valid_enroll = enroll_date >=  "2017-05-22" & enroll_date <= "2018-10-31"
  df$valid_enroll = valid_enroll == T
  elig_enroll_date = table(df$valid_enroll, useNA = 'always')
  dates = subset(df, select = c(anc1date_anc, date_ancfuvst_2,
                                date_ancfuvst_2b, date_ancfuvst_2c, date_ancfuvst_2d))
  number_missing = is.na(dates)
  number_missing_dates = rowSums(number_missing)
  df$number_anc = 5 - number_missing_dates
  
  
  df$anc1ga_anc = as.numeric(as.character(df$anc1ga_anc))
  ##departed after without clinic visit
  df = df %>% filter(valid_enroll == T)
  
  df$enroll_but_no_anc1 = (!is.na(df$enroll_date) & is.na(df$anc1date_anc))
  departed_without_clinic = table(df$enroll_but_no_anc1, useNA = "always")
  (departed_without_clinic)
  
  ###anc1_ga_lt_24_weeks
  
  df$anc_ga_lt_24_wks = df$anc1ga_anc <= 24 & (df$enroll_but_no_anc1 == F)
  anc1_ga_lt_24 = table(df$anc_ga_lt_24_wks, useNA = 'always')
  anc1_ga_lt_24
  ### type in consort 
  #anc1_ga_lt_24_valid_enroll_date = table((df$valid_anc1 == T & !is.na(df$anc_ga_lt_24_wks) &
  #                                         df$anc_ga_lt_24_wks == T), useNA = "always")
  
  out_elig_enroll = data.frame(rbind(elig_enroll_date, departed_without_clinic, anc1_ga_lt_24))
  
  ## subset ##
  elig_enroll = df %>% filter(anc_ga_lt_24_wks)
  table(elig_enroll$anc1ga_anc < 16, useNA = "always")/nrow(elig_enroll)
  ## subset ##
  
  ## MOTHER BABY PAIRS ##
  #deliver_within_study_w_valid_ga_2_anc
  elig_enroll$date_del = as.Date(elig_enroll$date_delivery_mat, format = "%m/%d/%Y")
  elig_enroll$lmp = as.Date(elig_enroll$lmp_anc, format = "%m/%d/%Y")
  elig_enroll$ga_at_del_by_lmp = as.numeric((elig_enroll$date_del - elig_enroll$lmp)/7)
  
  elig_enroll$valid_ga_recorded = elig_enroll$ga_weeks >= 20 & elig_enroll$ga_weeks <= 44
  elig_enroll$valid_ga_lmp = elig_enroll$ga_at_del_by_lmp >= 20 & elig_enroll$ga_at_del_by_lmp <= 44
  ##all should be valid recorded GA
  valid_recorded_ga_del = c(table(elig_enroll$valid_ga_recorded, useNA = "always"), 0)[c(3,1,2)]
  names(valid_recorded_ga_del)[1] = FALSE
  valid_ga_lmp_del = table(elig_enroll$valid_ga_lmp, useNA = "always")
  
  delivered_before_march_31 = table(elig_enroll$date_del <= "2019-03-31", useNA = "always")
  delivered_before_march_31
  
  ## did not deliver within study period or didn't deliver at all 2271 + 91 
  ## did not have >= 2 visits+ 674
  ## did not have valid gestatdel + 116
  delivered_out = elig_enroll %>% filter(date_del > "2019-03-31" | is.na(date_del) )
  elig = (delivered_out$anc1ga_anc <= 24 & delivered_out$number_anc >= 2)
  anc_lt_24_and_2_anc_visits_del_out = table(elig, useNA = 'always')
  
  
  elig_enroll = elig_enroll %>% filter(date_del <= "2019-03-31") 
  elig = (elig_enroll$anc1ga_anc <= 24 & elig_enroll$number_anc >= 2)
  
  anc_lt_24_and_2_anc_visits = table(elig, useNA = 'always')
  
  elig_mother_baby = elig_enroll %>% filter(elig == T) 
  table(elig_mother_baby$valid_ga_lmp, useNA = 'always')
  
  ### NA COME FROM MOSTLY GA RECORDED
  ltfu = ((!is.na(elig_mother_baby$valid_ga_lmp) & elig_mother_baby$valid_ga_lmp == T | !is.na(elig_mother_baby$valid_ga_recorded) & elig_mother_baby$valid_ga_recorded == T ))
  ltfu = ( elig_mother_baby$valid_ga_lmp == T | elig_mother_baby$valid_ga_recorded == T )
  
  ##consort diag
  
  sum(is.na(elig_mother_baby$valid_ga_lmp) | elig_mother_baby$valid_ga_lmp == F)
  sum(is.na(elig_mother_baby$valid_ga_recorded))
  sum(elig_mother_baby$valid_ga_recorded == F, na.rm = T)
  
  ## 50 where both are NA
  sum(is.na(elig_mother_baby$valid_ga_lmp) & is.na(elig_mother_baby$valid_ga_recorded))
  sum(elig_mother_baby$valid_ga_lmp == F & is.na(elig_mother_baby$valid_ga_recorded), na.rm = T)
  #### this adds up to 116!!!!!!!
  ##either BOTH are NA OR when valid_ga_lmp is false, there no ga recorded to compensate. 
  table(elig_mother_baby$valid_ga_recorded[elig_mother_baby$valid_ga_lmp == F], useNA = "always")
  table(elig_mother_baby$valid_ga_recorded[is.na(elig_mother_baby$valid_ga_lmp)], useNA = "always")
  
  summary(elig_mother_baby$ga_at_del_by_lmp)
  sum(elig_mother_baby$ga_at_del_by_lmp < 20, na.rm = T)
  sum(elig_mother_baby$ga_at_del_by_lmp >= 44, na.rm = T)
  table(elig_mother_baby$valid_ga_lmp, useNA ='always')
  table(elig_mother_baby$valid_ga_recorded, useNA ='always')
  ##consort diagram
  
  attained_outcome = c(table(ltfu, useNA = 'always'), 0)[c(3, 1, 2)]
  attained_outcome
  names(attained_outcome)[1] = "FALSE"
  
  #df_16 = df %>% filter(anc1ga_anc <= 16 & number_anc >= 2)
  
  #valid_recorded_ga_del, valid_ga_lmp_del, 
  out_elig_mother_baby = data.frame(rbind(delivered_before_march_31, 
                                          anc_lt_24_and_2_anc_visits_del_out,
                                          anc_lt_24_and_2_anc_visits,
                                          attained_outcome))
  
  #out_elig_enroll, 
  out = rbind(out_elig_enroll, out_elig_mother_baby)
  out$total = rowSums(out)
  
  out
  
  
}



output_ptb_rate = function(df) {
  
  n_usdate = sum(!is.na(df$ga_del_by_us_date)) 
  n_usedd = sum(!is.na(df$ga_del_by_us_edd))
  n_gaweeks = sum(!is.na(df$ga_weeks_recorded))
  n_lmp = sum(!is.na(df$ga_at_deliv_lmp))
  
  by_us_date = c(table(df$ga_del_by_us_date < 37, useNA = "always"), n_usdate)
  by_us_edd = c(table(df$ga_del_by_us_edd < 37, useNA = "always"), n_usedd)
  by_ga_recorded = c(table(df$ga_weeks_recorded < 37, useNA = "always"), n_gaweeks)
  by_lmp = c(table(df$ga_at_deliv_lmp < 37, useNA = "always"), n_lmp)
  
  out = data.frame(rbind(by_us_date, by_us_edd, by_ga_recorded, by_lmp))
  names(out)[4] = "N"
  out$ptb_rate = round(out$TRUE./out$N, 3)
  
  out
}

output_ptb_rate = function(df) {
  out  = NULL
  n_usdate = sum(!is.na(df$ga_del_by_us_date)) 
  n_usedd = sum(!is.na(df$ga_del_by_us_edd))
  
  out$mean_ga_anc1 = get_n(df$ga_at_us)
  out$mean_ga_anc1_back_calc = get_n(df$back_calc_dod_ga_recorded)
  out$mean_ga_anc1_lmp = get_n(df$ga_at_anc1_by_lmp)
  ptb_usd2 = as.numeric(df$ga_del_by_us_date) < 37
  out$ptb_usd = (get_n(ptb_usd2))
  out$ga_usd = (get_n(df$ga_del_by_us_date))
  
  ptb_usd_edd2 = as.numeric(df$ga_del_by_us_edd) < 37
  out$ptb_usd_edd = (get_n(ptb_usd_edd2))
  out$ga_usd_edd = (get_n(df$ga_del_by_us_edd))
  
  out$among_lt_37wk_usd = get_n(df$ga_del_by_us_date[df$ga_del_by_us_date < 37])
  out$among_lt_37wk_edd = get_n(df$ga_del_by_us_edd[df$ga_del_by_us_edd < 37])
  
  out$lbw_2500 = get_n(df$weight < 2500)
  out = do.call(rbind, out)
  out
}


get_ga_us= function(df){
  ## point of 2anc , <24 wks, enrolled, delivered march, mothers age
  ## additional us data that's being collected
  
  # library(openxlsx)
  # out = list(out2, out4)
  # names(out) = c("arm2", "arm4")
  # write.xlsx(out, file = "ptb_rates_us_rec.xlsx")
  
  table(arm4$us_rec, useNA = "always")
  
  ##us_rec
  
  ##by us_edd
  summary(as.numeric(us_2$ga_del_us))
  summary(as.numeric(us_4$ga_del_us))
  sum(as.numeric(us_2$ga_del_us) < 2, na.rm = T)
  sum(as.numeric(us_4$ga_del_us) < 2, na.rm = T)
  sum(as.numeric(us_2$ga_del_us) > 45, na.rm = T)
  sum(as.numeric(us_4$ga_del_us) > 45, na.rm = T)
  
  ## by us_date and ga at US
  summary(as.numeric(us_2$ga_del_by_us_date))
  summary(as.numeric(us_4$ga_del_by_us_date))
  sum(as.numeric(us_2$ga_del_by_us_date) < 2, na.rm = T)
  sum(as.numeric(us_4$ga_del_by_us_date) < 2, na.rm = T)
  sum(as.numeric(us_2$ga_del_by_us_date) > 45, na.rm = T)
  sum(as.numeric(us_4$ga_del_by_us_date) > 45, na.rm = T)
  
  
  df2_edd = us_2 %>% filter(ga_del_us >= 10 & ga_del_us <= 44)
  df4_edd = us_4 %>% filter(ga_del_us >= 10 & ga_del_us <= 44)
  
  dim(df2_edd)
  dim(df4_edd)
  sum(!is.na(df2_edd$ga_del_by_us_date))
  sum(!is.na(df2_edd$ga_del_us))
  sum(!is.na(df4_edd$ga_del_by_us_date))
  sum(!is.na(df4_edd$ga_del_us))
  
  ##this provides bigger gap for using ga_del_by_us_date
  df2_usd = us_2 %>% filter(ga_del_by_us_date >= 10 & ga_del_by_us_date <= 43)
  df4_usd = us_4 %>% filter(ga_del_by_us_date >= 10 & ga_del_by_us_date <= 43)
  
  dim(df2_usd)
  dim(df4_usd)
  sum(!is.na(df2_usd$ga_del_by_us_date))
  sum(!is.na(df2_usd$ga_del_us))
  sum(!is.na(df4_usd$ga_del_by_us_date))
  sum(!is.na(df4_usd$ga_del_us))
  
  
  #df2 = us_2 %>% filter(ga_del_by_us_date >= 24 & ga_del_by_us_date <= 43)
  #df4 = us_4 %>% filter(ga_del_by_us_date >= 24 & ga_del_by_us_date <= 43)
  
  ## this is for us date variable (not edd date)
  # ptb2 = df2_usd[df2_usd$ga_del_by_us_date < 37, ]
  # ptb4 = df4_usd[df4_usd$ga_del_by_us_date < 37, ]
  # summary(as.numeric(ptb2$ga_del_by_us_date))
  # summary(as.numeric(ptb4$ga_del_by_us_date))
  # sum(is.na(ptb2$ga_del_by_us_date))
  
  
  #df$birth_weight < 2500 | df$birth_weight >= 2500 & df$birth_weight <= 2999 & df$ga < 37 |
  #                 df$birth_outcome == "Fresh_Still_Birth" & df$ga < 37, "elig_for_ptb", "not_elig_for_ptb"
  
  
  #defn_ptb2 = df2_usd$ga_weeks_recorded < 37 & df2_usd$weight >=2500 & df2_usd$weight <=2999 | df2_usd$weight < 2500
  #defn_ptb4 = df4_usd$ga_weeks_recorded < 37 & df4_usd$weight >=2500 & df4_usd$weight <=2999 | df4_usd$weight < 2500
  # sum(defn_ptb2, na.rm = T)
  # sum(defn_ptb4, na.rm = T)
  # sum(df2_usd$ga_weeks_recorded < 37, na.rm = T) /sum(!is.na(df2_usd$ga_weeks_recorded))
  # sum(df4_usd$ga_weeks_recorded < 37, na.rm = T)
  #ga_rec_usd = c(get_n(df2_usd$ga_weeks_recorded), get_n(df4_usd$ga_weeks_recorded))
  #ptb_rec_usd = c(get_n(defn_ptb2), get_n(defn_ptb4))
  
  ptb_usd2 = as.numeric(df2_usd$ga_del_by_us_date) < 37
  ptb_usd4 = as.numeric(df4_usd$ga_del_by_us_date) < 37
  ptb_usd = c(get_n(ptb_usd2), get_n(ptb_usd4))
  ga_usd = c(get_n(df2_usd$ga_del_by_us_date), get_n(df4_usd$ga_del_by_us_date))
  
  ptb_usd_edd2 = as.numeric(df2_usd$ga_del_us) < 37
  ptb_usd_edd4 = as.numeric(df4_usd$ga_del_us) < 37
  ptb_usd_edd = c(get_n(ptb_usd_edd2), get_n(ptb_usd_edd4))
  ga_usd_edd = c(get_n(df2_usd$ga_del_by_us_date), get_n(df4_usd$ga_del_by_us_date))
  
  
  ## edd variable
  ga_rec_edd = c(get_n(df2_edd$ga_weeks_recorded), get_n(df4_edd$ga_weeks_recorded))
  ptb_rec_edd = c(get_n(df2_edd$ga_weeks_recorded < 37), get_n(df4_edd$ga_weeks_recorded < 37))
  
  
  ptb_edd2 = as.numeric(df2_edd$ga_del_us) < 37
  ptb_edd4 = as.numeric(df4_edd$ga_del_us) < 37
  ptb_edd = c(get_n(ptb_edd2), get_n(ptb_edd4))
  ga_edd = c(get_n(df2_edd$ga_del_us), get_n(df4_edd$ga_del_us))
  
  
  ptb_edd_usd2 = as.numeric(df2_edd$ga_del_by_us_date) < 37
  ptb_edd_usd4 = as.numeric(df4_edd$ga_del_by_us_date) < 37
  ptb_edd_usd = c(get_n(ptb_edd_usd2), get_n(ptb_edd_usd4))
  ga_edd_usd = c(get_n(df2_edd$ga_del_by_us_date), get_n(df4_edd$ga_del_by_us_date))
  
  ##gestatdelbylmp
  ##ptbcalc
  ##ptbcalc2
  ##ptbcalc3
  ##ptbcalc_byLMP
  ##ptbcalc_US
  all_ptb2 = us_2$ga_weeks_recorded < 37 & us_2$weight >=2500 & us_2$weight <=2999 | us_2$weight < 2500
  all_ptb4 = us_4$ga_weeks_recorded < 37 & us_4$weight >=2500 & us_4$weight <=2999 | us_4$weight < 2500
  #ptb_all2 = as.numeric(as.character(us_2$ga_weeks_recorded)) < 37
  #ptb_all4 = as.numeric(as.character(us_4$ga_weeks_recorded)) < 37
  ptb_all = c(get_n(all_ptb2), get_n(all_ptb4))
  
  ga_all2 = get_n(as.numeric(as.character(us_2$ga_weeks_recorded)))
  ga_all4 = get_n(as.numeric(as.character(us_4$ga_weeks_recorded)))
  ga_all = c(ga_all2, ga_all4)
  
  # sum(is.na(df2_ptb))
  # ptb_usd_edd, ptb_edd, ptb_edd_usd,
  #ga_usd_edd, ga_edd, ga_edd_usd,
  # ptb_rec_usd, ptb_rec_edd,
  # ga_rec_usd, ga_rec_edd,
  out_ptb = (data.frame(rbind(ptb_usd, ptb_all)))
  out_ga = data.frame(rbind(ga_usd, ga_all))
  names(out_ptb) = c("std. anc", "grp anc")
  names(out_ga) = c("std. anc", "grp_anc")
  out_ptb
  out_ga
  return(out)
  

}