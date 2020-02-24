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

  df$anc_ga_lt_24_wks = df$anc1ga_anc <= 24 
  summary(df$anc_ga_lt_24_wks)
  ## subset ##
  elig_enroll = df %>% filter(anc_ga_lt_24_wks == TRUE)
  elig_enroll = elig_enroll %>% filter(NumberANCs >= 2)
  dim(elig_enroll)

  
  if (type == "elig_enroll") {
    return(elig_enroll)
  }
  

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

  
  df2_edd = us_2 %>% filter(ga_del_us >= 10 & ga_del_us <= 44)
  df4_edd = us_4 %>% filter(ga_del_us >= 10 & ga_del_us <= 44)

  ##this provides bigger gap for using ga_del_by_us_date
  df2_usd = us_2 %>% filter(ga_del_by_us_date >= 10 & ga_del_by_us_date <= 43)
  df4_usd = us_4 %>% filter(ga_del_by_us_date >= 10 & ga_del_by_us_date <= 43)
  

  
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
  

  all_ptb2 = us_2$ga_weeks_recorded < 37 & us_2$weight >=2500 & us_2$weight <=2999 | us_2$weight < 2500
  all_ptb4 = us_4$ga_weeks_recorded < 37 & us_4$weight >=2500 & us_4$weight <=2999 | us_4$weight < 2500
  #ptb_all2 = as.numeric(as.character(us_2$ga_weeks_recorded)) < 37
  #ptb_all4 = as.numeric(as.character(us_4$ga_weeks_recorded)) < 37
  ptb_all = c(get_n(all_ptb2), get_n(all_ptb4))
  
  ga_all2 = get_n(as.numeric(as.character(us_2$ga_weeks_recorded)))
  ga_all4 = get_n(as.numeric(as.character(us_4$ga_weeks_recorded)))
  ga_all = c(ga_all2, ga_all4)
  
 
  out_ptb = (data.frame(rbind(ptb_usd, ptb_all)))
  out_ga = data.frame(rbind(ga_usd, ga_all))
  names(out_ptb) = c("std. anc", "grp anc")
  names(out_ga) = c("std. anc", "grp_anc")
  return(out)
  
}