anc4 = function(df, day, lookup) {
  df[df == "#NULL!" | df == " " ] <- NA
  anc5 = df %>% filter(is.na(date_ancfuvst_2e) &
                         !is.na(date_ancfuvst_2d) & (is.na(date_ancfuvst_2c) |
                                                       is.na(date_ancfuvst_2b) | is.na(date_ancfuvst_2) | is.na(anc1date_anc)) )

  anc4 = df %>% filter(is.na(date_ancfuvst_2d) & 
                         !is.na(date_ancfuvst_2c) & (is.na(date_ancfuvst_2b) | is.na(date_ancfuvst_2) | is.na(anc1date_anc)) )
  return(c(nrow(anc4), nrow(anc5)))
}

event_compl = function(df, day, lookup) {
  
  df[df == "#NULL!" | df == " " ] <- NA
  #facilities = split(df, df[, sprintf('study_id_dhc.enrollment_arm_%s', arm)])
  #df$edd.asdatae = as.Date(df$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%Y-%m-%d"))
  
  #anc_index = df[, sprintf('anc1ga_anc.anc1_visit_arm_%s', arm)]
  
  #facilities = split(df, df$dhc)
    j = df
    mothers_nm = j$m_name
    study_id_full = j$study_id_full_new
    study_id_anc = j$study_id_anc
    record_id = j$record_id
    

    anc_var = sprintf('anc1ga_anc')
    #df = df[anc_index <= 24, ]
    
    #edd.asdate = as.Date(df[,sprintf('edd_anc.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d")
    #anc1_date_a1 = sapply(x, function(j) sum(!is.na(j$date_delivery_mat.delivery_arm_4))/nrow(j))
    enroll_compl = j$enrollment_complete == 2
    
    anc1_date_compl = fix_date(j$anc1date_anc)

    anc1_age_compl = (j$m_age)
    edd_at_anc1 = fix_date(j$edd_anc)
  
    edd.asdate = fix_date(j$edd_anc)

    #edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%m/%d/%y"))
    ancfuvst_date = j[, grep('date_ancfuvst', names(j), value = T)]
    index_keep1 = sapply(ancfuvst_date, function(x) (mean(!is.na(x))))
    #ancfuvst_date = (apply(ancfuvst_date[, index_keep1 > 0], 2, fix_date))
    ancfuvst_date = do.call(data.frame, lapply(ancfuvst_date[, index_keep1 > 0], fix_date))
    #t = do.call(data.frame, ancfuvst_date)
    
    datefile_fuvst = j[, grep('datefile_fuvst', names(j), value = T)]
    index_keep2 = sapply(datefile_fuvst, function(x) (mean(!is.na(x))))
    datefile_fuvst = do.call(data.frame, lapply(datefile_fuvst[, index_keep2 > 0], fix_date))

    edd_expected = edd.asdate < day
    
    #delivered_a1 = lapply(x, function(j) sum(j$del_ancreg.anc_followup_visit_arm_1 == 1, na.rm = T))
    delivered = fix_date(j$date_delivery_mat)
    
    
    #ancpnc1 = lapply(x, function(j) sum(j$ancpnc_file_followup_visits_complete.anc_followup_visit_arm_1 == 2, na.rm = T))
    
    if(mean(is.na(j$date_pnc4)) == 1) {
      
      date_pnc = (j$date_pnc4)
      
    } else {
    
      date_pnc = fix_date(j$date_pnc4)
    }

  
    mo_yr_enrolled = format.Date(anc1_date_compl, "%m-%Y")
    mo_yr_delivered = format.Date(delivered, "%m-%Y")
    
    compl_2_anc = !is.na(j$date_ancfuvst_2) | !is.na(j$date_ancfuvst_2b) | !is.na(j$datefile_fuvst_2) | !is.na(j$datefile_fuvst_2b)
    #this used to be 'num_complete_anc2_lt_24_wks_del'
    is_eligible_delivered = j$anc1ga_anc < 24 & compl_2_anc & !is.na(j$date_delivery_mat)
    elig_anc2_lt_24_wks = j$anc1ga_anc < 24 & compl_2_anc
    
    
    dates_to_check = data.frame(anc1_date_compl, datefile_fuvst, ancfuvst_date, delivered, date_pnc)
    check_dates = apply(dates_to_check, 2, function(x) date_eligibility(x, day))
    #dates_to_check = dates_to_check[rowSums(check_dates, na.rm = T) > 0,]
    
    # head(dates_to_check)
    # head(anc1_date_compl)
    # head(datefile_fuvst)
    # head(ancfuvst_date)
    
    out = cbind(record_id, study_id_full, study_id_anc, mothers_nm, enroll_compl, 
                anc1_date_compl, anc1_age_compl, 
                datefile_fuvst, ancfuvst_date,
                edd_at_anc1, edd_expected,
                delivered, date_pnc, elig_anc2_lt_24_wks, is_eligible_delivered,
                mo_yr_enrolled, mo_yr_delivered)
    out = out[rowSums(check_dates, na.rm = T) > 0 & is_eligible_delivered, ]
    
    if(!is.null(lookup)) {
      
      out = out[out$study_id_full %in% lookup, ]
                                                                                                   
    }
    
    ###t this is for tabulations by facility whereas if we want to simply output all data for certain criteria,
    ##we don't tabulate Feb 21st
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
