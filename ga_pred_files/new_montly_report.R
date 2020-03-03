library(parsedate)

fix_date = function(date) {
  
  ancdate_missing = as.Date(rep(NA, length(date)))
  
  ancdate = parse_date(as.character(date)[!is.na(date)])
  class(ancdate)
  ancdate = as.Date(ancdate, format = "%Y-%m-%d")
  ancdate_missing[!is.na(date)] <- ancdate
  
  return(ancdate_missing)
  
}

get_expected = function(x, dt) {
  
  #out = subset(x, select = c(startdate, endate))
  #dim(out[which(out$startdate != out$endate | is.na(out$endate) | is.na(out$startdate)),])
  x$startdate = fix_date(x$anc1date_anc)
  x$endate = fix_date(x$enroll_date)
  
  try(sum(!is.na(x$endate)) >= sum(!is.na(x$startdate)))
  x$ga = x$anc1ga_anc
  summary(x$ga)
  x$study_id_dhc = as.numeric(x$study_id_dhc) / 1000000
  #by_pat = split(x, x$record_id)
  
  en_date_bypat = x$endate
  ga_bypat = x$anc1ga_anc
  
  # 
  # get_date = sapply(en_date_bypat, getmin_date, simplify = FALSE)
  # get_ga = sapply(ga_bypat, getmin_date)
  
  if (sum(is.na(en_date_bypat)) > sum(is.na(x$startdate)) ) {
    
    get_date = x$startdate
    
  } else {
    get_date = en_date_bypat
    
  }
    
  get_ga = x$ga

  #ga = rep(NA, length(get_ga))
  # 
  # class(gdt) <- "Date"
  # for (j in 1:length(get_date)) {
  #   if (!is.na(get_date[[j]])) {
  #     gdt[j] = get_date[[j]]
  #     
  #   }
  #   
  # }
  # 
  get_date[get_date > Sys.Date()] <- NA
  get_date[get_date < "2015-01-01"] <- NA

  
  ##only thing needed is GA on first visit to know when eligible for visit
  #get this by mindate on x (more general) or do by arm using new_frame
  
  ##anc2 should only be from 20-28weeks, anc3 should only be from 28-36 weeks, anc4 should be 36wk+
  ga_days = get_ga * 7
  #56 dayas
  
  #dt = "2018-01-01"
  
  ga2 = (ga_days + 56) 
  ga3 = (ga_days + 112) 
  ga4 = (ga_days + 168) 
  
  date2 = get_date + 56
  date3 = get_date + 112
  date4 = get_date + 168
  
  expected2 = (date2 < dt & ga2 <= 196)
  expected3 = (date2 < dt & ga2 > 196 & ga2 <= 252) | (date3 < dt & ga3 > 196 & ga3 <= 252)
  expected4 = (date2 < dt & ga2 > 252 & ga2 <= 280) | (date3 < dt & ga3 > 252 & ga3 <= 280) | 
    (date4 < dt & ga4 > 252 & ga4 <= 280)
  #advance to anc 2 if 1. past that date 2. if ga wks in appropriate range 
  # length(by_pat)
  # length(expected2)
  # ids_pat = (unique(main_frame$record_id))
  
  #new_frame$expect4 = expect4  
  
  x$expected2 = expected2
  x$expected3 = expected3
  x$expected4 = expected4
  
  return(x)
  
}


make_table = function(df, arm, day) {
  
  df[df == "#NULL!" | df == " " ] <- NA
  #facilities = split(df, df[, sprintf('study_id_dhc.enrollment_arm_%s', arm)])
  #df$edd.asdatae = as.Date(df$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%Y-%m-%d"))
  
  #anc_index = df[, sprintf('anc1ga_anc.anc1_visit_arm_%s', arm)]
  
  #facilities = split(df, df$dhc)
  j = get_expected(df, day)
  pct_different_ids = mean(is.na(j$DifferentIDs) | j$DifferentIDs == 1)
  j = j[!is.na(j$DifferentIDs) & j$DifferentIDs == 0, ]
  num.enroll = nrow(j)
  
  if (arm == 1) {
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
    #edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%m/%d/%y"))
    ancfuvst_date = j[, grep('date_ancfuvst', names(j), value = T)]
    lapply(ancfuvst_date, function(x) (mean(!is.na(x))))
    
    datefile_fuvst = j[, grep('datefile_fuvst', names(j), value = T)]
    
    ancfuvst_ga = j[, grep('ga_anc_fuvst', names(j), value = T)]
    
    #lapply(ancfuvst_ga, function(x) (mean(!is.na(x))))
    
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
 
    out = rbind(num.enroll, pct_different_ids, pct_redcap_2_is_anc1, pct_redcap_2_is_na, anc1_enroll_compl_a1, anc1_date_compl_a1, 
                anc1_ga_compl_a1, anc1_lmp_compl_a1, anc1_age_compl_a1, anc1_edd_compl_a1,
                actual2, actual3, actual4,  
                expected2_a1, expected3_a1, expected4_a1, delivered_a1, anc1_edd_expect_arm1, date_pnc,
                expected_pnc_1,
                num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
                num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
    
    out = round(out, 3)
    # out = rbind(num.enroll, anc1_enroll_compl_a1, anc1_date_compl_a1, 
    #             anc1_ga_compl_a1, anc1_lmp_compl_a1, anc1_age_compl_a1, anc1_edd_compl_a1,
    #             actual2_a1, actual3_a1, actual4_a1, 
    #             expected2_a1, expected3_a1, expected4_a1, delivered_a1, anc1_edd_expect_arm1, date_pnc,
    #             expected_pnc_1,
    #             num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
    #             num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
    # 


  }
  output = out

  #id_arm$HC[id_arm$DHC %in% colnames(output)]
  return(output)
}


# 
# make_table = function(df, arm, day) {
#   
#   #facilities = split(df, df[, sprintf('study_id_dhc.enrollment_arm_%s', arm)])
#   #df$edd.asdatae = as.Date(df$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%Y-%m-%d"))
#   
#   #anc_index = df[, sprintf('anc1ga_anc.anc1_visit_arm_%s', arm)]
#   anc_var = sprintf('anc1ga_anc.anc1_visit_arm_%s', arm)
#   #df = df[anc_index <= 24, ]
#   
#   df$edd.asdate = as.Date(df[,sprintf('edd_anc.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d")
#   #facilities = split(df, df$dhc)
#   
#   x = facilities
#   
#   num.enroll = sapply(x, nrow)
#   if (arm == 1) {
#     
#     #anc1_date_a1 = sapply(x, function(j) sum(!is.na(j$date_delivery_mat.delivery_arm_4))/nrow(j))
#     anc1_enroll_compl_a1 = sapply(x, function(j) sum(j$enrollment_complete.enrollment_arm_1 == 2, na.rm = T)/nrow(j))
#     
#     anc1_date_compl_a1 = sapply(x, function(j) sum(!is.na(j$anc1date_anc.anc1_visit_arm_1))/nrow(j))
#     anc1_ga_compl_a1 = sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_1))/nrow(j))
#     anc1_lmp_compl_a1 = sapply(x, function(j) sum(!is.na(j$lmp_anc.anc1_visit_arm_1))/nrow(j))
#     anc1_age_compl_a1 = sapply(x, function(j) sum(!is.na(j$m_age.enrollment_arm_1))/nrow(j))
#     anc1_edd_compl_a1 = sapply(x, function(j) sum(!is.na(j$edd_anc.anc1_visit_arm_1))/nrow(j))
#     
#     edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%Y-%m-%d"))
#     #edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_1[!is.na(j$edd_anc.anc1_visit_arm_1)], format = "%m/%d/%y"))
#     
#     
#     actual2_a1 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.anc_followup_visit_arm_1) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     actual3_a1 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.anc_followup_visit_arm_1b) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     
#     actual4_a1 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.anc_followup_visit_arm_1c) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     #actual4_a2 = sapply(x, function(j) sum(!is.na(j$anc1_file_vstdate.anc_followup_visit_arm_1d)))
# 
#     # expected2_a1 = sapply(x, function(j) sum(!is.na(j$expected2) & j$expected2 < day))
#     # expected3_a1 = sapply(x, function(j) sum(!is.na(j$expected3) & j$expected3 < day))
#     # expected4_a1 = sapply(x, function(j) sum(!is.na(j$expected4) & j$expected4 < day))
#     # 
#     expected2_a1 = sapply(x, function(j) sum(j$expected2 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     expected3_a1 = sapply(x, function(j) sum(j$expected3 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     expected4_a1 = sapply(x, function(j) sum(j$expected4 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     
#     anc1_edd_expect_arm1 = lapply(edd.asdate, function(x) sum(x < day))
#     
#     #delivered_a1 = lapply(x, function(j) sum(j$del_ancreg.anc_followup_visit_arm_1 == 1, na.rm = T))
#     delivered_a1 = sapply(x, function(j) sum(!is.na(j$date_delivery_mat.delivery_arm_1)))
#     
#     
#     #ancpnc1 = lapply(x, function(j) sum(j$ancpnc_file_followup_visits_complete.anc_followup_visit_arm_1 == 2, na.rm = T))
#     
#     expected_pnc_1 = lapply(edd.asdate, function(x) sum((x + 56) < day))
#     date_pnc = sapply(x, function(j) sum(!is.na(j$date_pnc4.pnc_visit_arm_1)))
#     
#     edd.asdate_all = as.Date(df$edd_anc.anc1_visit_arm_1, format = "%Y-%m-%d")
#     pnc_ids = df$study_id_full.enrollment_arm_1[(!is.na(edd.asdate_all) & edd.asdate_all + 56 < day)]
#     
#     
#     num_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_1 < 24, na.rm = T))
#     
#     
#     pct_lt_24_wks = num_lt_24_wks / sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_1)))
#     
#     
#     #num_complete_anc2_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_1 < 24 & !is.na(j$anc_rf_fuvst.anc_followup_visit_arm_1), na.rm = T))
#     num_complete_anc2_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_1 < 24 & 
#                                                               !is.na(j$date_ancfuvst.anc_followup_visit_arm_1), na.rm = T))
#     
#     # delivered_a1 
#     # 
#     pct_complete_anc2_lt_24_wks = num_complete_anc2_lt_24_wks / num_lt_24_wks
#     # 
#     num_expected_anc2_lt_24_wks = sapply(x, function(j) sum(j$expected2 == T & j$anc1ga_anc.anc1_visit_arm_1 < 24, na.rm = T))
#     # 
#     # 
#     
#     
#     num_complete_anc2_lt_24_wks_del = lapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_1 < 24 & !is.na(j$date_ancfuvst.anc_followup_visit_arm_1)
#                                                                                   & !is.na(j$date_delivery_mat.delivery_arm_1), na.rm = T))
#     
#     num_expected_anc2_lt_24_wks_del = sapply(x, function(j) sum(j$expected2 == T & (j$edd.asdate < day) & 
#                                                                   j$anc1ga_anc.anc1_visit_arm_1 < 24, na.rm = T))
#     
# 
#     # 
#     num.eligible.monthly = c(66, 57, 48, 56, 65, 74, 95, 77, 52)
#     
#     out = rbind(num.enroll, anc1_enroll_compl_a1, anc1_date_compl_a1, 
#                 anc1_ga_compl_a1, anc1_lmp_compl_a1, anc1_age_compl_a1, anc1_edd_compl_a1,
#                 actual2_a1, actual3_a1, actual4_a1, 
#                 expected2_a1, expected3_a1, expected4_a1, delivered_a1, anc1_edd_expect_arm1, date_pnc,
#                 expected_pnc_1,
#                 num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
#                 num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
#     
#     colnames(out) = c("Gaku", "Gash", "Ruhu", "Cyan", "Kira", "Nyamasheke", "Coru", "Mura", "Nyak") 
#     
#     ############## VISIT  2 ###########
#     dim(x[[2]])
#     #sapply(x, function(j) sum(!is.na(j$anc_register_followup_visits_complete.anc_followup_visit_arm_1))/ nrow(j))
#     
#     #anc2_date_a1 = sapply(x, function(j) sum(!is.na(j$ga_anc_fuvst.ganc2_visit_arm_3))/nrow(j))
#     
#     # anc2_date_a1 = sapply(x, function(j) sum(!is.na(j$ga_anc_fuvst.anc_followup_visit_arm_1))/nrow(j))
#     # 
#     # anc2_ga_a1 = sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_1))/nrow(j))
#     # 
#     # anc2_date_a1 = sapply(x, function(j) sum(!is.na(j$arm1))/nrow(j))
#     # anc2_ga_a1 = sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_1))/nrow(j))
#     # 
#     # anc3_date_a1 = sapply(x, function(j) sum(!is.na(j$anc1_file_vstdate.anc1_visit_arm_1))/nrow(j))
#     # anc3_ga_a1 = sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_1))/nrow(j))
#     # 
#     # anc4_date_a1 = sapply(x, function(j) sum(!is.na(j$anc1_file_vstdate.anc1_visit_arm_1))/nrow(j))
#     # anc4_ga_a1 = sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_1))/nrow(j))
#     
#     
#     
#   } else if (arm == 2) {
#     #anc1_date_a2 = sapply(x, function(j) sum(!is.na(j$anc1_file_vstdate.anc1_visit_arm_2))/nrow(j))
#     #anc1_ga_a2 = sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_2))/nrow(j))
#     
#     
#     anc1_enroll_compl_a2 = sapply(x, function(j) sum(j$enrollment_complete.enrollment_arm_2 == 2, na.rm = T)/nrow(j))
#     
#     anc1_date_compl_a2 = sapply(x, function(j) sum(!is.na(j$anc1date_anc.anc1_visit_arm_2))/nrow(j))
#     anc1_ga_compl_a2 = sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_2))/nrow(j))
#     anc1_lmp_compl_a2 = sapply(x, function(j) sum(!is.na(j$lmp_anc.anc1_visit_arm_2))/nrow(j))
#     anc1_age_compl_a2 = sapply(x, function(j) sum(!is.na(j$m_age.enrollment_arm_2))/nrow(j))
#     anc1_edd_compl_a2 = sapply(x, function(j) sum(!is.na(j$edd_anc.anc1_visit_arm_2))/nrow(j))
#     
#     edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_2[!is.na(j$edd_anc.anc1_visit_arm_2)], format = "%Y-%m-%d"))
#     
#     edd.asdate_all = as.Date(df$edd_anc.anc1_visit_arm_2, format = "%Y-%m-%d")
#     pnc_ids = df$study_id_full.enrollment_arm_2[(!is.na(edd.asdate_all) & edd.asdate_all + 56 < day)]
#     
#     
#     expected_pnc_2 = lapply(edd.asdate, function(x) sum((x + 56) < day))
#     #edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_2[!is.na(j$edd_anc.anc1_visit_arm_2)], format = "%m/%d/%y"))
#     
#     actual2_a2 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.anc_followup_visit_arm_2) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     #sum(!s.na(arms[[2]]$date_ancfuvst.anc_followup_visit_arm_2))
#     actual3_a2 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.anc_followup_visit_arm_2b) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     actual4_a2 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.anc_followup_visit_arm_2c) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     
#     actual5_a2 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.anc_followup_visit_arm_2d) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     #actual4_a2 = sapply(x, function(j) sum(!is.na(j$anc1_file_vstdate.anc_followup_visit_arm_1d)))
#     
#     
#     
#     anc1_edd_expect_arm2 = sapply(edd.asdate, function(x) sum(x < day))
#     
#     delivered_a2 = lapply(x, function(j) sum(!is.na(j$date_delivery_mat.delivery_arm_2)))
#     
#     ancpnc2 = lapply(x, function(j) sum(j$ancpnc_file_followup_visits_complete.anc_followup_visit_arm_2 == 2, na.rm = T))
#     
#     
#     
#     # expected2_a2 = sapply(x, function(j) sum(!is.na(j$expected2) & j$expected2 < day))
#     # expected3_a2 = sapply(x, function(j) sum(!is.na(j$expected3) & j$expected3 < day))
#     # expected4_a2 = sapply(x, function(j) sum(!is.na(j$expected4) & j$expected4 < day))
#     
#     expected2_a2 = sapply(x, function(j) sum(j$expected2 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     expected3_a2 = sapply(x, function(j) sum(j$expected3 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     expected4_a2 = sapply(x, function(j) sum(j$expected4 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     date_pnc2 = sapply(x, function(j) sum(!is.na(j$date_pnc4.pnc_visit_arm_2)))
#     
#     num.eligible.monthly = c(37, 69, 34, 44, 50, 71, 103, 114, 69)
#     
#     num_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_2 < 24, na.rm = T))
#     
#     pct_lt_24_wks = num_lt_24_wks / sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_2)))
#     
#     
#     #num_complete_anc2_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_2 < 24 & !is.na(j$anc_rf_fuvst.anc_followup_visit_arm_2), na.rm = T))
#     num_complete_anc2_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_2 < 24 & 
#                                                               !is.na(j$date_ancfuvst.anc_followup_visit_arm_2), na.rm = T))
#     
#     
#     
#     pct_complete_anc2_lt_24_wks = num_complete_anc2_lt_24_wks / num_lt_24_wks
#     
#     num_expected_anc2_lt_24_wks = sapply(x, function(j) sum(j$expected2 == T & j$anc1ga_anc.anc1_visit_arm_2 < 24, na.rm = T))
#     
# 
#     
#     
#     num_complete_anc2_lt_24_wks_del = lapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_2 < 24 & !is.na(j$date_ancfuvst.anc_followup_visit_arm_2)
#                                                                 & !is.na(j$date_delivery_mat.delivery_arm_2), na.rm = T))
#     
#     num_expected_anc2_lt_24_wks_del = sapply(x, function(j) sum(j$expected2 == T & (j$edd.asdate < day) & 
#                                                                   j$anc1ga_anc.anc1_visit_arm_2 < 24, na.rm = T))
#     
#     ################################ ga + 56 days is < date, 
#     
#     
#     out = rbind(num.enroll, anc1_enroll_compl_a2, anc1_date_compl_a2, anc1_ga_compl_a2, anc1_lmp_compl_a2, 
#                 anc1_age_compl_a2, 
#                 anc1_edd_compl_a2, actual2_a2, actual3_a2, actual4_a2, actual5_a2,
#                 expected2_a2, expected3_a2, expected4_a2, delivered_a2, anc1_edd_expect_arm2, 
#                 expected_pnc_2, date_pnc2,
#                 num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
#                 num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
#     
#     colnames(out) = c("Kama", "Nger", "Ndon", "Kibi", "Muge", "Mwez", "Biry", "Busa", "Kara")
#     
#   } else if(arm == 3) {
#     
#     anc1_enroll_compl_a3 = sapply(x, function(j) sum(j$enrollment_complete.enrollment_arm_3 == 2, na.rm = T)/nrow(j))
#     
#     anc1_date_compl_a3 = sapply(x, function(j) sum(!is.na(j$anc1date_anc.anc1_visit_arm_3))/nrow(j))
#     anc1_ga_compl_a3 = sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_3))/nrow(j))
#     anc1_lmp_compl_a3 = sapply(x, function(j) sum(!is.na(j$lmp_anc.anc1_visit_arm_3))/nrow(j))
#     anc1_age_compl_a3 = sapply(x, function(j) sum(!is.na(j$m_age.enrollment_arm_3))/nrow(j))
#     anc1_edd_compl_a3 = sapply(x, function(j) sum(!is.na(j$edd_anc.anc1_visit_arm_3))/nrow(j))
#     
#     edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_3[!is.na(j$edd_anc.anc1_visit_arm_3)], format = "%Y-%m-%d"))
#     
#     
#     expected_pnc_3 = lapply(edd.asdate, function(x) sum((x + 56) < day))
#     
#     edd.asdate_all = as.Date(df$edd_anc.anc1_visit_arm_3, format = "%Y-%m-%d")
#     pnc_ids = df$study_id_full.enrollment_arm_3[(!is.na(edd.asdate_all) & edd.asdate_all + 56 < day)]
#     
#     #edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_3[!is.na(j$edd_anc.anc1_visit_arm_3)], format = "%m/%d/%y"))
#     anc1_edd_expect_arm3 = sapply(edd.asdate, function(x) sum(x < day))
#     
#     delivered_a3 = lapply(x, function(j) sum(j$del_ancreg.ganc2_visit_arm_3 == 1, na.rm = T))
#     
#     delivered_a3 = lapply(x, function(j) sum(!is.na(j$date_delivery_mat.delivery_arm_3)))
#     
#     
#     ancpnc3 = lapply(x, function(j) sum(j$ancpnc_file_followup_visits_complete.anc_followup_visit_arm_3 == 2, na.rm = T))
#     date_pnc3 = sapply(x, function(j) sum(!is.na(j$date_pnc4.gpnc_visit_arm_3)))
#     
#     
#     #### VISIT 2 ###
#     #anc2_enroll_comp_a3 = sapply(x, function(j) sum(j$ga_anc_fuvst.ganc2_visit_arm_3 == 2, na.rm = T)/nrow(j))
#     anc2_a3 = sapply(x, function(j) sum(!is.na(j$anc_register_followup_visits_complete.ganc2_visit_arm_3))/nrow(j))
#     
#     summary(df$date_ancfuvst.ganc2_visit_arm_3)
#     summary(df$date_ancfuvst.ganc3_visit_arm_3)
#     summary(df$date_ancfuvst.ganc4_visit_arm_3)
#     
#     #### VISIT 2 ####
#     
#     
#     #### VISIT 3 ###
#     anc3_a3 = sapply(x, function(j) sum(!is.na(j$anc_register_followup_visits_complete.ganc3_visit_arm_3))/nrow(j))
#     #anc2_enroll_comp_a3 = sapply(x, function(j) sum(j$ga_anc_fuvst.ganc2_visit_arm_3 == 2, na.rm = T)/nrow(j))
#     
#     
#     #### VISIT 3 ####
#     
#     #### VISIT 4 ###
#     anc4_a3 = sapply(x, function(j) sum(!is.na(j$anc_register_followup_visits_complete.ganc4_visit_arm_3))/nrow(j))
#     
#     actual2_a3 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.ganc2_visit_arm_3) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     
#     actual3_a3 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.ganc3_visit_arm_3) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     
#     actual4_a3 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.ganc4_visit_arm_3) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     
#     
#     expected2_a3 = sapply(x, function(j) sum(j$expected2 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     expected3_a3 = sapply(x, function(j) sum(j$expected3 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     expected4_a3 = sapply(x, function(j) sum(j$expected4 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     
#     # summary(j$anc_register_followup_visits_complete.ganc4_visit_arm_3)
#     # 
#     # 
#     #### VISIT 4 ####
#     
#     
#     num.eligible.monthly = c(58, 64, 132, 45, 53, 60, 89, 177, 68)
#     
#     num_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_3 < 24, na.rm = T))
#     
#     pct_lt_24_wks = num_lt_24_wks / sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_3)))
#     
#     
#     num_complete_anc2_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_3 < 24 & !is.na(j$date_ancfuvst.ganc2_visit_arm_3), na.rm = T))
#     pct_complete_anc2_lt_24_wks = num_complete_anc2_lt_24_wks / num_lt_24_wks
#     
#     
#     num_expected_anc2_lt_24_wks = sapply(x, function(j) sum(j$expected2 == T & j$anc1ga_anc.anc1_visit_arm_3 < 24, na.rm = T))
#     
#     
#     num_complete_anc2_lt_24_wks_del = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_3 < 24 & 
#                                                                   !is.na(j$date_ancfuvst.ganc2_visit_arm_3) & 
#                                                                            !is.na(j$date_delivery_mat.delivery_arm_3), na.rm = T))
#     
#     num_expected_anc2_lt_24_wks_del = sapply(x, function(j) sum(j$expected2 == T & (j$edd.asdate < day) & 
#                                                                   j$anc1ga_anc.anc1_visit_arm_3 < 24, na.rm = T))
# 
#     out = rbind(num.enroll, anc1_enroll_compl_a3, anc1_date_compl_a3, anc1_ga_compl_a3, anc1_lmp_compl_a3, 
#                 anc1_age_compl_a3, anc1_edd_compl_a3, anc2_a3, anc3_a3, anc4_a3,
#                 actual2_a3, expected2_a3, actual3_a3, expected3_a3, actual4_a3, expected4_a3, 
#                 delivered_a3, anc1_edd_expect_arm3, date_pnc3, expected_pnc_3, 
#                 num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
#                 num_complete_anc2_lt_24_wks_del,  num_expected_anc2_lt_24_wks_del)
#     
#     colnames(out) =  c("Juru", "Ntar", "Nyamata", "Gita", "Kare", "Kibo", "Ruga", "Gise", "Kigu")
#     
#   } else {
#     
#     anc1_enroll_compl_a4 = sapply(x, function(j) sum(j$enrollment_complete.enrollment_arm_4 == 2, na.rm = T)/nrow(j))
#     
#     anc1_date_compl_a4 = sapply(x, function(j) sum(!is.na(j$anc1date_anc.anc1_visit_arm_4))/nrow(j))
#     anc1_ga_compl_a4 = sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_4))/nrow(j))
#     anc1_lmp_compl_a4 = sapply(x, function(j) sum(!is.na(j$lmp_anc.anc1_visit_arm_4))/nrow(j))
#     anc1_age_compl_a4 = sapply(x, function(j) sum(!is.na(j$m_age.enrollment_arm_4))/nrow(j))
#     anc1_edd_compl_a4 = sapply(x, function(j) sum(!is.na(j$edd_anc.anc1_visit_arm_4))/nrow(j))
#     
#     edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_4[!is.na(j$edd_anc.anc1_visit_arm_4)], format = "%Y-%m-%d"))
#     
#     expected_pnc_4 = lapply(edd.asdate, function(x) sum((x + 56) < day))
#     
#     edd.asdate_all = as.Date(df$edd_anc.anc1_visit_arm_4, format = "%Y-%m-%d")
#     pnc_ids = df$study_id_full.enrollment_arm_4[(!is.na(edd.asdate_all) & edd.asdate_all + 56 < day)]
#     
#     #delivered_a4 = lapply(x, function(j) sum(j$del_ancreg.ganc2_visit_arm_4 == 1, na.rm = T))
#     delivered_a4 = sapply(x, function(j) sum(!is.na(j$date_delivery_mat.delivery_arm_4)))
#     
#     
#     ancpnc4 = lapply(x, function(j) sum(j$ancpnc_file_followup_visits_complete.anc_followup_visit_arm_4 == 2, na.rm = T))
#     date_pnc4 = sapply(x, function(j) sum(!is.na(j$date_pnc4.gpnc_visit_arm_4)))
#     
#     
#     #edd.asdate = lapply(x, function(j) as.Date(j$edd_anc.anc1_visit_arm_4[!is.na(j$edd_anc.anc1_visit_arm_4)], format = "%m/%d/%y"))
#     
#     #edd, date of delivery,  follow-up PNC dates/visits (expected and actual),
#     
#     anc1_edd_expect_arm4 = sapply(edd.asdate, function(x) sum(x < day))
#     
#     ### VISIT 2 ####
#     anc2_a4 = sapply(x, function(j) sum(!is.na(j$anc_register_followup_visits_complete.ganc2_visit_arm_4))/nrow(j))
#     
#     anc3_a4 = sapply(x, function(j) sum(!is.na(j$anc_register_followup_visits_complete.ganc3_visit_arm_4))/nrow(j))
#     
#     anc4_a4 = sapply(x, function(j) sum(!is.na(j$anc_register_followup_visits_complete.ganc_4_visit_arm_4))/nrow(j))
#     
#     
#     
#     actual2_a4 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.ganc2_visit_arm_4) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     
#     actual3_a4 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.ganc3_visit_arm_4) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     
#     actual4_a4 = sapply(x, function(j) sum(!is.na(j$date_ancfuvst.ganc_4_visit_arm_4) & !is.na(j[,anc_var]) & j[,anc_var] <= 24))
#     
#     
#     # expected2_a4 = sapply(x, function(j) sum(!is.na(j$expected2) & j$expected2 < day))
#     # expected3_a4 = sapply(x, function(j) sum(!is.na(j$expected3) & j$expected3 < day))
#     # expected4_a4 = sapply(x, function(j) sum(!is.na(j$expected4) & j$expected4 < day))
#     
#     expected2_a4 = sapply(x, function(j) sum(j$expected2 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     expected3_a4 = sapply(x, function(j) sum(j$expected3 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     expected4_a4 = sapply(x, function(j) sum(j$expected4 == T & j[,anc_var] <= 24, na.rm = T))
#     
#     
#     summary(df$date_ancfuvst.ganc3_visit_arm_4)
#   
#     
#     ### VISIT 3 ####
#     
#     num.eligible.monthly = c(118, 43, 69, 51, 40, 54, 42, 64, 63)
#     
#     num_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_4 < 24, na.rm = T))
#     
#     pct_lt_24_wks = num_lt_24_wks / sapply(x, function(j) sum(!is.na(j$anc1ga_anc.anc1_visit_arm_4)))
#     
#     
#     num_complete_anc2_lt_24_wks = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_4 < 24 & !is.na(j$date_ancfuvst.ganc2_visit_arm_4), na.rm = T))
#     
#     
#     pct_complete_anc2_lt_24_wks = num_complete_anc2_lt_24_wks / num_lt_24_wks
#     
#     num_expected_anc2_lt_24_wks = sapply(x, function(j) sum(j$expected2 == T & j$anc1ga_anc.anc1_visit_arm_4 < 24, na.rm = T))
#     
#     
#     num_complete_anc2_lt_24_wks_del = sapply(x, function(j) sum(j$anc1ga_anc.anc1_visit_arm_4 < 24 & 
#                                                                   !is.na(j$date_ancfuvst.ganc2_visit_arm_4) & 
#                                                                   !is.na(j$date_delivery_mat.delivery_arm_4), na.rm = T))
#     
#     num_expected_anc2_lt_24_wks_del = sapply(x, function(j) sum(j$expected2 == T & (j$edd.asdate < day) & 
#                                                                   j$anc1ga_anc.anc1_visit_arm_4 < 24, na.rm = T))
#     
#     
#     
#     out = rbind(num.enroll, anc1_enroll_compl_a4, anc1_date_compl_a4, anc1_ga_compl_a4, 
#                 anc1_lmp_compl_a4, anc1_age_compl_a4, anc1_edd_compl_a4, anc2_a4, anc3_a4, anc4_a4,
#                 actual2_a4, expected2_a4, actual3_a4, expected3_a4, actual4_a4, expected4_a4, delivered_a4, anc1_edd_expect_arm4, 
#                 date_pnc4, expected_pnc_4,
#                 num_lt_24_wks, pct_lt_24_wks, num_complete_anc2_lt_24_wks, pct_complete_anc2_lt_24_wks, num_expected_anc2_lt_24_wks,
#                 num_complete_anc2_lt_24_wks_del, num_expected_anc2_lt_24_wks_del)
#     
#     colnames(out) = c("Maya", "Nyar", "Nzan", "Gahu", "Muca", "Gisa", "Yove", "Kabu", "Mude")
#     
#   }
#   output = list(out, pnc_ids)
#   
#   return(output)
# }




getmin_date = function(x) {
  #dts = x$startdate
  if(sum(is.na(x)) == length(x)) {
    return(NA)
    
  } else {
    out = min(x, na.rm = T)
    return(out)
  }
} 

geten_date = function(x) {
  dts = x$endate
  if(sum(is.na(dts)) == length(dts)) {
    return(NA)
    
  } else {
    out = min(dts, na.rm = T)
    return(out)
  }
} 


##GET FRAME INTO WIDE ##

### HERE IS A PROCESS THAT GIVES NO WARNINGS ###
################################################
#
get_wide_form = function(df, A) {
  
  df[is.na(df) | df == "" ] <- NA
  
  by_id = split(df, df$record_id)
  out_ids = sapply(by_id, function(x) length(table(x$study_id_full)))
  summary(out_ids)
  
  #by_id = lapply(by_id, function(x) x[,-c(1)])
  
  var_cols = lapply(by_id, function(x) apply(x, 2, function(j) length(unique(j)) == 1))
  
  #var_cols_1 = lapply(by_id, function(x) apply(x, 2, function(j) length(unique(j)) == 1 & length(j) > 1 & sum(is.na(j)) == 0))
  
  which_cols = do.call(rbind, var_cols)
  #rowSums(which_cols)
  whichcols = colSums(which_cols)
  
  num_record_ids = length(unique(df$record_id))
  
  length(by_id)
  
  vnames = names(df)[whichcols != num_record_ids]
  head(vnames)
  
  lcl = reshape(df, timevar = "redcap_event_name", v.names = vnames[-1],
                idvar = "record_id", ids = "record_id", direction = "wide")
  
  dim(lcl)
  #one_val = apply(lcl, 2, function(j) length(unique(j)) == 1)
  one_val = apply(lcl, 2, function(j) sum(is.na(j)) != num_record_ids)
  
  #rmcols <- which(sapply(1:ncol(lcl), function(j) length(unique(lcl[,j])))==1)
  #keep = lcl[, one_val]
  #
  # summ0 = apply(keep, 2, function(j) sum(j == 0 | is.na(j)))
  # summary(summ0)
  # summary(keep[,which(summ0 == 4874)])
  #
  # #keep = lcl[, -rmcols]
  #
  # #rmcols <- which(sapply(1:ncol(keep), function(j) length(unique(keep[,j])))==1)
  # #rmcols
  # dim(keep)
  #
  # #View(head(lcl[1:5,one_val]))
  # names(lcl)[(one_val == 3859)]
  #
  # names(lcl)
  # names(whichcols[whichcols == 3859])
  #
  # length(vnames)
  # names(df)[3]
  # names(lcl)[grep("enroll_date", names(lcl), ignore.case = T)]
  # #grep(check_these_fixed[30], names(keep), ignore.case = T)
  # #keep[1, grep(check_these_fixed[9], names(keep), ignore.case = T)]
  #
  # #check_these_fixed = names(whichcols[whichcols == 3859])
  # names(keep)[grep("enroll_date", names(keep), ignore.case = T)]
  #
  
  write.csv(lcl, file = sprintf("clean_wide_%s_%s.csv", A, Sys.Date()))
  #write.csv(keep, file = "clean_wide_format.csv")
  #df = read.csv('clean_wide_format.csv')
  
  ##accrual table across all health centers, 36 sites. code
  ##study_id_dhc
  ## eligible # from separate source
  ##complete data for anc visits
  ##main 1 for dataset is table 1, another datasource,
  
  #return(keep)
  return(lcl)
}

get_id = function(j) {
  
  if (sum(is.na(j)) == length(j)) {
    
    return(j[1])
  } else{
    return(unique(j)[!is.na(unique(j))])
  }
}

get_wide_form = function(df, A) {
  
  df[is.na(df) | df == "" ] <- NA
  
  by_id = split(df, df$record_id)
  #by_id = split(df, df$study_id_full)
  #out = sapply(by_id, function(x) nrow(x) == length(unique(x$redcap_event_name)))
  #out = sapply(by_id, function(x) unique(x$study_id_full)[!is.na(unique(x$study_id_full))])
  #out = sapply(by_id, function(x) get_id(x$redcap_event_name))  
  #out = sapply(by_id, function(x) get_id(x$study_id_full))
  
  ##ids_not_na = out[!is.na(out)]
  ##duplicated_ids = ids_not_na[duplicated(ids_not_na)]
  #duplicated_id_1 = which(sapply(by_id, function(x) (duplicated_ids[1] %in% x$study_id_full)) == T)
  
  
  
  #ids = do.call(c, out)
  
  #by_id = lapply(by_id, function(x) x[,-c(1)])
  
  cols_w_one_val_per_id = lapply(by_id, function(x) apply(x, 2, function(j) length(unique(j)) == 1))
  redcap_names = lapply(by_id, function(x) x$redcap_event_name)
  
  #var_cols_1 = lapply(by_id, function(x) apply(x, 2, function(j) length(unique(j)) == 1 & length(j) > 1 & sum(is.na(j)) == 0))
  
  cols_w_one_val_all = do.call(rbind, cols_w_one_val_per_id)
  #rowSums(which_cols)
  var_in_cols_by_id = colSums(cols_w_one_val_all)
  
  num_record_ids = length(unique(df$record_id))
  #num_study_ids = length(unique(df$study_id_full))
  
  length(by_id)
  
  varying_names = names(df)[var_in_cols_by_id != length(by_id)]
  
  length(varying_names)
  
  lcl = reshape(df, timevar = "redcap_event_name", v.names = varying_names[-c(1)],
                idvar = "record_id", ids = "record_id", direction = "wide")
  
  dim(lcl)
  #one_val = apply(lcl, 2, function(j) length(unique(j)) == 1)
  one_val = apply(lcl, 2, function(j) sum(is.na(j)) != num_record_ids)
  
  #rmcols <- which(sapply(1:ncol(lcl), function(j) length(unique(lcl[,j])))==1)
  #keep = lcl[, one_val]
  # 
  # summ0 = apply(keep, 2, function(j) sum(j == 0 | is.na(j)))
  # summary(summ0)
  # summary(keep[,which(summ0 == 4874)])
  # 
  # #keep = lcl[, -rmcols]
  # 
  # #rmcols <- which(sapply(1:ncol(keep), function(j) length(unique(keep[,j])))==1)
  # #rmcols
  # dim(keep)
  # 
  # #View(head(lcl[1:5,one_val]))
  # names(lcl)[(one_val == 3859)]
  # 
  # names(lcl)
  # names(whichcols[whichcols == 3859])
  # 
  # length(vnames)
  # names(df)[3]
  # names(lcl)[grep("enroll_date", names(lcl), ignore.case = T)]
  # #grep(check_these_fixed[30], names(keep), ignore.case = T)
  # #keep[1, grep(check_these_fixed[9], names(keep), ignore.case = T)]
  # 
  # #check_these_fixed = names(whichcols[whichcols == 3859])
  # names(keep)[grep("enroll_date", names(keep), ignore.case = T)]
  # 
  
  write.csv(lcl, file = sprintf("clean_wide_%s_%s.csv", A, Sys.Date()))
  #write.csv(keep, file = "clean_wide_format.csv")
  #df = read.csv('clean_wide_format.csv')
  
  ##accrual table across all health centers, 36 sites. code 
  ##study_id_dhc
  ## eligible # from separate source
  ##complete data for anc visits
  ##main 1 for dataset is table 1, another datasource, 
  
  #return(keep)
  return(lcl)
}


############################################
######## PROCESS WITH NO WARNINGS ##########

#### functions to fix the counts in each arm ###

run_fac_q = function(df, A) {
  
  facility = split(df, df[, sprintf('study_id_dhc.enrollment_arm_%s', A)])
  
  tab = make_table(facilities, A)
  return(tab)
  
}





fix_enroll = function(df, A) {
  #df[,sprintf("study_id_anc.anc1_visit_arm_%s", A)] =   df[,sprintf("study_id_anc.anc1_visit_arm_%s", A)]/100000
  
  #df$study_id_dhc.enrollment_arm_1 = (df$study_id_dhc.enrollment_arm_1/1000000)
  
  (table(df$study_id_dhc.enrollment_arm_1))
  (table(df$study_id_dhc.enrollment_arm_2))
  (table(df$study_id_dhc.enrollment_arm_3))
  (table(df$study_id_dhc.enrollment_arm_4))
  
  
  fixmat = data.frame(df[[1]]$study_id_dhc.enrollment_arm_1, df[[2]]$study_id_dhc.enrollment_arm_2, 
                      df[[3]]$study_id_dhc.enrollment_arm_3, df[[4]]$study_id_dhc.enrollment_arm_4)
  
  
  
  names(fixmat) = c("arm1", "arm2", "arm3", "arm4")
  dim(fixmat)
  head(fixmat)
  
  #fixmat[,1][!(fixmat$arm1 %in% arm1) & !is.na(fixmat$arm1)] 
  x = fixmat$arm1
  x = fixmat$arm2
  x = fixmat$arm3
  x = fixmat$arm4
  arms = list(arm1, arm2, arm3, arm4)
  
  update_arm = function(x, f, a) {
    am = arms[[a]]
    temp = x[!(x %in% am) & !is.na(x)] 
    update = apply(data.frame(arm1, arm2, arm3, arm4), 2, function(j) ifelse(temp %in% j, temp, NA))
    f[!(x %in% am) & !is.na(x), ] = update
    max(apply(update, 1, function(x) sum(!is.na(x))))
    return(f)
  }
  
  temp = update_arm(fixmat$arm1, fixmat, 1)
  temp = update_arm(fixmat$arm2, temp, 2)
  temp = update_arm(fixmat$arm3, temp, 3)
  temp = update_arm(fixmat$arm4, temp, 4)
  
  
  fixmat[(apply(fixmat, 1, function(x) sum(!is.na(x)))) == 2,]
  
  write.csv(fixmat, file = sprintf("fixed_dhc_tab_%s.csv", Sys.Date()))
  fixmat = temp
  wide_format_tab = c(table(fixmat[,1]), table(fixmat[,2]), table(fixmat[,3]), table(fixmat[,4]))
  
  sum(wide_format_tab[order(as.numeric(names(wide_format_tab)))] == table(main_frame$study_id_dhc))
  
  
  df$study_id_dhc.enrollment_arm_1 = temp$arm1
  df$study_id_dhc.enrollment_arm_2 = temp$arm2
  df$study_id_dhc.enrollment_arm_3 = temp$arm3
  df$study_id_dhc.enrollment_arm_4 = temp$arm4
  
  table(which_arm)
  #write.csv(df, file = "fixed_dhc_wide_format.csv")
  #write.csv(df, sprintf("fixed_dhc_wide_format_%s.csv", Sys.Date()))
  #write.csv(df, file = "fixed_dhc_wide_format_nov5.csv")
  #write.csv(fixmat, file = "fixmat_nov5.csv")
  #read.csv('fixed')
  ##accrual table across all health centers, 36 sites. code 
  ##study_id_dhc
  ## eligible # from separate source
  ##complete data for anc visits
  ##main 1 for dataset is table 1, another datasource, 
  
}



get_expected = function(x, new_frame, dt) {
  
  x$startdate = as.Date(as.character(x$anc1date_anc), format = "%Y-%m-%d")
  x$endate = as.Date(x$enroll_date, format = "%Y-%m-%d")
  x$ga = x$anc1ga_anc
  
  x$study_id_dhc = x$study_id_dhc / 1000000
  by_pat = split(x, x$record_id)
  
  en_date_bypat = lapply(by_pat, function(x) x$startdate)
  ga_bypat = lapply(by_pat, function(x) x$anc1ga_anc)
  
  
  get_date = sapply(en_date_bypat, getmin_date, simplify = FALSE)
  get_ga = sapply(ga_bypat, getmin_date)
  
  gdt = rep(NA, length(get_date))
  #ga = rep(NA, length(get_ga))
  
  class(gdt) <- "Date"
  for (j in 1:length(get_date)) {
    if (!is.na(get_date[[j]])) {
      gdt[j] = get_date[[j]]
      
    }
    
  }
  
  gdt[gdt > Sys.Date()] <- NA
  gdt[gdt < "2000-12-01"] <- NA
  summary(gdt)
  
  ##only thing needed is GA on first visit to know when eligible for visit
  #get this by mindate on x (more general) or do by arm using new_frame
  
  ##anc2 should only be from 20-28weeks, anc3 should only be from 28-36 weeks, anc4 should be 36wk+
  ga_days = get_ga * 7
  #56 dayas
  
  #dt = "2018-01-01"
  
  ga2 = (ga_days + 56) 
  ga3 = (ga_days + 112) 
  ga4 = (ga_days + 168) 
  
  date2 = gdt + 56
  date3 = gdt + 112
  date4 = gdt + 168
  
  expected2 = (date2 < dt & ga2 <= 196)
  expected3 = (date2 < dt & ga2 > 196 & ga2 <= 252) | (date3 < dt & ga3 > 196 & ga3 <= 252)
  expected4 = (date2 < dt & ga2 > 252 & ga2 <= 280) | (date3 < dt & ga3 > 252 & ga3 <= 280) | (date4 < dt & ga4 > 252 & ga4 <= 280)
  #advance to anc 2 if 1. past that date 2. if ga wks in appropriate range 
  # length(by_pat)
  # length(expected2)
  # ids_pat = (unique(main_frame$record_id))
  
  #new_frame$expect4 = expect4  
  
  new_frame$expected2 = expected2
  new_frame$expected3 = expected3
  new_frame$expected4 = expected4
  
  return(new_frame)
  
}

## Using cohort dataset

# 1 – generate list of study IDs that has been queried for being 8-weeks post-edd 

# 2 – match those study IDs with study IDs of baseline participants dataset 