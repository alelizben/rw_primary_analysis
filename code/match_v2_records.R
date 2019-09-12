
match_records_enroll_v2 = function(missing_event, event_v2, criteria) {
  ## this is the missing event from the file made by nancy
  dt = missing_event
  ## this is the event in v2 where we will try to find a match
  event = event_v2
  
  #match: positions of matches of its first argumemt in its second
  #c(1, 2, 3) %in% c(3,3,2,2, 1)
  #c(3, 3, 2, 2, 1)[match(c(1, 2, 3), c(3, 3, 2, 2, 1)) ]
  match(c(1, 2, 3), c(3, 3, 2, 2, 1))
  # event$study_id_anc[match(anc2$study_id_full_new, event$study_id_anc, nomatch = NULL)]
  # anc2$study_id_full_new[match(event$study_id_anc, anc2$study_id_full_new)]
  # 
  dt$record = paste(dt$dhc, dt$record_id)
  event$record = paste(event$dhc, event$record_id)
  
  if (criteria == "record") {
    
    ids_anc2 = dt %>% 
      filter((record %in% event$record) & !is.na(record)) %>%
      select(record, study_id_full_new)
    out = event %>% filter(event$record %in% ids_anc2$record) 
    # sum(mean(!is.na(out$study_id_anc)))
    # sum(!is.na(out$study_id_anc))
  } else {
    
    ids_anc2 = dt %>% 
      filter((study_id_full_new %in% event$study_id_full) & !is.na(study_id_full_new)) %>%
      select(study_id_full_new)
    out = event %>% filter(event$study_id_full %in% ids_anc2$study_id_full_new)
    # dim(out)
    # sum(mean(!is.na(out$study_id_anc)))
    # sum(!is.na(out$study_id_anc))
  }
  data_dump = dt %>% filter(study_id_full_new %in% ids_anc2$study_id_full_new) %>% 
    select(dhc, record_id, m_name, study_id_full_new, record)
  data_dump = data_dump[order(data_dump$dhc), ]
  
  output_v2 = out %>% 
    select(dhc, redcap_event_name, record_id, m_name, study_id_full, record)
  output_v2 = output_v2[order(output_v2$dhc), ]
  options("scipen"=100, "digits"=4)
  
  out = (list(data_dump, output_v2, length(unique(output_v2$study_id_full))/nrow(dt)))
  names(out) = c("data_dump_missing_events", "v2_matches", "rate_matched")
  
  return(out)
}

match_records_anc1_v2 = function(missing_event, event_v2, criteria, arm) {
  ## this is the missing event from the file made by nancy
  dt = missing_event
  ## this is the event in v2 where we will try to find a match
  event = event_v2
  
  #match: positions of matches of its first argumemt in its second
  #c(1, 2, 3) %in% c(3,3,2,2, 1)
  #c(3, 3, 2, 2, 1)[match(c(1, 2, 3), c(3, 3, 2, 2, 1)) ]
  match(c(1, 2, 3), c(3, 3, 2, 2, 1))
  # event$study_id_anc[match(anc2$study_id_full_new, event$study_id_anc, nomatch = NULL)]
  # anc2$study_id_full_new[match(event$study_id_anc, anc2$study_id_full_new)]
  # 
  dt$record = paste(dt$dhc, dt$record_id)
  event$record = paste(event$dhc, event$record_id)
  
  if (criteria == "record") {
    
    ids_anc2 = dt %>% 
      filter((record %in% event$record) & !is.na(record)) %>%
      select(record, study_id_full_new)
    out = event %>% filter(event$record %in% ids_anc2$record) 
    # sum(mean(!is.na(out$study_id_anc)))
    # sum(!is.na(out$study_id_anc))
  } else {
    
    ids_anc2 = dt %>% 
      filter((study_id_full_new %in% event$study_id_anc) & !is.na(study_id_full_new)) %>%
      select(study_id_full_new)
    out = event %>% filter(event$study_id_anc %in% ids_anc2$study_id_full_new)
    # dim(out)
    # sum(mean(!is.na(out$study_id_anc)))
    # sum(!is.na(out$study_id_anc))
  }
  
  data_dump = dt %>% filter(study_id_full_new %in% ids_anc2$study_id_full_new) %>% 
    select(dhc, record_id, m_name, study_id_full_new, record)
  data_dump = data_dump[order(data_dump$dhc),] 
  
  if (arm == 2 | arm == 4) {
    output_v2 = out %>% 
      select(dhc, redcap_event_name, record_id, study_id_anc, study_id_file, study_id_us, 
             study_id_upt, m_namelast_anc, m_namegiven_anc, record)
    options("scipen"=100, "digits"=4)   
  } else {
    output_v2 = out %>% 
      select(dhc, redcap_event_name, record_id, study_id_anc, study_id_file,
             m_namelast_anc, m_namegiven_anc, record)
    options("scipen"=100, "digits"=4)
  }
  output_v2 = output_v2[order(output_v2$dhc), ]
  out = (list(data_dump, output_v2, length(unique(output_v2$study_id_anc))/nrow(dt)))
  names(out) = c("data_dump_missing_events", "v2_matches", "rate_matched")
  return(out)
  
}


match_records_fu_v2 = function(missing_event, event_v2, criteria) {
  ## this is the missing event from the file made by nancy
  dt = missing_event
  ## this is the event in v2 where we will try to find a match
  event = event_v2
  
  #match: positions of matches of its first argumemt in its second
  #c(1, 2, 3) %in% c(3,3,2,2, 1)
  #c(3, 3, 2, 2, 1)[match(c(1, 2, 3), c(3, 3, 2, 2, 1)) ]
  match(c(1, 2, 3), c(3, 3, 2, 2, 1))
  # event$study_id_anc[match(anc2$study_id_full_new, event$study_id_anc, nomatch = NULL)]
  # anc2$study_id_full_new[match(event$study_id_anc, anc2$study_id_full_new)]
  # 
  dt$record = paste(dt$dhc, dt$record_id)
  event$record = paste(event$dhc, event$record_id)
  
  if (criteria == "record") {
    
    ids_anc2 = dt %>% 
      filter((record %in% event$record) & !is.na(record)) %>%
      select(record, study_id_full_new)
    out = event %>% filter(event$record %in% ids_anc2$record) 
    # sum(mean(!is.na(out$study_id_anc)))
    # sum(!is.na(out$study_id_anc))
  } else {
    
    ids_anc2 = dt %>% 
      filter((study_id_full_new %in% event$study_id_anc_fuvst) & !is.na(study_id_full_new)) %>%
      select(study_id_full_new)
    out = event %>% filter(event$study_id_anc_fuvst %in% ids_anc2$study_id_full_new)
    # dim(out)
    # sum(mean(!is.na(out$study_id_anc)))
    # sum(!is.na(out$study_id_anc))
  }
  
  data_dump = dt %>% filter(study_id_full_new %in% ids_anc2$study_id_full_new) %>% 
    select(m_name, study_id_full_new, record)
  
    output_v2 = out %>% 
      select(redcap_event_name, record_id, study_id_anc_fuvst, record)
    
    options("scipen"=100, "digits"=4)
    out = (list(data_dump, output_v2, nrow(dt)/nrow(output_v2)))
    names(out) = c("data_dump_missing_events", "v2_matches", "rate_matched")
  return(out)
  
}


##anc6
v2_anc6 = match_records_fu_v2(anc6, v2_fu, "other")

nrow(anc6)/nrow(v2_anc6$data_dump_missing_events)

dim(anc6)
dim(v2_anc6)

##anc5
v2_anc5 = match_records_fu_v2(anc5, v2_fu, "other")
v2_anc5
dim(anc5)
dim(v2_fu)
dim(v2_anc5)

##anc4
v2_anc4 = match_records_fu_v2(anc4, v2_fu, "other")
(v2_anc4)

##anc3
v2_anc3 = match_records_fu_v2(anc3, v2_fu, "other")
(v2_anc3)

##anc2 has but is missing anc1?
a = match_records_anc1_v2(anc2, v2_anc1_a13, "other", 1)
b = match_records_anc1_v2(anc2, v2_anc1_a24, "other", 2)
a$rate_matched
b$rate_matched
library(openxlsx)
write.xlsx(a, file = "arm1_3_v2_matches_missing_anc1.xlsx")
write.xlsx(b, file = "arm2_4_v2_matches_missing_anc1.xlsx")

dim(a)
dim(b)
##has delivery but no enroll
enroll = match_records_enroll_v2(delivery_enroll, v2_enroll, "other")
enroll$rate_matched
write.xlsx(enroll, file = "v2_matches_delivery_no_enroll.xlsx")
