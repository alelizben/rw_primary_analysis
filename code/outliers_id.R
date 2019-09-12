setwd("~/Documents/ptbi/rwanda")
library(parsedate)
library(openxlsx)
t = read.csv('query_template.csv', header = T)
names(t) = paste0("V", 1:ncol(t))

t_bd = t[-c(1, 5:7),]

#View(t)

var_names = sapply(t$V2, function(x) strsplit(as.character(x), " "))
var_names_t_bd = sapply(t_bd$V2, function(x) strsplit(as.character(x), " "))
var_names
var_names_t_bd
t$V2
as.character(t$V2)
bounds = strsplit(gsub(" ", "", as.character(t_bd$V4)), ",")

#new_bound = do.call(rbind, split_bounds)

check_bounds <- function(row, df, lookup_bounds) {
  #clean_x = gsub(pattern = ",", replacement = ".", as.character(df[,row]))
  x = as.numeric(df[,row])
  #x2 = as.numeric(levels(df[, row]))[df[,row]]
  if(is.na(lookup_bounds[row]) | mean(is.na(x)) == 1) {
    out = rep(NA, nrow(df))
  } else {
    bds = as.numeric(bounds[lookup_bounds[row]][[1]])
    # xbool = x < bds[1] | x > bds[2]
    # xbool_na = is.na(x)
    # x[xbool_na] <- "missing"
    # x[xbool == F] <- NA
    #out = x
    out = x < bds[1] | x > bds[2]
  }
  return(out)
}

## blank if OK, NA if missing, value if outlier
get_outliers = function(df) {
  df[df == "#NULL!" | df == " " | df == ""] <- NA
  #frame = df[, names(df) %in% c("study_id_full_new", "record_id", unlist(var_names))]
  #removed new
  frame = subset(df, select = c("study_id_full_new", "record_id", unique(unlist(var_names))))
  dates_in_frame = sapply(1:ncol(frame), function(x) class(frame[, x]))
  not_dates = c("study_id_full_new", "record_id", "m_age", "m_age_anc", "anc1ga_anc", "bwt_birth", "ga_weeks")
  #frame_dates = frame[, dates_in_frame == "factor" & !(names(frame) %in% not_dates)]
  frame_dates = frame[, !(names(frame) %in% not_dates)]
  #dates = apply(frame_dates, 2, fix_date)
  dates_fr = lapply(1:ncol(frame_dates), function(x) (fix_date(frame_dates[, x])))
  dates = do.call(data.frame, dates_fr)
  names(dates) = names(frame_dates)
  ##delivered? ##
  delivered = !is.na(frame_dates$date_delivery_mat)
  ##delivered ##

  outlier_dates_bool = data.frame(sapply(dates, function(x) x < "2017-05-22" | x > "2018-12-28"))
  head(outlier_dates_bool)
  outlier_dates_bool$lmp_anc = outlier_dates_bool$lmp_anc & outlier_dates_bool$enroll_date
  outlier_dates_bool$date_delivery_mat = outlier_dates_bool$date_delivery_mat & delivered
  outlier_dates_bool$edd_anc = dates$edd_anc < "2017-05-22" | dates$edd_anc > "2019-08-31"

  #colnames(outlier_dates_bool) = paste0(colnames(outlier_dates_bool), "_is_outlier?")
  bound_var_names = setdiff(names(frame), names(frame_dates))[-c(1:2)]
  bound_vars = subset(frame, select = bound_var_names)
  #names(frame)
  index_bound = lapply(bound_var_names, function(j) sapply(var_names_t_bd, function(x) j %in% x))
  index_bound = sapply(index_bound, function(g) which(g == T))
  index_bound
  lookup_bounds = sapply(index_bound, function(x) ifelse(length(x) > 0, x, NA))
  lookup_bounds
  length(lookup_bounds) == length(bound_var_names)
  positions <- 1:length(bound_var_names)
  bool_frame = data.frame(sapply(positions, function(x) check_bounds(x, bound_vars, lookup_bounds)))
  #bool_frame = sapply(positions, check_bounds, bound_vars, lookup_bounds)
  names(bool_frame) = names(bound_vars)
  bool_frame$`ga_weeks` = bool_frame$`ga_weeks` & delivered
  bool_frame$`bwt_birth` = bool_frame$`bwt_birth` & delivered

  #special cases
  ##anc1 ga
  lower = 1
  upper = 44
  anc1_lmp  = ((dates$anc1date_anc - dates$lmp_anc)/7) > lower & ((dates$anc1date_anc - dates$lmp_anc)/7) < upper 
  anc1_edd = (40 - ((dates$edd_anc - dates$anc1date_anc)/7)) > lower & (40 - ((dates$edd_anc - dates$anc1date_anc)/7)) < upper
  
  outlier_anc_condition = !((!is.na(bound_vars$anc1ga_anc) | anc1_lmp | anc1_edd) & (!is.na(dates$enroll_date) | !is.na(dates$anc1date_anc)))
  ## at least 1 visit
  if (is.null(dates$date_ancfuvst_2d) & is.null(dates$date_ancfuvst_2e)) {
    
    no_anc_followup = !(!is.na(dates$date_ancfuvst_2) | !is.na(dates$date_ancfuvst_2b) | !is.na(dates$date_ancfuvst_2c))
    
  } else if (is.null(dates$date_ancfuvst_2e) & !is.null(dates$date_ancfuvst_2d)) {
    
    no_anc_followup = !(!is.na(dates$date_ancfuvst_2) | !is.na(dates$date_ancfuvst_2b) | !is.na(dates$date_ancfuvst_2c) | 
                                  !is.na(dates$date_ancfuvst_2d))
    
  } else if (!is.null(dates$date_ancfuvst_2e) & is.null(dates$date_ancfuvst_2d)){
    
    no_anc_followup = !(!is.na(dates$date_ancfuvst_2) | !is.na(dates$date_ancfuvst_2b) | !is.na(dates$date_ancfuvst_2c) | 
                          !is.na(dates$date_ancfuvst_2e))
    
  } else if (is.null(dates$date_ancfuvst_2c)) {
    no_anc_followup = !(!is.na(dates$date_ancfuvst_2) | !is.na(dates$date_ancfuvst_2b))

  } else if (is.null(dates$date_ancfuvst_2b)) {
    no_anc_followup = is.na(dates$date_ancfuvst_2)

  }  else {
    no_anc_followup = !(!is.na(dates$date_ancfuvst_2) | !is.na(dates$date_ancfuvst_2b) | !is.na(dates$date_ancfuvst_2c) | 
                        !is.na(dates$date_ancfuvst_2d) | !is.na(dates$date_ancfuvst_2e))
  }

    #(!is.null(dates$date_ancfuvst_2e) & !is.na(dates$date_ancfuvst_2e)) 
  ## ga weeks at delivery
  lower = 24
  upper = 44
  ga_lmp = (dates$date_delivery_mat - dates$lmp_anc)/7 > lower & (dates$date_delivery_mat - dates$lmp_anc)/7 < upper
  
  if (!is.null(dates$dob_newborn_pnc)) {
    dob_lmp = (dates$dob_newborn_pnc - dates$lmp_anc)/7 > lower & (dates$dob_newborn_pnc - dates$lmp_anc)/7 < upper
  } else {
    dob_lmp = rep(NA, nrow(dates))
  }

  outlier_ga_weeks_condition = !(!is.na(bound_vars$ga_weeks) | ga_lmp | dob_lmp)
  #### end conditions ###
  
  table(outlier_anc_condition)

  table(outlier_ga_weeks_condition)
  table(no_anc_followup)

  table(delivered)
 
  
  order_vars = c("enroll_date", "m_age", ###enrollmt form
                 ##ancregister
                 "m_age_anc", "lmp_anc", "edd_anc",  "anc1date_anc", "anc1ga_anc",  
                 ##FUVst form
                 "date_ancfuvst_2",  "date_ancfuvst_2b", "date_ancfuvst_2c", 
                 "date_ancfuvst_2d", "date_ancfuvst_2e", 
                 #mat_register
                 "ga_weeks",  "date_delivery_mat", "bwt_birth",
                 ##PNCform
                 "dob_newborn_pnc")
  
  #final_frame = data.frame(bound_vars, dates, outlier_anc_condition, no_anc_followup, outlier_ga_weeks_condition, delivered)
  #final_bool = data.frame(bool_frame, outlier_dates_bool, outlier_anc_condition, no_anc_followup, outlier_ga_weeks_condition)

  dates = sapply(dates_fr, as.character)
  colnames(dates) = names(frame_dates)
  dates[is.na(outlier_dates_bool)] <- "missing"
  bound_vars[is.na(bool_frame)] <- "missing"

  final_frame = data.frame(bound_vars, dates)
  final_bool = data.frame(bool_frame, outlier_dates_bool)
  head(final_frame)
  head(final_bool)
  final_frame[final_bool == F] <- NA
  colsum = colSums(final_bool, na.rm = T)
  colsum
  #names(final_frame)[which(colsum > 0)] = paste0(names(final_frame)[which(colsum > 0)], "_has_outlier")
  
  # xbool = x < bds[1] | x > bds[2]
  # xbool_na = is.na(x)
  # x[xbool_na] <- "missing"
  # x[xbool == F] <- NA
  #out = x

  #write.xlsx(read_frame, file = "test_cor.xlsx")
  #changed new
  read_frame = data.frame(df$study_id_full_new, df$record_id, final_frame, no_anc_followup, 
                          outlier_anc_condition, outlier_ga_weeks_condition)

  #read_frame = data.frame(df$study_id_full_new, df$record_id, final_bool, final_frame[,-c(17:19)])
  index_fc = match(order_vars, names(read_frame))

  order_index = c(1:2, index_fc, (ncol(read_frame) - 2):ncol(read_frame))
  final_bool = data.frame(final_bool, no_anc_followup, outlier_anc_condition, outlier_ga_weeks_condition)
  all = read_frame[(rowSums(final_bool, na.rm = T)) > 0, order_index ]
  all = all[!duplicated(all$df.study_id_full_new),]
  dim(all)
  #out = frame[(rowSums(bool_frame, na.rm = T)) > 0, ]
  #out = bound_vars[(rowSums(bool_frame, na.rm = T)) > 0, ]
  # if (fc){
  #   out = fc_only
  # } else {
  #   out = all
  # }

  return(all)
}

fix_date = function(date) {
  
  ancdate_missing = as.Date(rep(NA, length(date)))
  
  if (mean(is.na(date)) == 1){
    ancdate_missing = ancdate_missing
    
  } else {
    ancdate = parse_date(as.character(date)[!is.na(date)])
    class(ancdate)
    ancdate = as.Date(ancdate, format = "%Y-%m-%d")
    ancdate_missing[!is.na(date)] <- ancdate
    
  }
  
  return(ancdate_missing)
  
}

#files = list.files(path = "monthly_report/data/ForAlejandra", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
# files = list.files(path = "RestructuredThruFeb19 (2)/", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
# file_names = list.files(path = "RestructuredThruFeb19 (2)/", pattern = "*.csv", full.names = F, recursive = FALSE)

files = list.files(path = "monthly_report/data/AleV423May2019/", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
file_names = list.files(path = "monthly_report/data/AleV423May2019/", pattern = "*.csv", full.names = F, recursive = FALSE)

files
file_names
out = lapply(files, read.csv)
names(out) = sapply(strsplit(file_names, "\\D+"), function(x) x[[2]])
(sapply(out, ncol))
id_arm = read.csv('monthly_report/code/RW_Study ID.csv')

names(out) = id_arm$HC[match(names(out), id_arm$DHC)]
lapply(out, function(x) table(substr(x$study_id_full_new, 1, 3)))
sapply(out, function(x) sum(duplicated(x$study_id_full_new)))
sapply(out, nrow)

#df = arm1[[6]]
#dim(df)

A1 = c(333, 436, 544, 120, 110, 545, 111, 224, 221)
A2 = c(435, 331, 541, 115, 113, 539, 226, 329, 332)

A3 = c(330, 438, 540, 117, 116, 542, 112, 328, 223)
A4 = c(437, 327, 543, 114, 119, 334, 118, 222, 225)

A1 = c(333, 436, 544, 120, 110, 545, 111, 224)
A2 = c(435, 331, 541, 115, 113, 539, 226, 329, 332)

A3 = c(438, 540, 117, 116, 542, 112, 328, 223)
A4 = c(437, 327, 543, 114, 119, 334, 118, 222, 225)

arm1 = out[names(out) %in% A1]
arm2 = out[names(out) %in% A2]
arm3 = out[names(out) %in% A3]
arm4 = out[names(out) %in% A4]
length(arm4)
lapply(arm4, dim)

#arm1$'221'$study_id_full_new = arm1$'221'$study_id_full

outliers1 = lapply(arm1, function(x) get_outliers(x))
outliers2 = lapply(arm2, function(g) get_outliers(g))
outliers3 = lapply(arm3, function(g) get_outliers(g))
outliers4 = lapply(arm4, function(g) get_outliers(g))
# 
#outliers4 = lapply(out, function(g) get_outliers(g))
sapply(outliers1, nrow)/sapply(arm1, nrow)
sapply(outliers2, nrow)/sapply(arm2, nrow)
sapply(outliers3, nrow)/sapply(arm3, nrow)
sapply(outliers4, nrow)/sapply(arm4, nrow)
# (bounds[lookup_bounds,])
# length(out)
# out[[1]]
# lapply(out, length)
#bound_frame = frame[, (names(frame) %in% do.call(c, all_vars_bounds))]

#names(frame)[(names(frame) %in% do.call(c, all_vars_bounds))]
#source('code/visit completion.R')
id_arm = read.csv('monthly_report/code/RW_Study ID.csv')

names(outliers1) = id_arm$HC[match(names(outliers1), id_arm$DHC)]
names(outliers2) = id_arm$HC[match(names(outliers2), id_arm$DHC)]
names(outliers3) = id_arm$HC[match(names(outliers3), id_arm$DHC)]
names(outliers4) = id_arm$HC[match(names(outliers4), id_arm$DHC)]

#for (i in 1:length(arm3)) {
 # get_outliers(arm3[[i]])
  #}

index_nonempty1 = sapply(outliers1, nrow)
index_nonempty2 = sapply(outliers2, nrow)
index_nonempty3 = sapply(outliers3, nrow)
index_nonempty4 = sapply(outliers4, nrow)
by_fac1 = outliers1[which(index_nonempty1 > 0)]
by_fac2 = outliers2[which(index_nonempty2 > 0)]
by_fac3 = outliers3[which(index_nonempty3 > 0)]
by_fac4 = outliers4[which(index_nonempty4 > 0)]

write.xlsx(by_fac1, file = sprintf('outlier_arm1_fc_%s.xlsx', Sys.Date()))
write.xlsx(by_fac2, file = sprintf('outlier_arm2_fc_%s.xlsx', Sys.Date()))
write.xlsx(by_fac3, file = sprintf('outlier_arm3_fc_%s.xlsx', Sys.Date()))
write.xlsx(by_fac4, file = sprintf('outlier_arm4_fc_%s.xlsx', Sys.Date()))
##write one for mucaca-mudende

write.xlsx(by_fac1, file = sprintf('outlier_arm1_all_%s.xlsx', Sys.Date()))
write.xlsx(by_fac2, file = sprintf('outlier_arm2_all_%s.xlsx', Sys.Date()))
write.xlsx(by_fac3, file = sprintf('outlier_arm3_all_%s.xlsx', Sys.Date()))
write.xlsx(by_fac4, file = sprintf('outlier_arm4_all_%s.xlsx', Sys.Date()))

# 
df = arm1[[1]]
count = list(index_nonempty1, index_nonempty2, index_nonempty3, index_nonempty4)
list_count = lapply(count, function(x) paste(names(x), x)) 
table_counts = do.call(rbind, list_count)
rownames(table_counts) = c("arm1", "arm2", "arm3", "arm4")
table_counts
write.csv(table_counts, file = "counts.csv")
