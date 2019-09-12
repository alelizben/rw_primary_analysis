#implausibles query

##

#Among women who delivered at their hc, (Location_birth_1=1 on maternity reg event): (expected n=27)
#List all women who had a c-section (mode_delivery=2)
#Among all women, list all maternal deaths mat_outcome=3  (expected n-66)
#Include in list: date_delivery_mat, location_birth_1, location_birth_2, mode_delivery
#Among all women with delivery outcomes, delivered with a non-plausible GA (expected n = 587)
#IF ga_weeks on maternity register event is less than 20 or more than 44

#List DHC, record ID, Study ID, ga_weeks
##
get_implausibles = function(df) {
  
  df[df == "#NULL!" | df == " " | df == ""] <- NA
  #frame = df[, names(df) %in% c("study_id_full_new", "record_id", unlist(var_names))]
  #removed new
  frame = subset(df, select = c("study_id_full_new", "record_id"))
  df = df %>% select(study_id_full_new, record_id, 
                     location_birth_1, location_birth_2, 
                     mode_delivery, mat_outcome,
                     ga_weeks, date_delivery_mat)
  library(dplyr)
  sum(df$location_birth_1 == 1, na.rm = T)
  location_birth_1 = df %>% filter((location_birth_1 == 1 | location_birth_2 == 1) & mode_delivery == 2)
  #location_birth_2 = df %>% filter(location_birth_2 == 1)

  mat_outcome = df %>% filter(mat_outcome == 3)
  ga_weeks = df %>% filter(!is.na(date_delivery_mat) & (ga_weeks < 20 | ga_weeks > 44))
  list_out = list(location_birth_1, mat_outcome, ga_weeks)
  count_out = sapply(list_out, nrow)
  names(count_out) = c("location_birth1 = 1 & C-Section", "mat_outcome = 3", 
                      "delivered & ga_weeks_outlier")
  head1 =  rep(NA, ncol(df))
  head2 = rep(NA, ncol(df))
  head3 = rep(NA, ncol(df))
  output = rbind(head1, location_birth_1, head2, mat_outcome, head3, ga_weeks)
  row.names(output)[1] = "c_section_at_anc_site"
  row.names(output)[2 + nrow(location_birth_1)] = "mat_outcome_3"
  row.names(output)[3 + nrow(location_birth_1) + nrow(mat_outcome)] = "ga_wks_outlier"
  #output = df %>% filter(location_birth_1 == 1 | location_birth_2 == 1 | mode_delivery == 2 | 
   #                        mat_outcome == 3 | ga_weeks < 20 | ga_weeks > 44)

  ##delivered? ##
  #delivered = !is.na(frame_dates$date_delivery_mat)
  return(list(count_out, output))
  
}

setwd('~/Documents/ptbi/rwanda/monthly_report/')


#hc1  = read.csv('ForAlejandraRestructuresCSVs/Cor-unum - 436_1538130822891ExcludesWrongHCWrongArmRestructured.csv')
#table(hc1$studyid_bugesera)
#compl = apply(hc1, 2, function(x) mean(!is.na(x)))
source('code/visit completion.R')
id_arm = read.csv('code/RW_Study ID.csv')
library(imputeTS)


files = list.files(path = "data/AleV423May2019/", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
file_names = list.files(path = "data/AleV423May2019/", pattern = "*.csv", full.names = FALSE, recursive = FALSE)


out = lapply(files, read.csv)
num_out = sapply(strsplit(file_names, "\\D+"), function(x) x[[2]])
num_out

names(out) = id_arm$HC[match(num_out, id_arm$DHC)]
lapply(out, function(x) table(substr(x$study_id_full_new, 1, 3)))
sapply(out, function(x) sum(duplicated(x$study_id_full_new)))


##221 cyanika
A1 = c(333, 436, 544, 120, 110, 545, 111, 224, 221)
A2 = c(435, 331, 541, 115, 113, 539, 226, 329, 332)

A3 = c(330, 438, 540, 117, 116, 542, 112, 328, 223)
A4 = c(437, 327, 543, 114, 119, 334, 118, 222, 225)


arm1 = out[num_out %in% A1]
arm2 = out[num_out %in% A2]
arm3 = out[num_out %in% A3]
arm4 = out[num_out %in% A4]


#arm1$'221'$study_id_full_new = arm1$'221'$study_id_full

outlier1 = t(sapply(arm1, function(x) get_implausibles(x)[[1]]))
outlier2 = t(sapply(arm2, function(g) get_implausibles(g)[[1]]))
outlier3 = t(sapply(arm3, function(g) get_implausibles(g)[[1]]))
outlier4 = t(sapply(arm4, function(g) get_implausibles(g)[[1]]))


outliers1 = (lapply(arm1, function(x) get_implausibles(x)[[2]]))
outliers2 = (lapply(arm2, function(g) get_implausibles(g)[[2]]))
outliers3 = (lapply(arm3, function(g) get_implausibles(g)[[2]]))
outliers4 = (lapply(arm4, function(g) get_implausibles(g)[[2]]))

t1 = c(list(outlier1), outliers1)
t2 = c(list(outlier2), outliers2)
t3 = c(list(outlier3), outliers3)
t4 = c(list(outlier4), outliers4)

library(openxlsx)

write.xlsx(t1, file = sprintf('implaus_arm1_%s.xlsx', Sys.Date()), row.names = T)
write.xlsx(t2, file = sprintf('implaus_arm2_%s.xlsx', Sys.Date()), row.names = T)
write.xlsx(t3, file = sprintf('implaus_arm3_%s.xlsx', Sys.Date()), row.names = T)
write.xlsx(t4, file = sprintf('implaus_arm4_%s.xlsx', Sys.Date()), row.names = T)
#t1 = (c(outliers1, outlier1))

# 