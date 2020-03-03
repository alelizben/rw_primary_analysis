setwd('~/Documents/ptbi/rwanda')
load('REDCap cohort datasets 20 June 2018/no_duplicates_arms_keepall_2018-06-20.Rdata')
arm1 = arms_fix[[1]]
arm2 = arms_fix[[2]]
arm3 = arms_fix[[3]]
arm4 = arms_fix[[4]]


x = arm4
arm = 4

get_ga_measures = function(data, arm) {
  
  x = data[[arm]]
  
  study_id = x[, sprintf('study_id_full.enrollment_arm_%s', arm)]
  dhc = substr(study_id, 1, 3)
  
  date_del = date_cleaning(as.Date(x[, sprintf('date_delivery_mat.delivery_arm_%s', arm)], "%Y-%d-%m"))
  date_anc1 = date_cleaning(as.Date(x[, sprintf('anc1date_anc.anc1_visit_arm_%s', arm)], "%Y-%d-%m"))
  
  days_from_anc1_to_del = date_del - date_anc1
  
  ## Question 1 ##
  lmp = date_cleaning(as.Date(x[, sprintf('lmp_anc.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d"))
  
  ga_at_anc1 = date_anc1 - lmp
  
  ga_at_deliv_lmp = (ga_at_anc1 + days_from_anc1_to_del) / 7
  ##create new variable "GA at delivery by LMP"
  
  arm4$anc1ga_anc.anc1_visit_arm_4
  
  
  ## Question 2 ### Create GA at delivery by EDD at ANC1 ##

  edd = date_cleaning(as.Date(x[, sprintf('edd_anc.anc1_visit_arm_%s', arm)], '%Y-%d-%m'))
  
  ga_at_delivery_by_edd_anc1 = (280 + (date_del - edd)) / 7

  #Question 3
  ##GA AT DELIVERY BY GA AT ANC1 ##
  ga_anc1 = (x[, sprintf('anc1ga_anc.anc1_visit_arm_%s', arm)])
  ga_anc1
  
  ga_at_delivery_by_ga_anc1 = (ga_anc1 + days_from_anc1_to_del) / 7
  
  ########
  
  
  ## Question 4 ### Create GA at delivery by HC US##
  if (arm == 2 | arm == 4) {
    us = date_cleaning(as.Date(x[, sprintf('edd_us.anc1_visit_arm_%s', arm)], format = "%Y-%m-%d"))
    us
    ga_us_diff = date_del - us
    ga_at_delivery_by_us = (280 + ga_us_diff) / 7
    
  } else {
    ga_at_delivery_by_us = rep(NA, nrow(x))
  }

  
  ######## Question 5 ## GA at delivery by FH @ anc1 ### 
 
  ####
  fun_ht = x[, sprintf('fundalht_1stvisit.anc1_visit_arm_%s', arm)]
  
  ga_at_delivery_by_fh_anc1 = fun_ht + (days_from_anc1_to_del/7)
  ga_at_delivery_by_fh_anc1
  
  
  
  ### GA WEEKS ################
  
  final = data.frame(study_id, dhc, date_anc1, date_del, lmp, days_from_anc1_to_del, ga_at_deliv_lmp,  
                ga_at_delivery_by_ga_anc1, ga_at_delivery_by_edd_anc1, ga_at_delivery_by_us, ga_at_delivery_by_fh_anc1)
  #index_bounds = filter_dates(date_anc1, "2016", "2019") & filter_dates(date_del, "2016", "2019") & filter_dates(lmp, "2016", "2019")
  index_bounds = filter_dates(date_anc1, "2016", "2019") & filter_dates(date_del, "2016", "2019") & filter_dates(lmp, "2016", "2019")
  final_bounds = final[which(index_bounds == T), ]
  #return(final)  
  return(final_bounds)
}


date_cleaning = function(date) {
  x = as.numeric(make_year(date))
  index1 = which(x == 1017 | x == 0217 | x == 2027 | x == 3017 | x == 2107)
  index2 = which(x == 3018 | x == 2008 | x == 2028)
  char_date = as.character(date)
  
  if (length(index1 > 0)) {
    fixed_date1 = mapply(function(x, y) sub(x, "2017", y), substr(char_date[index1], 1, 4), char_date[index1])
    char_date[index1] <- fixed_date1
  } 
  if (length(index2 > 0)) {
    fixed_date2 = mapply(function(x, y) sub(x, "2018", y), substr(char_date[index2], 1, 4), char_date[index2])
    char_date[index2] <- fixed_date2
    
  }
  
  out = as.Date(char_date)
  
  return(out)
  
}

filter_dates = function(x, year_lower, year_upper){
  
  year = format(x, "%Y")
  year_bound = (year <= year_upper & year >= year_lower)

  return(year_bound)
}


make_year = function(x) {
  year = format(x, "%Y")
  return(year)
}


a1_ga = get_ga_measures(arms_fix, 1)
a2_ga = get_ga_measures(arms_fix, 2)
a3_ga = get_ga_measures(arms_fix, 3)
a4_ga = get_ga_measures(arms_fix, 4)

head(a1_ga)
dim(a1_ga)
table(a1_ga$dhc)
table(a2_ga$dhc)
table(a3_ga$dhc)
table(a4_ga$dhc)

summary(a1_ga)
summary(a2_ga)
summary(a3_ga)
summary(a4_ga)
dim(a1_ga)
dim(a2_ga)
dim(a3_ga)
dim(a4_ga)


a1_ga = a1_ga[,2:4]
a2_ga = a2_ga[,2:4]
a3_ga = a3_ga[,2:4]
a4_ga = a4_ga[,2:4]


cor(as.numeric(a4_ga$ga_at_deliv_lmp), as.numeric(a4_ga$ga_at_delivery_by_ga_anc1))

cor(as.numeric(a4_ga$ga_at_deliv_lmp), as.numeric(a4_ga$ga_at_delivery_by_edd_anc1), use = "complete.obs")

cor(as.numeric(a4_ga$ga_at_delivery_by_ga_anc1), as.numeric(a4_ga$ga_at_delivery_by_edd_anc1), use = "complete.obs")

cor(as.numeric(a4_ga$ga_at_delivery_by_fh_anc1), as.numeric(a4_ga$ga_at_delivery_by_ga_anc1), use = "complete.obs")
cor(as.numeric(a4_ga$ga_at_deliv_lmp), as.numeric(a4_ga$ga_at_delivery_by_fh_anc1), use = "complete.obs")

mean(!is.na(a4_ga$ga_at_deliv_lmp))
mean(!is.na(a4_ga$ga_at_delivery_by_ga_anc1))
mean(!is.na(a4_ga$ga_at_delivery_by_edd_anc1))
length(!is.na(a4_ga$ga_at_delivery_by_fh_anc1))

sum(!is.na(a4_ga$ga_at_delivery_by_us))

sum(!is.na(a2_ga$ga_at_delivery_by_us))

summary(a4_ga)
head(a4_ga)
sum(duplicated(a2_ga$study_id))

sum(complete.cases(a2_ga))
sum(complete.cases(a4_ga))

dim(final_bounds)
summary(final_bounds)
summary(final)
x = arm4
arm = 4

apply(a2_ga, 2, function(x) sum(!is.na(x)))

arm2_us = a2_ga[!is.na(a2_ga$ga_at_delivery_by_us), ]

arm4_us = a4_ga[!is.na(a4_ga$ga_at_delivery_by_us), ]

## Question 2 ##
## compare GAs
ga_wks = x[ ,sprintf('ga_weeks.delivery_arm_%s', arm)]

ga_at_delivery_by_edd_anc1
ga_at_delivery_by_fh_anc1
ga_at_delivery_by_hc_us
ga_at_deliv_lmp


x[, sprintf('ga_weeks.delivery_arm_%s', arm)]
x[, sprintf('ga_weeks.delivery_arm_%s', arm)]
x[, sprintf('ga_weeks.delivery_arm_%s', arm)]
x[, sprintf('ga_weeks.delivery_arm_%s', arm)]



a1date = check_dates(arms_fix, 1)
table(a1date$del_yr)
table(a1date$anc1_yr)

a2date = check_dates(arms_fix, 2)
table(a2date$del_yr)
table(a2date$anc1_yr)

a3date = check_dates(arms_fix, 3)
table(a3date$del_yr)
table(a3date$anc1_yr)

a4date = check_dates(arms_fix, 4)
table(a4date$del_yr)
table(a4date$anc1_yr)

lapply(1:3, function(i) table(make_year(a1_ga[,i])))
lapply(1:3, function(i) table(make_year(a2_ga[,i])))
lapply(1:3, function(i) table(make_year(a3_ga[,i])))
lapply(1:3, function(i) table(make_year(a4_ga[,i])))


date[date_cleaning(a1_ga$date_anc1)[[1]] == T]
sum(date_cleaning(a1_ga$date_anc1)[[1]], na.rm = T)
dim(a2_ga)
dim(a3_ga)
dim(a4_ga)
#
#26-34 weeks
##Up to 34 weeks if the ga was listed at 34 wks, and fundal height was +-4
#if 4 or more weeks apart, throw out record
## 2 weeks apart within 4 wks, use LMP
