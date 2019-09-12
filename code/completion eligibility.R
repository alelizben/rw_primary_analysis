##look up missing IDS
t = read.csv('Women192WithoutEnrollmentOrANC1DateThruDec2018.csv')
look = t$study_id_full_new.2c..Study.ID
by_fac = lapply(out, function(g) event_compl(g, "2018-12-30", look))
names(out)
names(by_fac) = id_arm$HC[match(names(out), id_arm$DHC)]
index_nonempty = sapply(by_fac, nrow)
by_fac = by_fac[which(index_nonempty > 0)]
write.xlsx(by_fac, sprintf("incomplete_ID_%s.xlsx", Sys.Date()))
## look up missing IDs

##get faulty dates ##
test = lapply(out, function(x) event_compl(x, "2018-12-31", lookup = NULL))
names(test)
names(test) = id_arm$HC[match(names(test), id_arm$DHC)]
index_nonempty = sapply(test, nrow)
out_dates = test[index_nonempty > 0]
library(openxlsx)
write.xlsx(out_dates, sprintf("bad_dates.xlsx", Sys.Date()))
names(out) = id_arm$HC[match(names(out), id_arm$DHC)]
names(out)[sapply(out, function(x) mean(is.na(x$date_pnc4)) == 1)]

##get faulty dates ##

length(by_fac)
library(zoo)
##inelglible after signing consent 
do.call(rbind, by_fac)

enroll_mo_yr = sapply(by_fac, function(x) as.character(x$mo_yr_enrolled[which(x$enroll_compl == T)]))
enroll_mo_yr = sapply(by_fac, function(x) length(which(x$enroll_compl == T)))
table_enroll = table(do.call(c,enroll_mo_yr))
dts_yearmon = as.yearmon(names(table_enroll), "%m-%Y")
dts_ordered =  table_enroll[order(dts_yearmon)]
sum(dts_ordered[19:30])
write.csv(dts_ordered, "enrolled_mo_yr.csv")

enroll = sapply(by_fac, function(x) sum(x$enroll_compl, na.rm = T))
sum(enroll)/sum(sapply(by_fac, nrow))
sum(enroll)
eligible_del = sapply(by_fac, function(j) sum(!is.na(j$delivered)))

eligible_del = sapply(by_fac, function(j) sum(j$elig_anc2_lt_24_wks & 
                                                !is.na(j$delivered), na.rm = T))
eligible = sapply(by_fac, function(j) sum(j$elig_anc2_lt_24_wks, na.rm = T))

eligible_mo_yr = sapply(by_fac, function(j) as.character(j$mo_yr_delivered[which(j$elig_anc2_lt_24_wks & 
                                                                                   !is.na(j$delivered))]))
dts_eligible = (table(do.call(c, eligible_mo_yr)))
dts_yearmon = as.yearmon(names(dts_eligible), "%m-%Y")
dts_ordered =  dts_eligible[order(dts_yearmon)]
write.csv(dts_ordered, file = "dates_eligible_delivered.csv")

dts_eligible

sum(eligible)
sum(eligible)/sum(enroll)

lapply(by_fac, class)

compl = sapply(by_fac, function(x) sapply(x, function(j) mean(!is.na(j))))
compl = round(compl, 3)
colnames(compl) = id_arm$HC[match(colnames(compl), id_arm$DHC)]
View(compl)
write.csv(compl, file = sprintf("hc_visits_jan/completion_rates_%s.csv", Sys.Date()))



out1 = lapply(arm1, function(g) make_us_rep(g, 1, "2018-11-22"))
col_names = colnames(out1[[2]])
out1 = sapply(arm1, function(x) make_us_rep(x, 1, "2018-11-22"))
out2 = lapply(arm2, function(g) make_us_rep(g, 2, "2018-11-22"))
out3 = sapply(arm3, function(g) make_us_rep(g, 3, "2018-11-22"))
out4 = lapply(arm4, function(g) make_us_rep(g, 4, "2018-11-22"))
# 
lapply(out2, length)
lapply(out4, length)
# 
us_report2 = t(do.call(rbind, lapply(out2, function(x) x[[1]])))
us_report4 = t(do.call(rbind, lapply(out4, function(x) x[[1]])))
colnames(us_report2) = id_arm$HC[match(as.numeric(colnames(us_report2)), table = id_arm$DHC)]
colnames(us_report4) = id_arm$HC[match(as.numeric(colnames(us_report4)), table = id_arm$DHC)]
us_report2
out = list(us_report2, us_report4)
out = list(out2, out4)
names(out) = c("Arm2", "Arm4")
write.xlsx(out, file = sprintf('ultrasound_report_%s.xlsx', Sys.Date()), row.names = T)
# 
date_summaries2 = lapply(out2, function(x) x[[2]])
date_summaries4 = lapply(out4, function(x) x[[2]])

sum(table(do.call(c, date_summaries2)))

dts2 = table(do.call(c, date_summaries2))
dts4 = table(do.call(c, date_summaries4))

library(zoo)
#dts = names(table(dat$myr))
dts_yearmon2 = as.yearmon(names(dts2), "%m-%Y")

dts_yearmon4 = as.yearmon(names(dts4), "%m-%Y")

dts2 = dts2[order(dts_yearmon2)]
dts4 = dts4[order(dts_yearmon4)]
sum(sapply(date_summaries2, function(x) sum(table(x))))

out = list(dts2, dts4)
names(out) = c("Arm1", "Arm2")
write.xlsx(out, file = sprintf('ultrasound_report_dates_%s.xlsx', Sys.Date()), row.names = T)
# 