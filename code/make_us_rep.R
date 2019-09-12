setwd('~/Documents/ptbi/rwanda/monthly_report/')

#hc1  = read.csv('ForAlejandraRestructuresCSVs/Cor-unum - 436_1538130822891ExcludesWrongHCWrongArmRestructured.csv')
#table(hc1$studyid_bugesera)
#compl = apply(hc1, 2, function(x) mean(!is.na(x)))
source('code/visit completion.R')
source('code/new_montly_report.R')
id_arm = read.csv('code/RW_Study ID.csv')
library(imputeTS)
files = list.files(path = "data/ForAlejandra", pattern = "*.csv", full.names = TRUE, recursive = FALSE)

files = list.files(path = "data/RestructuredV4_Thru8Ap2019/", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
file_names = list.files(path = "data/RestructuredV4_Thru8Ap2019/", pattern = "*.csv", full.names = FALSE, recursive = FALSE)


files = list.files(path = "data/AleV423May2019/", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
file_names = list.files(path = "data/AleV423May2019/", pattern = "*.csv", full.names = FALSE, recursive = FALSE)


out = lapply(files, read.csv)
fac_by_number = sapply(strsplit(file_names, "\\D+"), function(x) x[[2]])
names(out) = id_arm$HC[match(as.numeric(fac_by_number), table = id_arm$DHC)]
lapply(out, nrow)



A1 = c(333, 436, 544, 120, 110, 545, 111, 224, 221)
A2 = c(435, 331, 541, 115, 113, 539, 226, 329, 332)

A3 = c(330, 438, 540, 117, 116, 542, 112, 328, 223)
A4 = c(437, 327, 543, 114, 119, 334, 118, 222, 225)

arm1 = out[fac_by_number %in% A1]
arm2 = out[fac_by_number %in% A2]
arm3 = out[fac_by_number %in% A3]
arm4 = out[fac_by_number %in% A4]


out2 = lapply(arm2, function(g) make_us_rep(g, 2, "2019-03-22"))
lapply(out2, dim)
out4 = lapply(arm4, function(g) make_us_rep(g, 4, "2019-03-22"))
#lapply(out4, dim)


o2 = do.call(rbind, out2)
o4 = do.call(rbind, out4)

library(openxlsx)
final_out = list(o2, o4)
names(final_out) = paste("Arm", c(2,4))
write.xlsx(final_out, sprintf("us_rep_%s.xlsx", Sys.Date()), row.names = T)

out2 = t(do.call(rbind, out2))
out4 = t(do.call(rbind, out4))

us_exams2 = sum(o2$us_received, na.rm = T)
us_exams4 = sum(o4$us_received, na.rm = T)

make_prop = function(a, b){
  out = sprintf("%s / %s = %s%%", a, b, round((a/b), 2) * 100)
  return(out)
  
}
make_prop(us_exams2, nrow(o2))
make_prop(us_exams4, nrow(o4))
#make_prop(sum(full_data$us_received, na.rm = T), 
 #         nrow(full_data))
mean(us_exams2$min_us, na.rm = T)

get_summary(o2)
get_summary(o4)
full_data = rbind(o2, o4)# 
get_summary = function(full_data) {

  minut = mean(full_data$min_us, na.rm = T)
  ga = mean(full_data$ga_at_us, na.rm = T)
  #pct_lt = mean(full_data$us_lt_22wks, na.rm = T) 
  pct_lt = mean(full_data$us_lt_14wks, na.rm = T) 
  return(c(minut, ga, pct_lt))
}

# #col_names = colnames(out4$`327`)
# 
# library(openxlsx)
# final_out = list(fin1, fin2, fin3, fin4)
# 
# #final_out = list((out1), (out2), (out3), (out4))
# 
# names(final_out) = paste("Arm", 1:4)
# write.xlsx(final_out, sprintf("report_delivery_rates_%s.xlsx", Sys.Date()), row.names = T)


colnames(out1) = id_arm$HC[match(as.numeric(colnames(out1)), table = id_arm$DHC)]

colnames(out2) = id_arm$HC[match(as.numeric(colnames(out2)), table = id_arm$DHC)]

colnames(out3) = id_arm$HC[match(as.numeric(colnames(out3)), table = id_arm$DHC)]

colnames(out4) = id_arm$HC[match(as.numeric(colnames(out4)), table = id_arm$DHC)]

row.names(out1) = row_names
row.names(out2) = row_names
row.names(out3) = row_names
row.names(out4) = row_names
out1
library(openxlsx)


#final_out = list((out1), (out2), (out3), (out4))

lapply(out, dim)
lapply(out, class)
out_full = do.call(rbind, out)

out_arm = split(out_full, out_full$arm) 

write.csv(out_arm[[1]], "long_form_clean1.csv")
write.csv(out_arm[[2]], "long_form_clean2.csv")
write.csv(out_arm[[3]], "long_form_clean3.csv")
write.csv(out_arm[[4]], "long_form_clean4.csv")

out1 = sapply(arm1, function(x) make_anc_rt(x, 1, "2018-11-22"))
out2 = sapply(arm2, function(g) make_anc_rt(g, 2, "2018-11-22"))
out3 = sapply(arm3, function(g) make_anc_rt(g, 3, "2018-11-22"))
out3[[2]] = out3[[2]][1:4]
out3
out3 = do.call(cbind, out3)
out4 = sapply(arm4, function(g) make_anc_rt(g, 4, "2018-11-22"))

do.call(rbind, out3)
colnames(out1) = id_arm$HC[match(as.numeric(colnames(out1)), table = id_arm$DHC)]

colnames(out2) = id_arm$HC[match(as.numeric(colnames(out2)), table = id_arm$DHC)]

colnames(out3) = id_arm$HC[match(as.numeric(colnames(out3)), table = id_arm$DHC)]

colnames(out4) = id_arm$HC[match(as.numeric(colnames(out4)), table = id_arm$DHC)]

final_out = list(out1, out2, out3, out4)

#final_out = list((out1), (out2), (out3), (out4))
library(openxlsx)
names(final_out) = paste("Arm", 1:4)
write.xlsx(final_out, sprintf("anc_report_%s.xlsx", Sys.Date()), row.names = T)
