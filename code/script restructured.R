setwd('~/Documents/ptbi/rwanda/monthly_report/')
library(dplyr)

#hc1  = read.csv('ForAlejandraRestructuresCSVs/Cor-unum - 436_1538130822891ExcludesWrongHCWrongArmRestructured.csv')
#table(hc1$studyid_bugesera)
#compl = apply(hc1, 2, function(x) mean(!is.na(x)))
source('code/visit completion.R')
id_arm = read.csv('code/RW_Study ID.csv')
library(imputeTS)
#files = list.files(path = "data/ForAlejandra", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
#test = read.csv('~/Downloads/Gashora111NoWrongRestructured.csv')
#sum(duplicated(test$study_id_full_new))
files = list.files(path = "data/RestructuredThruFeb19/", pattern = "*.csv", full.names = F, recursive = FALSE)
files_read = list.files(path = "data/RestructuredThruFeb19/", pattern = "*.csv", full.names = T, recursive = FALSE)

files = list.files(path = "data/AleV423May2019/", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
file_names = list.files(path = "data/AleV423May2019/", pattern = "*.csv", full.names = FALSE, recursive = FALSE)

 ##

out = lapply(files, read.csv)
num_out = sapply(strsplit(file_names, "\\D+"), function(x) x[[2]])
num_out

names(out) = id_arm$HC[match(num_out, id_arm$DHC)]
##
##names(out) = id_arm$HC[match(as.numeric(names(out)), table = id_arm$DHC)]
#duplicats = sapply(out, function(x) sum(duplicated(x$study_id_full_new)))
#write.csv(duplicats, file = sprintf("duplicates_%s.csv", Sys.Date()))
lapply(out, nrow)


A1 = c(333, 436, 544, 120, 110, 545, 111, 224, 221)
A2 = c(435, 331, 541, 115, 113, 539, 226, 329, 332)

A3 = c(330, 438, 540, 117, 116, 542, 112, 328, 223)
A4 = c(437, 327, 543, 114, 119, 334, 118, 222, 225)

ctrl = out[num_out %in% A1 | num_out %in% A2]
interv = out[num_out %in% A3 | num_out %in% A4]
names(ctrl)
names(interv)

arm1 = out[num_out %in% A1]
arm2 = out[num_out %in% A2]
arm3 = out[num_out %in% A3]
arm4 = out[num_out %in% A4]


#count = list(index_nonempty1, index_nonempty2, index_nonempty3, index_nonempty4)
#list_count = lapply(count, function(x) paste(names(x), x)) 
#table_counts = do.call(rbind, list_count)
#rownames(table_counts) = c("arm1", "arm2", "arm3", "arm4")
#table_counts
#write.csv(table_counts, file = "counts.csv")

arm1 = out[names(out) %in% A1]
arm2 = out[names(out) %in% A2]
arm3 = out[names(out) %in% A3]
arm4 = out[names(out) %in% A4]

# 
# for (i in c(1:length(out))) {
# 
#   fr = event_compl(out[[i]], "2018-12-31", lookup = NULL)
#   print(dim(fr))
#   name = id_arm$HC[id_arm$DHC == names(out)[i]]
#   print(name)
#   write.csv(fr, file = sprintf('hc_visits_feb/visits_compl_%s_%s.csv', name, Sys.Date()))
# }

out1 = lapply(arm1, function(g) make_table(g, 1, "2019-02-18"))
row_names = row.names(out1[[2]])
out1 = sapply(arm1, function(x) make_table(x, 1, "2019-02-18"))
out2 = sapply(arm2, function(g) make_table(g, 1, "2019-02-18"))
out3 = sapply(arm3, function(g) make_table(g, 1, "2019-02-18"))
out4 = sapply(arm4, function(g) make_table(g, 1, "2019-02-18"))

out1 = lapply(arm1, function(g) anc4(g, 1, "2019-02-18"))
row_names = row.names(out1[[2]])
out1 = sapply(arm1, function(x) anc4(x, 1, "2019-02-18"))
out2 = sapply(arm2, function(g) anc4(g, 1, "2019-02-18"))
out3 = sapply(arm3, function(g) anc4(g, 1, "2019-02-18"))
out4 = sapply(arm4, function(g) anc4(g, 1, "2019-02-18"))
# 
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
final_out = list(out1, out2, out3, out4)

names(final_out) = paste("Arm", 1:4)
write.xlsx(final_out, sprintf("monthly_report_direct_data_%s.xlsx", Sys.Date()), row.names = T)
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
