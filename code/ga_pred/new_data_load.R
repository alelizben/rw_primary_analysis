setwd('~/Documents/ptbi/rwanda/monthly_report/')
setwd('~/Documents/ptbi/ga_prediction/')
#hc1  = read.csv('ForAlejandraRestructuresCSVs/Cor-unum - 436_1538130822891ExcludesWrongHCWrongArmRestructured.csv')
#table(hc1$studyid_bugesera)
#compl = apply(hc1, 2, function(x) mean(!is.na(x)))

id_arm = read.csv('data/RW_Study ID.csv')
library(imputeTS)
files = list.files(path = "data/FinalRestructureReCorrectedii", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
#source('~/Documents/PTBi/rwanda/redcap_query_functions.R')

out = lapply(files, read.csv)
names(out) = sapply(strsplit(files, "\\D+"), function(x) x[[2]])

# df = out[[which(names(out) == 113)]]
# df = out[[which(names(out) == 331)]]
df = out[[which(names(out) == 437)]]

A1 = c(333, 436, 544, 120, 110, 545, 111, 224, 221)
A2 = c(435, 331, 541, 115, 113, 539, 226, 329, 332)

A3 = c(330, 438, 540, 117, 116, 542, 112, 328, 223)
A4 = c(437, 327, 543, 114, 119, 334, 118, 222, 225)

arm1 = out[names(out) %in% A1]
arm2 = out[names(out) %in% A2]
arm3 = out[names(out) %in% A3]
arm4 = out[names(out) %in% A4]

#save(arm2, file = "arm2_facs.Rdata")
#save(arm4, file = "arm4_facs.Rdata")
load('data/arm2_facs.Rdata')
load('data/arm4_facs.Rdata')

lapply(arm2, function(x) make_us_rep(x, 2, "2018-10-29"))
lapply(arm4, function(x) make_us_rep(x, 4, "2018-10-29"))

source('~/Documents/PTBi/rwanda/monthly_report/code/new_montly_report.R')
source('~/Documents/PTBi/ga_prediction/code/updated_ultrasound.R')
source('~/Documents/PTBi/ga_prediction/code/functions.R')

a2_us = lapply(arm2, function(x) get_us(x, 2))
a4_us = lapply(arm4, function(x) get_us(x, 4))


lapply(a2_us, function(x) sum(is.na(x$study_id_anc)))
lapply(a2_us, function(x) sum(is.na(x$study_id_enroll)))

lapply(a4_us, function(x) sum(is.na(x$study_id_anc)))
lapply(a4_us, function(x) sum(is.na(x$study_id_enroll)))

a2out = do.call(rbind, a2_us)
a4out = do.call(rbind, a4_us)


trainb = make_data(a2out, a4out, "binomial")
traing = make_data(a2out, a4out, "gaussian")

X = make_x(trainb)
X = make_x(traing)

#X$y = trainb$y
#save(X, file = sprintf('code/sample_ga_data_%s.Rdata', Sys.Date()))
