setwd("~/Documents/ptbi/rwanda/monthly_report/")

## list of recovered matches
deliver_no_enroll = read.csv('data/v2_matches_delivery_no_enroll.csv')
a13_anc2_anc1 = read.csv('data/arm1_3_v2_matches_missing_anc1.csv')
a24_anc2_anc1 = read.csv('data/arm2_4_v2_matches_missing_anc1.csv')
View(a24_anc2_anc1)
## list of recovered matches

dim(deliver_no_enroll)
head(deliver_no_enroll)
library(dplyr)

get_matched_recs = function(x) {
  out = x %>% filter(Assessment == "match")

  out
}

del_no_enroll = get_matched_recs(deliver_no_enroll)
del_no_enroll$uniqueid = paste(del_no_enroll$record, del_no_enroll$study_id_full)
#View(del_no_enroll)
#View(deliver_no_enroll)
a13_anc2 = get_matched_recs(a13_anc2_anc1)
a13_anc2$uniqueid = paste(a13_anc2$record, a13_anc2$study_id_anc)

a24_anc2 = get_matched_recs(a24_anc2_anc1)
a24_anc2$uniqueid = paste(a24_anc2$record, a24_anc2$study_id_anc)
View(a24_anc2)
## pull from v2 data ## 
v2_enroll = read.csv("data/REDCap V2 Data, Exported by EVENT (2Apr2019)/1. Enrollment/V2PTBiRwandaCohort-EnrollmentInfoOnly_DATA_2019-04-02_1526.csv")
v2_enroll$dhc = substr(v2_enroll$study_id_full, 1, 3)
v2_enroll$uniqueid = paste(v2_enroll$dhc, v2_enroll$record_id, v2_enroll$study_id_full)

v2_anc1_a13 = read.csv("data/REDCap V2 Data, Exported by EVENT (2Apr2019)/2. ANC1/Arms 1, 3 (no US & UPT)/V2PTBiRwandaCohort-ANC1EventArms13_DATA_2019-04-02_1530.csv")
v2_anc1_a13$dhc = substr(v2_anc1_a13$study_id_anc, 1, 3)
v2_anc1_a13$uniqueid = paste(v2_anc1_a13$dhc, v2_anc1_a13$record_id, v2_anc1_a13$study_id_anc)

v2_anc1_a24 = read.csv('data/REDCap V2 Data, Exported by EVENT (2Apr2019)/2. ANC1/Arms 2, 4 (with US & UPT)/V2PTBiRwandaCohort-ANC1EventArms24_DATA_2019-04-02_1532.csv')
v2_anc1_a24$dhc = substr(v2_anc1_a24$study_id_anc, 1, 3)
v2_anc1_a24$uniqueid = paste(v2_anc1_a24$dhc, v2_anc1_a24$record_id)


#pull_enroll = v2_enroll %>% filter(study_id_full %in% del_no_enroll$study_id_full & record %in% del_no_enroll$record)
pull_enroll = v2_enroll %>% filter(uniqueid %in% del_no_enroll$uniqueid)
pull_enroll = pull_enroll[order(pull_enroll$dhc),]
View(pull_enroll)
dim(pull_enroll)
dim(del_no_enroll)
#write.csv(pull_enroll, file = "v2_enrollment_matches.csv")

pull_a13_anc1 = v2_anc1_a13 %>% filter(uniqueid %in% a13_anc2$uniqueid)
pull_a13_anc1 = pull_a13_anc1[order(pull_a13_anc1$dhc),]
pull_a13_anc1 = pull_a13_anc1[-c(4),]
View(pull_a13_anc1)
dim(pull_a13_anc1)
sum(a13_anc2$uniqueid %in% pull_a13_anc1$uniqueid)
dim(pull_a13_anc1)
#write.csv(pull_a13_anc1, file = "v2_anc1_matches_a13.csv")
sum(duplicated(pull_a13_anc1$uniqueid))
dim(a13_anc2)
sum(duplicated(a13_anc2$uniqueid))

###### resulted in 37 even though requested 36


pull_a24_anc1 = v2_anc1_a24 %>% filter(uniqueid %in% a24_anc2$record)
pull_a24_anc1 = pull_a24_anc1[order(pull_a24_anc1$dhc),]
table(a24_anc2$Assessment)

dim(pull_a24_anc1)
View(pull_a24_anc1)
sum(duplicated(pull_a24_anc1$uniqueid))
dim(a24_anc2)
paste(a24_anc2$study_id_anc, a24_anc2$record)
pull_a24_anc1$uniqueid
sum(duplicated(a24_anc2$uniqueid))
write.csv(pull_a24_anc1, file = "v2_anc1_matches_a24.csv")

