#missing events records
setwd('~/Documents/ptbi/rwanda/monthly_report/')
library(openxlsx)
df = read.xlsx("data/MissingEventsRecords19June2019.xlsx")

v2_enroll = read.csv("data/REDCap V2 Data, Exported by EVENT (2Apr2019)/1. Enrollment/V2PTBiRwandaCohort-EnrollmentInfoOnly_DATA_2019-04-02_1526.csv")
v2_enroll$dhc = substr(v2_enroll$study_id_full, 1, 3)
v2_anc1_a13 = read.csv("data/REDCap V2 Data, Exported by EVENT (2Apr2019)/2. ANC1/Arms 1, 3 (no US & UPT)/V2PTBiRwandaCohort-ANC1EventArms13_DATA_2019-04-02_1530.csv")
v2_anc1_a13$dhc = substr(v2_anc1_a13$study_id_anc, 1, 3)

v2_anc1_a24 = read.csv('data/REDCap V2 Data, Exported by EVENT (2Apr2019)/2. ANC1/Arms 2, 4 (with US & UPT)/V2PTBiRwandaCohort-ANC1EventArms24_DATA_2019-04-02_1532.csv')
v2_anc1_a24$dhc = substr(v2_anc1_a24$study_id_anc, 1, 3)

v2_fu = read.csv('data/REDCap V2 Data, Exported by EVENT (2Apr2019)/3. ANC Follow-up Visits/V2PTBiRwandaCohort-ANCFollowupVisitsAll_DATA_2019-04-02_1539.csv')
v2_fu$dhc = substr(v2_fu$study_id_anc_fuvst, 1, 3)

###
##delivery but no enroll


v2_deliv = read.csv('data/REDCap V2 Data, Exported by EVENT (2Apr2019)/4. Delivery/V2PTBiRwandaCohort-DeliveryEventAllArms_DATA_2019-04-02_1544.csv')
v2_deliv$dhc = substr(v2_deliv$study_id_mat, 1, 3)

v2_pnc = read.csv('data/REDCap V2 Data, Exported by EVENT (2Apr2019)/5. PNC/V2PTBiRwandaCohort-PostnatalEventArms14_DATA_2019-04-02_1548.csv')
v2_pnc$dhc = substr(v2_pnc$study_id_pnc, 1, 3)

sum(is.na(v2_pnc$dhc))
length(table(v2_pnc$dhc))
head(df)
dim(df)
library(dplyr) 


sort_by_dhc = function(df) {
  
  sum(!is.na(df$date_ancfuvst_2e))
  anc6 = df %>% filter(!is.na(date_ancfuvst_2e) & (is.na(date_ancfuvst_2d) | is.na(date_ancfuvst_2c) | 
                                                     is.na(date_ancfuvst_2b) | is.na(date_ancfuvst_2) | is.na(anc1date_anc)) )
  nrow(anc6)/sum(!is.na(df$date_ancfuvst_2e))
  
  anc5 = df %>% filter(is.na(date_ancfuvst_2e) & 
                         !is.na(date_ancfuvst_2d) & (is.na(date_ancfuvst_2c) | 
                                                       is.na(date_ancfuvst_2b) | is.na(date_ancfuvst_2) | is.na(anc1date_anc)) )
  nrow(anc5)/sum(!is.na(df$date_ancfuvst_2d))
  
  anc4 = df %>% filter(is.na(date_ancfuvst_2d) & 
                         !is.na(date_ancfuvst_2c) & (is.na(date_ancfuvst_2b) | is.na(date_ancfuvst_2) | is.na(anc1date_anc)) )
  nrow(anc4)/sum(!is.na(df$date_ancfuvst_2c))
  
  
  anc3 = df %>% filter(is.na(date_ancfuvst_2d) & is.na(date_ancfuvst_2c) & 
                         !is.na(date_ancfuvst_2b) & (is.na(date_ancfuvst_2) | is.na(anc1date_anc)) )
  nrow(anc3)/sum(!is.na(df$date_ancfuvst_2b))
  
  anc2 = df %>% filter(is.na(date_ancfuvst_2d) & is.na(date_ancfuvst_2c) & is.na(date_ancfuvst_2b) &
                         !is.na(date_ancfuvst_2) & (is.na(anc1date_anc)) )
  nrow(anc2)/sum(!is.na(df$date_ancfuvst_2))
  
  pnc = df %>% filter(is.na(date_delivery_mat | is.na(enroll_date)))
  delivery_enroll = df %>% filter(!is.na(date_delivery_mat) & is.na(enroll_date))
  delivery_enroll$dhc[is.na(delivery_enroll$dhc)] <- substr(delivery_enroll$study_id_full_new[is.na(delivery_enroll$dhc)], 1, 3)
  all_anc = list(anc2, anc3, anc4, anc5, anc6, pnc, other)
  #sum(sapply(all_anc, nrow))
  #dim(df)
  out = all_anc
  
}




match_records_anc1_v2(anc2, v2_anc1_a13, "other", 1)

match_records_anc1_v2(anc2, v2_anc1_a24, "other", 2)



#(anc2 %>% filter(record == output$record[2]) %>% select(study_id_full_new))

(anc2 %>% filter(study_id_full_new == output$study_id_anc[4]) %>% select(record, study_id_full_new, m_name))

anc2 %>% filter(study_id_full_new == out$study_id_anc[1])
event %>% filter(record_id == 18465 & dhc == 541)
head(out) 
dim(a24)

###  anc2[anc2$study_id_full_new == 541000247,]
length(ids_anc2)

head(anc2)
View(v2_anc1_a24[(!is.na(v2_anc1_a24$study_id_anc) & v2_anc1_a24$study_id_anc == 541000247),])

length(ids_anc2)
keep_ids_anc2 = v2_fu[(v2_fu$study_id_anc_fuvst %in% ids_anc2) & !is.na(v2_fu$study_id_anc_fuvst),]
library(dplyr)##be careful with mass package
keep_ids = keep_ids_anc2 %>% select(record_id, study_id_anc_fuvst, type_ancfuvst, date_ancfuvst)
View(anc2[!is.na(anc2$study_id_full_new) & anc2$study_id_full_new == 541000247,])
###


table(df$dhc)
by_fac = split(df, df$dhc)
x = sort_by_dhc(by_fac$`111`)
#if she had a pnc record, was she missing a maternity or enrollment record;  
#if she did not have a pnc record but had a maternity record was she missing an enrollment record

pnc = df %>% filter(is.na())