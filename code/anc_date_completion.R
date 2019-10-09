# table(df$NumberANCs)

completion = subset(df, select = c(anc_register_initial_visit_complete, anc_register_followup_visits_complete_2,
                                   anc_register_followup_visits_complete_2b, anc_register_followup_visits_complete_2c, anc_register_followup_visits_complete_2d))

#lapply(1:ncol(completion), function(x) table(completion[,x]))
complete = completion != 2 | is.na(completion)
#head(completion)
#head(complete)
num_incomplete = rowSums(complete)
#head(num_incomplete)
#head(completion)
num_anc = 6 - num_incomplete

##identifying those who gave birth by data item; creating dataset of those 
## nancy's output only included those who had anc1 < 24wks
## and had at least 2 anc visits
##delivery data avail
