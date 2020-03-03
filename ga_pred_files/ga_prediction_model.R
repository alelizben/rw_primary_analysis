setwd("~/Documents/ptbi/ga_prediction")

source('code/ci_withIC.R')
source('code/functions.R')
library(cvAUC.plus)
#load('data/no_duplicates_arms_keepall_old_2018-08-28.Rdata')

source('code/new_montly_report.R')
id_arm = read.csv('data/RW_Study ID.csv')
library(imputeTS)
#files = list.files(path = "../rwanda/monthly_report/data/ForAlejandra", pattern = "*.csv", full.names = TRUE, recursive = FALSE)

files_read = list.files(path = "../rwanda/monthly_report/data/AleV423May2019/", pattern = "*.csv", full.names = T, recursive = FALSE)
files = list.files(path = "../rwanda/monthly_report/data/AleV423May2019/", pattern = "*.csv", full.names = F, recursive = FALSE)


files_read = list.files(path = "../rwanda/monthly_report/data/RestructuredV4_Thru8Ap2019/", pattern = "*.csv", full.names = T, recursive = FALSE)
files = list.files(path = "../rwanda/monthly_report/data/RestructuredV4_Thru8Ap2019/", pattern = "*.csv", full.names = F, recursive = FALSE)

out = lapply(files_read, read.csv)
names(out) = sapply(strsplit(files, "\\D+"), function(x) x[[2]])
lapply(out, nrow)

A1 = c(333, 436, 544, 120, 110, 545, 111, 224, 221)
A2 = c(435, 331, 541, 115, 113, 539, 226, 329, 332)

A3 = c(330, 438, 540, 117, 116, 542, 112, 328, 223)
A4 = c(437, 327, 543, 114, 119, 334, 118, 222, 225)

arm1 = out[names(out) %in% A1]
arm2 = out[names(out) %in% A2]
arm3 = out[names(out) %in% A3]
arm4 = out[names(out) %in% A4]

a2_us = lapply(arm2, function(x) get_us(x, 2))
a4_us = lapply(arm4, function(x) get_us(x, 2))

a2out = do.call(rbind, a2_us)
a4out = do.call(rbind, a4_us)
##variable complete (sort them by complete = 1, incomplete 2)
trainb = make_data(a2out, a4out, "binomial", wk_thresh = 19)
traing = make_data(a2out, a4out, "gaussian", wk_thres = 19)
dim(trainb)
dim(traing)

#write.csv(trainb, "lmp_sample_data_binomial.csv")
#write.csv(traing, "lmp_sample_data_gauss.csv")
summary(traing$y)
mean(trainb$y)
summary(traing$y)

train = make_x(trainb)
train$y = trainb$y
summary(train)
train = make_x(traing)
train$y = traing$y
table(X$fuel)
#X$y = trainb$y
#X$y = traing$y
write.csv(train, file = sprintf("sample_ga_data_binomial_%s.csv", Sys.Date()))
write.csv(train, file = sprintf("sample_ga_data_gaussian_%s.csv", Sys.Date()))
#ga_at_delivery_by_fh_anc1
#dhc

summary(X)
names(X)
plot(X$y)

#lib.g = list("SL.bartMachine", "SL.bayesglm", "SL.xgboost", "SL.nnet")
#lib.b = list("SL.bartMachine2", "SL.bayesglm", "SL.xgboost", "SL.nnet")

#lib.g = list("SL.bayesglm", "SL.xgboost", "SL.nnet")
lib.g = list( "SL.xgboost", "SL.ranger", "SL.glmnet", "SL.bartMachine")
lib.b = list( "SL.xgboost", "SL.randomForest", "SL.ranger", "SL.glmnet", "SL.bartMachine2")

library(SuperLearner)
start_time = Sys.time()
#gaussian model
out = CV.SuperLearner(Y = traing$y, X = X, family = gaussian(), 
                      cvControl = list(V = 5, shuffle = T, validRows = NULL),
                      innerCvControl = list(list(V = 5)),
                       method = "method.NNLS", SL.library = lib.g)

out10 = CV.SuperLearner(Y = trainb$y, X = X, family = binomial(), 
                      cvControl = list(V = 5, shuffle = T, validRows = NULL, stratifyCV = T),
                      innerCvControl = list(list(V = 5)),
                      method = "method.NNloglik", SL.library = lib.b)

out = out10
train = trainb
get_auc = function(out, train, folds) {
  yhat = out$SL.predict
  pred = lapply(out$folds, function(x) yhat[x])
  labs = lapply(out$folds, function(x) train$y[x])
  
  plot_val = cvAUC(pred, labs, label.ordering = NULL, folds = NULL)
  val = ci.cvAUC_withIC(yhat, train$y, label.ordering = NULL, folds = out$folds)
  val
  jpeg(sprintf('%s_fold_plot_%s.jpg', folds, Sys.Date()), width = 1600, height = 1200)

  plot(plot_val$perf, col="grey82", lty=3, main=sprintf("%s-fold CV ROC", folds))
  plot(plot_val$perf, col="red", avg="vertical", add=TRUE, cex.lab=1.5, cex.axis=3, cex.main=3)
  
  abline(0, 1, lty = 2)
  #abline(v = 0.4)
  abline(v = 0.3)
  #abline(v = 0.2)
  abline(h = 0.8)
  #abline(h = 0.9)
  dev.off()

  #Plot CV AUC
  p <- pnorm(-abs((val$cvAUC - 0.5)/val$se))
  return(c(val$cvAUC, p, val$ci)) 
}

#get_auc(out5_binom, trainb, 5)

#get_auc(out5_nnet, trainb, 5)
get_auc(out10, trainb, 5)

gaus_perf = function(out) {
  p1 = sqrt(mean((out$SL.predict  - traing$y)^2))
  p2 = median(abs(out$SL.predict  - traing$y))
  print(summary(abs(out$SL.predict  - traing$y)))
  return(c(p1, p2))
  
}

gaus_perf(out)

wks_off = (out$SL.predict  - (traing$y))
summary(wks_off)
hist(wks_off)
mean(wks_off <= 2)
plot(traing$y, out$SL.predict, xlim = c(10,45), ylim = c(10,45), 
     main = "Recorded vs. Predicted Gestational Age at Delivery (weeks)", 
     xlab = "Recorded GA (US and LMP Based)", ylab = "Predicted")
#plot(traing$y, out5_gaus_cc_ls$SL.predict, xlim = c(0,45), ylim = c(0,45))
abline(0,1)
abline(v = 37, lty = 2)
#abline(v = 0.2)
abline(h = 37, lty = 2)

X1 = subset(train, select = -c(y, ga_us, study_id, date_del))
X2 = subset(train, select = -c(y, ga_us, study_id, date_del, days_from_anc1_to_del, 
                               ga_at_deliv_lmp, ga_at_delivery_by_fh_anc1))
X3 = subset(train, select = c(dhc, ga_at_delivery_by_ga_anc1, ga_at_delivery_by_edd_anc1,
                              ga_at_deliv_lmp, ga_at_delivery_by_fh_anc1))


install.packages('corrplot')
source("http://www.sthda.com/upload/rquery_cormat.r")

get_cor = function(x) {
  out = rquery.cormat(x)
  outlist = list(out$r, out$p)
  return(outlist)
}

X$y = train$y
get_cor(X[,-1])
summary(out$SL.predict)
library(cvAUC)

#results table


#### what is this stuff ###
69
801
sd = sample(1:1000, 1)
set.seed(801)
out5_nnet = CV.SuperLearner(Y = trainb$y, X = subset(X, select = -c(hh_smoker)), family = binomial(), 
                            cvControl = list(V = 5, shuffle = T, validRows = NULL),
                            innerCvControl = list(list(V = 5)),
                            method = "method.AUC", SL.library = lib.b)

sd = sample(1:1000, 1)
set.seed(831)
831
out5_gaus_nnet = CV.SuperLearner(Y = traing$y, X = X, family = gaussian(), 
                                 cvControl = list(V = 5, shuffle = T, validRows = NULL),
                                 innerCvControl = list(list(V = 5)),
                                 method = "method.NNLS2", SL.library = lib.g)

gaus_perf(out5_gaus_nnet)

out5_gaus_cc_ls = CV.SuperLearner(Y = traing$y, X = X, family = gaussian(), 
                                  cvControl = list(V = 5, shuffle = T, validRows = NULL),
                                  innerCvControl = list(list(V = 5)),
                                  method = "method.CC_LS", SL.library = lib.g)
gaus_perf(out5_gaus_cc_ls)





sort(apply(X[,-1], 2, function(x) AUC(as.vector(x), train$y, label.ordering = c(1, 0))))

out = out5_nnet


