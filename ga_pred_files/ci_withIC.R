
ci.cvAUC_withIC <- function(predictions, labels, label.ordering = NULL, folds = NULL, confidence = 0.95) {
  
  # Pre-process the input
  clean <- cvAUC:::.process_input(predictions = predictions, labels = labels, 
                                  label.ordering = label.ordering, folds = folds,
                                  ids = NULL, confidence = confidence)
  
  predictions <- clean$predictions  # Length-V list of predicted values
  labels <- clean$labels  # Length-V list of true labels
  pos <- levels(labels[[1]])[[2]]  # Positive class label
  neg <- levels(labels[[1]])[[1]]  # Negative class label
  n_obs <- length(unlist(labels))  # Number of observations
  
  # Inverse probability weights across entire data set
  w1 <- 1/(sum(unlist(labels) == pos)/n_obs)  # Inverse weights for positive class
  w0 <- 1/(sum(unlist(labels) == neg)/n_obs)  # Inverse weights for negative class
  
  # This is required to cleanly get past R CMD CHECK
  # https://stackoverflow.com/questions/8096313/no-visible-binding-for-global-variable-note-in-r-cmd-check
  # pred <- label <- NULL
  # fracNegLabelsWithSmallerPreds <- fracPosLabelsWithLargerPreds <- icVal <- NULL  
  
  .IC <- function(fold_preds, fold_labels, pos, neg, w1, w0) {
    n_rows <- length(fold_labels)
    n_pos <- sum(fold_labels == pos)
    n_neg <- n_rows - n_pos
    auc <- cvAUC:::AUC(fold_preds, fold_labels)
    DT <- data.table(pred = fold_preds, label = fold_labels)
    DT <- DT[order(pred, -xtfrm(label))]
    DT[, `:=`(fracNegLabelsWithSmallerPreds, cumsum(label == 
                                                      neg)/n_neg)]
    DT <- DT[order(-pred, label)]
    DT[, `:=`(fracPosLabelsWithLargerPreds, cumsum(label == 
                                                     pos)/n_pos)]
    DT[, `:=`(icVal, ifelse(label == pos, w1 * (fracNegLabelsWithSmallerPreds - 
                                                  auc), w0 * (fracPosLabelsWithLargerPreds - auc)))]
    return(DT$icVal)
  }
  
  icOut <- mapply(FUN = .IC, SIMPLIFY = FALSE, fold_preds = predictions, 
                  fold_labels = labels, MoreArgs = list(pos = pos, neg = neg, w1 = w1, w0 = w0))
  ic <- rep(NA, n_obs)
  ic[unlist(folds)] <- unlist(icOut)
  # Estimated variance
  sighat2 <- mean(unlist(lapply(icOut, function(x){mean(x^2)})))
  se <- sqrt(sighat2/n_obs)  
  cvauc <- cvAUC::cvAUC(predictions, labels)$cvAUC
  z <- qnorm(confidence + (1 - confidence)/2)
  ci_cvauc <- c(cvauc - (z * se), cvauc + (z * se))
  ci_cvauc[1] <- ifelse(ci_cvauc[1] < 0, 0, ci_cvauc[1])  #Truncate CI at [0,1]
  ci_cvauc[2] <- ifelse(ci_cvauc[2] > 1, 1, ci_cvauc[2]) 
  
  return(list(cvAUC = cvauc, se = se, ci = ci_cvauc, confidence = confidence, ic = ic))
}

#' .getPredictions
#' 
#' This function calls the specified \code{learner} over each fold and
#' returns a list of cross-validated predictions and model fits. 
#' 

.getPredictions <- function(learner, Y, X, V, folds, returnFits, parallel){
  
  .doFit <- function(x, tmpX, Y, folds, learner){
    out <- do.call(learner, args=list(Y=Y[-folds[[x]]], X=tmpX[-folds[[x]],,drop=FALSE], newX=tmpX[folds[[x]],,drop=FALSE]))
    return(out)
  }
  
  if(parallel){
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    predFitList <- foreach(v = 1:length(folds), .export=learner) %dopar% 
      .doFit(v, tmpX = X, Y = Y, folds = folds, learner = learner)
    stopCluster(cl)
  }else{
    predFitList <- lapply(split(seq(V),factor(seq(V))),FUN=.doFit, tmpX = X, Y=Y, folds = folds, learner = learner)
  }
  
  # separate predictions from model fits
  predList <- lapply(predFitList, function(x){x$pred})
  if(returnFits){
    fitList <- lapply(predFitList, function(x){x$fit})
  }else{
    fitList <- NULL
  }
  # re-order predictions
  tmp <- unlist(predList)
  pred <- rep(NA, length(tmp))
  pred[unlist(folds)] <- tmp
  
  # return results
  return(list(pred=pred,fit=fitList))
}
