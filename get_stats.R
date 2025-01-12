################################################################################
## Input: 
# model (character): model name (column name)
# df (data.frame): dataset containing predicted probabilities for model evaluation and cross-validation folds
# k (character): fold to be evaluated
# seed (numeric): seed for bootstrap
# remove_ROC_AUC_Sex (logical): whether to substract performance from sex data 

## Output:
# c1 (data.frame): data frame including:
# - areas under the performance curves (ROC_AUC, PR_AUC, NPV_Spec_AUC)
# - sample-sizes within fold, complete and for cases (n, n_ID)
################################################################################
get_stats <- function(model, dataset, k, outcome, seed, remove_ROC_AUC_Sex=TRUE, remove_ROC_AUC_Dev=FALSE){
  dataset$outcome <- dataset[,outcome]
  dataset$probability <- dataset[,model]
  dataset$probability_sex <- dataset[,paste0('mod_sex')]
  
  # For bootstrap 
  set.seed(seed)
  n <- nrow(dataset)
  index <- sample(1:n, n, replace = TRUE)
  dataset <- dataset[index,]
  
  # Compute areas under the curves
  if(sum(na.omit(dataset$probability))!=0){
    n <- length(which(!is.na(dataset[,paste0('',model)])))
    n_ID <- length(intersect(which(!is.na(dataset[,paste0('',model)])), which(dataset$outcome==1)))
    g <- prediction(dataset$probability[!is.na(dataset$probability)], dataset$ID_binary[!is.na(dataset$probability)])
    g_sex <- prediction(dataset$probability_sex[!is.na(dataset$probability_sex)], dataset$ID_binary[!is.na(dataset$probability_sex)])
    
    # Area under the ROC curve
    if(remove_ROC_AUC_Sex==TRUE){
      ROC_AUC <- performance(g, "auc")@y.values[[1]] - performance(g_sex, "auc")@y.values[[1]] + 0.5
    }else{
      ROC_AUC <- performance(g, "auc")@y.values[[1]]
    }
    
    # Area under the PPV-sensitivity curve
    PR_AUC <- performance(g, "aucpr")@y.values[[1]]
    
    # Area under the NPV-specificity curve
    dataset$inv_outcome <- factor(ifelse(dataset$outcome==1, 0, 1))
    dataset$inv_probability <- 1 - dataset$probability
    g <- prediction(dataset$inv_probability[!is.na(dataset$inv_probability)], dataset$inv_outcome[!is.na(dataset$inv_probability)])
    NPV_Spec_AUC <- performance(g, "aucpr")@y.values[[1]]
    
  }else{
    ROC_AUC <- NA
    PR_AUC <- NA
    NPV_Spec_AUC <- NA
    n_ID <- NA
    n <- NA
  }
  c1 <- data.frame(k, model, ROC_AUC, PR_AUC, NPV_Spec_AUC, n, n_ID)
  return(c1)
}