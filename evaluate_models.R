################################################################################
## Input: 
# dat (data.frame): dataset containing predicted probabilities for model evaluation and cross-validation folds
# model_list (list): list of model name (column name) to be evaluated
# outcome (character): outcome (column name)
# seed (numeric): seed for bootstrap
# remove_ROC_AUC_Sex (logical): whether to substract performance from sex data 

## Output:
# model_eval (data.frame): data frame including:
# - areas under the performance curves (ROC_AUC, PR_AUC, NPV_Spec_AUC)
# - sample-sizes within fold, complete and for cases (n, n_ID)
# - sums of sample-sizes across fold, complete and for cases (sum_n, sum_n_ID)
################################################################################
evaluate_models <- function(dat, model_list, outcome, seed, remove_ROC_AUC_Sex){
  model_eval <- c()
  for(model in model_list){
    for(k in levels(as.factor(dat[,'fold']))){
      df <- dat[dat[,'fold']==k,]
      stats <- get_stats(model,df,k,outcome, seed, remove_ROC_AUC_Sex)
      model_eval <- rbind(model_eval, stats)
    }
  }
  
  # Sample-sizes
  model_eval$sum_n <- NA
  model_eval$sum_n[model_eval$k == 'SSC' | model_eval$k == 'MSSNG'] <- model_eval$n[model_eval$k == 'SSC' | model_eval$k == 'MSSNG']
  model_eval$sum_n_ID <- NA
  model_eval$sum_n_ID[model_eval$k == 'SSC' | model_eval$k == 'MSSNG'] <- model_eval$n_ID[model_eval$k == 'SSC' | model_eval$k == 'MSSNG']
  for(i in levels(factor(model_eval$model))){
    model_eval$sum_n[model_eval$model==i & model_eval$k!='validation'] <- sum(model_eval$n[model_eval$model==i & model_eval$k != 'SSC' & model_eval$k != 'MSSNG'])
    model_eval$sum_n_ID[model_eval$model==i & model_eval$k!='validation'] <- sum(model_eval$n_ID[model_eval$model==i & model_eval$k != 'SSC' & model_eval$k != 'MSSNG'])
  }
  model_eval$model <- factor(model_eval$model, levels = model_list)
  return(model_eval)
}