################################################################################
## Input: 
# dat (data.frame): dataset containing predicted probabilities for model evaluation and cross-validation folds
# model_list (list): list of model name (column name) to be evaluated
# seed (numeric): seed for bootstrap
# remove_ROC_AUC_Sex (logical): whether to substract performance from sex data 

## Output:
# model_summary (data.frame): data frame including:
# - model names
# - mean areas under the performance curves (ROC_AUC, PR_AUC, NPV_Spec_AUC) across folds
# - their standard deviations (ROC_AUC_sd, PR_AUC_sd, NPV_Spec_AUC_sd) across folds
# - sample-size (n)
# - fold (k)
################################################################################
k_fold_summarize <- function(dat, model_list, seed, remove_ROC_AUC_Sex){
  model_eval <- evaluate_models(dat, model_list,'ID_binary', seed, remove_ROC_AUC_Sex)
  
  model_summary1 <- data.frame(model = names(with(model_eval[model_eval$k != 'SSC' & model_eval$k != 'MSSNG',], tapply(ROC_AUC, model, mean))),
                               ROC_AUC = with(model_eval[model_eval$k != 'SSC' & model_eval$k != 'MSSNG',], tapply(ROC_AUC, model, mean)),
                               ROC_AUC_sd = with(model_eval[model_eval$k != 'SSC' & model_eval$k != 'MSSNG',], tapply(ROC_AUC, model, sd)),
                               PR_AUC = with(model_eval[model_eval$k != 'SSC' & model_eval$k != 'MSSNG',], tapply(PR_AUC, model, mean)),
                               PR_AUC_sd = with(model_eval[model_eval$k != 'SSC' & model_eval$k != 'MSSNG',], tapply(PR_AUC, model, sd)),
                               NPV_Spec_AUC = with(model_eval[model_eval$k != 'SSC' & model_eval$k != 'MSSNG',], tapply(NPV_Spec_AUC, model, mean)),
                               NPV_Spec_AUC_sd = with(model_eval[model_eval$k != 'SSC' & model_eval$k != 'MSSNG',], tapply(NPV_Spec_AUC, model, sd)),
                               n = with(model_eval[model_eval$k != 'SSC' & model_eval$k != 'MSSNG',], tapply(n, model, sum)),
                               k = 'SPARK')
  
  model_summary2 <- data.frame(model = names(with(model_eval[model_eval$k == 'SSC',], tapply(ROC_AUC, model, mean))),
                               ROC_AUC = with(model_eval[model_eval$k == 'SSC',], tapply(ROC_AUC, model, mean)),
                               ROC_AUC_sd = with(model_eval[model_eval$k == 'SSC',], tapply(ROC_AUC, model, sd)),
                               PR_AUC = with(model_eval[model_eval$k == 'SSC',], tapply(PR_AUC, model, mean)),
                               PR_AUC_sd = with(model_eval[model_eval$k == 'SSC',], tapply(PR_AUC, model, sd)),
                               NPV_Spec_AUC = with(model_eval[model_eval$k == 'SSC',], tapply(NPV_Spec_AUC, model, mean)),
                               NPV_Spec_AUC_sd = with(model_eval[model_eval$k == 'SSC',], tapply(NPV_Spec_AUC, model, sd)),
                               n = with(model_eval[model_eval$k == 'SSC',], tapply(n, model, sum)),
                               k = 'SSC')
  
  model_summary3 <- data.frame(model = names(with(model_eval[model_eval$k == 'MSSNG',], tapply(ROC_AUC, model, mean))),
                               ROC_AUC = with(model_eval[model_eval$k == 'MSSNG',], tapply(ROC_AUC, model, mean)),
                               ROC_AUC_sd = with(model_eval[model_eval$k == 'MSSNG',], tapply(ROC_AUC, model, sd)),
                               PR_AUC = with(model_eval[model_eval$k == 'MSSNG',], tapply(PR_AUC, model, mean)),
                               PR_AUC_sd = with(model_eval[model_eval$k == 'MSSNG',], tapply(PR_AUC, model, sd)),
                               NPV_Spec_AUC = with(model_eval[model_eval$k == 'MSSNG',], tapply(NPV_Spec_AUC, model, mean)),
                               NPV_Spec_AUC_sd = with(model_eval[model_eval$k == 'MSSNG',], tapply(NPV_Spec_AUC, model, sd)),
                               n = with(model_eval[model_eval$k == 'MSSNG',], tapply(n, model, sum)),
                               k = 'MSSNG')
  
  model_summary <- rbind(model_summary1, model_summary2, model_summary3)
  return(model_summary)
}