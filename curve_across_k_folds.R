################################################################################
## Input: 
# df (data.frame): dataset containing predicted probabilities for model evaluation and cross-validation folds
# model_list (list): model names (column names) to be assessed across different folds
# type (character): performance curve to be computed
#   - "ROC": ROC curve
#   - "PR": precision-recall curve 
#   - "npv_specificity": NPV - specificity curve

## Output:
# data_fit_full (data.frame): estimated performance curve values (mi) 
#   with 95% confidence intervals (lo, hi)
################################################################################
curve_across_k_folds <- function(df, model_list, type){
  
  # Compute the performance curve for each fold
  performance_curve <- c()
  for(model in model_list){
    for(k in levels(factor(df$fold))){
      stats <- get_performance_curve(model, df[df$fold==k,], outcome='ID_binary', fold=k, type=type)
      performance_curve <- rbind(performance_curve, stats)
    }
  }
  
  # Summarize the performance curve across folds
  quant <- seq(0.01, 1, 0.01)
  performance_curve_summary <- c()
  for(model in levels(factor(performance_curve$model))){
    for(threshold in quant){
      dat_subset <- performance_curve[performance_curve$model==model & performance_curve$x_measure>=threshold & performance_curve$x_measure<(threshold+0.01) & !(performance_curve$fold %in% c('SSC','MSSNG')),]
      summary <- data.frame(model=model,
                            y_measure=mean(dat_subset$y_measure),
                            y_measure_sd=sd(dat_subset$y_measure),
                            x_measure=threshold
      )
      performance_curve_summary <- rbind(performance_curve_summary, summary)
    }
  }
  
  # Compute a non-linear regression
  performance_curve_summary <- na.omit(performance_curve_summary)
  data_fit_full <- c()
  for(model in (levels(factor(performance_curve_summary$model)))){
    data_fit <- performance_curve_summary[performance_curve_summary$model==model,]
    assign("data_fit", data_fit, envir = .GlobalEnv)
    mod <- gamlss(y_measure ~ lo(~x_measure, span = 0.5), data=data_fit)
    mat <- centiles.pred(mod, xname="x_measure", xvalues=data_fit$x_measure, data=data_fit, cent = c(2.5, 50, 97.5))
    data_fit$lo <- mat[,colnames(mat)=='2.5']
    data_fit$mi <- mat[,colnames(mat)=='50']
    data_fit$hi <- mat[,colnames(mat)=='97.5']
    data_fit_full <- rbind(data_fit_full, data_fit)
  }
  
  return(data_fit_full)
}