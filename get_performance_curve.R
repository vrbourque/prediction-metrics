################################################################################
## Input: 
# model (character): model name (column name) to be evaluated
# dataset (data.frame): dataset containing predicted probabilities for model evaluation and cross-validation folds
# outcome (character): outcome name (column name)
# fold (character): cross-validation fold to be evaluated
# type (character): performance curve to be computed
#   - "ROC": ROC curve
#   - "PR": PPV - sensitivity (precision - recall) curve 
#   - "npv_specificity": NPV - specificity curve

## Output:
# c1 (data.frame): dataframe with performance curve values:
# - x_measure: x-axis measure for drawing the curve
# - y_measure: y-axis measure for drawing the curve
# - alpha: threshold
################################################################################
get_performance_curve <- function(model, dataset, outcome='ID_binary', fold, type){
  dataset$outcome <- dataset[,outcome]
  dataset$probability <- dataset[,model]
  g <- prediction(dataset$probability[!is.na(dataset$probability)], dataset$outcome[!is.na(dataset$probability)])
  if(type=='PR'){
    prf <- performance(g, measure = "ppv", x.measure = "tpr") # tpr = sensitivity
  }else if(type=='ROC'){
    prf <- performance(g, measure = "tpr", x.measure = "fpr") 
  }else if(type=='npv_specificity'){
    prf <- performance(g, measure = "npv", x.measure = "tnr") # tnr = specificity
  }
  c1 <- data.frame(x_measure=unlist(prf@x.values), 
                   y_measure=unlist(prf@y.values),
                   alpha=unlist(prf@alpha.values),
                   model=model,
                   fold=fold)
  return(c1)
}