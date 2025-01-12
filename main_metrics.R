# ==============================================================================
# title           : Prediction performance metrics
# description     : 
# author          : V.-R. Bourque
# date            : 2024-12
# version         : 1
# usage           : R version 4.4.1 (2024-06-14)
# notes           : example with simulated data
# ==============================================================================

# Load packages
rm(list = ls())
library(ROCR)
library(ggplot2)
library(gamlss)
library(ggrepel)
library(knitr)

# Define working variables
directory <- "~/Documents/prediction_in_autism"
B <- 100 # Number of bootstrap iterations 10000

# Load custom functions
setwd(paste0(directory, "/package_version/"))
source('get_stats.R')
source('evaluate_models.R')
source('get_performance_curve.R')
source('k_fold_summarize.R')
source('curve_across_k_folds.R')

# Load dataset
setwd(paste0(directory, "/package_version/data"))
df <- read.csv(file='simulated_data_predictions.csv')
df$fold[df$fold=='validation'] = df$Cohort[df$fold=='validation']

# Print
kable(head(df))

# List models
model_list <- c('mod_sex.PGS',
                'mod_sex.PGS.DEL.DUP',
                'mod_sex.PGS.DEL.DUP.LOF.MIS'
                )

# Bootstrap to obtain confidence intervals
boot_res <- c()
for(iteration in 1:B){
  seed = iteration
  k_fold_res <- k_fold_summarize(df, model_list, seed, remove_ROC_AUC_Sex=TRUE) 
  boot_res <- rbind(boot_res, k_fold_res)
}

# Summarize across bootstrap iterations
create_model_summary <- function(df, group_name) {
  data.frame(
    model = names(with(df, tapply(ROC_AUC, model, mean))),
    ROC_AUC = with(df, tapply(ROC_AUC, model, mean)),
    ROC_AUC_sd = with(df, tapply(ROC_AUC, model, sd)),
    PR_AUC = with(df, tapply(PR_AUC, model, mean)),
    PR_AUC_sd = with(df, tapply(PR_AUC, model, sd)),
    NPV_Spec_AUC = with(df, tapply(NPV_Spec_AUC, model, mean)),
    NPV_Spec_AUC_sd = with(df, tapply(NPV_Spec_AUC, model, sd)),
    n = with(df, tapply(n, model, median)),
    k = group_name
  )
}

# Apply to each cohort
model_summary1 <- create_model_summary(
  boot_res[boot_res$k != 'SSC' & boot_res$k != 'MSSNG', ], "SPARK"
)

model_summary2 <- create_model_summary(
  boot_res[boot_res$k == 'SSC', ], "SSC"
)

model_summary3 <- create_model_summary(
  boot_res[boot_res$k == 'MSSNG', ], "MSSNG"
)

model_summary <- rbind(model_summary1, model_summary2, model_summary3)
rownames(model_summary) <- NULL

# Print
kable(model_summary)

# Strings for graphical output
model_summary$ROC_AUC_string <- paste0(round((model_summary$ROC_AUC+0.5),3), " (95%CI: ", round((model_summary$ROC_AUC-1.96*model_summary$ROC_AUC_sd+0.5),3), ", ", round((model_summary$ROC_AUC+1.96*model_summary$ROC_AUC_sd+0.5),3), ')')
model_summary$PR_AUC_string <- paste0(round((model_summary$PR_AUC),3), " (95%CI: ", round((model_summary$PR_AUC-1.96*model_summary$PR_AUC_sd),3), ", ", round((model_summary$PR_AUC+1.96*model_summary$PR_AUC_sd),3), ')')
model_summary$NPV_Spec_AUC_string <- paste0(round((model_summary$NPV_Spec_AUC),3), " (95%CI: ", round((model_summary$NPV_Spec_AUC-1.96*model_summary$NPV_Spec_AUC_sd),3), ", ", round((model_summary$NPV_Spec_AUC+1.96*model_summary$NPV_Spec_AUC_sd),3), ')')

### PPV - Sensitivity (precision-recall) Curve
data_fit_full <- curve_across_k_folds(df=df, model_list=model_list, type='PR')

# Add stats into the label for graphical output
model_summary <- model_summary[model_summary$k=='SPARK',]
data_fit_full <- merge(data_fit_full, model_summary, by='model', all.x=TRUE, all.y=FALSE)
data_fit_full$string <- paste0(round(data_fit_full$PR_AUC, 3), ' (', round(data_fit_full$PR_AUC-1.96*data_fit_full$PR_AUC_sd, 3), ', ', round(data_fit_full$PR_AUC+1.96*data_fit_full$PR_AUC_sd, 3), ')')
data_fit_full$label <- NA
for(model in levels(factor(data_fit_full$model))){
  data_fit_full$label[data_fit_full$model==model] <- paste0(model, '\nAUC=', data_fit_full$string[data_fit_full$model==model])
}

ggplot(data_fit_full, aes(x=x_measure, y=y_measure, group=label, color=label, fill=label))+
  # geom_point()+
  geom_line(size=0.5, aes(y=mi))+
  geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.1, color=NA)+
  scale_x_continuous(breaks=c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))+
  xlab('sensitivity')+
  ylab('positive predictive value')+
  theme(legend.position=c(1,1), legend.justification = c("right", "top"), legend.background = element_rect(colour = 'grey', fill = 'white', linetype='solid'), legend.title=element_blank())+
  scale_color_manual(values=c("#332288", "#44AA99","#AA4499"))+
  scale_fill_manual(values=c("#332288", "#44AA99","#AA4499"))+
  scale_shape_manual(values=c(19, 1))+
  geom_hline(yintercept=length(which(df$ID_binary==1))/length(which(!is.na(df$ID_binary))), color='grey', linetype='dashed')+
  geom_vline(xintercept=0.1, color='grey', linetype='dashed')+
  geom_vline(xintercept=0.25, color='grey', linetype='dashed')+
  geom_vline(xintercept=0.5, color='grey', linetype='dashed')+
  geom_point(data=data_fit_full[factor(data_fit_full$x_measure)=='0.1',], aes(x=0.1, y=mi, color=label))+
  geom_point(data=data_fit_full[factor(data_fit_full$x_measure)=='0.25',], aes(x=0.25, y=mi, color=label))+
  geom_point(data=data_fit_full[factor(data_fit_full$x_measure)=='0.5',], aes(x=0.5, y=mi, color=label))+
  geom_text_repel(data=data_fit_full[factor(data_fit_full$x_measure)=='0.1',], aes(x=0.1, y=mi, color=label, label=round(mi, 3)), hjust=-0.1, vjust=-0.1, size=2.5, color='black')+
  geom_text_repel(data=data_fit_full[factor(data_fit_full$x_measure)=='0.25',], aes(x=0.25, y=mi, color=label, label=round(mi, 3)), hjust=-0.1, vjust=-0.1, size=2.5, color='black')+
  geom_text_repel(data=data_fit_full[factor(data_fit_full$x_measure)=='0.5',], aes(x=0.5, y=mi, color=label, label=round(mi, 3)), hjust=-0.1, vjust=-0.1, size=2.5, color='black')+
  geom_text(x=0.5, y=length(which(df$ID_binary==1))/length(which(!is.na(df$ID_binary))), label='baseline probability of ID', color='black', size=3)

### NPV - Specificity Curve
data_fit_full <- curve_across_k_folds(df=df, model_list=model_list, type='npv_specificity')

# Add stats into the label for graphical output
model_summary <- model_summary[model_summary$k=='SPARK',]
data_fit_full <- merge(data_fit_full, model_summary, by='model', all.x=TRUE, all.y=FALSE)
data_fit_full$string <- paste0(round(data_fit_full$NPV_Spec_AUC, 3), ' (', round(data_fit_full$NPV_Spec_AUC-1.96*data_fit_full$NPV_Spec_AUC_sd, 3), ', ', round(data_fit_full$NPV_Spec_AUC+1.96*data_fit_full$NPV_Spec_AUC_sd, 3), ')')
data_fit_full$label <- NA
for(model in levels(factor(data_fit_full$model))){
  data_fit_full$label[data_fit_full$model==model] <- paste0(model, '\nAUC=', data_fit_full$string[data_fit_full$model==model])
}

ggplot(data_fit_full, aes(x=x_measure, y=y_measure,group=label, color=label, fill=label))+
  # geom_point()+
  geom_line(linewidth=0.5, aes(y=mi))+
  geom_ribbon(aes(ymin=lo, ymax=hi), alpha=0.1, color=NA)+
  scale_x_continuous(breaks=c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))+
  xlab('specificity')+
  ylab('negative predictive value')+
  theme(legend.position=c(1,1), legend.justification = c("right", "top"), legend.background = element_rect(colour = 'grey', fill = 'white', linetype='solid'), legend.title=element_blank())+
  scale_color_manual(values=c("#332288", "#44AA99","#AA4499"))+
  scale_fill_manual(values=c("#332288", "#44AA99","#AA4499"))+
  scale_shape_manual(values=c(19, 1))+
  geom_hline(yintercept=length(which(df$ID_binary!=1))/length(which(!is.na(df$ID_binary))), color='grey', linetype='dashed')+
  geom_vline(xintercept=0.1, color='grey', linetype='dashed')+
  geom_vline(xintercept=0.25, color='grey', linetype='dashed')+
  geom_vline(xintercept=0.5, color='grey', linetype='dashed')+
  geom_point(data=data_fit_full[factor(data_fit_full$x_measure)=='0.1',], aes(x=0.1, y=mi, color=label))+
  geom_point(data=data_fit_full[factor(data_fit_full$x_measure)=='0.25',], aes(x=0.25, y=mi, color=label))+
  geom_point(data=data_fit_full[factor(data_fit_full$x_measure)=='0.5',], aes(x=0.5, y=mi, color=label))+
  geom_text_repel(data=data_fit_full[factor(data_fit_full$x_measure)=='0.1',], aes(x=0.1, y=mi, color=label, label=round(mi, 3)), hjust=-0.1, vjust=-0.1, size=2.5, color='black')+
  geom_text_repel(data=data_fit_full[factor(data_fit_full$x_measure)=='0.25',], aes(x=0.25, y=mi, color=label, label=round(mi, 3)), hjust=-0.1, vjust=-0.1, size=2.5, color='black')+
  geom_text_repel(data=data_fit_full[factor(data_fit_full$x_measure)=='0.5',], aes(x=0.5, y=mi, color=label, label=round(mi, 3)), hjust=-0.1, vjust=-0.1, size=2.5, color='black')+
  geom_text(x=0.5, y=length(which(df$ID_binary!=1))/length(which(!is.na(df$ID_binary))), label='baseline probability of ID', color='black', size=3)

