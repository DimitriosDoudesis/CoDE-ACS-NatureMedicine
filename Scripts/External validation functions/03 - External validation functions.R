
 
library(tidyverse)
library(glue)
library(qwraps2)
library(forestplot)
library(berryFunctions)
library(Cairo)

round_DD = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Function External validation -----------------------------------------------------

External_validation_metric_function_model_classic <-
  function(dataset,
           model_npv,
           rule_out_threshold,
           model_ppv,
           rule_in_threshold,
           model_name,
           model) {
  
model_predictions <- rbind.data.frame(model_npv, model_ppv)


model_predictions <-
  model_predictions %>%
  dplyr::group_by(PatientId) %>% dplyr::summarise_all(median)

dataset <-
  dplyr::left_join(dataset, model_predictions, by = "PatientId")
dataset <-
  dataset %>% dplyr::rename(!!model_name := predictions)

data_metrics <- data.frame()

# rule out

preds <- dataset[model_name]
label <- dataset$label_index
label <- factor(label, levels = c("0", "1"))
preds <- as.numeric(preds >= rule_out_threshold)
preds <- factor(preds, levels = c("0", "1"))


# New addition CHECK AGAIN
set.seed(1234)
CM <-
  qwraps2::confusion_matrix(preds,
                            label,
                            positive = "1",
                            boot = FALSE,
                            boot_samples = 1000L)

CM_metrics <- data.frame(
  threshold = rule_out_threshold*100,
  npv = round_DD(CM$stats["NPV", "Est"]*100, 1),
  npv_lcl = round_DD(CM$stats["NPV", "LCL"]*100, 1),
  npv_ucl = round_DD(CM$stats["NPV", "UCL"]*100, 1),
  sensitivity = round_DD(CM$stats["Sensitivity", "Est"]*100, 1),
  sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"]*100, 1),
  sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"]*100, 1),
  ppv = NA,
  ppv_lcl = NA,
  ppv_ucl = NA,
  specificity = NA,
  specificity_lcl = NA,
  specificity_ucl = NA,
  TN = CM$cells$true_negatives,
  FN = CM$cells$false_negatives,
  TP = CM$cells$true_positives,
  FP = CM$cells$false_positives,
  Rule_out = round_DD(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
  Rule_in = NA
)

data_metrics <- data_metrics %>%
  rbind(CM_metrics)


# rule in

preds <- dataset[model_name]
label <- dataset$label_index
label <- factor(label, levels = c("0", "1"))
preds <- as.numeric(preds >= rule_in_threshold)
preds <- factor(preds, levels = c("0", "1"))


# New addition CHECK AGAIN
set.seed(1234)
CM <-
  qwraps2::confusion_matrix(preds,
                            label,
                            positive = "1",
                            boot = FALSE,
                            boot_samples = 1000L)

CM_metrics <- data.frame(
  threshold = rule_in_threshold*100,
  npv = NA,
  npv_lcl = NA,
  npv_ucl = NA,
  sensitivity = round_DD(CM$stats["Sensitivity", "Est"]*100, 1),
  sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"]*100, 1),
  sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"]*100, 1),
  ppv = round_DD(CM$stats["PPV", "Est"]*100, 1),
  ppv_lcl = round_DD(CM$stats["PPV", "LCL"]*100, 1),
  ppv_ucl = round_DD(CM$stats["PPV", "UCL"]*100, 1),
  specificity = round_DD(CM$stats["Specificity", "Est"]*100, 1),
  specificity_lcl = round_DD(CM$stats["Specificity", "LCL"]*100, 1),
  specificity_ucl = round_DD(CM$stats["Specificity", "UCL"]*100, 1),
  TN = CM$cells$true_negatives,
  FN = CM$cells$false_negatives,
  TP = CM$cells$true_positives,
  FP = CM$cells$false_positives,
  Rule_out = NA,
  Rule_in = round_DD(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0)
)

data_metrics <- data_metrics %>%
  rbind(CM_metrics)

# Round numbers
data_metrics <- data_metrics %>% 
  mutate_if(is.numeric, round, 1)

# function for creating CIs
CI <- function(ce, ll, ul){
  paste0(
    formatC(ce, format = "f", digits = 1),
    " (",
    formatC(ll, format = "f", digits = 1),
    "-",
    formatC(ul, format = "f", digits = 1),
    ")")
}


data_metrics$npv_ci <- CI(data_metrics$npv, data_metrics$npv_lcl, data_metrics$npv_ucl)
data_metrics$ppv_ci <- CI(data_metrics$ppv, data_metrics$ppv_lcl, data_metrics$ppv_ucl)
data_metrics$sensititivy_ci <- CI(data_metrics$sensitivity, data_metrics$sensitivity_lcl, data_metrics$sensitivity_ucl)
data_metrics$specificity_ci <- CI(data_metrics$specificity, data_metrics$specificity_lcl, data_metrics$specificity_ucl)


data_metrics <- data.frame(
  Threshold = data_metrics$threshold,
  NPV = data_metrics$npv_ci,
  Sensitivity = data_metrics$sensititivy_ci,
  PPV = data_metrics$ppv_ci,
  Specificity = data_metrics$specificity_ci,
  TN = data_metrics$TN,
  FN = data_metrics$FN,
  TP =data_metrics$TP,
  FP = data_metrics$FP,
  `Rule out` = data_metrics$Rule_out,
  `Rule_in` = data_metrics$Rule_in
)



return(data_metrics)


}

# # END
