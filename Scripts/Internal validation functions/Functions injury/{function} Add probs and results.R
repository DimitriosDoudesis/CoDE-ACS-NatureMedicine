
library(tidyverse)

# Function to add probabilities to dataset
Add_preds_to_dataset <- function(dataset, model_predictions, model_name) {
  
  # Be careful with the ID if there are duplicates
  
  model_predictions <-
    model_predictions %>%
    dplyr::group_by(PatientId) %>% dplyr::summarise_all(median)
  
  dataset <-
    dplyr::full_join(dataset, model_predictions, by = "PatientId")
  dataset <-
    dataset %>% dplyr::rename(!!model_name := predictions)
  
  return(dataset)
}


# XGBoost -----------------------------------------------------------------

# change later (i.e. change to *100)
# Changed to 100%


results_PPV <- function(model_table, ppv_value, specificity_value){
  
  high_thres_ppv <-
    model_table %>%
    dplyr::filter(ppv >= ppv_value) %>%
    data.table::first()
  
  
  # high_thres_ppv_and_spec <-
  #   model_table %>%
  #   dplyr::filter(ppv >= ppv_value & specificity >= specificity_value) %>%
  #   data.table::last()
  
  
  high_thres_print <-
    glue::glue(
      "For the threshold of {round(high_thres_ppv$threshold,1)}/100 
    the PPV is {formatC(high_thres_ppv$ppv, 3)}% and Specificity is {formatC(high_thres_ppv$specificity, 3)}% with Rule in of {formatC(high_thres_ppv$Rule_in, 3)}%
    TN: {high_thres_ppv$TN}, FN: {high_thres_ppv$FN}, FP: {high_thres_ppv$FP} and TP: {high_thres_ppv$TP}
    "
    )
  
  return(high_thres_print)
  
}


results_PPV_Spec <- function(model_table, ppv_value, specificity_value){
  
  # high_thres_ppv <-
  #   model_table %>%
  #   dplyr::filter(ppv >= ppv_value) %>%
  #   data.table::last()
  
  
  high_thres_ppv_and_spec <-
    model_table %>%
    dplyr::filter(ppv >= ppv_value & specificity >= specificity_value) %>%
    data.table::first()
  
  
  high_thres_print <-
    glue::glue(
      "For the threshold of {round(high_thres_ppv_and_spec$threshold,1)}/100 
    the PPV is {formatC(high_thres_ppv_and_spec$ppv, 3)}% and Specificity is {formatC(high_thres_ppv_and_spec$specificity, 3)}% with Rule in of {formatC(high_thres_ppv_and_spec$Rule_in, 3)}%
    TN: {high_thres_ppv_and_spec$TN}, FN: {high_thres_ppv_and_spec$FN}, FP: {high_thres_ppv_and_spec$FP} and TP: {high_thres_ppv_and_spec$TP}
    "
    )
  
  return(high_thres_print)
  
}










# results_NPV <- function(model_table, npv_value, sensitivity_value){
#   
#   low_thres_npv <-
#     model_table %>%
#     dplyr::filter(npv >= npv_value) %>%
#     data.table::last()
#   
#   
#   low_thres_npv_and_sens <-
#     model_table %>%
#     dplyr::filter(npv >= npv_value & sensitivity >= sensitivity_value) %>%
#     data.table::last()
#   
#   
#   low_thres_print <-
#     glue::glue(
#       "For the threshold of {round(low_thres_npv$threshold,1)}/100 
#     the NPV is {formatC(low_thres_npv$npv, 3)}% and Sensitivity is {formatC(low_thres_npv$sensitivity, 3)}% with Rule out of {formatC(low_thres_npv$Rule_out, 3)}%
#     TN: {low_thres_npv$TN}, FN: {low_thres_npv$FN}, FP: {low_thres_npv$FP} and TP: {low_thres_npv$TP}
#     "
#     )
#   
#   return(low_thres_print)
#   
# }
# 
# 
# 
# results_with_sensitivity <- function(model_table, npv_value, sensitivity_value){
#   
#   low_thres_npv <-
#     model_table %>%
#     dplyr::filter(npv >= npv_value) %>%
#     data.table::last()
#   
#   
#   low_thres_npv_and_sens <-
#     model_table %>%
#     dplyr::filter(npv >= npv_value & sensitivity >= sensitivity_value) %>%
#     data.table::last()
#   
#   
#   low_thres_print <-
#     glue::glue(
#       "For the threshold of {round(low_thres_npv$threshold,1)}/100 
#     the NPV is {formatC(low_thres_npv$npv, 3)}% and Sensitivity is {formatC(low_thres_npv$sensitivity, 3)}% with Rule out of {formatC(low_thres_npv$Rule_out, 3)}%
#     TN: {low_thres_npv$TN}, FN: {low_thres_npv$FN}, FP: {low_thres_npv$FP} and TP: {low_thres_npv$TP}
#     while
#     For the threshold of {formatC(low_thres_npv_and_sens$threshold, 1)}/100 
#     the NPV is {formatC(low_thres_npv_and_sens$npv, 3)}% and Sensitivity is {formatC(low_thres_npv_and_sens$sensitivity, 3)}% with Rule out of {formatC(low_thres_npv_and_sens$Rule_out, 3)}%
#     TN: {low_thres_npv_and_sens$TN}, FN: {low_thres_npv_and_sens$FN}, FP: {low_thres_npv_and_sens$FP} and TP: {low_thres_npv_and_sens$TP}
#     "
#     )
#   
#   return(low_thres_print)
#   
# }
# 
# 
# 
# 
# 
# 
# 
# results_Lancet <- function(model_table, predictions, troponin_value){
#   
#   
#   preds <- model_table[predictions]
#   label <- model_table$label
#   label <- factor(label, levels = c("0", "1"))
#   preds <- as.numeric(preds >= troponin_value)
#   preds <- factor(preds, levels = c("0", "1"))
#   
#   CM <-
#     qwraps2::confusion_matrix(preds,
#                               label,
#                               positive = "1",
#                               boot = TRUE, # CHANGE HERE
#                               boot_samples = 1000L)
#   
#   CM_metrics <- data.frame(
#     troponin_less_than = troponin_value,
#     npv = CM$stats["NPV", "Boot Est"]*100,
#     npv_lcl = CM$stats["NPV", "Boot LCL"]*100,
#     npv_ucl = CM$stats["NPV", "Boot UCL"]*100,
#     ppv = CM$stats["PPV", "Boot Est"]*100,
#     ppv_lcl = CM$stats["PPV", "Boot LCL"]*100,
#     ppv_ucl = CM$stats["PPV", "Boot UCL"]*100,
#     sensitivity = CM$stats["Sensitivity", "Boot Est"]*100,
#     sensitivity_lcl = CM$stats["Sensitivity", "Boot LCL"]*100,
#     sensitivity_ucl = CM$stats["Sensitivity", "Boot UCL"]*100,
#     specificity = CM$stats["Specificity", "Boot Est"]*100,
#     specificity_lcl = CM$stats["Specificity", "Boot LCL"]*100,
#     specificity_ucl = CM$stats["Specificity", "Boot UCL"]*100,
#     # npv = CM$stats["NPV", "Est"]*100,
#     # npv_lcl = CM$stats["NPV", "LCL"]*100,
#     # npv_ucl = CM$stats["NPV", "UCL"]*100,
#     # ppv = CM$stats["PPV", "Est"]*100,
#     # ppv_lcl = CM$stats["PPV", "LCL"]*100,
#     # ppv_ucl = CM$stats["PPV", "UCL"]*100,
#     # sensitivity = CM$stats["Sensitivity", "Est"]*100,
#     # sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
#     # sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
#     # specificity = CM$stats["Specificity", "Est"]*100,
#     # specificity_lcl = CM$stats["Specificity", "LCL"]*100,
#     # specificity_ucl = CM$stats["Specificity", "UCL"]*100,
#     TN = CM$cells$true_negatives,
#     FN = CM$cells$false_negatives,
#     TP = CM$cells$true_positives,
#     FP = CM$cells$false_positives,
#     Rule_out = round(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 1),
#     Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 1)
#   )
#   
#   # CM_metrics
#   
#   low_thres_print <-
#     glue::glue(
#       "For the troponin only (less than) threshold of {troponin_value} 
#     the NPV is {formatC(CM_metrics$npv, 3)}%,
#     the Sensitivity is {formatC(CM_metrics$sensitivity, 3)}% 
#     with Rule out of {formatC(CM_metrics$Rule_out, 3)}%
#     TN: {CM_metrics$TN}, FN: {CM_metrics$FN}, FP: {CM_metrics$FP} and TP: {CM_metrics$TP}
#     "
#     )
#   
#   return(low_thres_print)
#   
# }


