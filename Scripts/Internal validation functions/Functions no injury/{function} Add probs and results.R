

library(tidyverse)

# Function to add probabilities to dataset
Add_preds_to_dataset <-
  function(dataset, model_predictions, model_name) {
    # Be careful with the Patient ID in case you have duplicates
    
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

results <- function(model_table,
                    npv_value,
                    sensitivity_value) {
  low_thres_npv <-
    model_table %>%
    dplyr::filter(npv >= npv_value) %>%
    data.table::last()
  
  low_thres_print <-
    glue::glue(
      "Threshold = {low_thres_npv$threshold}
    NPV = {formatC(low_thres_npv$npv, 3)}%, Sensitivity = {formatC(low_thres_npv$sensitivity, 3)}%,
    Rule out = {formatC(low_thres_npv$Rule_out, 3)}%
    TN: {low_thres_npv$TN}, FN: {low_thres_npv$FN}
    "
    )
  
  return(low_thres_print)
  
}


results_with_sensitivity <-
  function(model_table,
           npv_value,
           sensitivity_value) {
    low_thres_npv <-
      model_table %>%
      dplyr::filter(npv >= npv_value &
                      sensitivity >= sensitivity_value) %>%
      data.table::last()
    
    
    low_thres_npv_and_sens_print <-
      glue::glue(
        "Threshold = {low_thres_npv$threshold}
    NPV = {formatC(low_thres_npv$npv, 3)}%, Sensitivity = {formatC(low_thres_npv$sensitivity, 3)}%,
    Rule out = {formatC(low_thres_npv$Rule_out, 3)}%
    TN: {low_thres_npv$TN}, FN: {low_thres_npv$FN}
    "
      )
    
    return(low_thres_npv_and_sens_print)
    
  }

# End