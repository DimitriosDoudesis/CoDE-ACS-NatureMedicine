

library(tidyverse)
library(glue)
library(qwraps2)

round_DD = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Function External validation -----------------------------------------------------

ESC_01_algorithm <-
  function(dataset,
           assay) {
    
    dataset = dataset %>%
      mutate(
        # outcome = label_index,
        Tn0h = first_troponin,
        Tn1h = second_troponin,
        CPO = as.integer(CPO),
        CPM = as.integer(CPM))
    
    
    # Cutoffs from assays----
    if (assay == "Roche") {
      a <- 5; b <- 12; c <- 3; d <- 52; e <- 5
    } else if (assay == "Abbott") {
      a <- 4; b <- 5; c <- 2; d <- 64; e <- 6 
    } else {
      stop(glue("{assay} was incorrectly specified. Name of assay is case sensitive.
               Only the following assays name's are recognized in this function:
               Roche
               Abbott"))
    }
    
    #create early presenters----
    dataset <- dataset %>%
      mutate(Early_Presenter = ifelse(is.na(CPO), CPM, CPO),
             Early_Presenter = ifelse(Early_Presenter <= 3, 1, 0),
             Early_Presenter = ifelse(is.na(Early_Presenter), 1, Early_Presenter))
    
    
    #Only drop those patients without 1h values which cannot be directly ruled out or ruled in with the 0h value
    dataset <- dataset %>%
      mutate(Require_2_Tn = ifelse(Tn0h < a & Early_Presenter == 1 & is.na(Tn1h), 1, 0),
             Require_2_Tn = ifelse(Tn0h >= a & Tn0h < d  & is.na(Tn1h), 1, Require_2_Tn))
    
    nbr_rq_2Tn <- table(dataset$Require_2_Tn)[2]
    # print(glue("Number of patients requiring a Tn at 1h: ", nbr_rq_2Tn))
    
    dataset <- dataset %>% filter(Require_2_Tn == 0)
    
    
    # ESC 0/1h Algo----
    dataset$delta <- abs(dataset$Tn1h - dataset$Tn0h)
    dataset <- dataset %>%
      mutate(esc_presentation = case_when(Tn0h < a & Early_Presenter == 0 ~ 0,
                                          Tn0h >= d ~ 1,
                                          TRUE ~ 0.5))
    
  
    data_metrics <- data.frame()
    
    # presentation - rule out
    
    preds <- dataset$esc_presentation
    label <- dataset$label_index
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= 0.5) # EQUAL here
    preds <- factor(preds, levels = c("0", "1"))
    
    
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE,
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      threshold = "presentation rule-out",
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
    
    # presentation - rule in
    
    preds <- dataset$esc_presentation
    label <- dataset$label_index
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds > 0.5) # NOT equal here
    preds <- factor(preds, levels = c("0", "1"))
    
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE,
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      threshold = "presentation rule-in",
      ppv = round_DD(CM$stats["PPV", "Est"]*100, 1),
      ppv_lcl = round_DD(CM$stats["PPV", "LCL"]*100, 1),
      ppv_ucl = round_DD(CM$stats["PPV", "UCL"]*100, 1),
      sensitivity = round_DD(CM$stats["Sensitivity", "Est"]*100, 1),
      sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"]*100, 1),
      sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"]*100, 1),
      specificity = round_DD(CM$stats["Specificity", "Est"]*100, 1),
      specificity_lcl = round_DD(CM$stats["Specificity", "LCL"]*100, 1),
      specificity_ucl = round_DD(CM$stats["Specificity", "UCL"]*100, 1),
      npv = NA,
      npv_lcl = NA,
      npv_ucl = NA,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = NA,
      Rule_in = round_DD(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
    
    # serial
    dataset <- dataset %>%
        mutate(First_step = ifelse(Tn0h < a & Early_Presenter == 0, 0, 0.5),
               First_step = ifelse(Tn0h >= d, 1, First_step),
               Second_step = ifelse(First_step == 0.5 & Tn0h < b & delta < c, 0, 0.5),
               Second_step = ifelse(First_step == 0.5 & delta >= e, 1, Second_step),
               esc_serial = ifelse(First_step == 0 | Second_step == 0, 0, 0.5),
               esc_serial = ifelse(First_step == 1 | Second_step == 1, 1, esc_serial))
    
    preds <- dataset$esc_serial
    label <- dataset$label_index
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= 0.5) # EQUAL here
    preds <- factor(preds, levels = c("0", "1"))
    
    
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE,
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      threshold = "serial rule-out",
      ppv = NA,
      ppv_lcl = NA,
      ppv_ucl = NA,
      sensitivity = round_DD(CM$stats["Sensitivity", "Est"]*100, 1),
      sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"]*100, 1),
      sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"]*100, 1),
      specificity = NA,
      specificity_lcl = NA,
      specificity_ucl = NA,
      npv = round_DD(CM$stats["NPV", "Est"]*100, 1),
      npv_lcl = round_DD(CM$stats["NPV", "LCL"]*100, 1),
      npv_ucl = round_DD(CM$stats["NPV", "UCL"]*100, 1),
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = round_DD(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
      Rule_in = NA
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
    # serial rule-in
    preds <- dataset$esc_serial
    label <- dataset$label_index
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds > 0.5) # NOT equal here
    preds <- factor(preds, levels = c("0", "1"))
    
    
    # New addition CHECK AGAIN
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE, # CHANGE HERE
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      threshold = "serial rule-in",
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
    
    
    
    # aesthitics
    
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




# HEART score -------------------------------------------------------------

Heart_pathway <- function(dataset) {
  # Heart score -------------------------------------------------------------
  
  # Risk factors
  # table(dataset$hypertension, useNA = "always")
  # table(dataset$hyperlipidemia, useNA = "always")
  # table(dataset$diabetes, useNA = "always")
  # table(dataset$BMI, useNA = "always")
  # table(dataset$smoking, useNA = "always")
  #
  # table(apace$AnamneseFamanamnese_KHK)
  
  # Family history
  dataset <- dataset %>%
    mutate(
      family_history = if_else(AnamneseFamanamnese_KHK == "erstgradig", 1, 0),
      obesity = case_when(BMI > 30 ~ 1,
                          is.na(BMI) ~ NA_real_,
                          TRUE ~ 0)
    )
  
  # table(dataset$family_history, useNA = "always")
  # table(dataset$obesity, useNA = "always")
  #
  
  
  # ECG
  # table(dataset$ECGNormalAbnormal, useNA = "always") # == 0 point 0
  
  # table(dataset$EKGSchenkelblock)
  # EKGSchenkelblock == "LSB" # this is 1 point
  # table(dataset$EKGLVHypertrophie, useNA = "always")
  # EKGLVHypertrophie == 1 # 1 point
  # EIther of the above 1 point, if both 1 then still 1 point
  
  # for 2 points
  # dataset$ischemia # if 1 then 2 points
  
  
  # History
  # NOT USE dataset$Symptomatik1SchmerzartStechendscharf # sharp pain # 1 means yes, so 0 points
  # table(dataset$Symptomatik2Schmerzgroesse, useNA = "always") # small area of pain (well localised) # kleiner_als_3cm then 0 points
  # table(dataset$Symptomatik3SchmerzabhaengigDruckneu, useNA = "always") # get worse on pressure, # 1 means yes, so 0 points
  # table(dataset$Symptomatik4SchmerzausstrahlungkeineAusstrahlung, useNA = "always") # not radiating, # 1 means yes, so 0 points
  # table(dataset$Symptomatik4SchmerzausstrahlungNausea, useNA = "always") # nausea, # 1 means yes, you need 0 to get 0 points
  # table(dataset$Symptomatik3SchmerzabhaengigBelastungneu, useNA = "always") # initiated by exertion, 1 yes, gives 2 points
  # table(dataset$Symptomatik1Schmerzartmediothorakal, useNA = "always") # central chest pain # 1 means yes which gives 2 points, you need 0 to get 0 points
  
  # sligthly suspicious
  # all okay, 0 points
  
  
  # moderate suspicious
  # Either of the above gives 1 point
  
  # highly suspicious
  # two or more then 2 points
  
  dataset <- dataset %>%
    # History
    mutate(
      pain_pressure_historydx = case_when(
        Symptomatik1SchmerzartDruckdumpf == FALSE ~ 0,
        Symptomatik1SchmerzartDruckdumpf == TRUE ~ 1,
        TRUE ~ 0
      ),
      not_radiating_historydx = case_when(
        Symptomatik4SchmerzausstrahlungkeineAusstrahlung == TRUE ~ 0,
        Symptomatik4SchmerzausstrahlungkeineAusstrahlung == FALSE ~ 1,
        TRUE ~ 0
      ),
      nausea_historydx = case_when(
        Symptomatik4SchmerzausstrahlungNausea == 0 ~ 0,
        Symptomatik4SchmerzausstrahlungNausea == 1 ~ 1,
        TRUE ~ 0
      ),
      pain_initiated_by_exertion_historydx = case_when(
        Symptomatik3SchmerzabhaengigBelastungneu == "no" ~ 0,
        Symptomatik3SchmerzabhaengigBelastungneu == "yes" ~ 1,
        TRUE ~ 0
      ),
      central_chest_pain_historydx = case_when(
        Symptomatik1Schmerzartmediothorakal == FALSE ~ 0,
        Symptomatik1Schmerzartmediothorakal == TRUE ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      history_heart_pathway = rowSums(select(., ends_with("historydx"))),
      history_heart_pathway = case_when(
        history_heart_pathway < 2 ~ 0,
        history_heart_pathway == 2 ~ 1,
        history_heart_pathway > 2 ~ 2
      )
    ) %>%
    # ECG
    mutate(
      ECG_heart_pathway = case_when(
        ECGNormalAbnormal == 0 ~ 0,
        EKGSchenkelblock == "LSB" | EKGLVHypertrophie == 1 ~ 1,
        ischemia == 1 ~ 2,
        TRUE ~ 0 # as I am adding everything
      )
    ) %>%
    # Age
    mutate(age_heart_pathway = case_when(age < 45 ~ 0,
                                         age >= 45 & age < 65 ~ 1,
                                         age >= 65 ~ 2,
                                         TRUE ~ 0)) %>%
    # Risk factors
    mutate(
      hypertension_rfdx = as.numeric(as.character(hypertension)),
      hyperlipidemia_rfdx = as.numeric(as.character(hyperlipidemia)),
      diabetes_rfdx = as.numeric(as.character(diabetes)),
      smoking_rfdx = smoking,
      family_history_rfdx = family_history,
      obesity_rfdx = obesity,
      previous_ihd_rfdx = case_when(previous_ihd == 0 ~ 0,
                                    previous_ihd == 1 ~ 2,
                                    TRUE ~ 0)
    ) %>%
    mutate(risk_factors_heart_pathway = rowSums(select(., ends_with("rfdx")), na.rm = T)) %>%
    mutate(
      first_troponin_heart_pathway = case_when(
        sex == 1 & first_troponin < 34.5 ~ 0,
        sex == 0 & first_troponin < 16.5 ~ 0,
        sex == 1 &
          (first_troponin >= 34.5 & first_troponin < 34.5 * 3) ~ 1,
        sex == 0 &
          (first_troponin >= 16.5 & first_troponin < 16.5 * 3) ~ 1,
        sex == 1 & first_troponin >= 34.5 * 3 ~ 2,
        sex == 0 & first_troponin >= 16.5 * 3 ~ 2,
        TRUE ~ NA_real_
      )
    ) %>%
    # heart index score
    mutate(heart_score = rowSums(select(., ends_with("heart_pathway")))) %>%
    mutate(
      troponin_presentation = case_when(
        sex == 1 & first_troponin < 34.5 ~ 0,
        sex == 0 & first_troponin < 16.5 ~ 0,
        sex == 1 & first_troponin >= 34.5 ~ 1,
        sex == 0 & first_troponin >= 16.5 ~ 1,
        TRUE ~ NA_real_
      ),
      troponin_3hour = case_when(
        sex == 1 & fourth_troponin < 34.5 ~ 0,
        sex == 0 & fourth_troponin < 16.5 ~ 0,
        sex == 1 & fourth_troponin >= 34.5 ~ 1,
        sex == 0 & fourth_troponin >= 16.5 ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      pathway = case_when(
        heart_score <= 3 &
          troponin_presentation == 0 & troponin_3hour == 0 ~ 0,
        heart_score >= 4 |
          troponin_presentation == 1 | troponin_3hour == 1 ~ 1,
        TRUE ~ NA_real_
      )
    )
  
  
  # table(dataset$heart_score, useNA = "always")
  # table(dataset$troponin_presentation, useNA = "always")
  # table(dataset$troponin_3hour, useNA = "always")
  # table(dataset$pathway, useNA = "always")
  
  
  data_metrics <- data.frame()
  
  # presentation - rule out
  
  preds <- dataset$pathway
  label <- dataset$label_index
  label <- factor(label, levels = c("0", "1"))
  preds <- as.numeric(preds >= 0.5)
  preds <- factor(preds, levels = c("0", "1"))
  
  
  # New addition CHECK AGAIN
  set.seed(1234)
  CM <-
    qwraps2::confusion_matrix(
      preds,
      label,
      positive = "1",
      boot = FALSE,
      boot_samples = 1000L
    )
  
  CM_metrics <- data.frame(
    threshold = "rule-out",
    npv = round_DD(CM$stats["NPV", "Est"] * 100, 1),
    npv_lcl = round_DD(CM$stats["NPV", "LCL"] * 100, 1),
    npv_ucl = round_DD(CM$stats["NPV", "UCL"] * 100, 1),
    sensitivity = round_DD(CM$stats["Sensitivity", "Est"] * 100, 1),
    sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"] * 100, 1),
    sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"] * 100, 1),
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
    Rule_out = round_DD(
      CM$cells$negatives / (CM$cells$negatives + CM$cells$positives) * 100,
      0
    ),
    Rule_in = NA
  )
  
  data_metrics <- data_metrics %>%
    rbind(CM_metrics)
  
  # presentation - rule in
  
  preds <- dataset$pathway
  label <- dataset$label_index
  label <- factor(label, levels = c("0", "1"))
  preds <- as.numeric(preds > 0.5) # NOT equal here
  preds <- factor(preds, levels = c("0", "1"))
  
  
  set.seed(1234)
  CM <-
    qwraps2::confusion_matrix(
      preds,
      label,
      positive = "1",
      boot = FALSE,
      boot_samples = 1000L
    )
  
  CM_metrics <- data.frame(
    threshold = "rule-in",
    ppv = round_DD(CM$stats["PPV", "Est"] * 100, 1),
    ppv_lcl = round_DD(CM$stats["PPV", "LCL"] * 100, 1),
    ppv_ucl = round_DD(CM$stats["PPV", "UCL"] * 100, 1),
    sensitivity = round_DD(CM$stats["Sensitivity", "Est"] * 100, 1),
    sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"] * 100, 1),
    sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"] * 100, 1),
    specificity = round_DD(CM$stats["Specificity", "Est"] * 100, 1),
    specificity_lcl = round_DD(CM$stats["Specificity", "LCL"] * 100, 1),
    specificity_ucl = round_DD(CM$stats["Specificity", "UCL"] * 100, 1),
    npv = NA,
    npv_lcl = NA,
    npv_ucl = NA,
    TN = CM$cells$true_negatives,
    FN = CM$cells$false_negatives,
    TP = CM$cells$true_positives,
    FP = CM$cells$false_positives,
    Rule_out = NA,
    Rule_in = round_DD(
      CM$cells$positives / (CM$cells$negatives + CM$cells$positives) * 100,
      0
    )
  )
  
  data_metrics <- data_metrics %>%
    rbind(CM_metrics)
  
  
  # aesthitics
  
  # Round numbers
  data_metrics <- data_metrics %>%
    mutate_if(is.numeric, round, 1)
  
  # function for creating CIs
  CI <- function(ce, ll, ul) {
    paste0(
      formatC(ce, format = "f", digits = 1),
      " (",
      formatC(ll, format = "f", digits = 1),
      "-",
      formatC(ul, format = "f", digits = 1),
      ")"
    )
  }
  
  
  data_metrics$npv_ci <-
    CI(data_metrics$npv,
       data_metrics$npv_lcl,
       data_metrics$npv_ucl)
  data_metrics$ppv_ci <-
    CI(data_metrics$ppv,
       data_metrics$ppv_lcl,
       data_metrics$ppv_ucl)
  data_metrics$sensititivy_ci <-
    CI(
      data_metrics$sensitivity,
      data_metrics$sensitivity_lcl,
      data_metrics$sensitivity_ucl
    )
  data_metrics$specificity_ci <-
    CI(
      data_metrics$specificity,
      data_metrics$specificity_lcl,
      data_metrics$specificity_ucl
    )
  
  
  data_metrics <- data.frame(
    Threshold = data_metrics$threshold,
    NPV = data_metrics$npv_ci,
    Sensitivity = data_metrics$sensititivy_ci,
    PPV = data_metrics$ppv_ci,
    Specificity = data_metrics$specificity_ci,
    TN = data_metrics$TN,
    FN = data_metrics$FN,
    TP = data_metrics$TP,
    FP = data_metrics$FP,
    `Rule out` = data_metrics$Rule_out,
    `Rule_in` = data_metrics$Rule_in
  )
  
  
  
  return(data_metrics)
  
  
}


# Troponin_guideline_pathway ----------------------------------------------

Troponin_pathway <- function(dataset) {
  
  dataset <- dataset %>% 
    mutate(troponin_change = abs(serial_troponin - first_troponin) / first_troponin * 100) %>% 
    mutate(troponin_pathway = case_when(
      sex == 1 & first_troponin < 34.5 & troponin_change >= 50 ~ 1,
      sex == 0 & first_troponin < 16.5 & troponin_change >= 50 ~ 1,
      sex == 1 & first_troponin >= 34.5 & troponin_change >= 20 ~ 1,
      sex == 0 & first_troponin >= 16.5 & troponin_change >= 20 ~ 1,
      TRUE ~ 0
    ))
  

  data_metrics <- data.frame()
  
  # presentation - rule out
  
  preds <- dataset$troponin_pathway
  label <- dataset$label_index
  label <- factor(label, levels = c("0", "1"))
  preds <- as.numeric(preds >= 0.5)
  preds <- factor(preds, levels = c("0", "1"))
  
  
  set.seed(1234)
  CM <-
    qwraps2::confusion_matrix(
      preds,
      label,
      positive = "1",
      boot = FALSE,
      boot_samples = 1000L
    )
  
  CM_metrics <- data.frame(
    threshold = "rule-out",
    npv = round_DD(CM$stats["NPV", "Est"] * 100, 1),
    npv_lcl = round_DD(CM$stats["NPV", "LCL"] * 100, 1),
    npv_ucl = round_DD(CM$stats["NPV", "UCL"] * 100, 1),
    sensitivity = round_DD(CM$stats["Sensitivity", "Est"] * 100, 1),
    sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"] * 100, 1),
    sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"] * 100, 1),
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
    Rule_out = round_DD(
      CM$cells$negatives / (CM$cells$negatives + CM$cells$positives) * 100,
      0
    ),
    Rule_in = NA
  )
  
  data_metrics <- data_metrics %>%
    rbind(CM_metrics)
  
  # presentation - rule in
  
  preds <- dataset$troponin_pathway
  label <- dataset$label_index
  label <- factor(label, levels = c("0", "1"))
  preds <- as.numeric(preds > 0.5) # NOT equal here
  preds <- factor(preds, levels = c("0", "1"))
  
  
  set.seed(1234)
  CM <-
    qwraps2::confusion_matrix(
      preds,
      label,
      positive = "1",
      boot = FALSE,
      boot_samples = 1000L
    )
  
  CM_metrics <- data.frame(
    threshold = "rule-in",
    ppv = round_DD(CM$stats["PPV", "Est"] * 100, 1),
    ppv_lcl = round_DD(CM$stats["PPV", "LCL"] * 100, 1),
    ppv_ucl = round_DD(CM$stats["PPV", "UCL"] * 100, 1),
    sensitivity = round_DD(CM$stats["Sensitivity", "Est"] * 100, 1),
    sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"] * 100, 1),
    sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"] * 100, 1),
    specificity = round_DD(CM$stats["Specificity", "Est"] * 100, 1),
    specificity_lcl = round_DD(CM$stats["Specificity", "LCL"] * 100, 1),
    specificity_ucl = round_DD(CM$stats["Specificity", "UCL"] * 100, 1),
    npv = NA,
    npv_lcl = NA,
    npv_ucl = NA,
    TN = CM$cells$true_negatives,
    FN = CM$cells$false_negatives,
    TP = CM$cells$true_positives,
    FP = CM$cells$false_positives,
    Rule_out = NA,
    Rule_in = round_DD(
      CM$cells$positives / (CM$cells$negatives + CM$cells$positives) * 100,
      0
    )
  )
  
  data_metrics <- data_metrics %>%
    rbind(CM_metrics)
  
  
  # aesthitics
  
  # Round numbers
  data_metrics <- data_metrics %>%
    mutate_if(is.numeric, round, 1)
  
  # function for creating CIs
  CI <- function(ce, ll, ul) {
    paste0(
      formatC(ce, format = "f", digits = 1),
      " (",
      formatC(ll, format = "f", digits = 1),
      "-",
      formatC(ul, format = "f", digits = 1),
      ")"
    )
  }
  
  
  data_metrics$npv_ci <-
    CI(data_metrics$npv,
       data_metrics$npv_lcl,
       data_metrics$npv_ucl)
  data_metrics$ppv_ci <-
    CI(data_metrics$ppv,
       data_metrics$ppv_lcl,
       data_metrics$ppv_ucl)
  data_metrics$sensititivy_ci <-
    CI(
      data_metrics$sensitivity,
      data_metrics$sensitivity_lcl,
      data_metrics$sensitivity_ucl
    )
  data_metrics$specificity_ci <-
    CI(
      data_metrics$specificity,
      data_metrics$specificity_lcl,
      data_metrics$specificity_ucl
    )
  
  
  data_metrics <- data.frame(
    Threshold = data_metrics$threshold,
    NPV = data_metrics$npv_ci,
    Sensitivity = data_metrics$sensititivy_ci,
    PPV = data_metrics$ppv_ci,
    Specificity = data_metrics$specificity_ci,
    TN = data_metrics$TN,
    FN = data_metrics$FN,
    TP = data_metrics$TP,
    FP = data_metrics$FP,
    `Rule out` = data_metrics$Rule_out,
    `Rule_in` = data_metrics$Rule_in
  )
  
  return(data_metrics)
  
}

# END
