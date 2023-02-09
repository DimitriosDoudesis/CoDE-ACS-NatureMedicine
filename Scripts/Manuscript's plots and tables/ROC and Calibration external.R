
library(tidyverse)
library(flextable)
library(gtsummary)
library(pROC)
library(knitr)
library(kableExtra)
library(gridExtra)
library(janitor)
library(DescTools)
library(cowplot)

# load functions


# NPV MODELS

model_npv_presentation <- read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_NPV_external")

model_npv_serial <- read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_serial_NPV_external")



# PPV MODELS

model_ppv_presentation <- read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_PPV_external")

model_ppv_serial <- read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_serial_PPV_external")



model_npv_presentation_threshold <-
  read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_NPV_final_thresholds_table") %>%
  dplyr::filter(npv >= 99.5 & sensitivity > 90) %>% 
  data.table::last() %>%
  select(threshold) %>%
  unlist() %>%
  as.vector()/100


model_ppv_presentation_threshold <-
  read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_PPV_final_thresholds_table") %>%
  dplyr::filter(ppv >= 80 & specificity >= 80) %>%
  data.table::first() %>%
  select(threshold) %>%
  unlist() %>%
  as.vector()/100



# Presentation ------------------------------------------------------------


dataset <-
  read_rds("Outputs/Data/Dataset_external_cleaned") %>% as_tibble()


model_npv = model_npv_presentation
model_ppv = model_ppv_presentation
model_name = "XGBoost_external_presentation"

model_predictions <- rbind.data.frame(model_npv, model_ppv)


model_predictions <-
  model_predictions %>%
  dplyr::group_by(PatientId) %>% dplyr::summarise_all(median)

dataset <-
  dplyr::left_join(dataset, model_predictions, by = "PatientId")
dataset <-
  dataset %>% dplyr::rename(!!model_name := predictions)

observezone_ids_external <- model_predictions %>%
  mutate(observe = case_when(predictions < model_npv_presentation_threshold ~ "rule-out",
                             predictions >= model_ppv_presentation_threshold ~ "rule-in",
                             TRUE ~ "observe")) %>%
  filter(observe == "observe") %>%
  select(PatientId) %>%
  unlist()


## Serial
model_npv = model_npv_serial
model_ppv = model_ppv_serial
model_name = "XGBoost_external_serial"

model_predictions <- rbind.data.frame(model_npv, model_ppv)


model_predictions <-
  model_predictions %>%
  dplyr::group_by(PatientId) %>% dplyr::summarise_all(median)

dataset <-
  dplyr::left_join(dataset, model_predictions, by = "PatientId")
dataset <-
  dataset %>% dplyr::rename(!!model_name := predictions)




# Forest plots - serial but with observe ----------------------------------

source("Scripts/Rule out/0x - Functions/{function} Forest plot.R")

source("Scripts/Rule in/0x - Functions/{function} Forest plot.R")


# forest_plot_rule_out_external_perc(
#   data = dataset %>% rename(label = label_index),
#   predictions = "XGBoost_external_serial",
#   thres = model_npv_presentation_threshold,
#   title = "XGBoost_serial_observe_external_percentage",
#   folder = glue::glue("Outputs/Plots/Manuscript")
# )
# 
# 
# forest_plot_rule_in_external_perc(
#   data = dataset %>% rename(label = label_index),
#   predictions = "XGBoost_external_serial",
#   thres = model_ppv_presentation_threshold,
#   title = "XGBoost_serial_observe_external_percentage",
#   folder = glue::glue("Outputs/Plots/Manuscript")
# )




# Change the serial probs to presentation plus observe --------------------

dataset <- dataset %>% 
  mutate(XGBoost_external_serial_observe = case_when(
    XGBoost_external_presentation < model_npv_presentation_threshold ~ XGBoost_external_presentation,
    XGBoost_external_presentation >= model_ppv_presentation_threshold ~ XGBoost_external_presentation,
    TRUE ~ XGBoost_external_serial
  ))


forest_plot_rule_out_external_perc(
  data = dataset %>% rename(label = label_index),
  predictions = "XGBoost_external_serial_observe",
  thres = model_npv_presentation_threshold,
  title = "XGBoost_serial_observe_external_percentage",
  folder = glue::glue("Outputs/Plots/Manuscript")
)


forest_plot_rule_in_external_perc(
  data = dataset %>% rename(label = label_index),
  predictions = "XGBoost_external_serial_observe",
  thres = model_ppv_presentation_threshold,
  title = "XGBoost_serial_observe_external_percentage",
  folder = glue::glue("Outputs/Plots/Manuscript")
)


# Diagnostic metrics

round_DD = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

metrics_observe_zone <- function(dataset, model_name){

data_metrics <- data.frame()

# rule out

preds <- dataset[model_name]
label <- dataset$label_index
label <- factor(label, levels = c("0", "1"))
preds <- as.numeric(preds >= model_npv_presentation_threshold)
preds <- factor(preds, levels = c("0", "1"))


set.seed(1234)
CM <-
  qwraps2::confusion_matrix(preds,
                            label,
                            positive = "1",
                            boot = FALSE,
                            boot_samples = 1000L)

CM_metrics <- data.frame(
  threshold = model_npv_presentation_threshold*100,
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
preds <- as.numeric(preds >= model_ppv_presentation_threshold)
preds <- factor(preds, levels = c("0", "1"))


set.seed(1234)
CM <-
  qwraps2::confusion_matrix(preds,
                            label,
                            positive = "1",
                            boot = FALSE,
                            boot_samples = 1000L)

CM_metrics <- data.frame(
  threshold = model_ppv_presentation_threshold*100,
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



print(data_metrics)

}



metrics_observe_zone(dataset = dataset, model_name = "XGBoost_external_serial_observe")


# TYpe 1 or type 2

auc_format <- function(x) {
  roc_auc <- paste0(
    formatC(x[["auc"]] / 100, format = "f", digits = 3),
    " (95% CI [",
    formatC(x[["ci"]][1] / 100, format = "f", digits =
              3),
    "-",
    formatC(x[["ci"]][3] / 100, format = "f", digits =
              3) ,
    "])"
  )
  return(roc_auc)
}

# Roc curves split external -----------------------------------------------

model_npv = model_npv_presentation
model_ppv = model_ppv_presentation
model_name = "XGBoost_external_presentation"

Dataset_external_AUS <- read_rds("Outputs/Data/Dataset_AUS_cleaned")

model_predictions <- rbind.data.frame(model_npv, model_ppv)

model_predictions <-
  model_predictions %>%
  dplyr::group_by(PatientId) %>% dplyr::summarise_all(median)

Dataset_external_AUS <-
  dplyr::left_join(Dataset_external_AUS, model_predictions, by = "PatientId")
Dataset_external_AUS <-
  Dataset_external_AUS %>% dplyr::rename(!!model_name := predictions)


roc_XGBoost_presentation_AUS <- roc(
  Dataset_external_AUS$label_index,
  Dataset_external_AUS$XGBoost_external_presentation,
  levels = levels(as.factor(Dataset_external_AUS$label_index)),
  direction = "<",
  percent = TRUE,
  ci = TRUE,
  col = "#7E7F83"
)

# aucs format
roc_XGBoost_presentation_auc_AUS <- auc_format(roc_XGBoost_presentation_AUS)

Dataset_external_NZ <- read_rds("Outputs/Data/Dataset_NZ_cleaned")

Dataset_external_NZ <-
  dplyr::left_join(Dataset_external_NZ, model_predictions, by = "PatientId")
Dataset_external_NZ <-
  Dataset_external_NZ %>% dplyr::rename(!!model_name := predictions)



roc_XGBoost_presentation_NZ <- roc(
  Dataset_external_NZ$label_index,
  Dataset_external_NZ$XGBoost_external_presentation,
  levels = levels(as.factor(Dataset_external_NZ$label_index)),
  direction = "<",
  percent = TRUE,
  ci = TRUE,
  col = "#7E7F83"
)

# aucs format
roc_XGBoost_presentation_auc_NZ <- auc_format(roc_XGBoost_presentation_NZ)

Dataset_external_APACE <- read_rds("Outputs/Data/Dataset_APACE_cleaned")

Dataset_external_APACE <-
  dplyr::left_join(Dataset_external_APACE, model_predictions, by = "PatientId")
Dataset_external_APACE <-
  Dataset_external_APACE %>% dplyr::rename(!!model_name := predictions)


roc_XGBoost_presentation_APACE <- roc(
  Dataset_external_APACE$label_index,
  Dataset_external_APACE$XGBoost_external_presentation,
  levels = levels(as.factor(Dataset_external_APACE$label_index)),
  direction = "<",
  percent = TRUE,
  ci = TRUE,
  col = "#7E7F83"
)

# aucs format
roc_XGBoost_presentation_auc_APACE <- auc_format(roc_XGBoost_presentation_APACE)


presentation_roc_split <-
  ggroc(
    list(
      roc_apace = roc_XGBoost_presentation_APACE,
      roc_aus = roc_XGBoost_presentation_AUS,
      roc_nz = roc_XGBoost_presentation_NZ
    ),
    size = 1.0
  ) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_reverse(breaks = seq(0, 100, 10)) +
  scale_color_manual(labels = c(glue::glue("Europe (5,995): AUC = {roc_XGBoost_presentation_auc_APACE}"),
                                glue::glue("Australia (1,883): AUC = {roc_XGBoost_presentation_auc_AUS}"),
                                glue::glue("New Zealand (2,408): AUC = {roc_XGBoost_presentation_auc_NZ}")),
    values = c("#D1AC00", "#004643", "#D34E24"),
                     name = "") +
  geom_segment(aes(
    x = 0,
    xend = 100,
    y = 100,
    yend = 0
  ),
  color = "darkgrey",
  size = 0.7) +
  labs(x = "Specificity (%)", y = "Sensitivity (%)") +
  theme_half_open() +
  theme(legend.justification = c(1,0), legend.position = c(1, 0)) +
  coord_fixed()

presentation_roc_split

ggsave(presentation_roc_split,
       filename = "Outputs/Plots/Manuscript/roc_external_presentation_split.png",
       bg = "white",
       type = "cairo",
       dpi = 1200,
       height = 6,
       width = 6,
       units = "in")

# Combined single curve ---------------------------------------------------


roc_XGBoost_presentation <- roc(
  dataset$label_index,
  dataset$XGBoost_external_presentation,
  levels = levels(as.factor(dataset$label_index)),
  direction = "<",
  percent = TRUE,
  ci = TRUE,
  col = "#7E7F83"
)

# aucs format
roc_XGBoost_presentation_auc <- auc_format(roc_XGBoost_presentation)

presentation_roc <- ggroc(roc_XGBoost_presentation,  color = "#7E7F83", size = 1.2) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_reverse(breaks = seq(0, 100, 10)) +
  geom_segment(
    aes(
      x = 0,
      xend = 100,
      y = 100,
      yend = 0
    ),
    color = "darkgrey",
    size = 0.7
  ) +
  labs(x = "Specificity (%)", y = "Sensitivity (%)") +
  theme_half_open() +
  geom_text(
    x = -40 ,
    y = 0,
    label = paste0(
      "CoDE-ACS at presentation (AUC = ",
      roc_XGBoost_presentation_auc,
      ")"
    ),
    size = 3
  ) +
  coord_fixed()


roc_XGBoost_serial <- roc(
  dataset$label_index,
  dataset$XGBoost_external_serial_observe,
  levels = levels(as.factor(dataset$label_index)),
  direction = "<",
  percent = TRUE,
  ci = TRUE,
  col = "#7E7F83"
)

roc_XGBoost_serial_auc <- auc_format(roc_XGBoost_serial)

serial_roc <- ggroc(roc_XGBoost_serial,  color = "#7E7F83", size = 1.2) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_reverse(breaks = seq(0, 100, 10)) +
  geom_segment(
    aes(
      x = 0,
      xend = 100,
      y = 100,
      yend = 0
    ),
    color = "darkgrey",
    size = 0.7
  ) +
  labs(x = "Specificity (%)", y = "Sensitivity (%)") +
  theme_half_open() +
  geom_text(
    x = -40 ,
    y = 0,
    label = paste0(
      "CoDE-ACS serial testing (AUC = ",
      roc_XGBoost_serial_auc,
      ")"
    ),
    size = 3
  ) +
  coord_fixed()


# Callibration like MI3 ---------------------------------------------------


library(DescTools)
library(survminer)
library(survival)
library(ExPanDaR)
library(glue)
library(cowplot)

dat = dataset

dat$predict1 <- dataset$XGBoost_external_presentation

dat$predict2 <- dataset$XGBoost_external_serial_observe

BS_presentation <- formatC(DescTools::BrierScore(resp = dataset$label_index, pred = dataset$XGBoost_external_presentation), format="f", digits=3)
BS_presentation

BS_serial <- formatC(DescTools::BrierScore(resp = dataset$label_index, pred = dataset$XGBoost_external_serial), format="f", digits=3)
BS_serial


calibration.plot <- function(scoring.table, yaxis, test.y, pred.prob) {
  require(tidyverse)
  require(lemon)
  require(tidyverse)
  require(pROC)
  library(cowplot)
  library(scales)
  library(extrafont)
  library(data.table)
  
  calibration.data <- arrange(scoring.table, Score)
  
  group_number <-
    rep(1:(round(length(
      scoring.table$Score
    ) / 100)), each = 100)
  group_number <-
    factor(group_number[1:length(scoring.table$Score)])
  
  calibration.data <- cbind(calibration.data, group_number)

  temp1 <- calibration.data %>%
    group_by(group_number) %>%
    count(MI = MI == 0) %>%
    mutate(obs.MI = (1 - n / 100)) %>%
    filter(MI == TRUE)
  
  model_npv_presentation_threshold <-
    read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_NPV_final_thresholds_table") %>%
    dplyr::filter(npv >= 99.5 & sensitivity > 90) %>%
    data.table::last() %>%
    select(threshold) %>%
    unlist() %>%
    as.vector()/100
  

  model_ppv_presentation_threshold <-
    read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_PPV_final_thresholds_table") %>%
    dplyr::filter(ppv >= 80 & specificity >= 80) %>%
    data.table::first() %>%
    select(threshold) %>%
    unlist() %>%
    as.vector()/100
  
  
  temp2 <- calibration.data %>%
    group_by(group_number) %>%
    summarise(Scoremean = mean(Score)) %>%
    mutate(
      risk = case_when(
        Scoremean < model_npv_presentation_threshold*100 ~ "Low",
        Scoremean >= model_npv_presentation_threshold*100 & Scoremean < model_ppv_presentation_threshold*100 ~ "Intermediate",
        Scoremean >= model_ppv_presentation_threshold*100 ~ "High"
      )
    )
  
  
  calplot <- inner_join(temp1, temp2, by = "group_number")
  
  nbuckets = 10
  bucket_array <- seq(0.0, 1.0, by = 0.1)
  positive_in_band <- function(bucket){
    in_bucket_indicator <- pred.prob >= bucket_array[bucket] & pred.prob < bucket_array[bucket + 1]
    bucket_size <- sum(in_bucket_indicator)
    positive <- sum(test.y[in_bucket_indicator] == 1)
    return(qbeta(c(llb = 0.025, lb = 0.25, y = 0.5, ub = 0.75, uub = 0.965), 0.5 + positive, 0.5 + bucket_size - positive))
  }
  
  tbl <- data.table(bucket = 1:nbuckets, percentage = 5+bucket_array[1:nbuckets]*100,
                    blb = bucket_array[1:nbuckets], bub = bucket_array[(1:nbuckets) + 1])
  tbl <- cbind(tbl, 100*t(sapply(tbl$bucket, positive_in_band)))
  
  
  g <- ggplot(tbl, aes(x = percentage, y = y)) +
    geom_ribbon(aes(ymin = llb, ymax = uub), fill="grey50", alpha = 0.2) +
    geom_ribbon(aes(ymin = lb, ymax = ub), fill="grey50", alpha = 0.4) +
    geom_point(data = calplot, aes(Scoremean, obs.MI*100, colour = factor(risk))) +
    scale_color_manual(values = c("#BC3C29FF", "#7876B1FF", "#0072B5FF"),
                       name = "Probability group") +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    geom_segment(
      aes(
        x = 0,
        xend = 100,
        y = 0,
        yend = 100
      ),
      color = "darkgrey",
      linetype = "dashed",
      size = 0.8
    ) +
    labs(x = "CoDE-ACS score", y = yaxis) +
    theme_half_open() +
    background_grid() +
    theme(
      legend.position = c(1, 0), 
      legend.justification = c(1, 0)
    ) +
    coord_fixed()
  
  return(g)
}

cal1 <-
  calibration.plot(
    scoring.table = data.frame(
      Score = dataset$XGBoost_external_presentation * 100,
      MI = dataset$label_index
    ),
    test.y = dataset$label_index,
    pred.prob = dataset$XGBoost_external_presentation,
    yaxis = "Observed \n myocardial infarction (%)"
  )

cal1

cal4 <-
  calibration.plot(
    scoring.table = data.frame(
      Score = dataset$XGBoost_external_serial_observe * 100,
      MI = dataset$label_index
    ),
    test.y = dataset$label_index,
    pred.prob = dataset$XGBoost_external_serial_observe,
    yaxis = "Observed \n myocardial infarction (%)"
  )

cal4



# Combine -----------------------------------------------------------------

merge_1 <- plot_grid(presentation_roc,cal1, nrow=1, ncol=2, labels = c("A)", "B)"), label_size = 12, align = "hv")
merge_1

ggsave(merge_1,
       filename = "Outputs/Plots/Manuscript/combined_roc_calibration_external_presentation_external.png",
       type = "cairo",
       dpi = 1200,
       height = 5,
       width = 12,
       bg = "white",
       units = "in")

merge_2 <- plot_grid(serial_roc,cal4, nrow=1, ncol=2, labels = c("A)", "B)"), label_size = 12, align = "hv")
merge_2

ggsave(merge_2,
       filename = "Outputs/Plots/Manuscript/combined_roc_calibration_external_serial_external.png",
       type = "cairo",
       dpi = 300,
       height = 5,
       width = 12,
       bg = "white",
       units = "in")

# Rebuttal ROC curve with troponin ----------------------------------------


roc_troponin_presentation <- roc(
  dataset$label_index,
  dataset$first_troponin,
  levels = levels(as.factor(dataset$label_index)),
  direction = "<",
  percent = TRUE,
  ci = TRUE,
  col = "black"
)

# aucs format
roc_troponin_presentation_auc <- auc_format(roc_troponin_presentation)

roc_troponin_serial <- roc(
  dataset$label_index,
  dataset$second_troponin,
  levels = levels(as.factor(dataset$label_index)),
  direction = "<",
  percent = TRUE,
  ci = TRUE,
  col = "red"
)

# aucs format
roc_troponin_serial_auc <- auc_format(roc_troponin_serial)


roc_XGBoost_presentation <- roc(
  dataset$label_index,
  dataset$XGBoost_external_presentation,
  levels = levels(as.factor(dataset$label_index)),
  direction = "<",
  percent = TRUE,
  ci = TRUE,
  col = "#7E7F83"
)

# aucs format
roc_XGBoost_presentation_auc <- auc_format(roc_XGBoost_presentation)


roc_XGBoost_serial <- roc(
  dataset$label_index,
  dataset$XGBoost_external_serial_observe,
  levels = levels(as.factor(dataset$label_index)),
  direction = "<",
  percent = TRUE,
  ci = TRUE,
  col = "#7E7F83"
)

roc_XGBoost_serial_auc <- auc_format(roc_XGBoost_serial)


troponin_vs_model <-
  ggroc(
    list(
      roc_troponin_pres = roc_troponin_presentation,
      roc_troponin_ser = roc_troponin_serial,
      roc_model_pres = roc_XGBoost_presentation,
      roc_model_ser = roc_XGBoost_serial
    ),
    size = 1.0
  ) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_reverse(breaks = seq(0, 100, 10)) +
  scale_color_manual(labels = c(glue::glue("hs-cTnI presentation: AUC = {roc_troponin_presentation_auc}"),
                                glue::glue("hs-cTnI serial: AUC = {roc_troponin_serial_auc}"),
                                glue::glue("CoDE-ACS presentation: AUC = {roc_XGBoost_presentation_auc}"),
                                glue::glue("CoDE-ACS serial: AUC = {roc_XGBoost_serial_auc}")),
                     values = c("#D1AC00", "#004643", "#D34E24", "#55286F"),
                     name = "") +
  geom_segment(aes(
    x = 0,
    xend = 100,
    y = 100,
    yend = 0
  ),
  color = "darkgrey",
  size = 0.7) +
  labs(x = "Specificity (%)", y = "Sensitivity (%)") +
  theme_half_open() +
  theme(legend.justification = c(1,0), legend.position = c(1, 0),
        legend.text = element_text(size = 10.5)) +
  coord_fixed()

troponin_vs_model

ggsave(troponin_vs_model,
       filename = "Outputs/Plots/Manuscript/roc_external_troponin_vs_model.png",
       bg = "white",
       type = "cairo",
       dpi = 1200,
       height = 6,
       width = 6,
       units = "in")

# End