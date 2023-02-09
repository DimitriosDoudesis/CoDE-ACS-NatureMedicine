
# NRI ---------------------------------------------------------------------

round_DD = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

library(PredictABEL)
packageVersion("PredictABEL")

Dataset_external_combined <- read_rds("Outputs/Data/Dataset_external_cleaned")

Data_nri <- Dataset_external_combined %>% select(label_index, everything())

w1 <- read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_NPV_external") %>%
  dplyr::group_by(PatientId) %>%
  dplyr::summarise_all(median) %>%
  rename(predictions_presentation_npv = predictions)

w2 <- read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_PPV_external") %>%
  dplyr::group_by(PatientId) %>%
  dplyr::summarise_all(median) %>%
  rename(predictions_presentation_ppv = predictions)

Data_nri <- left_join(Data_nri, w1, by = "PatientId")
Data_nri <- left_join(Data_nri, w2, by = "PatientId")

Data_nri <- Data_nri %>% 
  transmute(
    label_index = label_index,
    troponin = case_when(first_troponin < 5 & ischemia == 0 & hours_since_symptoms > 3 ~ 0.01,
                          sex == 1 & first_troponin >= 34.5 ~ 0.99,
                          sex == 0 & first_troponin >= 16.5 ~ 0.99,
                          TRUE ~ 0.25 # random number
                          ),
    CoDEACS = coalesce(predictions_presentation_npv, predictions_presentation_ppv)
  )


table(is.na(Data_nri$troponin))
table(is.na(Data_nri$CoDEACS))
table(is.na(Data_nri$label_index))


model_npv_presentation_threshold <-
  read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_NPV_final_thresholds_table") %>%
  dplyr::filter(npv >= 99.5 & sensitivity > 90) %>%
  data.table::last() %>%
  select(threshold) %>%
  unlist() %>%
  round_DD(0) %>%
  as.vector()/100


model_ppv_presentation_threshold <-
  read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_PPV_final_thresholds_table") %>%
  dplyr::filter(ppv >= 80 & specificity >= 80) %>%
  data.table::first() %>%
  select(threshold) %>%
  unlist() %>%
  round_DD(0) %>%
  as.vector()/100

cutoff <- c(0, 0.03, 0.61, 1)


cOutcome <- 1
predRisk1 <- as.vector(Data_nri$troponin)
predRisk2 <- as.vector(Data_nri$CoDEACS)

predRisk1 <- as.numeric(Data_nri$troponin)
predRisk2 <- as.numeric(Data_nri$CoDEACS)


reclassification(
  data = Data_nri,
  cOutcome = cOutcome,
  predrisk1 = predRisk1,
  predrisk2 = predRisk2,
  cutoff
)


# DCA ---------------------------------------------------------------------


library (dcurves)
library(cowplot)

Dataset_external_combined <- read_rds("Outputs/Data/Dataset_external_cleaned")

Data_dca <- Dataset_external_combined %>% select(label_index, everything())

w1 <- read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_NPV_external") %>%
  dplyr::group_by(PatientId) %>%
  dplyr::summarise_all(median) %>%
  rename(predictions_presentation_npv = predictions)

w2 <- read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_PPV_external") %>%
  dplyr::group_by(PatientId) %>%
  dplyr::summarise_all(median) %>%
  rename(predictions_presentation_ppv = predictions)

Data_dca <- left_join(Data_dca, w1, by = "PatientId")
Data_dca <- left_join(Data_dca, w2, by = "PatientId")

Data_dca <- Data_dca %>% 
  mutate(
    troponin = case_when(first_troponin < 5 & ischemia == 0 & hours_since_symptoms > 3 ~ 0,
                         sex == 1 & first_troponin >= 34.5 ~ 1,
                         sex == 0 & first_troponin >= 16.5 ~ 1,
                         TRUE ~ 0.5
    ),
    CoDEACS = coalesce(predictions_presentation_npv, predictions_presentation_ppv)
  )


plot <- dca(label_index ~ troponin + CoDEACS,
            Data_dca,
            thresholds = seq (0, 0.80, 0.01),
            label = list(troponin = "hs-cTnI",
                         CoDEACS = "CoDE-ACS")) %>% 
  plot(smooth = TRUE) +
  scale_colour_manual(values = c("gray70", "black", "#72B5FF", "#BC3C29FF"),
                      labels = c("Diagnose All", "Diagnose None", "hs-cTnI", "CoDE-ACS")) +
  expand_limits(y = -1) +
  theme_half_open() +
  background_grid() +
  theme(legend.position = c(1, 1),
        legend.justification = c(1.35, 0.93))


plot

