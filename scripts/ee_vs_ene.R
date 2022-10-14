
# Load Packages ----------------------------------------------------------------
pacman::p_load(here,         # For managing directory paths
               magrittr,     # For the assignment pipe operator %<>%
               tidyverse,    # For data manipulation tools
               data.table,   # For reading .csv files
               openxlsx,     # For reading .xlsx files
               tictoc,       # For timing and benchmarking functions
               gtsummary)    # For creating tables

# Load data --------------------------------------------------------------------

## *** Load dat.full for the CONSORT diagram ----
# load(paste0(dir0, "/baseline_characteristics/data/220121_2020-03-17_to_2021-03-16_wpv1_full.Rda"))
# dat.full <- join2
# rm(join2) 

## Load seq_df2 for the tables ---- ---------------------------------------------
load(
  here("data", 
       "seq_df2_repeat_encounters_v1.rda")
  )

visits <- seq_df2
rm(seq_d2)


# Create Eligible and Enrolled variables ---------------------------------------
# Adults (age >=18) with BMI>=25 at index visit, if eligible is na set to 0
visits %<>%
  mutate(Eligible = ifelse(Age >= 18 & BMI >=25, 1, 0),
         Eligible = ifelse(is.na(Eligible), 0, Eligible))

# Set the Enrolled variable which is eligible (age and bmi criteria) + wpv > 0
visits %<>%
  mutate(Enrolled = ifelse(WPV > 0 & Eligible == 1, 1, 0))



# Reproduce Table 1 from Baseline Characteristics paper
visits %>%
       drop_na(ProviderNpi) %>%
       filter(EncounterDate == IndexDate) %>% 
  select(Cohort, Age, Sex, Race_Ethnicity, Insurance) %>% 
  tbl_summary(by = Cohort,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label = list(Race_Ethnicity ~ "Race/Ethnicity",
                           Age ~ "Age (years)"),
              missing = "no") %>%
  modify_header(n = "**N missing (%)**") %>%
  bold_labels() %>% 
  add_overall()


# Eligible and enrolled: Must have had a WPV
ee <- visits %>%
  drop_na(ProviderNpi) %>%
  filter(EncounterDate == IndexDate)

# Eligible but not enrolled: Meet eligibility criteria but no WPV
# *** Many cohort values are NA. Why is that?
visits %>%
  drop_na(ProviderNpi) %>%
  filter(Enrolled == 0) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  slice_head() %>%
  ungroup() %>%
  select(Cohort, Age, Sex, Race_Ethnicity, Insurance) %>% 
  tbl_summary(by = Cohort,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label = list(Race_Ethnicity ~ "Race/Ethnicity",
                           Age ~ "Age (years)"),
              missing = "no") %>%
  modify_header(n = "**N missing (%)**") %>%
  bold_labels() %>% 
  add_overall()


# Table: Demographics %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#tab1 <- 
visits %>% 
  select(Cohort, Age, Sex, Race_Ethnicity, Insurance) %>% 
  # mutate(Sex = as_factor(Sex)) %>%
  # mutate(Sex = fct_explicit_na(Sex, na_level = "Unknown")) %>%
  tbl_summary(by = Cohort,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label = list(Race_Ethnicity ~ "Race/Ethnicity",
                           Age ~ "Age (years)"),
              missing = "no") %>%
  # missing="ifany",
  # missing_text="Unknown") %>%
  add_n(statistic="{N_miss} ({p_miss}%)") %>%
  modify_header(n = "**N missing (%)**") %>%
  bold_labels() %>% 
  add_overall()


















# Demographics table -----------------------------------------------------------
# tab1 <- data %>%
#   select(Cohort, Age, Sex, Race_Ethnicity, Insurance, Enrolled) %>% 
#   mutate(Sex = na_if(Sex, "Unknown")) %>%
#   tbl_strata(
#     strata = Cohort,
#     .tbl_fun = 
#       ~ .x %>%
#       tbl_summary(by = Enrolled,
#                     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#                     label = list(Race_Ethnicity ~ "Race/Ethnicity",
#                            Age ~ "Age (years)"),
#                     #missing = "no",
#                     sort = all_categorical() ~ "frequency") %>%
#         #add_n(statistic="{N_miss} ({p_miss}%)") %>%
#         #modify_header(n = "**N missing (%)**") %>%
#         bold_labels() %>% 
#         add_overall()
#   )
# 
# tab1 %>%
#   as_gt() %>%
#   gt::gtsave(
#     filename = here("tables", "ee_vs_ene - demographics_patients.pdf"))
# 
# as_flex_table(tab1)
# 
# # Patient Health Characteristics --------------------------------------
# tab2 <- data %>% 
#   mutate(PHQ2_Completed = ifelse(is.na(PHQ2_Completed), 0, PHQ2_Completed),
#          BMI = as.numeric(BMI),
#          N_MedsWeightGain_period = ifelse(N_MedsWeightGain_period > 0, 1, N_MedsWeightGain_period),
#          N_MedsWeightLoss_period = ifelse(N_MedsWeightLoss_period > 0, 1, N_MedsWeightLoss_period),
#          PHQ8 = as.numeric(PHQ8),
#          PHQ9 = as.numeric(PHQ9),
#          GAD7 = as.numeric(GAD7)) %>%
#   select(Cohort, Height_cm, Weight_kgs, BMI, HeartRate, Respiratoryrate,
#          Systolic_blood_pressure, Diastolic_blood_pressure, Temperature,
#          A1C:TSH, N_MedsWeightGain_period, N_MedsWeightLoss_period, 
#          O2CPAPBIPAP, PHQ8, PHQ9, GAD7, Smoking_Status, Enrolled) %>% 
#   # mutate(`Smoking Status` = factor(SmokingStatus)) %>%
#   # select(-SmokingStatus) %>%
#   tbl_strata(
#     strata = Cohort,
#     ~ .x %>%
#     tbl_summary(by = Enrolled,
#                 label = list(Height_cm ~ "Height (cm)",
#                              Weight_kgs ~ "Weight (kg)",
#                              BMI ~ "BMI (kg/m^2)",
#                              HeartRate ~ "Heart Rate (bpm)", 
#                              Respiratoryrate ~ "Respiratory Rate (rpm)",
#                              Systolic_blood_pressure ~ "Systolic BP (mmHg)", 
#                              Diastolic_blood_pressure ~ "Diastolic BP (mmHg)", 
#                              N_MedsWeightGain_period ~ "Using meds that cause weight gain",
#                              N_MedsWeightLoss_period ~ "Using meds that cause weight loss",
#                              O2CPAPBIPAP ~ "O2/CPAP/BIPAP Users",
#                              Smoking_Status ~ "Smoking Status",
#                              Temperature ~ "Temperature (F)"),
#                 #type = list(all_continuous() ~ "continuous2"),
#                 missing = "no", 
#                 statistic = list(all_continuous() ~ c("{mean} ({sd})"))) %>%
#   #add_n(statistic="{N_miss} ({p_miss}%)") %>%
#   #modify_header(n = "**N missing (%)**") %>%
#   bold_labels() %>% 
#   add_overall()
#   )
# 
# tab2 %>%
#   as_gt() %>%
#   gt::gtsave(
#     filename = here("tables", "ee_vs_ene - labs_vitals_patients.pdf"))
# 
# as_flex_table(tab2)
# 
# # *** Check for duplicate encounter Ids just to be sure
# 
# length(unique(year1$Arb_EncounterId))



# # Check that all observations meet eligible criteria
# if (range(dat.id$BMI_use)[1] < 25){
#   stop("Eligibility criteria for BMI not met for some patients!!!")
# }
# 
# if (range(dat.id$Age)[1] < 18){
#   stop("Eligibility criteria for Age not met for some patients!!!")
# }
