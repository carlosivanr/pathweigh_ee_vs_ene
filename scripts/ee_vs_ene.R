# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. Dept. of Family Medicine, CU Anschutz Medical Campus

# Eligible and enrolled vs eligible and not-enrolled paper
# Description:
# This script is the first step in attempting to put together the data, tables
# and figures for the PATHWEIGH EE vs ENE paper.

# Requirements:
# Depends on a data set derived from the baseline characteristics processing
# script. The data in the baseline characteristics paper were initally only
# processed for those with index visits (i.e. EE only). Additional processing
# was needed to include ENE. The data are found in the data directory as 
# seq_df2*.rda. Seq_df2 was a subset of the join2 dataframe and have been
# renamed as visits. Seq_df2 was a subset of visits that where the encounter
# date was equal to or greater than the index data essentially filtering for all
# visits after the index date and has been renamed as visits_post_id.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load Packages ----------------------------------------------------------------
pacman::p_load(here,         # For managing directory paths
               magrittr,     # For the assignment pipe operator %<>%
               tidyverse,    # For data manipulation tools
               data.table,   # For reading .csv files
               openxlsx,     # For reading .xlsx files
               tictoc,       # For timing and benchmarking functions
               gtsummary)    # For creating tables

## Load seq_df2 ----------------------------------------------------------------
# Keep in workspace as a back up
load(
  here("data", 
       "seq_df2_repeat_encounters_v1.rda")
  )

# Copy seq_df2 as visits for newer nomenclature of data frames
visits <- seq_df2


# # *** Determine if the missing visits with valid npi need to be fixed ----------
# # In seq_df2 index dates are assigned to WPVs even if there is no valid NPI. 
# # The following section, re-assigns index dates only to visits for which a valid
# # NPI is available. Index dates are assigned to only eligible and enrolled
#   visits %<>% 
#     select(-IndexDate, -Seq)
#   
#   # Create the tempdat.id dataframe
#   # Patients were assigned to the sequence of the clinic where they had an 
#   # eligible visit (satisfying Age, BMI, and WPV criteria)
#   # Compared to baseline characteristics paper, there are an additional 11 
#   # patients that should have been included, but were not.
#   tempdat.id <- visits %>%
#     drop_na(ProviderNpi) %>% # To reproduce baseline car, this line must be commented out
#     filter(Eligible == 1,
#            WPV >= 1) %>%
#     group_by(Arb_PersonId,
#              intervention) %>%
#     arrange(Arb_PersonId,
#             EncounterDate) %>%
#     slice_head() %>%
#     mutate(IndexDate = EncounterDate, Seq = GroupID) %>%
#     ungroup()
#   
#   # Join the IndexDate and Seq columns to the visits data frame
#   visits <- left_join(visits, tempdat.id[
#     ,c("Arb_PersonId","intervention","IndexDate","Seq")],
#     by = c("Arb_PersonId","intervention"))
#   
#   # Create the Cohort column based on Seq to use when displaying tables
#   visits$Cohort=paste0("Cohort ", visits$Seq)



# Process data -----------------------------------------------------------------
# - BMI_use to BMI - Set names BMI_use was old, BMI is new
# - Enrolled == Eligible (age and bmi criteria) and WPV > 0
# - Drop Npis - Drop rows if NPI is na
visits %<>%
  drop_na(ProviderNpi) %>%
  select(-BMI, -BMI_comp) %>%
  rename(BMI = BMI_use) %>%
  mutate(Enrolled = ifelse(WPV > 0 & Eligible == 1, 1, 0),
         Enrolled = ifelse(is.na(ProviderNpi), 0, Enrolled))
  

# Reproduce Table 1 from Baseline Characteristics paper ------------------------
# Table using the indexdate to identify those that are enrolled
visits %>%
  drop_na(ProviderNpi) %>%
  filter(EncounterDate == IndexDate) %>% 
  select(Cohort, Age, Sex, Race_Ethnicity, Insurance) %>% 
  tbl_summary(by = Cohort,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label = list(Race_Ethnicity ~ "Race/Ethnicity",
                           Age ~ "Age (years)"),
              missing = "no") %>%
  add_n(statistic="{N_miss} ({p_miss}%)") %>%
  modify_header(n = "**N missing (%)**") %>%
  bold_labels() %>% 
  add_overall()

# Reproduce Table 1 from the baseline char paper using the Enrolled variable
# This test is critical to ensure the Enrolled variable is functioning
visits %>%
  drop_na(ProviderNpi) %>%
  filter(Enrolled == 1) %>%
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
  add_n(statistic="{N_miss} ({p_miss}%)") %>%
  modify_header(n = "**N missing (%)**") %>%
  bold_labels() %>% 
  add_overall()


# Create eligible and enrolled -------------------------------------------------
# Clean up the 11 patients who were not captured in the bl char paper
# Unique person_ids for all that were supposed to be in baseline
all_unique_ee_ids <- 
  visits %>% 
  filter(Enrolled == 1) %>%
  distinct(Arb_PersonId)

# EE defined as meeting eligibility criteria and at least one WPV
# Enrolled == 1 is not enough to satisfy to the patient level because of 
# multiple visits per patients, but EncounterDate == IndexDate does the job bc
# only those that have a WPV are assigned an IndexDate.
ee <- visits %>%
  filter(EncounterDate == IndexDate)

# What are the IDs that are in visits, but not all_unique_ids
ids_to_exclude <- all_unique_ee_ids %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId)

# Exclude the 11 patients
visits %<>%
  filter(!Arb_PersonId %in% ids_to_exclude$Arb_PersonId)


# Check ee --------------------------------------------------------------------- 
# Do all visits in ee have matching encounter and index dates?
nrow(visits %>% filter(EncounterDate == IndexDate)) == dim(ee)[1]

# Are all visits in ee enrolled?
nrow(ee %>% filter(Enrolled == 1)) == dim(ee)[1]

# Is the number of unique enrolled patients equal to the number of patients in ee
# Should match if the 11 patients are excluded
visits %>% filter(Enrolled == 1) %>% pull(Arb_PersonId) %>% n_distinct() == dim(ee)[1]

# Check that there are no patients in ee that are coded in intervention
nrow(ee %>% filter(intervention == 1)) == 0

# Check that only 3 Cohorts are available 
length(table(ee$Cohort)) == 3


# Create eligible but not enrolled ---------------------------------------------
# ENE defined as meeting eligibility criteria but no WPV

# How many unique ene patients have more than one visit?
nrow(visits %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  filter(n > 1))
  
# How many unique ene patients have more than one clinic?  
nrow(visits %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  summarise(clinics = n_distinct(GroupID)) %>%
  filter(clinics > 1))

# Get the Arb_PersonIds of those that went to more than one clinic
gt1_clinic_ids <- visits %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  summarise(clinics = n_distinct(GroupID)) %>%
  filter(clinics > 1)

# Get the number of visits for each patient at each clinic
n_visits_per_patient_clinic <- visits %>%
  filter(Enrolled == 0,
         Eligible == 1) %>%
  group_by(Arb_PersonId, GroupID) %>%
  count() %>%
  filter(Arb_PersonId %in% gt1_clinic_ids$Arb_PersonId)
    

# For ene patients who were seen at multiple clinics, cohort is assigned as the 
# clinic with the most visits, if tied select the first observation (Group)
ene_gt1_clinic_cohorts <- 
  n_visits_per_patient_clinic %>% 
  group_by(Arb_PersonId) %>%
  arrange(n) %>% 
  slice_head() %>%
  mutate(Cohort = str_c("Cohort ", GroupID)) %>%
  select(Arb_PersonId, Cohort)

# Assign a cohort to those with out visits at multiple clinic which is the first
# Cohort after arranging visits by date as was done for EE. 
ene_gt1 <- visits %>%
  filter(Eligible == 1, 
         Arb_PersonId %in% ene_gt1_clinic_cohorts$Arb_PersonId) %>%
  left_join(ene_gt1_clinic_cohorts, by = "Arb_PersonId") %>%
  mutate(Cohort = coalesce(Cohort.y, Cohort.x)) %>%
  select(-Cohort.y, -Cohort.x) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  select(Arb_PersonId:WPV_v2, Cohort, everything())

ene <- 
  visits %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId,
         !Arb_PersonId %in% ene_gt1$Arb_PersonId,
         Eligible == 1,
         Enrolled == 0) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  slice_head() %>%
  mutate(Cohort = str_c("Cohort ", GroupID))


# Merge ene which initially contains only the first visit of ENE patients with
# ene_gt1_clinic_cohorts which contains the same variables but Cohort has been
# assigned to the clinic in which the most frequent visits take place for those
# with visits across multiple clinics
data <- bind_rows(ee, ene, ene_gt1)
rm(ene_gt1, ene_gt1_clinic_cohorts, gt1_clinic_ids, n_visits_per_patient_clinic, tempdat.id)

data %<>%
  mutate(Enrolled = ifelse(Enrolled == 1, "EE", "ENE"))


# Probably save the data here at this point to then be loaded into a .qmd doc
save(data, file = here("data", "ee_vs_ene_processed.rda"))


# %%%%%%%%%%%%%%%%%%%%%% TABLES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Table 1 demographics with enrolled == 0 patients only ------------------------
tab1 <- data %>%
  select(Cohort, Age, Sex, Race_Ethnicity, Insurance, Enrolled) %>% 
  tbl_strata(strata = Cohort,
            .tbl_fun = 
              ~ .x %>%
              tbl_summary(by = Enrolled,
                          statistic = list(all_continuous() ~ "{mean} ({sd})"),
                          label = list(Race_Ethnicity ~ "Race/Ethnicity",
                                       Age ~ "Age (years)"),
                          missing = "no",
                          sort = list(everything() ~ "frequency")) %>%
                          #add_n(statistic="{N_miss} ({p_miss}%)") %>%
                          #modify_header(n = "**N missing (%)**") %>%
                          bold_labels() #%>% 
                          #add_overall()
  )


# Save Table 1
tab1 %>%
    as_gt() %>%
    gt::gtsave(
      filename = here("tables", "ee_vs_ene_test_tab1.pdf"))

# %%%%%%%%%%%%%%%%%%%%%% FENCE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
