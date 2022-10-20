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
               gtsummary,    # For creating tables
               furrr)        # For parallel processing

# Set parallel processing
plan(multisession, workers = 4)

## Load seq_df2 ----------------------------------------------------------------
# Keep in workspace as a back up
load(here("data", "ee_vs_ene_processed.rda"))


# %%%%%%%%%%%%%%%%%%%%%% TABLES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Table 1 demographics
tab1.a <- data %>%
  select(Age, Sex, Race_Ethnicity, Insurance, Enrolled) %>%
  tbl_summary(by = Enrolled,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label = list(Race_Ethnicity ~ "Race/Ethnicity",
                           Age ~ "Age (years)"),
              missing = "no",
              sort = list(everything() ~ "frequency")) %>%
  #add_n(statistic="{N_miss} ({p_miss}%)") %>%
  #modify_header(n = "**N missing (%)**") %>%
  bold_labels() %>%
  add_p(test = list(all_continuous() ~ "t.test")) %>%
  modify_spanning_header(all_stat_cols() ~ "**Overall**")


#Table generating function
make_sub_tab1 <- function(cohort){
  table <- data[data$Cohort == cohort,] %>%
    select(Age, Sex, Race_Ethnicity, Insurance, Enrolled) %>%
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

  return(table)

  }

# Apply table generating function to all cohorts
cohorts <- c("Cohort 1", "Cohort 2", "Cohort 3")

tab1_sub_tables <- map(cohorts, make_sub_tab1)
        

# Merge all Table 1 sub tables
tab1 <- tbl_merge(tbls = list(tab1.a, 
                              tab1_sub_tables[[1]], 
                              tab1_sub_tables[[2]], 
                              tab1_sub_tables[[3]]),
          tab_spanner = c("**Overall**", 
                          "**Sequence 1**", 
                          "**Sequence 2**", 
                          "**Sequence 3**"))

# Save Table 1
tab1 %>%
    as_gt() %>%
    gt::gtsave(
      filename = here("tables", "ee_vs_ene_table_1.pdf"))


# Patient Health Characteristics --------------------------------------
tab2_cols <- names(data %>% select(Height, Weight_kgs, BMI, HeartRate, Respiratoryrate,
                            Systolic_blood_pressure, Diastolic_blood_pressure, Temperature,
                            TSH, Triglycerides, HDL, ALT, AST, A1C, eGFR, N_MedsWeightGain_period,
                            N_MedsWeightLoss_period, O2CPAPBIPAP, PHQ2_Completed, PHQ8, PHQ9, GAD7, 
                            SmokingStatus, Enrolled))

tab2.a <- data %>%
  select(all_of(tab2_cols)) %>%
    tbl_summary(by = Enrolled,
                label = list(Height ~ "Height (cm)",
                             Weight_kgs ~ "Weight (kg)",
                             BMI ~ "BMI (kg/m^2)",
                             HeartRate ~ "Heart Rate (bpm)",
                             Respiratoryrate ~ "Respiratory Rate (rpm)",
                             Systolic_blood_pressure ~ "Systolic BP (mmHg)",
                             Diastolic_blood_pressure ~ "Diastolic BP (mmHg)",
                             N_MedsWeightGain_period ~ "Using meds that cause weight gain",
                             N_MedsWeightLoss_period ~ "Using meds that cause weight loss",
                             O2CPAPBIPAP ~ "O2/CPAP/BIPAP Users",
                             PHQ2_Completed ~ "PHQ2 Completed",
                             SmokingStatus ~ "Smoking Status",
                             Temperature ~ "Temperature (F)"),
                #type = list(all_continuous() ~ "continuous2"),
                missing = "no",
                statistic = list(all_continuous() ~ c("{mean} ({sd})")),
                sort = list(everything() ~ "frequency")) %>%
  #add_n(statistic="{N_miss} ({p_miss}%)") %>%
  #modify_header(n = "**N missing (%)**") %>%
  bold_labels()  %>%
  add_p(test = list(all_continuous() ~ "t.test")) %>%
  modify_spanning_header(all_stat_cols() ~ "**Overall**")

# #Table 2 generating function
make_sub_tab2 <- function(cohort){
  table <- data[data$Cohort == cohort,] %>%
    select(all_of(tab2_cols)) %>%
    tbl_summary(by = Enrolled,
                label = list(Height ~ "Height (cm)",
                             Weight_kgs ~ "Weight (kg)",
                             BMI ~ "BMI (kg/m^2)",
                             HeartRate ~ "Heart Rate (bpm)",
                             Respiratoryrate ~ "Respiratory Rate (rpm)",
                             Systolic_blood_pressure ~ "Systolic BP (mmHg)",
                             Diastolic_blood_pressure ~ "Diastolic BP (mmHg)",
                             N_MedsWeightGain_period ~ "Using meds that cause weight gain",
                             N_MedsWeightLoss_period ~ "Using meds that cause weight loss",
                             O2CPAPBIPAP ~ "O2/CPAP/BIPAP Users",
                             SmokingStatus ~ "Smoking Status",
                             PHQ2_Completed ~ "PHQ2 Completed",
                             Temperature ~ "Temperature (F)"),
                #type = list(all_continuous() ~ "continuous2"),
                missing = "no",
                statistic = list(all_continuous() ~ c("{mean} ({sd})")),
                sort = list(everything() ~ "frequency")) %>%
    #add_n(statistic="{N_miss} ({p_miss}%)") %>%
    #modify_header(n = "**N missing (%)**") %>%
    bold_labels()

  return(table)

}

# Apply table generating function to all cohorts
tab2_sub_tables <- future_map(cohorts, make_sub_tab2)

# Merge all Table 1 sub tables
tab2 <- tbl_merge(tbls = list(tab2.a,
                              tab2_sub_tables[[1]],
                              tab2_sub_tables[[2]],
                              tab2_sub_tables[[3]]),
                  tab_spanner = c("**Overall**",
                                  "**Sequence 1**",
                                  "**Sequence 2**",
                                  "**Sequence 3**"))

tab2 %>%
  as_gt() %>%
  gt::gtsave(
    filename = here("tables", "ee_vs_ene_table_2.pdf"))

#as_flex_table(tab2)


# Comorbidities --------------------------------------
# Load the top comorbidities .csv file
top_comorbidities <- read.csv(here("../baseline_characteristics/working_files/top_comorbidities_220121.csv"))
newnames <- top_comorbidities$disease.state
oldnames <- gsub(" ", ".", top_comorbidities$disease.state)

tab3.a <- data %>%
  select(all_of(top_comorbidities$disease.state), Enrolled) %>%
  select(-Asthma, -`Vitamin D deficiency`) %>%
  tbl_summary(by = Enrolled) %>%
  add_p(test = list(all_continuous() ~ "t.test")) %>%
  modify_spanning_header(all_stat_cols() ~ "**Overall**")

# #Table 3 generating function
make_sub_tab3 <- function(cohort){
  table <- data[data$Cohort == cohort,] %>%
    select(all_of(top_comorbidities$disease.state), Enrolled) %>%
    select(-Asthma, -`Vitamin D deficiency`) %>%
    tbl_summary(by = Enrolled) %>%
    bold_labels()
    
    return(table)
  
}

# Apply table generating function to all cohorts
tab3_sub_tables <- future_map(cohorts, make_sub_tab3)

# Merge all Table 1 sub tables
tab3 <- tbl_merge(tbls = list(tab3.a,
                              tab3_sub_tables[[1]],
                              tab3_sub_tables[[2]],
                              tab3_sub_tables[[3]]),
                  tab_spanner = c("**Overall**",
                                  "**Sequence 1**",
                                  "**Sequence 2**",
                                  "**Sequence 3**"))



# Referrals --------------------------------------

tab4.1.a <- data %>%
  select(Enrolled,Ref_BariatricSurgery:Ref_WellnessClinic, BariatricSurgery) %>%
  tbl_summary(by = Enrolled,
              type = all_continuous() ~ "continuous2",
              missing="ifany",
              label = list(Ref_BariatricSurgery ~ "Bariatrics",
                           Ref_BehavioralHealth ~ "Behavioral Health",
                           Ref_Endo ~ "Endocrinology",
                           Ref_Dietician ~ "Dietician",
                           Ref_WellnessClinic ~ "Health and Wellness",
                           BariatricSurgery ~ "Bariatric procedure performed")) %>%
  add_p(test = list(all_continuous() ~ "t.test")) %>%
  modify_spanning_header(all_stat_cols() ~ "**Overall**")
# modify_header(label = "**Referrals**")


# #Table 4 generating function
make_sub_tab4.1 <- function(cohort){
  table <- data[data$Cohort == cohort,] %>%
    select(Enrolled,Ref_BariatricSurgery:Ref_WellnessClinic, BariatricSurgery) %>%
    tbl_summary(by = Enrolled,
                type = all_continuous() ~ "continuous2",
                missing="ifany",
                label = list(Ref_BariatricSurgery ~ "Bariatrics",
                             Ref_BehavioralHealth ~ "Behavioral Health",
                             Ref_Endo ~ "Endocrinology",
                             Ref_Dietician ~ "Dietician",
                             Ref_WellnessClinic ~ "Health and Wellness",
                             BariatricSurgery ~ "Bariatric procedure performed")) %>%
    bold_labels()
  
  return(table)
  
}


# Apply table generating function to all cohorts
tab4.1_sub_tables <- future_map(cohorts, make_sub_tab4.1)

# Merge all Table 1 sub tables
tab4.1 <- tbl_merge(tbls = list(tab4.1.a,
                              tab4.1_sub_tables[[1]],
                              tab4.1_sub_tables[[2]],
                              tab4.1_sub_tables[[3]]),
                  tab_spanner = c("**Overall**",
                                  "**Sequence 1**",
                                  "**Sequence 2**",
                                  "**Sequence 3**"))

# Version 2 of the meds table
# dat.AOM is supposed to be made from meds_AOM
meds <- meds_AOM %>%
  # select(-IndexDate, -Cohort) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = uniqueMedName, values_from = count) %>%
  select(-IndexDate, -Cohort) %>%
  distinct() %>%
  group_by(Arb_PersonId) %>%
  summarise_each(funs(sum(., na.rm = TRUE)))

# Merge data and meds together
data <- left_join(data, meds, by = "Arb_PersonId")

#Make table 4.2.a
tab4.2.a <-  data %>%
  select(Enrolled, all_of(names(meds %>% select(-Arb_PersonId)))) %>%
  mutate_at(vars(all_of(names(meds %>% select(-Arb_PersonId)))),
            ~replace(., is.na(.),0)) %>%
  tbl_summary(by = Enrolled) %>%
  add_p(test = list(all_continuous() ~ "t.test")) %>%
  modify_spanning_header(all_stat_cols() ~ "**Overall**") %>%
  bold_labels()





  # Table 4.2 generating function
  make_sub_tab4.2 <- function(cohort){
    table <- data[data$Cohort == cohort,] %>%
      select(Enrolled, all_of(names(meds %>% select(-Arb_PersonId)))) %>%
      mutate_at(vars(all_of(names(meds %>% select(-Arb_PersonId)))),
                ~replace(., is.na(.),0)) %>%
      tbl_summary(by = Enrolled) %>%
      bold_labels()
    
    return(table)
    
  }


  # Apply table generating function to all cohorts
  tab4.2_sub_tables <- future_map(cohorts, make_sub_tab4.2)
  
  # Merge all Table 1 sub tables
  tab4.2 <- tbl_merge(tbls = list(tab4.2.a,
                                  tab4.2_sub_tables[[1]],
                                  tab4.2_sub_tables[[2]],
                                  tab4.2_sub_tables[[3]]),
                      tab_spanner = c("**Overall**",
                                      "**Sequence 1**",
                                      "**Sequence 2**",
                                      "**Sequence 3**"))



tbl_stack(list(tab4.1, tab4.2))



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FENCE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Determine why dat.idIndex is difference than EE
# 
# # Create dat.id
# dat.id <- read.csv("S:/FM/PATHWEIGH/Quantitative/Projects/baseline_characteristics/data/220121_2020-03-17_to_2021-03-16_wpv1_idIndex.csv", 
#                    header=TRUE,
#                    colClasses=c("Arb_PersonId"="character","Arb_EncounterId"="character"))
# # Processing variables in dat.id for the medications table
# dat.id <- dat.id %>% drop_na(ProviderNpi)
# dat.id$weight.gain <- ifelse(dat.id$N_MedsWeightGain_period>0, 1, 0)
# dat.id$weight.loss <- ifelse(dat.id$N_MedsWeightLoss_period>0, 1, 0)
# 
# 
# # Create EE
# ee <- visits %>%
#   drop_na(ProviderNpi) %>%
#   filter(EncounterDate == IndexDate)
# 
# write.csv(ee, file = "ee_test.csv")
# ee <- read.csv("ee_test.csv",
#                header = TRUE,
#                colClasses = c("Arb_PersonId"="character","Arb_EncounterId"="character"))
# 
# 
# # dat.id has 92 variables whereas dat.id only has 82
# names_dat.id <- names(dat.id)
# names_ee <- names(ee)
# 
# # Which names are not in names_ee
# missing_vars <- names_dat.id[!names_dat.id %in% names_ee]
# 
# # *** The extra comorbidities columns are missing which is on the to-do list
# # The comorbidities may be off because they were created in the subset of  EE 
# # patients instead of the aggregated EE vs ENE. May need to just get the names
# # and use them to filter out the comorbidities instead of using the 3% algorithm
# dat.id %<>%
#   select(all_of(names_ee[names_ee %in% names_dat.id]))
# 
# ee %<>%
#   select(all_of(names_ee[names_ee %in% names_dat.id]))
# # %>%
# #   mutate(Arb_PersonId = as.character(Arb_PersonId),
# #          Arb_EncounterId = as.character(Arb_EncounterId),
# #          EncounterDate = as.character(EncounterDate),
# #          BirthDate = as.character(BirthDate),
# #          IndexDate = as.character(IndexDate),
# #          GAD7 = as.integer(GAD7),
# #          PHQ2 = as.integer(PHQ2),
# #          PHQ2_Completed = as.integer(PHQ2_Completed),
# #          PHQ8 = as.integer(PHQ8),
# #          PHQ9 = as.integer(PHQ9),
# #          PHQ9_Completed = as.integer(PHQ9_Completed),
# #          ReferralDate = as.character(ReferralDate)) %>%
# #   mutate_if(is.factor, as.character)
# # 
# # 
# # unique(bind_rows(ee, dat.id))
# 
# # Returns whether or not the first rows are equal to one another
# non_matching_obs <- anti_join(dat.id, ee, by = names(ee))
# 
# test <- bind_rows(ee[ee$Arb_PersonId == non_matching_obs$Arb_PersonId[1],  ],
#                   dat.id[dat.id$Arb_PersonId == non_matching_obs$Arb_PersonId[1],  ])
# 
# 
# # Remove the ethnicity column because it is not used and causing mismatches
# table(dat.id$Ethnicity)
# table(ee$Ethnicity)
# 
# # Remove Ethnicity, count.nas
# dat.id %<>% select(-count.nas)
# ee %<>% select(-count.nas)
# 
# non_matching_obs <- anti_join(dat.id, ee, by = names(ee))
# 
# # Seems like race_ethnicity is a culprit there are some that don't get the same
# # Values
# table(dat.id$Race_Ethnicity) #
# table(ee$Race_Ethnicity)
# 
# # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FENCE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%