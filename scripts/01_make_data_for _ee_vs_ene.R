#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Carlos Rodriguez, PhD. CU Anschutz Dept. of Family Medicine
# 10/20/2022
# Make EE vs ENE data set

# DESCRIPTION:
# This script is used to generate data for the eligible and enrolled vs eligible
# not enrolled paper (EE vs ENE).

# Creates 3 principal data frames
# 1. seq_df2
# 2. visits
# 3. data - used to make the tables for the EE vs ENE paper

# seq_df2 ___________________________
# seq_df2 was the main data frame from which all of the baseline characteristics
# measurements were taken from and contains all the encounters for the baseline
# period.

# visits ____________________________
# visits is initially made from seq_df2, but then further eliminates encounters
# for which ProviderNpi contains an NA and creates the enrolled variable

# data _______________________________
# data is a data frame that contains one and only one visit per person. In order
# to capture only one visit per person, an "index visit" had to be assigned to
# those who were eligible but not enrolled. The strategy to do this was based
# off of previous code. However, one key difference is in the order of which
# providers with NA as NPI were excluded when compared to the baseline 
# characteristics paper. In bl char, providers with NA NPI were excluded at the
# end of the processing stream and after the index visit had been assigned.
# Here, the providers with NA NPI are excluded first, which allows the algorithm
# to assign other eligible visits as the index visit. However, this resulted in
# 11 extra patients in the data that were not present in baseline char.
# In order to match the baseline char figures, these 11 patients must be 
# excluded. 

# The baseline characteristics paper and the ee vs ene paper differ in the 
# number of patients that meet eligibility. In baseline char, it is a total of 
# 164,432. However, in the ee vs ene data it is 164,443, resulting in a 
# difference of 11 patients.The reason for this is because of the order of when 
# encounters that have an NA for Provider NPI are removed. In the baseline char
# paper, providers with NA as an NPI were removed towards the end of data proc-
# essing, which resulted in eliminating some patients that were eligible and 
# had a WPV, but did not have a provider NPI. In the ee vs ene, visits with NA
# in provider NPI were removed earlier, giving the algorithm a chance to find
# additional eligible visits. In order to match the ee vs ene data to the data
# in the baseline characteristics paper, 11 patients were excluded from the 
# ee vs ene data.


# NOTES:___________________
# n.b. for the EE patients:
# 20,411 in seq_df2.ee was made after processing the ee group from the baseline
#  characteristics paper. Not filtered by provider NPI != NA. Filtering this
#  data frame results in 20,383 (Index date was assigned first).
# 20,394, is what should have been assigned if the order of processing was to
#  first filter for provider NPI, then assign index date. because then the 
#  index date would have been assigned to a subsequent visit. We needed NPIs to 
#  ensure that visit could have been billed. Can't bill without NPI.
# 20,383, filtered for provider NPI != NA, and indexDate == EncounterDate
#  corresponds to the same group of patients in the baseline char paper.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load libraries ---------------------------------------------------------------
pacman::p_load(tidyverse,
               magrittr,
               data.table,
               openxlsx,
               here,
               tictoc,
               furrr)

tic()
# Load the data  ---------------------------------------------------------------
# These are the individual epic compass tables in Rdata format that need 
# processing
load(here("../baseline_characteristics/data/220121.RData"))


# Specify parameters -----------------------------------------------------------
Date.min <- "2020-03-17"
Date.max <- "2021-03-16"


# Load the provider npi and sex data from the 06/06/2022 delivery --------------
provider_sex <- fread(
  here("../../../PATHWEIGH_DATA_SECURE/20220606/C2976_Table2_Encounter_20220606.csv"),
  select = c("ProviderNpi", "ProviderSex"))


# Prep Encounter Table ---------------------------------------------------------
# Filter encounters that are in the specified date range
encounter_sub <- encounter %>% 
  filter(EncounterDate >= Date.min,
         EncounterDate <= Date.max)

# Filter the NPIs in encounter_sub, omit NAs, and then keep only the distinct 
# rows. NAs omitted to prevent left_join from introducing additional rows
provider_sex <- provider_sex %>% 
  filter(ProviderNpi %in% encounter_sub$ProviderNpi) %>%
  na.omit() %>%
  distinct()

# join the encounter_sub and provider sex and reorganize columns
encounter_sub <- left_join(encounter_sub, 
                           provider_sex, 
                           by = "ProviderNpi") %>%
                 select(Arb_PersonId:ProviderNpi, 
                        ProviderSex, 
                        everything())


# Merge Encounter, Patient, and Clinic data sets -------------------------------
join2 <- left_join(encounter_sub,
                   patient,
                   by = "Arb_PersonId") %>%
         left_join(.,
                   clinic %>% select(DepartmentEpicId, GroupID),
                   by = "DepartmentEpicId")

# Create intervention variable depending on GroupID and time period seen in
time1change <- "2021-03-17"
time2change <- "2022-03-17"
time3change <- "2023-03-17"


# Intervention  ----------------------------------------------------------------
# Intervention is based on date, not whether or not they had a WPV in the 
# intervention period
join2$intervention <- NA

join2$intervention[join2$GroupID==1] <-
  ifelse(join2$EncounterDate[join2$GroupID==1]<time1change, 0, 1)               

join2$intervention[join2$GroupID==2] <-
  ifelse(join2$EncounterDate[join2$GroupID==2]<time2change, 0, 1)               

join2$intervention[join2$GroupID==3] <-
  ifelse(join2$EncounterDate[join2$GroupID==3]<time3change, 0, 1)               

# Age --------------------------------------------------------------------------
# Calculate age of patient at visit using EncounterDate and BirthDate and censor
# individuals older than 90, but keep them in the analysis (10/27/21) 
join2 <- join2 %>% 
  mutate(Age = as.numeric((EncounterDate - BirthDate)/365)) %>%
  mutate_at("Age", 
            ~ifelse(. > 90, 90, .))

# Weight -----------------------------------------------------------------------
# Set patient weights to NA if they are equal to - 999
join2 <- join2 %>%
  mutate_at(c("Height", 
              "Weight"),
            ~ifelse(. == -999, NA, .))

# Weight unit is originally in ounces, convert to pounds
join2$Weight_lbs <- join2$Weight/16


# Insurance --------------------------------------------------------------------
# Collapsed insurance categories according to values from Mark Gritz
join2 <- join2 %>% 
  mutate(Insurance = as.factor(FinancialClass))

join2$Insurance <- fct_collapse(join2$Insurance,
                                Commercial = c("CU - UA NET",
                                               "Managed Care", 
                                               "Special Accounts", 
                                               "Tricare", 
                                               "UCHEALTH EMPLOYEE PLAN", 
                                               "Worker's Comp"),
                                `Self-Pay`= c("Indigent Care"),
                                Medicaid = c("Managed Medicaid", 
                                             "Colorado Medicaid", 
                                             "Out of State Medicaid"),
                                Medicare = c("Managed Medicare",
                                             "Medicare")) 


# Sex --------------------------------------------------------------------------
# Clean up the sex variable, set to NA if it equal "X"
join2$Sex <- na_if(join2$Sex, "X")


# Race & Ethnicity -------------------------------------------------------------
# Create a new variable, Race_Ethnicity where any Hispanic in Ethnicity is set 
# to Hispanic or Latino and where any NA in Race is set to Unknown
join2 <- join2 %>% 
  mutate(Race_Ethnicity = 
           ifelse(Ethnicity == "Hispanic", "Hispanic or Latino", Race),
         Race_Ethnicity = 
           ifelse(is.na(Race_Ethnicity), "Unknown", Race_Ethnicity))

# Convert to factor and collapse AA, AN, Mult Race, NH, PI, to Other 
join2$Race_Ethnicity <- join2$Race_Ethnicity %>% 
  as_factor() %>%
  fct_collapse(.,
               `Non-Hispanic White` = "White or Caucasian",
               "Other" = c("American Indian and Alaska Native",
                           "Multiple Race",
                           "Native Hawaiian and Other Pacific Islander",
                           "Other Pacific Islander"))


# Smoking ----------------------------------------------------------------------
# Collapse smoking categories - current, former, never, other
join2$SmokingStatus <- join2$Smoking_Status %>%
  as_factor() %>%
  fct_collapse(., 
               "Current" = c("Current Every Day Smoker",
                             "Current Some Day Smoker",
                             "Heavy Tobacco Smoker",
                             "Light Tobacco Smoker"),
               "Former" = "Former Smoker",
               "Never" = c("Never Smoker",
                           "Passive Smoke Exposure - Never Smoker"),
               "Unknown" = c("Smoker, Current Status Unknown",
                             "Unknown If Ever Smoked",
                             "Never Assessed")) %>%
  recode_factor(., Unknown = NA_character_)

# Factor the original Smoking_status columns 
join2$Smoking_Status <- factor(join2$Smoking_Status,
                               levels=c("Current Every Day Smoker",
                                        "Heavy Tobacco Smoker",
                                        "Current Some Day Smoker", 
                                        "Light Tobacco Smoker",
                                        "Smoker, Current Status Unknown",
                                        "Former Smoker",
                                        "Never Smoker",
                                        "Passive Smoke Exposure - Never Smoker",
                                        "Unknown If Ever Smoked",
                                        "Never Assessed"))


# Clean up vitals --------------------------------------------------------------
# Set vitals variables to modify
cols_to_mod <- c("HeartRate",
                 "Respiratoryrate",
                 "Systolic_blood_pressure",
                 "Diastolic_blood_pressure",
                 "Temperature")

# Set *Restricted to NA at the vitals columns and convert them to numeric
join2 <- join2 %>%
  mutate_at(cols_to_mod,
            ~na_if(., "*Restricted")) %>%
  mutate_at(cols_to_mod,
            as.numeric)


# BMI --------------------------------------------------------------------------
#Create a computed BMI variable based on Height and Weight
join2 <- join2 %>%
  mutate(BMI_comp = (Weight/16)/(Height^2) * 703)

# Set any BMIs > 65 to NA and create a BMI_use variable that is a blend of the 
# two (use computed variable if COMPASS is not available)
# Create new BMI variable, using calculated BMI only if COMPASS BMI is not 
# available.
join2 <- join2 %>%
  mutate_at(c("BMI", "BMI_comp"),
            ~ifelse(. > 65, NA, .))

# Create the new BMI_use variable and fill in NAs with BMI_comp
join2 <- join2 %>%
  mutate(BMI_use = BMI,
         BMI_use = ifelse(is.na(BMI), BMI_comp, BMI_use))


# Eligible Variable ------------------------------------------------------------
# Eligible patients: Adults (age >=18) with BMI>=25 at index visit. 

# Create variable to indicate if the encounter is eligible (Age >=18, BMI>=25)
join2 <- join2 %>%
  mutate(Eligible = ifelse(Age >= 18 & BMI_use >=25, 1, 0),
         Eligible = ifelse(is.na(Eligible), 0, Eligible))

# Patients with height <54in (4.5'), height >90in (7.5'), or weight >9600oz 
#(600 lbs) are excluded.
join2$Eligible[which(join2$Weight>9600 | join2$Height<54 | join2$Height>90)] <- 0


# Repeat Encounters ------------------------------------------------------------
# Count the number of NAs in each row, arrange them, then take the head to keep 
# for duplicated encounters
join2 <- join2 %>%
  mutate(count.nas = rowSums(is.na(.))) %>%
  arrange(Arb_PersonId, Arb_EncounterId, count.nas, Smoking_Status) %>%
  group_by(Arb_EncounterId) %>%
  #  filter(n() > 1) %>% # used to filter which are duplicated
  slice_head() %>%
  ungroup()

# WPV by Chief Complaints ------------------------------------------------------
## Relevant Chief Complaints
all_chief_complaints <- names(
  table(join2$EncounterChiefComplaints[which(join2$Eligible==1)]))              

weight_complaints <- all_chief_complaints[
  which(str_detect(all_chief_complaints,"WEIGHT")==TRUE)]                       

obese_complaints <- all_chief_complaints[
  which(str_detect(all_chief_complaints,"OBES")==TRUE)]                         

#There are no chief complaints related to overweight or over-weight
overweight_complaints <- all_chief_complaints[
  which(str_detect(all_chief_complaints,"OVERWEIGHT")==TRUE)]

##Leigh says to exclude all chief complaints that use "weight loss"             
# UCH AMB WEIGHT CHECK; LABS ONLY; EKG VISIT
# |which(str_detect(weight_complaints,"WEIGHT CHECK")
weight_complaints_exclude <- weight_complaints[
  which(str_detect(weight_complaints,"WEIGHT LOSS")==TRUE)]

#We want to keep weight loss complaints if they appear with weight management,
# or as weight loss consult though....
weight_complaints_use <- weight_complaints[
  which(str_detect(weight_complaints,"WEIGHT LOSS")==FALSE)]

##Find complaints that are weight loss consult and weight management and add back in
# determine which complaints from the exclusion list have wt mgmt and wt loss complaints
weight_complaints_use2 <- weight_complaints_exclude[
  which(str_detect(weight_complaints_exclude,
                   c("WEIGHT MANAGEMENT|WEIGHT LOSS CONSULT"))==TRUE)]

relevant_chief_complaints <- c(weight_complaints_use,
                               weight_complaints_use2,
                               obese_complaints,
                               overweight_complaints)

##Create a flag if patient had a WPV from chief complaint in join2
join2$WPV_CC <- 0
join2$WPV_CC[which(
  join2$EncounterChiefComplaints %in% relevant_chief_complaints)] <- 1


# WPV by ICD 10 Codes ----------------------------------------------------------
# Relevant ICD codes were the E66 series (below) and Z68.25-Z68.45.
###Are only interested in codes for overweight, obesity, etc
#E66.01 E66.09  E66.1  E66.2  E66.3  E66.8  E66.9 Z68.25-Z68.45
#Z76.89 has been removed as inclusion criteria for WPV

e66_index <- str_detect(dx$DiagnosisCode,
                        c("E66.01|E66.09|E66.1|E66.2|E66.3|E66.8|E66.9"))


z68_index <- str_detect(dx$DiagnosisCode,
                        paste0("Z68.25",
                               paste0("|Z68.",25:45,collapse = ""),collapse=""))

# Create a vector of encounter ids that had relevant ICD 10 codes
icd_encounters <- names(
  table(dx$Arb_EncounterId[which(e66_index=="TRUE" | z68_index=="TRUE")]))      

# Create WPV icd codes indicator in join2
join2$WPV_ICD <- 0
join2$WPV_ICD[which(join2$Arb_EncounterId %in% icd_encounters)] <- 1              


# WPV by Flowsheets ------------------------------------------------------------
# Flowsheets - for Obesity Brief HPI and PATHWEIGH flowsheets
# Below are the number of instances the PATHWEIGH flowsheet was used since 
# roll-out. Neither flowsheet was used during the baseline period.
# Use of these flowsheets was defined using appropriate flowsheet row Epic IDs.

# Create flag for encounters that had flowsheets of interest pull out flowsheet 
# row ids for each type
OBHPI_ids <- flowsheet_ids$Flowsheet_RowID[
  which(flowsheet_ids$obesity_brief_HPI=="X")]                                  

PW_flow_ids <- flowsheet_ids$Flowsheet_RowID[
  which(flowsheet_ids$PATHWEIGH=="X")]                                          

join2$WPV_OBHPI <- 0 
join2$WPV_PW_flow <- 0

OBHPI_encounter_ids <- flowsheets$Arb_EncounterId[
  which(flowsheets$FlowsheetRowEpicId %in% OBHPI_ids)]                          

join2$WPV_OBHPI[which(join2$Arb_EncounterId %in% OBHPI_encounter_ids)] <- 1     

PW_encounter_ids <- flowsheets$Arb_EncounterId[
  which(flowsheets$FlowsheetRowEpicId %in% PW_flow_ids)]                        

join2$WPV_PW_flow[which(join2$Arb_EncounterId %in% PW_encounter_ids)] <- 1      

#The obesity brief HPI is a subset of the pathweigh flowsheet, and needs to 
# modified anytime PW is used, to prevent double counting
join2$WPV_OBHPI[which(join2$WPV_PW_flow==1)] <- 0                               


# WPV by Visit Type ------------------------------------------------------------
# WPVs can be defined using Visit Type IDs, where in-person VisitTypeID = 413519 
# and virtual VisitTypeID = 415110. 

#Create in person or telehealth (virtual) indicator variables
join2$WPV_IP <- 0
join2$WPV_TH <- 0

join2$WPV_IP[which(join2$VisitTypeId==413519)] <- 1
join2$WPV_TH[which(join2$VisitTypeId==415110)] <- 1                             


# WPV by use of PATHWEIGH Smart Set --------------------------------------------
# WPVs can be defined through use of the PATHWEIGH Smart Set.
join2$WPV_smart <- 0

join2$WPV_smart[which(join2$Arb_EncounterId %in% smart$Arb_EncounterId)] <- 1   

# Create final column that says if they had a WPV (any of the above happened)
join2$WPV <- apply(join2[,grepl("WPV_",names(join2))],1,sum)                    

# Set any row where WPV > 0 to 1 as a binary rather than count variable for WPVs
join2$WPV_v2 <- 0
join2$WPV_v2[join2$WPV > 0] <- 1


# Set Index Dates --------------------------------------------------------------
#baseline_index is determined by whether or not the patient has a visit during
# the specified date range.
baseline_index <- which(
  join2$EncounterDate >= Date.min & join2$EncounterDate <= Date.max)

# id.inrange is made by selecting cases that belong to baseline_index
id.inrange <- join2$Arb_EncounterId[baseline_index]

# Assign an Index date
tempdat.id <- join2 %>%
  filter(Eligible == 1,
         WPV >= 1,
         Arb_EncounterId %in% id.inrange) %>%
  group_by(Arb_PersonId,
           intervention) %>%
  arrange(Arb_PersonId,
          EncounterDate) %>%
  slice_head() %>%
  #  mutate(IndexDate = EncounterDate, Seq = GroupID) %>%
  mutate(IndexDate = EncounterDate) %>% 
  ungroup()

## Join IndexDate and Seq columns to join2
join2 <- left_join(join2, tempdat.id[
  ,c("Arb_EncounterId", "intervention", "IndexDate")],
  by = c("Arb_EncounterId", "intervention")) %>%
  mutate(Seq = GroupID)

# Create a sequence variable called Cohort that indicates Cohorts 1, 2, or 3 that 
# is based on the sequence number, which in turn is based on the Group ID
join2$Cohort<-paste0("Cohort ", join2$Seq)


# Create seq_df2 ---------------------------------------------------------------
# seq_df2 is a subset of join2(encounters) where the Encounter date is after
# the Index date. Only encounters with an Index date should be filtered and only
# encounters that satisfy eligibility + WPV should have an index date
seq_df2 <- join2 %>% 
  filter(EncounterDate>=IndexDate)

# Unique visits
seq_unique <- seq_df2 %>% 
  select(Arb_PersonId, IndexDate) %>%
  distinct()


# Get the top comorbidities from those in the baseline characteristics paper ----
# Load the comorbidities of interest (cois), an Rdata set that was prepared 
# using a .csv file from Leigh containing the comorbidities of interest.
load(
  here("../baseline_characteristics/working_files/comorbidities/comorbidities_of_interest_220121.RData"))

# Prepare the dx dataframe for processing and analysis
# Filter to patients of interest, within the time period of interest, create two
# new columns with modified ICD-10 code, merge in the Cohort # by personId, 
# merge in category and disease.state by code.
dx_sub <- dx %>% 
  filter(Arb_PersonId %in% seq_df2$Arb_PersonId,
         DiagnosisDate >= Date.min,
         DiagnosisDate <= Date.max) %>%
  mutate(ICD_Header = sub("\\..*", "", DiagnosisCode),
         code = sub("\\.", "", DiagnosisCode)) %>%
  left_join(., select(seq_df2, all_of(c("Arb_PersonId", "Cohort"))), by = "Arb_PersonId") %>%
  left_join(., select(cois, all_of(c("category", "disease.state", "code"))), by = "code")

# Ensure that there is only one count per code header group by patient, group by 
# disease.state because osteoarthritis spans multiple ICD-10 headers
dx_sub_coi_count <- dx_sub %>% 
  group_by(Arb_PersonId, disease.state) %>%
  slice_head() %>%
  filter(code %in% cois$code) %>%
  ungroup()

# Determine which comorbidities are found in >3% of the sample, i.e. the most 
# common which will be listed in the first column
top_comorbidities <- dx_sub_coi_count %>% 
  group_by(disease.state) %>%
  count(code) %>%
  summarise(sum = sum(n)) %>%
  arrange(desc(sum)) %>%
  mutate(percent_overall = sum/nrow(seq_df2) * 100) %>%
  filter(percent_overall >= 3)


# Medications for EE -----------------------------------------------------------
# Process the medications separately to avoid introducing medications for 
# EE patients that were captured before the index date

meds_sub <- meds %>% 
  filter(ActiveMed == "Y", 
         Arb_PersonId %in% seq_df2$Arb_PersonId) %>%
  mutate_at(c("MedicationName", "GenericName"), tolower)

# Read in Leigh's medications
meds_ref <- read.xlsx(
  here("../baseline_characteristics/working_files/medications/medications.xlsx"),
  sheet = "UniqueMedName")

# Process meds_ref
meds_ref <- meds_ref %>% 
  mutate(weight.gain = ifelse(is.na(weight.gain), 0, 1 ),
         weight.loss = ifelse(is.na(weight.loss), 0, 1 ),
         AOM = ifelse(is.na(AOM), 0, 1 ))

weightgain_meds <- unique(meds_ref$uniqueMedName[meds_ref$weight.gain==1])      
weightloss_meds <- unique(meds_ref$uniqueMedName[meds_ref$weight.loss==1])
AOM_meds <- unique(meds_ref$uniqueMedName[meds_ref$AOM==1])

# Select columns and keep distinct rows
meds_ref <- meds_ref %>% 
  select(-Generic.name, -trade.names) %>%
  distinct()

## load the meds list
meds_list <- read.xlsx(
  here("../baseline_characteristics/working_files/medications/medications.xlsx"),
  sheet="Medications")

## Process meds list
meds_list <-  meds_list %>%
  mutate_at(c("Name", "GenericName"), tolower) %>% 
  select(uniqueMedName, GenericName) %>%
  drop_na() %>%
  distinct()

# Merge data frames, select columns
meds_full <- meds_list %>% 
  left_join(., meds_ref, by = "uniqueMedName") %>%
  right_join(., meds_sub, by = "GenericName") %>%
  select(all_of(c("Arb_PersonId","OrderedDate","uniqueMedName","weight.gain","weight.loss","AOM")))

# filter out the meds_AOM data frame
meds_AOM <- meds_full %>% 
  filter(AOM == 1) %>%
  left_join(., distinct(seq_df2, Arb_PersonId, IndexDate), by = "Arb_PersonId")

#Subset to the time period of interest (baseline period)
meds_full <- meds_full %>%
  filter(OrderedDate >= Date.min, OrderedDate <= Date.max)

meds_AOM <- meds_AOM %>%
  filter(OrderedDate >= IndexDate, OrderedDate <= Date.max)

#Remove duplicate medication types per person
meds_full <- meds_full %>% 
  select(-OrderedDate) %>%
  distinct() %>%
  arrange(Arb_PersonId)

# Create an anti obesity medication data frame
# n.b. at this point there will be duplicate Arb_PersonIds in the dataframe
meds_AOM <- meds_AOM %>% distinct(Arb_PersonId, IndexDate, uniqueMedName)

#Count number of meds during the baseline period
meds_counts <- meds_full %>%
  group_by(Arb_PersonId) %>%
  summarise("N_MedsWeightGain_period" = sum(weight.gain),
            "N_MedsWeightLoss_period" = sum(weight.loss),
            .groups="rowwise")

meds_counts_bymeds <- meds_full %>%
  group_by(uniqueMedName) %>%
  summarise("N_MedsWeightGain_period" = sum(weight.gain),
            "N_MedsWeightLoss_period" = sum(weight.loss),
            .groups="rowwise")

# join meds_AOM, person ID and Cohort information
meds_AOM <- meds_AOM %>%
  left_join(., distinct(seq_df2, Arb_PersonId, Cohort), by = "Arb_PersonId")

meds_counts_gain <- subset(meds_counts_bymeds, meds_counts_bymeds$uniqueMedName%in%weightgain_meds)
meds_counts_loss <- subset(meds_counts_bymeds, meds_counts_bymeds$uniqueMedName%in%weightloss_meds)
meds_counts_AOM <- subset(meds_counts_bymeds, meds_counts_bymeds$uniqueMedName%in%AOM_meds)
meds_counts_AOM <- subset(meds_counts_bymeds, meds_counts_bymeds$uniqueMedName%in%AOM_meds)


# Join the meds_counts and seq_df2 via left join
seq_df2 <- left_join(seq_df2,
                     meds_counts[,c("Arb_PersonId","N_MedsWeightGain_period","N_MedsWeightLoss_period")],
                     by=c("Arb_PersonId"))

seq_df2$N_MedsWeightGain_period[is.na(seq_df2$N_MedsWeightGain_period)] <- 0
seq_df2$N_MedsWeightLoss_period[is.na(seq_df2$N_MedsWeightLoss_period)] <- 0

# Convert the meds_AOM data to wide format
meds_aom.ee <- meds_AOM %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = uniqueMedName, values_from = count) %>%
  select(-IndexDate, -Cohort) %>%
  distinct() %>%
  group_by(Arb_PersonId) %>%
  summarise_each(funs(sum(., na.rm = TRUE)))

# Merge the data frames
seq_df2 %<>%
  left_join(meds_aom.ee, by = "Arb_PersonId")

# Make a copy of the eligible and enrolled visits with their aom_medications and
# set aside
seq_df2.ee <- seq_df2


# Now we backtrack to create seqdf2 with all visits not just those that occurred
# on the index date. This will add an index date
seq_df2 <- join2

# Save the encounter ids that did not have an index date, these will need to 
# be replaced later on to accurately get the ee data frame
encounters_wo_id <- seq_df2 %>%
  filter(is.na(seq_df2$IndexDate)) %>%
  select(Arb_EncounterId)

# Change all of the NA index dates to the encounterdate to ensure that those in
# the ene group can get their labs and o2cpap counted
seq_df2 %<>%
  mutate(IndexDate = if_else(is.na(IndexDate), EncounterDate, IndexDate))
  
seq_unique <- seq_df2 %>%
  select(Arb_PersonId, IndexDate) %>%
  distinct()


# Medications for ENE ----------------------------------------------------------
# The 02_Rimage_to_analysis_datasets.R script was originally designed to only 
# fully processed data for encounters/patients that were eligible and enrolled.
# The following represents the strategy towards preparing meds, comorbidities, 
# labs, etc for the ene patients.

# Filter out the EncounterIds found in seq_df2.ee (EE group)
seq_df2 %<>%
  filter(!Arb_EncounterId %in% seq_df2.ee$Arb_EncounterId)

# Process meds for the ENE group
meds_sub <- meds %>% 
  filter(ActiveMed == "Y", 
         Arb_PersonId %in% seq_df2$Arb_PersonId) %>%
  mutate_at(c("MedicationName", "GenericName"), tolower)

# Read in Leigh's medications
meds_ref <- read.xlsx(
  here("../baseline_characteristics/working_files/medications/medications.xlsx"),
  sheet = "UniqueMedName")

# Process meds_ref
meds_ref <- meds_ref %>% 
  mutate(weight.gain = ifelse(is.na(weight.gain), 0, 1 ),
         weight.loss = ifelse(is.na(weight.loss), 0, 1 ),
         AOM = ifelse(is.na(AOM), 0, 1 ))

weightgain_meds <- unique(meds_ref$uniqueMedName[meds_ref$weight.gain==1])      
weightloss_meds <- unique(meds_ref$uniqueMedName[meds_ref$weight.loss==1])
AOM_meds <- unique(meds_ref$uniqueMedName[meds_ref$AOM==1])

# Select columns and keep distinct rows####
meds_ref <- meds_ref %>% 
  select(-Generic.name, -trade.names) %>%
  distinct()

# load the meds list
meds_list <- read.xlsx(
  here("../baseline_characteristics/working_files/medications/medications.xlsx"),
  sheet="Medications")

# Process meds list
meds_list <-  meds_list %>%
  mutate_at(c("Name", "GenericName"), tolower) %>% 
  select(uniqueMedName, GenericName) %>%
  drop_na() %>%
  distinct()

# Merge data frames, select columns
meds_full <- meds_list %>% 
  left_join(., meds_ref, by = "uniqueMedName") %>%
  right_join(., meds_sub, by = "GenericName") %>%
  select(all_of(c("Arb_PersonId","OrderedDate","uniqueMedName","weight.gain","weight.loss","AOM")))

# filter out the meds_AOM data frame
meds_AOM <- meds_full %>% 
  filter(AOM == 1) %>%
  left_join(., distinct(seq_df2, Arb_PersonId, IndexDate), by = "Arb_PersonId")

#Subset to the time period of interest (baseline period)
meds_full <- meds_full %>%
  filter(OrderedDate >= Date.min, OrderedDate <= Date.max)

meds_AOM <- meds_AOM %>%
  filter(OrderedDate >= IndexDate, OrderedDate <= Date.max)

#Remove duplicate medication types per person
meds_full <- meds_full %>% 
  select(-OrderedDate) %>%
  distinct() %>%
  arrange(Arb_PersonId)

# Create an anti obesity medication data frame
# n.b. at this point there will be duplicate Arb_PersonIds in the dataframe
meds_AOM <- meds_AOM %>% distinct(Arb_PersonId, IndexDate, uniqueMedName)

#Count number of meds during the baseline period
meds_counts <- meds_full %>%
  group_by(Arb_PersonId) %>%
  summarise("N_MedsWeightGain_period" = sum(weight.gain),
            "N_MedsWeightLoss_period" = sum(weight.loss),
            .groups="rowwise")

meds_counts_bymeds <- meds_full %>%
  group_by(uniqueMedName) %>%
  summarise("N_MedsWeightGain_period" = sum(weight.gain),
            "N_MedsWeightLoss_period" = sum(weight.loss),
            .groups="rowwise")

# join meds_AOM, person ID and Cohort information
meds_AOM <- meds_AOM %>%
  left_join(., distinct(seq_df2, Arb_PersonId, Cohort), by = "Arb_PersonId")

meds_counts_gain <- subset(meds_counts_bymeds, meds_counts_bymeds$uniqueMedName%in%weightgain_meds)
meds_counts_loss <- subset(meds_counts_bymeds, meds_counts_bymeds$uniqueMedName%in%weightloss_meds)
meds_counts_AOM <- subset(meds_counts_bymeds, meds_counts_bymeds$uniqueMedName%in%AOM_meds)
meds_counts_AOM <- subset(meds_counts_bymeds, meds_counts_bymeds$uniqueMedName%in%AOM_meds)


# join the meds counts
seq_df2 <- left_join(seq_df2,
                     meds_counts[,c("Arb_PersonId","N_MedsWeightGain_period","N_MedsWeightLoss_period")],
                     by=c("Arb_PersonId"))

seq_df2$N_MedsWeightGain_period[is.na(seq_df2$N_MedsWeightGain_period)] <- 0
seq_df2$N_MedsWeightLoss_period[is.na(seq_df2$N_MedsWeightLoss_period)] <- 0

# Convert the meds_AOM data to wide format
meds_aom.ene <- meds_AOM %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = uniqueMedName, values_from = count) %>%
  select(-IndexDate, -Cohort) %>%
  distinct() %>%
  group_by(Arb_PersonId) %>%
  summarise_each(funs(sum(., na.rm = TRUE)))

# Merge the data frames
# *** Will need to go back and make sure that the EE visits after the index date
# have been zeroed out, or that there is a way to filter out the EE patients
seq_df2 %<>%
  left_join(meds_aom.ene, by = "Arb_PersonId")


seq_df2 <- bind_rows(seq_df2, seq_df2.ee) 



# Comorbidities ----------------------------------------------------------------
# The comorbidities in the baseline paper were based on the total percentages 
# from only the EE patients. Thus this script also addresses using the same 
# comorbidities in the baseline characteristics paper to determine the 
# percentage of each comorbidity in the ENE patients. Otherwise, the algorithms 
# would count the comorbidities for all EE and ENE patients and the most 
# frequent comorbidities would not match those in the baseline characteristics
# paper.

# Prepare the dx dataframe for processing and analysis
# Filter to patients of interest, within the time period of interest, create two
# new columns with modified ICD-10 code, merge in the Cohort # by personId, 
# merge in category and disease.state by code.
dx_sub <- dx %>% 
  filter(Arb_PersonId %in% seq_df2$Arb_PersonId,
         DiagnosisDate >= Date.min,
         DiagnosisDate <= Date.max) %>%
  mutate(ICD_Header = sub("\\..*", "", DiagnosisCode),
         code = sub("\\.", "", DiagnosisCode)) %>%
  left_join(., select(seq_df2, all_of(c("Arb_PersonId", "Cohort"))), by = "Arb_PersonId") %>%
  left_join(., select(cois, all_of(c("category", "disease.state", "code"))), by = "code")

# Ensure that there is only one count per code header group by patient, group by 
# disease.state because osteoarthritis spans multiple ICD-10 headers
# *** Could be a bottle neck
dx_sub_coi_count <- dx_sub %>% 
  group_by(Arb_PersonId, disease.state) %>%
  slice_head() %>%
  filter(code %in% cois$code) %>%
  ungroup()

# Create a dataframe to merge into seq_df2 using only the most common 
# comorbidities. The mutate verb introduces a count column to mark with a 1
# Which will fill in a 1 for a given column when converting to wide format
# summarise_each may need to be modified since it is deprecated. Using lambdas 
# take out funs and put list(~ sum(., na.rm = TRUE)) then use summarise across
dx_sub_coi_count <- dx_sub_coi_count %>% 
  filter(disease.state %in% top_comorbidities$disease.state) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = disease.state, values_from = count) %>%
  select(Arb_PersonId, all_of(top_comorbidities$disease.state)) %>% 
  group_by(Arb_PersonId) %>% 
  summarise_each(funs(sum(., na.rm = TRUE)))

# Merge the data frames and then convert all of the NAs to zeros for table creation
seq_df2 <- left_join(seq_df2, dx_sub_coi_count, by = "Arb_PersonId") %>%
  mutate_at(vars(all_of(top_comorbidities$disease.state)), 
            ~replace(., is.na(.), 0))


# Reasonable Ranges/Units  -----------------------------------------------------
# Height inches to cm = 1 : 2.54
seq_df2$Height <- seq_df2$Height * 2.54

# weight pounds to kg = 1 : 2.20462
seq_df2$Weight_kgs <- seq_df2$Weight_lbs / 2.20462

# HeartRate
out_of_range_hr <- seq_df2 %>% 
  filter(HeartRate < 30, HeartRate > 200)

# Respiratory range is 6 - 50
out_of_range_rr <- filter(seq_df2,
                          Respiratoryrate < 6 | Respiratoryrate > 50)

# How many observations that fall outside of range?
out_of_range_bpd <-
  filter(seq_df2,
         Diastolic_blood_pressure < 40 | Diastolic_blood_pressure > 140)

# How many observations outside of temp range
out_of_range_temp <- filter(seq_df2, Temperature < 96)

# concatenate row wise the out of range folks
out_of_range_exclusions <- bind_rows(list(out_of_range_hr,
                                          out_of_range_rr,
                                          out_of_range_bpd,
                                          out_of_range_temp))
# Remove duplicates 
out_of_range_exclusions <- out_of_range_exclusions %>% 
  group_by(Arb_PersonId) %>%
  slice_head()

# # Set out of range exclusions to NA
seq_df2$Temperature[seq_df2$Temperature < 96] <- NA
seq_df2$HeartRate[seq_df2$HeartRate < 30] <- NA
seq_df2$Respiratoryrate[seq_df2$Respiratoryrate < 6] <- NA
seq_df2$Respiratoryrate[seq_df2$Respiratoryrate > 50] <- NA
seq_df2$Diastolic_blood_pressure[seq_df2$Diastolic_blood_pressure < 40] <- NA
seq_df2$Diastolic_blood_pressure[seq_df2$Diastolic_blood_pressure > 140] <- NA


# Labs and Procedures -----------------------------------------------------
#Time frame is between 6 months after (6*30 = 180 days) to 2 weeks prior (14 days)
# To their index date
##Now see how many are on CPAP/BIPAP/O2 using procedures table
#Create indicator for O2/CPAP/BIPAP
#cat("Processing labs and procedures ...", "\n")

# Assign an index date to get into seq_unique
# Or change the algorithm

proc_full <- left_join(seq_unique,
                       procedure[,c("Arb_PersonId","ProcedureDate","Category")],
                       by="Arb_PersonId")

O2CPAPBIPAP_dat <- proc_full %>%
  filter(Category=="CPAP/BIPAP/O2") %>%
  filter((IndexDate-ProcedureDate) >= -14 | (IndexDate-ProcedureDate) <= 180) %>%
  group_by(Arb_PersonId, IndexDate) %>%
  summarise("O2CPAPBIPAP"=length(names(table(Arb_PersonId))), .groups="rowwise")
  
seq_df2 <- left_join(seq_df2, O2CPAPBIPAP_dat, by=c("Arb_PersonId", "IndexDate"))

seq_df2$O2CPAPBIPAP <- ifelse(is.na(seq_df2$O2CPAPBIPAP), 0, 1)

##Bring in labs information: A1c (%),TSH (mIU/L),TG (mg/dl),HDL (mg/dl; * note decrease in value),
#AST (U/L),ALT (U/L),eGFR (ml/min/1.73 m2; * note decrease in value)
#Per our 11/10/21 meeting, use closest lab to first WPV – up to 6 months prior or 2 weeks after

##Restrict only to people of interest and labs of interest
str_labs <- c("A1C","TSH","Triglycerides","HDL","AST","ALT","eGFR","Cystatin C")
labs_full <- left_join(seq_unique,
                       labs[,c("Arb_PersonId","LabCollectionDate","Category","NumericValue")],
                       by="Arb_PersonId")
labs_full <- subset(labs_full, Category %in% str_labs)
labs_full <- subset(labs_full, !is.na(labs_full$NumericValue))

##Now for each person and each lab type, need to find the value that is closest to 0 (means it is the closest to  index WPV)
labs_dat <- labs_full %>%
  filter((IndexDate-LabCollectionDate) >= -14 | (IndexDate-LabCollectionDate) <= 180)

# ______________________________________________________________________________
# Slicing labs was a bottle neck for processing. Incorporating parallel 
# processing reduced processing time from about 7 minutes to about 2.5 minutes
# Set the options for furrr----
cores <- (future::availableCores() - 1)
labs <- seq_along(1:cores)

options(future.rng.onMisuse = "ignore")
plan(multisession, workers = cores)

# Set up the split_group
labs_dat$split_group <- cut_number(labs_dat$Arb_PersonId, 
                                   n = cores, 
                                   labels = labs)


# Slice labs function
slice_labs <- function(data, split_by){
  split_by <- enquo(split_by)
  
  data %>%
    group_split(!!split_by) %>%
    future_map_dfr(~.x %>%
                     group_by(Arb_PersonId, IndexDate, Category) %>%
                     slice((which.min(abs(IndexDate-LabCollectionDate))))
    )
}

gc()
labs_dat <- slice_labs(labs_dat, split_group)
labs_dat %<>% select(-split_group)

# ______________________________________________________________________________

labs_dat <- subset(labs_dat, select=-c(LabCollectionDate))

labs_dat_wide <- labs_dat %>%
  spread(Category, NumericValue)

#seq_df2 is the longitudinal data set used for analysis where risk factors (e.g., comborbidities, labs) are
seq_df2 <- left_join(seq_df2, labs_dat_wide, by=c("Arb_PersonId","IndexDate"))


# Screeners ---------------------------------------------------------------
screener <- flowsheets[grep("PHQ|GAD",flowsheets$FlowsheetRowDisplayName),]

screener <- subset(screener, !is.na(screener$Value))

#Checked with Leigh about dropping "A" scores
screener <- screener[-grep("Not Completed Reason|2A|9\\(A\\)",screener$FlowsheetRowDisplayName),]
screener$Value <- ifelse(screener$Value=="Yes", 1, ifelse(screener$Value=="No", 0, screener$Value))

screener_full <- left_join(seq_unique, screener[,c("Arb_PersonId","FlowsheetDate","FlowsheetRowDisplayName","Value")], by="Arb_PersonId")
screener_full <- subset(screener_full, !is.na(screener_full$Value))

screener_full$Category <- NA
screener_full$Category[screener_full$FlowsheetRowDisplayName=="GAD-7 Total Score"] <- "GAD7"
screener_full$Category[screener_full$FlowsheetRowDisplayName=="PHQ-2 SCORE"] <- "PHQ2"
screener_full$Category[screener_full$FlowsheetRowDisplayName=="PHQ-8 Total Score"] <- "PHQ8"
screener_full$Category[screener_full$FlowsheetRowDisplayName=="PHQ-9 Total Score"] <- "PHQ9"
screener_full$Category[screener_full$FlowsheetRowDisplayName=="PHQ-9 Completed Today"] <- "PHQ9_Completed"
screener_full$Category[screener_full$FlowsheetRowDisplayName=="PHQ2 Completed Today"] <- "PHQ2_Completed"

screener_dat <- screener_full %>%
  select(Arb_PersonId, IndexDate, FlowsheetDate, Value, Category) %>%
  filter((IndexDate-FlowsheetDate) >= -14 | (IndexDate-FlowsheetDate) <= 180)

# ______________________________________________________________________________
gc()

# Set up the split_group
screener_dat$split_group <- cut_number(screener_dat$Arb_PersonId, 
                                   n = cores, 
                                   labels = labs)

# Slice screener function
slice_screener <- function(data, split_by){
  split_by <- enquo(split_by)
  
  data %>%
    group_split(!!split_by) %>%
    future_map_dfr(~.x %>%
                     group_by(Arb_PersonId, IndexDate, Category) %>%
                     slice((which.min(abs(IndexDate-FlowsheetDate))))
    )
}

screener_dat <- slice_screener(screener_dat, split_group)
screener_dat %<>% select(-split_group)
# ______________________________________________________________________________

screener_dat <- subset(screener_dat, select=-c(FlowsheetDate))

screener_dat_wide <- screener_dat %>%
  spread(Category, Value)

seq_df2 <- left_join(seq_df2, screener_dat_wide, by=c("Arb_PersonId","IndexDate"))


# Bariatric surgery performed ---------------------------------------------
#Time frame: during baseline period after Index WPV
bariatric <- subset(procedure, Category == "bariatric procedure"&!is.na(ProcedureDate))
bariatric <- bariatric[, c("Arb_PersonId", "ProcedureDate")]
bariatric <- bariatric[!duplicated(bariatric), ]

bariatric_full <- left_join(seq_unique,
                            bariatric[,c("Arb_PersonId","ProcedureDate")],
                            by="Arb_PersonId")


bariatric_dat <- bariatric_full %>%
  filter(IndexDate <= ProcedureDate & ProcedureDate <= Date.max) %>%
  group_by(Arb_PersonId, IndexDate) %>%
  summarize("BariatricSurgery"=1*(any(!is.na(ProcedureDate))), .groups="rowwise")

seq_df2 <- left_join(seq_df2, bariatric_dat, by = c("Arb_PersonId","IndexDate"))
seq_df2$BariatricSurgery[is.na(seq_df2$BariatricSurgery)] <- 0


# Referrals --------------------------------------------------------------------
#Time frame: during baseline period after Index WPV
#cat("Processing referrals ...", "\n")

referrals_sub <- referrals[,c("Arb_PersonId", "ReferralDate", "Category")]
referrals_sub <- referrals_sub[!duplicated(referrals_sub), ]

referrals_full <- left_join(seq_unique, referrals_sub[,c("Arb_PersonId","ReferralDate", "Category")], by="Arb_PersonId")
referrals_full$Category[referrals_full$Category=="Referred to bariatric surgery"] <- "Ref_BariatricSurgery"
referrals_full$Category[referrals_full$Category=="Referred to Behavioral Health"] <- "Ref_BehavioralHealth"
referrals_full$Category[referrals_full$Category=="Referred to dietician"] <- "Ref_Dietician"
referrals_full$Category[referrals_full$Category=="Referred to endocrinology"] <- "Ref_Endo"
referrals_full$Category[referrals_full$Category=="Referred to wellness clinic"] <- "Ref_WellnessClinic"

referrals_dat <- referrals_full %>%
  filter(IndexDate <= ReferralDate & ReferralDate <= Date.max) %>%
  group_by(Arb_PersonId, IndexDate) %>%
  slice(which.min(abs(IndexDate-ReferralDate)))

referrals_dat$Value <- 1

referrals_dat_wide <- referrals_dat %>%
  spread(Category, Value)

seq_df2 <- left_join(seq_df2, referrals_dat_wide ,by=c("Arb_PersonId","IndexDate"))
seq_df2$Ref_BariatricSurgery[is.na(seq_df2$Ref_BariatricSurgery)] <- 0
seq_df2$Ref_BehavioralHealth[is.na(seq_df2$Ref_BehavioralHealth)] <- 0
seq_df2$Ref_Dietician[is.na(seq_df2$Ref_Dietician)] <- 0
seq_df2$Ref_Endo[is.na(seq_df2$Ref_Endo)] <- 0
seq_df2$Ref_WellnessClinic[is.na(seq_df2$Ref_WellnessClinic)] <- 0


# Replace the NAs in the index dates
seq_df2 %<>%
  mutate(IndexDate = ifelse(Arb_EncounterId %in% encounters_wo_id$Arb_EncounterId, NA, IndexDate))


# Visits  ----------------------------------------------------------------------
# Visits is created to perform some additional processing and exclusion of rows
# All visits with provider NPI as NA are dropped, including visits from the 11
# patients that were not captured in the baseline characteristics paper.
visits <- seq_df2

# - BMI_use to BMI - Set names BMI_use was old, BMI is new
# - Enrolled == Eligible (age and bmi criteria) and WPV > 0
# - Drop Npis - Drop rows if NPI is na
# - This will take the 745,074 visits down to 654,781
# - n.b. the Enrolled variable works on the visit level, not at the patient 
#   level. There are some patients who are Eligible and Enrolled in the study
#   but also have non-enrolled visits, perhaps due to lack of WPV
visits %<>%
  drop_na(ProviderNpi) %>%
  select(-BMI, -BMI_comp) %>%
  rename(BMI = BMI_use) %>%
  mutate(Enrolled = ifelse(WPV > 0 & Eligible == 1, 1, 0),
         Enrolled = ifelse(is.na(ProviderNpi), 0, Enrolled))

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


# What are the IDs that are in visits, but not all_unique_ee_ids
# all_unique_ee_ids represents the 20,394 patients that are eligible and 
# enrolled, but the baseline characteristics paper reports on 20,383. The ee 
# dataframe contains all of the 20,383. The 20,411 did not filter for provider
# NPIs with NAs.
ids_to_exclude <- all_unique_ee_ids %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId)

# Exclude the visits of the 11 patients that were excluded from baseline code 
# because NPI NA was dropped after, instead of before, the assignment of an indexDate. i.e.
# some patients were assigned an index date even though it was decided to 
# exclude them.
visits %<>%
  filter(!Arb_PersonId %in% ids_to_exclude$Arb_PersonId)

# Added 12/20/2022, but not tested, to make Enrolled a dichotomous variables
# visits %>%
#   mutate(Enrolled = ifelse(Arb_PersonId %in% ee$Arb_PersonId, 1, 0))

# Check ee --------------------------------------------------------------------- 
# Do all visits in ee have matching encounter and index dates?
nrow(visits %>% filter(EncounterDate == IndexDate)) == dim(ee)[1]

# Are all visits in ee enrolled?
nrow(ee %>% filter(Enrolled == 1)) == dim(ee)[1]

# Is the number of unique enrolled patients equal to the number of patients in ee
# Should match if the 11 patients with provider NPI as na are excluded
visits %>% filter(Enrolled == 1) %>% pull(Arb_PersonId) %>% n_distinct() == dim(ee)[1]

# Check that there are no patients in ee that are coded in intervention
nrow(ee %>% filter(intervention == 1)) == 0

# Check that only 3 Cohorts are available 
length(table(ee$Cohort)) == 3


# Create eligible but not enrolled ---------------------------------------------
# For EE, the cohort is assigned based on their index WPV. But since ENE do not
# have a WPV, then the following represents the approach towards assigning a
# cohort variable. ENE defined as meeting eligibility criteria but no WPV. For 
# ene patients who were seen at multiple clinics, cohort is assigned as the 
# clinic with the most visits, if tied select the first observation (Group). For
# all others, cohort is assigned according to the 

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
#_______________________________________________________________________________
# Capture the Arb_PersonIds of ENE patients who had visits at clinics assigned 
# to more than one cohort.
gt1_clinic_ids <- visits %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  summarise(clinics = n_distinct(GroupID)) %>%
  filter(clinics > 1)

# Get the number of visits for each patient that is ene in each cohort
n_visits_per_patient_clinic <- visits %>%
  filter(Enrolled == 0,
         Eligible == 1) %>%
  group_by(Arb_PersonId, GroupID) %>%
  count() %>%
  filter(Arb_PersonId %in% gt1_clinic_ids$Arb_PersonId)


# For ene patients who were seen at multiple clinics, the cohort to be assigned
# is the cohort with the most visits. If tied, then select the 1st obs after
# grouping by patient id.
ene_gt1_clinic_cohorts <- 
  n_visits_per_patient_clinic %>% 
  group_by(Arb_PersonId) %>%
  arrange(n) %>% 
  slice_head() %>%
  mutate(Cohort = str_c("Cohort ", GroupID)) %>%
  select(Arb_PersonId, Cohort)

# Assign a cohort to those with out visits at multiple clinics which is the first
# Cohort after arranging visits by date as was done for EE.
# Coalesce takes the first nonmissing element, but should assign Cohort.y which
# is set to the cohort where most visits occurred. Since this slices by person
# id, it should only be one visit per person
ene_gt1 <- visits %>%
  filter(Eligible == 1, 
         Arb_PersonId %in% ene_gt1_clinic_cohorts$Arb_PersonId) %>%
  left_join(ene_gt1_clinic_cohorts, by = "Arb_PersonId") %>%
  mutate(Cohort = coalesce(Cohort.y, Cohort.x)) %>%
  select(-Cohort.y, -Cohort.x) %>%
  group_by(Arb_PersonId) %>%
  slice_head() %>%
  select(Arb_PersonId:WPV_v2, Cohort, everything())

# ______________________________________________________________________________
# For ene patients that did not go to more than one clinic, we take the first 
# visit in the time period to capture outcome variables. Since there is only
# one clinic that they visit, their cohort stays constant.
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
  mutate(PHQ2_Completed = ifelse(is.na(PHQ2_Completed), 0, PHQ2_Completed),
         BMI = as.numeric(BMI),
         N_MedsWeightGain_period = ifelse(N_MedsWeightGain_period > 0, 1, N_MedsWeightGain_period),
         N_MedsWeightLoss_period = ifelse(N_MedsWeightLoss_period > 0, 1, N_MedsWeightLoss_period),
         PHQ8 = as.numeric(PHQ8),
         PHQ9 = as.numeric(PHQ9),
         GAD7 = as.numeric(GAD7))


# CONSORT Diagram Variables ----------------------------------------------------
## 1. Total number of patient encounters ----
# 745,074
nrow(seq_df2)

## 2. Total number of unique patients ----
# *** This counts those with providerNpi as NA
# 277,467
seq_df2 %>%
  pull(Arb_PersonId) %>%
  n_distinct()

## 3. Total number of unique patients that are eligible ----
# *** Does not exclude those that have NA NPI
# *** Does not exclude those that were restricted
# *** Matches baseline characteristics paper
# 164,904
seq_df2 %>% 
  filter(Age >= 18, BMI_use >= 25) %>% 
  pull(Arb_PersonId) %>% 
  n_distinct()

## 4. The number of eligible patients in baseline characteristics paper. ----
#  4.1 This is 11 more, because 11 extra patients were captured by a 
# modification in the order of when visits with NA for provider NPI were 
# excluded.when compared to baseline characterisitcs 
# 164,443 
seq_df2 %>%
     drop_na(ProviderNpi) %>%
     filter(Eligible == 1) %>% 
     pull(Arb_PersonId) %>% 
     n_distinct()

#  4,2
# 164,432 using seq_df2
seq_df2 %>%
  drop_na(ProviderNpi) %>%
  filter(Eligible == 1,
         !Arb_PersonId %in% ids_to_exclude$Arb_PersonId) %>% 
  pull(Arb_PersonId) %>% 
  n_distinct()

# 4.3 The number of eligible patients in ee vs ene, is different by 11, because
# There were 11 that were not captured in baseline char
# 164,432
visits %>%
  filter(Eligible == 1) %>% 
  pull(Arb_PersonId) %>% 
  n_distinct()

## 5. The number of eligible and enrolled patients (EE) ---- 
# 20,383
visits %>%
  filter(Eligible == 1, Enrolled == 1) %>% 
  pull(Arb_PersonId) %>% 
  n_distinct()

## 6. The number of eligible but not enrolled patients (ENE) ----
# 6.1
# If using seq_df2, exclude by ids_to exclude for 11 patients
# 144,049
seq_df2 %>%
  drop_na(ProviderNpi) %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId,
         !Arb_PersonId %in% ids_to_exclude$Arb_PersonId,
         Eligible == 1,
         WPV < 1) %>%
  pull(Arb_PersonId) %>% 
  n_distinct()

# 6.2
# The number of eligible but not enrolled patients.
# Must filter out those that are EE, because Enrolled functions on the visit
# level not the patient level.
# 144,049
visits %>%
  filter(Eligible == 1, Enrolled == 0,
         !Arb_PersonId %in% ee$Arb_PersonId) %>% 
  pull(Arb_PersonId) %>% 
  n_distinct()

#################################### FENCE ####################################
# *** Question is, do we want to count or average only the WPVs, from the 
# the EE patients, or do we want to count or average all the visits from those
# patients, or only the visits that took place after the Index Date?

# Are visits that take place after an index date considered "eligible" to be 
# considered for mean, averages, count's etc., because if that's the case
# then we can use the patient id/and date to filter out visits.

# If not, then we would have to find a way to classify visits, is it just any
# visit after the index date?

# Currently Eligible just means whether or not the patient met the age and BMI
# requirements, and is further restricted by whether or not a person's visit was
# deemed eligible, based on height (too tall or too short) and weight, anyone
# over 600 lbs.
# So the things to consider between seq_df2 and visits are 
# 1. The provider NPI
# 2. Eligible visits or not
# 3. WPV greater than or equal to 1.
# 4. Enrolled, means they are in the study.


# --------------------------------EE -------------------------------------------
# The Average number of WPVs per EE patient ------------------------------------
# The average number of WPVs per EE patient. This is the number for EE with 
# valid NPI and restricted eligibility. Includes WPVs only.
# 1.39
visits %>%
  filter(Arb_PersonId %in% ee$Arb_PersonId,
         WPV >= 1) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  mean(.)

# The Max of number of WPVs for EE patients ------------------------------------
# Restricting to only WPVs of the EE patients in the ee dataframe
# Max = 20
visits %>%
  filter(Arb_PersonId %in% ee$Arb_PersonId, WPV >= 1) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  max(.)


# The time between WPVs for ee patients ----------------------------------------
# 97.8
visits %>%
  filter(Arb_PersonId %in% (visits %>%
                            filter(Arb_PersonId %in% ee$Arb_PersonId, WPV >= 1) %>%
                            group_by(Arb_PersonId) %>%
                            count() %>%
                            filter(n > 1) %>%
                            pull(Arb_PersonId))) %>%
  filter( WPV >= 1) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  summarise(tbv = mean(diff(EncounterDate))) %>%
  summarise(mean(tbv))


# The number of WPVs during the period -----------------------------------------
# 31,682, exclude visits with provider NPI
visits %>%
  filter(WPV >= 1) %>%
  nrow()


# 32,306, includes visits with provider NPI as NA.
seq_df2 %>%
  filter(WPV >= 1) %>%
  nrow()

# Table of how WPVs were identified (icd-10, OBHPI, etc.) ----------------------
visits %>%
  filter(WPV >= 1) %>%
  select(starts_with("WPV")) %>%
  gtsummary::tbl_summary()



# The Average number of any visit per EE patient -------------------------------
# The average number of any visit per EE patient. This is the number for EE with 
# valid NPI and restricted eligibility.
# 3.05
visits %>%
  filter(Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  mean(.)

# The Max of number of any visits for EE patients ------------------------------
# Restricting to only WPVs of the EE patients in the ee dataframe
# Max = 56
visits %>%
  filter(Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  max(.)


# The time between visits for EE patients --------------------------------------
# 79.96
visits %>%
  filter(Arb_PersonId %in% (visits %>%
                              filter(Arb_PersonId %in% ee$Arb_PersonId) %>%
                              group_by(Arb_PersonId) %>%
                              count() %>%
                              filter(n > 1) %>%
                              pull(Arb_PersonId))) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  summarise(tbv = mean(diff(EncounterDate))) %>%
  summarise(mean(tbv))


# The number of any visit during the period for EE -----------------------------
# 62,193, excludes visits with provider NPI
visits %>%
  filter(Arb_PersonId %in% ee$Arb_PersonId) %>%
  nrow()


# --------------------- ENE ----------------------------------------------------
# The Average number of any visit per ENE patient ------------------------------
# This is the number for ENE with valid NPI and restricted eligibility
# *** do not use ene$Arb_PersonId bc that data frame is only one visit per 
# person
# 1.78
visits %>% 
  filter(Eligible == 1, 
         !Arb_PersonId %in% ee$Arb_PersonId) %>% 
  group_by(Arb_PersonId) %>% 
  count() %>%
  pull(n) %>%
  mean(.)



# The Max of number of any visits for ENE patients ------------------------------
# Restricting to only WPVs of the EE patients in the ee dataframe
# Max = 24
visits %>%
  filter(Eligible == 1, 
         !Arb_PersonId %in% ee$Arb_PersonId) %>% 
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  max(.)


# The time between visits for ENE patients --------------------------------------
# 79.02
visits %>%
  filter(Arb_PersonId %in% (visits %>%
                              filter(Eligible == 1,
                                !Arb_PersonId %in% ee$Arb_PersonId) %>%
                              group_by(Arb_PersonId) %>%
                              count() %>%
                              filter(n > 1) %>%
                              pull(Arb_PersonId))) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  summarise(tbv = mean(diff(EncounterDate))) %>%
  summarise(mean(tbv))


# The number of any visit during the period for ENE -----------------------------
# 256,178, excludes visits with provider NPI
visits %>%
  filter(Eligible == 1, 
         !Arb_PersonId %in% ee$Arb_PersonId) %>%
  nrow()


### LEFT OFF HERE ####
# Need to double check why these statements are here 

# The average number of days in between visits for those EE with more than one
# visit.
# tbv = 94.3 days
seq_df2 %>% 
  filter(Arb_PersonId %in% (seq_df2 %>%
                              filter(WPV >= 1) %>%
                              group_by(Arb_PersonId) %>%
                              count() %>%
                              filter(n > 1) %>%
                              pull(Arb_PersonId))) %>%
  filter(WPV >= 1) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  summarise(tbv = mean(diff(EncounterDate))) %>%
  summarise(mean(tbv))


# The average number of days in between visits for those EE with more than one
# visit, but restricting only to those reported on in the paper
# tbv = 65.35 days
seq_df2 %>% 
  filter(Arb_PersonId %in% (seq_df2 %>% 
                              filter(Arb_PersonId %in% ee$Arb_PersonId, WPV >=1) %>%
                              group_by(Arb_PersonId) %>%
                              count() %>%
                              filter(n > 1) %>%
                              pull(Arb_PersonId))) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  summarise(tbv = mean(diff(EncounterDate))) %>%
  summarise(mean(tbv))



# The total number of visits in EE, should be 32,306 as reported in bl_char
# Provider_Npi, not filtered out in this value.
seq_df2 %>%
  filter(WPV >= 1) %>%
  nrow()


# Average number of visits per EE patient regardless of WPV or not
visits %>%
  filter(Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  mean(.)



# Average number of visits per ENE patients and includes Eligibility restrictions
# and providers with NA
# 2.37
visits %>%
  filter(!Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  mean(.)
  






# T-test between the number of visits per patient in EE vs ENE -----------------
# Initially this section did not filter for Eligible == 1
x <- visits %>%
  filter(Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n)

y <- visits %>%
  filter(Eligible == 1, 
         !Arb_PersonId %in% ee$Arb_PersonId) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n)


t.test(x,y)


  






# #################################### FENCE ##################################
# The average number of visits per ENE patient.
seq_df2 %>%
  filter(Eligible == 1, WPV == 0) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  mean(.)

# Min should be 1

# Max of ENE visits 
seq_df2 %>%
  filter(Eligible ==1, WPV == 0) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  max(.)

visits %>%
  filter(Eligible ==1, WPV == 0) %>%
  group_by(Arb_PersonId) %>%
  count() %>%
  pull(n) %>%
  max(.)

# The average number of days in between visits for those ENE with more than one
# visit.
tic()
seq_df2 %>% 
  filter(Arb_PersonId %in% (seq_df2 %>%
                              filter(WPV == 0) %>%
                              group_by(Arb_PersonId) %>%
                              count() %>%
                              filter(n > 1) %>%
                              pull(Arb_PersonId))) %>%
  filter(WPV == 0) %>%
  group_by(Arb_PersonId) %>%
  arrange(EncounterDate) %>%
  summarise(tbv = mean(diff(EncounterDate))) %>%
  summarise(mean(tbv))
toc()

# The total number of visits in EE, should be 32,306 as reported in bl_char
# Provider_Npi, not filtered out in this value.
seq_df2 %>%
  filter(WPV >= 1) %>%
  nrow()

seq_df2 %>%
  filter(WPV >= 1) %>%
  drop_na(ProviderNpi) %>%
  nrow()

visits %>%
  filter(Enrolled == 1) %>%
  nrow()

# The total number of visits in ENE should be 745,074 - 32,306
seq_df2 %>%
  filter(WPV == 0) %>%
  nrow()

# The total number of visits in ENE





toc()



gc()



# Remove intermediary data frames-----------------------------------------------
#rm()





# Write file -------------------------------------------------------------------
data %<>%
  mutate(Enrolled = ifelse(Enrolled == 1, "EE", "ENE"))
#save(data, meds_aom.ee, meds_aom.ene, file = here("data", "ee_vs_ene_processed.rda"))