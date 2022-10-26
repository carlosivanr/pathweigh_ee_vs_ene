# ee_vs_ene_processed

Created 10/18/2022

ee_vs_ene_processed.rda was created via the ee_vs_ene.R script. This data set represents patients from the baseline period of 3/17/2020 through 3/16/2021. In this data set, patients who were not enrolled now have a variable that indicate so, and they have been assigned to a cohort. For patients with visits in multiple clinics, the Cohort is assigned to the clinic with the most visits. For example, if the patient has 4 visits at clinic A and 2 visits at clinic B, they will be assigned to the Cohort that contains Clinic A. In cases where there are 2 visits at clinic A and 2 visits at clinic B (ties), then the Cohort is assigned to the one containing Clinic A. For patients without visits to multiple clinics, then the Cohort is assigned as expected. For all patients that are not enrolled, the visit to take information from for tables and statistitcs is the first visit according to encounter date.



# check_na_labs

Created 10/21/2022:

check_na_labs was created to troubleshoot why some participants in the ENE group did not have any labs, O2CPAPBIPAP, and PHQ measures. Modified the make_data_for_ee_vs_ene script to tag on an index date to those that 