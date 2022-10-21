# seq_df2

Created 10/13/2022

seq_df2 was created with the 02_Rimage_to_analysis_datasets_v2.R script with the following modifications:
- Data initially processed as normal up to line 705
- Then, instead of creating seq_df2 via
```{r} [
join2 %>% 
filter(EncoungerDate >= IndexDate)
```
seq_df2 was created via
```{r} 
seq_df2 <- join2
```

- The rationale for this approach is to keep all patients regardless of whether or not they had a WPV to become enrolled
- The original way seq_df2 was created kept only enrolled visits and additional processing such as joining comorbidites, screeners, medications, etc., would not have applied to those that were not enrolled. 
- Data were then processed as normal up to line 1120
- At this point the data were saved as an Rdata file via [save(seq_df2, file = "seq_df2.rda")]

Nota Bene
In order to achieve the values reported in the baseline characteristics paper, the data need to be filtered for visits in which the encounter date is equal to the index date, WPV_v2 is equal to 1 OR WPV > 1 (WPV is count, WPV_v2 is binary), and any encounter with out a valid NPI is dropped.
```{r}
seq_df2 %>% 
	drop_na(ProviderNpi) %>% 
	filter(EncounterDate == IndexDate, 
		Seq ==1, WPV_v2 == 1))
```

The difference between v1 and v2 deals with how "repeat encounters" are handled. In v1, visits that are grouped by patient, and sorted then the visit with the most information is retained. In v2, visits are grouped by encounter id, sorted, then the visit with information is retained. The difference is subtle, but leads to the same number of WPVs.


# ee_vs_ene_processed

Created 10/18/2022

ee_vs_ene_processed.rda was created via the ee_vs_ene.R script. This data set represents patients from the baseline period of 3/17/2020 through 3/16/2021. In this data set, patients who were not enrolled now have a variable that indicate so, and they have been assigned to a cohort. For patients with visits in multiple clinics, the Cohort is assigned to the clinic with the most visits. For example, if the patient has 4 visits at clinic A and 2 visits at clinic B, they will be assigned to the Cohort that contains Clinic A. In cases where there are 2 visits at clinic A and 2 visits at clinic B (ties), then the Cohort is assigned to the one containing Clinic A. For patients without visits to multiple clinics, then the Cohort is assigned as expected. For all patients that are not enrolled, the visit to take information from for tables and statistitcs is the first visit according to encounter date.



# check_na_labs

Created 10/21/2022
check_na_labs was created to troubleshoot why some participants in the ENE group did not have any labs, O2CPAPBIPAP, and PHQ measures. Modified the make_data_for_ee_vs_ene script to tag on an index date to those that 