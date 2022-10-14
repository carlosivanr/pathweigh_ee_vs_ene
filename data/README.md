# seq_df2

Created 10/13/2022

seq_df2 was created with the 02_Rimage_to_analysis_datasets_v2.R script with the following modifications:
- Data processed as normal up to line 705
- Then, instead of creating seq_df2 via [join2 %>% filter(EncoungerDate >= IndexDate)], seq_df2 was created via [seq_df2 <- join2]
	- The rationale for this approach is to keep all patients regardless of whether or not they had a WPV to become enrolled
	- The original way seq_df2 was created kept only enrolled visits and additional processing such as joining comorbidites, screeners, medications, etc., would not have applied to those that were not enrolled. 
- Data were then processed as normal up to line 1120
- At this point the data were saved as an Rdata file via [save(seq_df2, file = "seq_df2.rda")]

Nota Bene
In order to achieve the values reported in the baseline characteristics paper, the data need to be filtered for visits in which the encounter date is equal to the index date, WPV_v2 is equal to 1, and any encounter with out a valid NPI is dropped.
```{r}
seq_df2 %>% drop_na(ProviderNpi) %>% filter(EncounterDate == IndexDate, Seq ==1, WPV_v2 == 1))]
```
The difference between v1 and v2 deals with how "repeat encounters" are handled. In v1, visits that are grouped by patient, and sorted then the visit with the most information is retained. In v2, visits are grouped by encounter id, sorted, then the visit with information is retained. The difference is subtle, but leads to the same number of WPVs.
