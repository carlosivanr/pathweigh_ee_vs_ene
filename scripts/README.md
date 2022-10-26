  # 01_make_data_for_ee_vs_ene.R
  - When using the original code to capture anti obesity, medications on data 
  containing all eligible visits, not just those that are eligible and enrolled, 
  an extra 7 EE patients were found to have qualifying anti obesity medications.
    - 4 in bupropion
    - 1 in phentermine
    - 1 in lisdexamfetan
    - 1 in orlistat
    
  - To address the discrepancy noted above, this file pulls in the AOM 
  data used in the baseline characteristics paper to ensure that all tables 
  in the ee vs ene paper match with the baseline characteristics paper. It's a 
  work around until it is determined why the 7 patients differ.
  
    - These observations are all within the date range (3/17/2020 - 3/16/2021).
    - These observations often represent the same medication, but are linked to 
    different IndexDates
    - All of the extra 7 patients are EE
      - Need to consider how an IndexDate gets merged into meds_AOM
      - The meds were captured before the IndexDate
  
  # 01_make_data_for_ee_vs_ene_investigate_aom.R
  - This file was created to investigate discrepancies in the way that the 
  medications algorithm worked. A new algorithm was created to capture the anti
  obesity medications. This approach, however, resulted in 17 different rows
  that were not in the baseline characteristics paper. 
    - It's likely that the new algorithm is capturing aom medications with dates
    occurring before their index date.
