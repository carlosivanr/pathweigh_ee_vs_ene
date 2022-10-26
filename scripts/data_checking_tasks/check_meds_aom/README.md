  # 01_make_data_for_ee_vs_ene_investigate_aom.R
  - This file was created to investigate discrepancies in the way that the 
  medications algorithm worked. A new algorithm was created to capture the anti
  obesity medications in this script. This approach, however, resulted in 17 new different rows
  that were not in the baseline characteristics paper. 
    - It's likely that the new algorithm is capturing aom medications with dates
    occurring before their index date.
    - This issue was fixed 10/26/2022 in the lates version of the make data for ee vs ene script.