# Load libraries ---------------------------------------------------------------
pacman::p_load(tidyverse,
               data.table,
               openxlsx,
               here)

# Load the data  ---------------------------------------------------------------
# These are the individual epic compass table in Rdata format that need 
# processing
load(here("../baseline_characteristics/data/220121.RData"))


# The load the patient Ids for which there are no labs data
load(here("data", "check_na_labs.rda"))


vals <- c("TSH", "Triglycerides", "HDL", "ALT", "AST", "A1C", "eGFR")
  
# This shows that the Patient Ids selected do in fact lab values  
labs %>% 
  filter(Arb_PersonId %in% check_na_labs$Arb_PersonId) %>%
  filter(Category %in% vals)

# Shows that some of the encounter in the ENE data set do in fact have lab values
labs %>% 
  filter(Arb_EncounterId %in% check_na_labs$Arb_EncounterId) %>%
             filter(Category %in% vals)

# *** Means that there is something else going on. will need to look at the meds
# processing and the procedure processing script in 01.make data for ee vs ene