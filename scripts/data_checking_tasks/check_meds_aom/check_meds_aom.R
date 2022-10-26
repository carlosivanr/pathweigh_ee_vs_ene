# 1. Semaglutide/Wegovy is found in the meds data frame for some patients, but it is not listed as an active med


#2 . Why the extra 7 in EE vs ENE
#   - will need the baseline and the ee vs ene data sets

#3. Meds_aom ###################################################################

# Let's start off by paring down the meds data frame by filtering for only those
# that we need to track meds for, either those in EE or ENE
meds_sub <-  meds %>% 
  filter(ActiveMed == "Y", 
         Arb_PersonId %in% seq_df2$Arb_PersonId) %>%
  mutate_at(c("MedicationName", "GenericName"), tolower)

# Load meds_list to get the UniqueMedNames
meds_list <- read.xlsx(
  here("../baseline_characteristics/working_files/medications/medications.xlsx"),
  sheet="Medications") %>%
  mutate_at(c("Name", "GenericName"), tolower) %>% 
  select(uniqueMedName, GenericName) %>%
  drop_na() %>%
  distinct()

# Read in Meds ref to know which medications are AOM
meds_AOM <- read.xlsx(
  here("../baseline_characteristics/working_files/medications/medications.xlsx"),
  sheet = "UniqueMedName") %>%
  filter(AOM == "X") %>%
  left_join(., meds_list, by = "uniqueMedName")


# Filter meds_sub for matching one of the AOM meds
meds_AOM_wide <- meds_sub %>% 
  filter(GenericName %in% meds_AOM$GenericName,
         OrderedDate >= Date.min, OrderedDate <= Date.max) %>%
  left_join(., meds_AOM %>% select(-(AOM:weight.loss)), by = "GenericName") %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = uniqueMedName, values_from = count) %>%
  distinct() %>%
  select(-(Arb_EncounterId:trade.names)) %>%
  mutate(Arb_PersonId = as.character(Arb_PersonId)) %>%
  group_by(Arb_PersonId) %>%
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  mutate_if(is.numeric, ~ifelse(. >= 1, 1, 0)) %>%
  mutate(Arb_PersonId = as.numeric(Arb_PersonId))


# Test to see how the AOM meds are joined and it works great!
# test <- seq_df2 %>% 
#   left_join(meds_AOM_wide, by = "Arb_PersonId") %>%
#   filter(Arb_PersonId %in% 
#            (seq_df2 %>%
#            group_by(Arb_PersonId) %>%
#            count() %>%
#            filter(n > 1) %>%
#            pull(Arb_PersonId))
#   ) %>%
#   filter(Arb_PersonId %in% meds_AOM_wide$Arb_PersonId) %>%
#   arrange(Arb_PersonId)
#   

# Merge seq_df2 with meds_AOM_wide
seq_df2 %<>%
  left_join(meds_AOM_wide, by = "Arb_PersonId")
