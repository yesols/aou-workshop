library(dplyr)
library(tidyr)
library(lubridate)

set.seed(789)

# Maintain the 70% / 20% exposure ratio
drug_patients_with_cond <- sample(condition_patients, length(condition_patients) * 0.70)
drug_patients_no_cond <- sample(setdiff(synthetic_person$person_id, condition_patients), 
                                length(setdiff(synthetic_person$person_id, condition_patients)) * 0.20)

all_drug_patients <- c(drug_patients_with_cond, drug_patients_no_cond)

# Extract only the incident condition dates for the temporal logic
incident_conditions <- synthetic_condition %>%
  group_by(person_id) %>%
  summarize(first_condition_date = min(condition_start_date), .groups = 'drop')

synthetic_drug <- synthetic_person %>%
  filter(person_id %in% all_drug_patients) %>%
  left_join(incident_conditions, by = "person_id") %>%
  rowwise() %>%
  mutate(
    drug_concept_id = 19073183, # Oxybutynin
    # Convert year_of_birth integer to a Date object anchored at Jan 1st 
    first_drug_date = if_else(
      is.na(first_condition_date),
      make_date(year_of_birth, 1, 1) + years(sample(50:80, 1)) + days(sample(1:365, 1)),
      first_condition_date - days(sample(30:3650, 1)) # Exposure strictly before diagnosis
    ),
    num_prescriptions = sample(1:15, 1)
  ) %>%
  ungroup() %>%
  uncount(num_prescriptions) %>%
  group_by(person_id) %>%
  mutate(
    days_after_first = c(0, sort(sample(30:2000, n() - 1, replace = TRUE))),
    drug_exposure_start_date = first_drug_date + days(days_after_first)
  ) %>%
  ungroup() %>%
  select(person_id, drug_concept_id, drug_exposure_start_date)

head(synthetic_drug, 10)

write_csv(synthetic_drug, "data/drug_exposures.csv")

