library(dplyr)
library(tidyr)
library(lubridate)

set.seed(456)
condition_patients <- sample(synthetic_person$person_id, 30)

synthetic_condition <- synthetic_person %>%
  filter(person_id %in% condition_patients) %>%
  rowwise() %>%
  mutate(
    # Convert year_of_birth integer to a Date object anchored at Jan 1st
    first_condition_date = make_date(year_of_birth, 1, 1) + years(sample(50:70, 1)) + days(sample(1:365, 1)),
    condition_concept_id = sample(c(4128031, 4182210), 1, prob = c(0.7, 0.3)),
    condition_source_value = ifelse(condition_concept_id == 4128031, "MCI", "Dementia"),
    num_records = sample(1:5, 1) 
  ) %>%
  ungroup() %>%
  # Duplicate rows based on num_records
  uncount(num_records) %>%
  group_by(person_id) %>%
  mutate(
    # Generate subsequent dates by adding sorted random days to the index date
    # The first row will have 0 added, keeping it as the minimum date
    days_after_first = c(0, sort(sample(1:1500, n() - 1, replace = TRUE))),
    condition_start_date = first_condition_date + days(days_after_first)
  ) %>%
  ungroup() %>%
  select(person_id, condition_concept_id, condition_start_date, condition_source_value)

head(synthetic_condition, 10)

write_csv(synthetic_condition, "data/conditions.csv")
