# scripts/generate_data.R
library(tidyverse)
library(lubridate)


# Set seed for reproducibility
set.seed(123)

# Define the date range for an adult cohort (e.g., roughly 18 to 90 years old in 2026)
start_date <- as.Date("1936-01-01")
end_date <- as.Date("2008-01-01")

# Generate the synthetic person table
synthetic_person <- tibble(
  person_id = 1:100,
  # Sample 100 random days between the start and end dates
  birth_datetime = sample(seq(start_date, end_date, by = "day"), 100, replace = TRUE)
) %>%
  mutate(
    year_of_birth = year(birth_datetime),
  )

# Define standard AoU race categories
race_options <- c(
  "White",
  "Black or African American",
  "Asian",
  "More than one population",
  "Other",
  "Skip",
  "None of these"
)

# Append race to the synthetic dataset
synthetic_person <- synthetic_person %>%
  mutate(
    race = sample(
      x = race_options, 
      size = 100, 
      replace = TRUE, 
      prob = c(0.40, 0.20, 0.15, 0.10, 0.05, 0.05, 0.05) 
    )
  )

# Define sex categories including specific survey responses
sex_options <- c(
  "Male",
  "Female",
  "Intersex",
  "Prefer not to answer",
  "Skip"
)

# Append sex to the synthetic dataset
synthetic_person <- synthetic_person %>%
  mutate(
    sex_at_birth = sample(sex_options, 100, replace = TRUE,
                 # Optional: Adjust probabilities for realistic distribution
                 prob = c(0.45, 0.45, 0.04, 0.03, 0.03))
  )

synthetic_person <- synthetic_person %>% 
  select(person_id, year_of_birth, race, sex_at_birth)

head(synthetic_person)

# Save to the data directory
write_csv(synthetic_person, "data/demographic_data.csv")
