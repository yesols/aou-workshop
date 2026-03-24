library(tidyverse)

cleaned_dataset <- read_csv("data/cleaned_dataset.csv")

# Set a seed to ensure all workshop participants generate the exact same synthetic values
set.seed(42)

cleaned_dataset <- cleaned_dataset %>%
  mutate(
    # Generate SBP values
    SBP = round(90 + (0.5 * (2026 - year_of_birth)) + rnorm(n(), mean = 0, sd = 10))
  )

# Maintain the seed for workshop reproducibility
set.seed(42)

cleaned_dataset <- cleaned_dataset %>%
  mutate(
    # Randomly select 5 rows to replace with NA
    SBP = if_else(
      condition = row_number() %in% sample(n(), size = 5), 
      true = NA_real_, 
      false = SBP
    )
  )

# Save to a file
write_csv(cleaned_dataset, "data/final_dataset.csv")
