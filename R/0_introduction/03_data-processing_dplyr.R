# ==================================================================
# SHOWCASE: the same steps as in 02_data_processing, but with dplyr
# ==================================================================
# dplyr (part of the "tidyverse") is a package built around a small
# set of "verbs" for the most common data-wrangling steps:
#   mutate()   - add or change columns
#   select()   - pick columns
#   filter()   - pick rows
#   rename()   - rename columns
#   summarise()- collapse to summary statistics
#   arrange()  - sort rows
# Steps are usually chained with the pipe operator %>% (or base R's
# own |>), so a sequence of transformations reads top-to-bottom
# instead of as nested function calls.
#
# This section is a SHOWCASE only: it re-reads the raw csv into a
# separate object (df_dplyr) so it doesn't disturb df or df_export
# above, and nothing here gets written back to disk.

# install.packages("dplyr")  # only needed once
library(dplyr)

df_dplyr <- read.csv("data/raw/height-weight-by-sex.csv", sep = ",", header = TRUE)

# --- inspection ---
# glimpse() is dplyr's version of str(): one line per column, with
# its type and a preview of the values
glimpse(df_dplyr)

# NA count per column and duplicate-row count, dplyr style
# (across() applies the same function to every column)
df_dplyr %>% summarise(across(everything(), ~ sum(is.na(.))))
df_dplyr %>% summarise(n_duplicates = sum(duplicated(df_dplyr)))
# dim() and nrow() don't need a dplyr version — plain base R already
# does the job in one line

# --- recode Gender into a dummy, convert units, and rename ---
# mutate() adds new columns (same "always create a new column" rule
# as in base R); case_when() is dplyr's more readable alternative to
# nested ifelse()
multi_in_to_cm <- 2.54    
multiLbToKg <- 0.453592   

df_dplyr <- df_dplyr %>%
  mutate(
    is_male = case_when(
      Gender == "Male"   ~ TRUE,
      Gender == "Female" ~ FALSE,
      TRUE ~ NA
    ),
    Gender_Factor = as.factor(Gender),
    height_cm = round(Height * multi_in_to_cm, 0),
    weight_kg = round(Weight * multiLbToKg, 1)
  ) %>%
  rename(
    height_in = Height,
    weight_lb = Weight,
    sex       = Gender
  )

glimpse(df_dplyr)

# --- sanity check: same correlation check as section 4 ---
df_dplyr %>% summarise(
  cor_height = cor(height_in, height_cm),
  cor_weight = cor(weight_lb, weight_kg)
)

# --- EXERCISE: Create BMI Column, dplyr style ---
# BMI = weight (kg) / [height (m)]^2 — same placeholder as section 5,
# left for you to fill in
df_dplyr <- df_dplyr %>% mutate(bmi = NA)

# --- preview the columns we'd export (showcase only — no write.csv here) ---
df_dplyr %>% select(is_male, height_cm, weight_kg, bmi)
