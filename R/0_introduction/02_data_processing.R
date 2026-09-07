# ============================================================
# 1. LOAD AND INSPECT THE DATA
# ============================================================
# Load data from csv file into a data.frame called df
df <- read.csv("data/raw/height-weight-by-sex.csv", sep = ",", header = TRUE)
# Also open in Excel!

# --- structural checks: shape, columns, types, and data integrity ---

# Check dimensions: how many rows and columns did we load?
dim(df)
nrow(df)

# Print just the column names (handy for copy-pasting into code)
colnames(df)

# Display the Structure of an R Object
str(df)

# Apply Any Function over a List or Vector
sapply(df, class)
sapply(df, typeof)

# Missing values and duplicate rows — always check before doing anything else
colSums(is.na(df))     # how many NAs per column
sum(duplicated(df))    # how many fully duplicated rows

# --- exploratory: actually look at the values ---

# Print df to the console
head(df) # only few rows
df # all of it

# Open df in "Excel-style" spreadsheet view
# (leave this tab open — it reflects every change we make below)
View(df)

# See a summary() for all columns
summary(df)


# You can create functions for data.frame inspection

# structure: shape, column names, types, and data integrity
inspect_structure <- function(df) {
  print(dim(df))
  print(colnames(df))
  str(df) # str() prints internally via cat(), so wrapping in print() would just add a stray NULL
  print(sapply(df, class))
  print(sapply(df, typeof))
  print(colSums(is.na(df)))
  print(sum(duplicated(df)))
}

# values: actually look at the data itself
inspect_values <- function(df) {
  print(head(df))
  print(summary(df))
  View(df) # View() opens the viewer as a side effect and returns invisible NULL, so no print() needed
}

inspect_structure(df)
inspect_values(df)


# ------------------------------------------------------------
# NOTE: character vs factor
# ------------------------------------------------------------
# sapply(df, class) showed Gender as "character" — just plain text,
# with no notion of categories or order. A "factor" is R's dedicated
# type for categorical variables: same underlying values, but R also
# remembers a fixed set of "levels" (the possible categories) and
# stores each value as an integer code under the hood, with the level
# labels attached for display.
#
# Why it matters:
# - Many functions (table(), lm(), ggplot2's discrete scales, ...)
#   treat factors as categorical automatically, but treat character
#   columns as plain text.
# - A factor's levels are fixed: assigning a value that isn't an
#   existing level produces NA with a warning — which catches a typo
#   a character column would silently accept.
# - summary() on a factor shows a count per category; on a character
#   column it just shows length/class/mode.
#
# read.csv() in R >= 4.0 reads text columns in as character by
# default (that's why Gender came in as character here) — convert it
# explicitly when you want factor behavior:

# see how it behaves when character
class(df$Gender) 
levels(df$Gender) 

# now recode and check again
df$Gender_Factor <- as.factor(df$Gender)
class(df$Gender_Factor)   # "factor"
levels(df$Gender_Factor)  # the categories R now knows about


### work with data.frame and vectors --- Recap from the introduction

#first column as data.frame
df["Gender"] # by name
df[1] # by order

df["Gender_Factor"] # no change when factorized

# first column as vector
df$Gender
df[["Gender"]]
df[[1]]

df$Gender_Factor # when factorized, it tells the levels

# a concrete vector value
df$Gender[1]
df$Gender_Factor[1] # when factorized, it tells the levels

# change value of the first entry
df$Gender[1] <- "Hello" # when character, you can change any value to any value
df$Gender[1] <- "Male" # Set the value back to the original

df$Gender_Factor[1] <- "Hello" # when factorized, changing to any other value than specified levels will throw a warning and coerces NA

# recode Gender into dummy using nested ifelse statement
# PRO TIP: when creating dummy (or recoding any variable), ALWAYS CREATE A NEW COLUMN, NEVER RECODE THE ORIGINAL ITSELF
df$is_male <- ifelse(df$Gender == "Male", TRUE,
                    ifelse(df$Gender == "Female", FALSE, NA))

typeof(df$is_male) # check the type, should be logical (TRUE/FALSE/NA)

# create new table column that is empty (NA)
df$new_column_na <- NA

# add data with vector
df$new_column_vector <- c("A", "B", NA, "C")

# delete a column
df$new_column_na <- NULL          # delete by name
df$new_column_vector <- NULL          # delete by name


# ============================================================
# 2. PROBLEM I.: Height and Weight are in inches and pounds
#    (not metric) — convert to cm and kg
# ============================================================
# Conversion factors.
# Named in two different styles on purpose, to compare conventions:
multi_in_to_cm <- 2.54    # snake_case (R's default/recommended style)
multiLbToKg <- 0.453592   # camelCase (shown here only for comparison — pick one style and stay consistent in real code)

# Step 1: convert units, store results as vectors first
height_in_cm_vector <- df$Height * multi_in_to_cm
weight_in_kg_vector <- df$Weight * multiLbToKg
height_in_cm_vector
weight_in_kg_vector

# Step 2: round to sensible precision
# (height: whole numbers, weight: one decimal place)
# NOTE: R's round() uses "round half to even" (banker's rounding), so
# round(0.5) gives 0, not 1 — don't be surprised if a hand-checked value
# looks "off" by one right at a .5 boundary.
height_in_cm_vector <- round(height_in_cm_vector, 0)
weight_in_kg_vector <- round(weight_in_kg_vector, 1)
height_in_cm_vector
weight_in_kg_vector

# Step 3: create two new (empty) columns in df (this step is redundant, in practice you can do Step 4 directly)
df$height_cm <- NA
df$weight_kg <- NA

# Step 4: fill those columns with the converted vectors
df$height_cm <- height_in_cm_vector
df$weight_kg <- weight_in_kg_vector

# --------------------------------------------------------------
# Steps 1-4 above are written out explicitly for clarity.
# In practice, you can do the same conversion in a single line:
# --------------------------------------------------------------
df$height_cm <- round(df$Height * multi_in_to_cm, 0)
df$weight_kg <- round(df$Weight * multiLbToKg, 1)

# we can also re-define height_cm as integer
df$height_cm <- as.integer(df$height_cm)

# ============================================================
# 3. CLEAN UP: rename original imperial columns to make
#    units explicit
#    Column is called "Gender", but it actually
#    records sex (male/female) — rename it accordingly
# ============================================================
names(df)[names(df) == "Height"] <- "height_in"
names(df)[names(df) == "Weight"] <- "weight_lb"
names(df)[names(df) == "Gender"] <- "sex"

# Confirm the renaming worked
colnames(df)
summary(df)

# ============================================================
# 4. SANITY CHECK: confirm the unit conversion didn't distort
#    the data — it should just rescale each value, not change
#    its relative position (i.e., the tallest person in inches
#    is still the tallest person in cm)
# ============================================================
# Numerically: correlation measures how strongly two variables
# move together. Since height_cm = height_in * 2.54 (a fixed
# multiplier), every point would fall on a perfectly straight
# line if there were no rounding — so correlation should be 1,
# or very close to it, since we rounded height_cm and weight_kg
# earlier (rounding introduces tiny deviations from the exact
# straight line).
# If it's noticeably less than ~0.999, something went wrong in
# the conversion (e.g. rows got reordered, or the wrong columns
# were compared).
cor(df$height_in, df$height_cm)
cor(df$weight_lb, df$weight_kg)

# ============================================================
# 5. EXERCISE: Create BMI Column
# ============================================================
# BMI = WEIGHT (in kg) / [height (in meters)]^2
df$bmi <- NA

# Once you've filled in the formula above, sanity-check it the same
# way we checked the unit conversion in step 4 — adult BMI typically
# falls somewhere around 15-40, so summary() and a quick histogram
# are enough to catch an inverted formula (kg/m^2 vs m^2/kg) or a
# units mistake (forgetting height_cm is still in cm, not m):
summary(df$bmi)

# ============================================================
# 6. SELECT COLUMNS AND SAVE THE CLEANED DATA
# ============================================================
df_export <- df[, c("is_male", "height_cm", "weight_kg", "bmi")]
names(df_export)[names(df_export) == "height_cm"] <- "height"
names(df_export)[names(df_export) == "weight_kg"] <- "weight"
str(df_export)

write.csv(df_export, "data/clean/height-weight-by-sex.csv", row.names = FALSE)
