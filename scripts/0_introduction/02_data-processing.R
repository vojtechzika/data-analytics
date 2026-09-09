# ===================================================================
# LOAD 00_setup.R
#
# 00_setup.R lives in the root of scripts folder (relative to Rproj)
# It contains code / functions / libraries we want to reuse in
# all scripts we work with so we do not need to repeat the same
# code again and again.
# Loading the file like this is as-if the code would be written
# in the actual script.
# ===================================================================

source("scripts/00_setup.R")  


### In this script, we will process a new dataset height-weight-by-sex.csv
# in both base R and dplyr.


# ============================================================
# 1. LOAD AND INSPECT THE DATA
# ============================================================
# Load data from csv file into a data.frame called df_hw
df_hw <- read.csv(file.path(dir_src, "height-weight-by-sex.csv"), sep = ",", header = TRUE)
# Also open in Excel!

### INSPECT THE DATA

## Data structure
str(df_hw) # in base R, use str()

glimpse(df_hw) # in dplyr, use glimpse()

## Missing values and duplicate rows — always check before doing anything else
# base R
colSums(is.na(df_hw))     # how many NAs per column
sum(duplicated(df_hw))    # how many fully duplicated rows

# dplyr
df_hw %>% summarise(
                across( # (across() applies the same function to every column)
                  everything(), ~ sum(is.na(.))
                  )
                )

df_hw %>% summarise(
                n_duplicates = sum(
                    duplicated(df_hw) # duplicated returns list of logical values, TRUE for a duplicate
                  )
                )


### VIEW THE RAW DATA
head(df_hw) # only few rows
df_hw # all of it
View(df_hw) # Excel-style table, the opened tab will reflect all changes below

### EXPLORE THE DATA
# base R
summary(df_hw) # summary of all values

# dplyr really doesn't have summary() equivalent, but allows you
# to easily create your own summaries. This will come handy later,
# for instance for confidence intervals computation, etc.

df_hw %>% # "perform all following actions on df_hw dataframe"
  group_by(Gender) %>% # perform all the following actions on df_hw grouped by Gender
  summarise(
    mean_height = mean(Height, na.rm = TRUE),
    sd_height   = sd(Height, na.rm = TRUE),
    min_height  = min(Height, na.rm = TRUE),
    max_height  = max(Height, na.rm = TRUE),
    n          = n() # number of rows
  )

# ============================================================
# 2. TRANSFORM DATA
# ============================================================

### recode Gender into dummy 
# when creating dummy (or recoding any variable), 
# ALWAYS CREATE A NEW COLUMN, NEVER RECODE THE ORIGINAL ITSELF

# use simple logical filtering (see 01_basics.R) if you would like the dummy to be LOGICAL
# PAY ATTENTION: this only works if the Gender variable has EXACTLY TWO LEVELS
# utilize your knowledge of factors to find out how many levels the variable has first
levels(factor(df_hw$Gender))
# also check for missing values, since levels() won't show these
sum(is.na(df_hw$Gender)) 
# if only Female and Male (and no missing values you need to handle separately), you can use logical filtering
df_hw$is_male <- df_hw$Gender == "Male" # TRUE/FALSE dummy, one per row
# this does the same job as a simple ifelse statement
df_hw$is_male <- ifelse(df_hw$Gender == "Male", TRUE, FALSE)

# if there are NAs, a single is_male dummy can't safely capture the whole variable — 
# collapsing everything else into "not Male" would silently misclassify NA as Female, 
# exactly the trap flagged above instead, explicitly map only Male/Female 
# using a nested ifelse statement 
df_hw$is_male <- ifelse(df_hw$Gender == "Male", TRUE,
                        ifelse(df_hw$Gender == "Female", FALSE, NA)
                        )

# dplyr offers a more user-friendly solution using case_when
# which clearly defines how should individual levels be re-coded
# this is especially effective for variables with many levels
df_hw <- df_hw %>%
  mutate( # mutate() creates a new column
      is_male = case_when( # the new column is_male is defined by case_when() function
      Gender == "Male"   ~ TRUE, # which recodes Male as TRUE
      Gender == "Female" ~ FALSE, # Female as FALSE
      TRUE               ~ NA   # and anything else, including actual NA, becomes NA
  ))


## LOGICAL vs INTEGER DUMMY
# alternatively you can code dummy also as integers 0L/1L
# These are mathematically identical for # math/modeling 
# (TRUE/FALSE IS 1/0 under the hood), but integer writes as
# literal 0/1 to a CSV/Excel export while logical writes as "TRUE"/"FALSE" text
# ultimately this is a matter of personal/field preference — e.g. in
# economics and other social sciences, 0/1 integer coding is the more
# conventional way to represent a dummy variable.
# Since I am in econ, I will stick with integers for now
df_hw$is_male <- ifelse(df_hw$Gender == "Male", 1L, 0L)



### RECODE IMPERIAL UNITS INTO METRIC (default for science)

## first, let's create functions for conversion of both units
# these will be MONOTONIC TRANSFORMATION

inches_to_cm <- function(inches){ 
  cm <- inches * 2.54 # transform input in inches to cm
  cm <- round(cm, 0) # round the result to 0 decimal places
  cm <- as.integer(cm) # make it an integer
  return(cm) 
}

pounds_to_kg <- function(pounds){ 
  kg <- pounds * 0.453592
  kg <- round(kg, 1) # round the result to 1 decimal place
  return(kg) 
}

## add columns with base R
df_hw$height_cm <- inches_to_cm(df_hw$Height)
df_hw$weight_kg <- pounds_to_kg(df_hw$Weight)
  
## or with dplyr
df_hw <- df_hw %>% mutate(
  height_cm = inches_to_cm(Height),
  weight_kg = pounds_to_kg(Weight)
)

# SANITY CHECK: confirm the unit conversion didn't distort
# the data — it should just rescale each value, not change
# its relative position (i.e., the tallest person in inches
# is still the tallest person in cm).
# Numerically: correlation measures how strongly two variables
# move together. Since height_cm = height * 2.54 (a fixed
# multiplier), every point would fall on a perfectly straight
# line if there were no rounding — so correlation should be 1,
# or very close to it, since we rounded height_cm and weight_kg
# earlier (rounding introduces tiny deviations from the exact
# straight line). If it's noticeably less than ~0.999, something 
# went wrong in the conversion (e.g. rows got reordered, or 
# the wrong columns were compared).

# base R
cor(df_hw$Height, df_hw$height_cm) # the correlation is 0.9995647
cor(df_hw$Weight, df_hw$weight_kg) # the correlation is 0.999998

# dplyr
correlation_check <- df_hw %>% summarise(
  cor_height = cor(Height, height_cm),
  cor_weight = cor(Weight, weight_kg)
)
correlation_check 

# ============================================================
# 3. CREATE INDICES 
# (variables that are a combination of other variables)
# ============================================================

# ====
# EXERCISE: Create a new column BMI and fill it with body-mass 
# index computed as (WEIGHT in kg) / (height in meters)^2
# use a function, round to 1 decimal place

compute_bmi <- function(kg, cm){
  m <- cm/100
  bmi <- kg/m^2
  bmi <- round(bmi, 1)
  return(bmi)
}

df_hw$bmi <- compute_bmi(df_hw$weight_kg, df_hw$height_cm) # base R
df_hw <- df_hw %>% mutate(bmi = compute_bmi(weight_kg, height_cm)) # dplyr

# Once you've filled in the formula above, sanity-check it the same
# way we checked the unit conversion above — adult BMI typically
# falls somewhere around 15-40, so summary() is enough to catch an 
# inverted formula (kg/m^2 vs m^2/kg) or a units mistake 
# (forgetting height_cm is still in cm, not m):
summary(df_hw$bmi)


# ============================================================
# 4. PREPARE NEW DATAFRAME FOR EXPORT
# We only want to export is_male, height_cm, weight_kg, bmi
# and rename columns to is_male, height, weight, bmi
# ============================================================

## Base R
# select columns into new dataframe
df_hw_export <- df_hw[c("is_male", "height_cm", "weight_kg", "bmi")] # or use a vector

# and change column names
names(df_hw_export)[names(df_hw_export) == "height_cm"] <- "height"
names(df_hw_export)[names(df_hw_export) == "weight_kg"] <- "weight"

## dplyr

df_hw_export <- df_hw %>% 
  select(is_male, height_cm, weight_kg, bmi) %>% # select columns by names, no quotes needed
  rename( # rename columns in the same go
    height = height_cm,
    weight = weight_kg
  )

# check the new data frame
summary(df_hw_export)


# ============================================================
# 5. EXPORT DATA
# ============================================================

## write the curated data frame to a new csv into output/data
write.csv(df_hw_export, file.path(dir_dat, "height-weight-bmi.csv"), row.names = FALSE)
# row.names = FALSE makes sure we do not add a column with row numbers

## we can also save our dplyr correlation_check as a diagnostics file into data/diagnostics
write.csv(correlation_check, file.path(dir_dia, "imperial-metric-cor.csv"), row.names = FALSE)

