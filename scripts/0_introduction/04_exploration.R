# ====
# LOAD DEPENDENCIES
source("scripts/00_setup.R")  


# ============================================================
# 1. LOAD NAD CHECK THE CLEANED DATA
# ============================================================
# This is the file we exported at the end of the cleaning script —
# it already has sex (Male/Female), height (cm), weight (kg), 
# and a bmi column
df_nd <- read.csv(file.path(dir_dat, "height-weight-bmi.csv"), sep = ",", header = TRUE)

str(df_nd) # base R
glimpse(df_nd) # dplyr

summary(df_nd) # quick look at values


# ============================================================
# 2. EXPLORE CATEGORICAL VARIABLES
# ============================================================

## 2a. convert sex to a factor
# sex is currently character — factorizing it now means every table(),
# summary(), group_by(), and later ggplot() call downstream in this
# script can just use df_nd$sex directly, with readable labels
df_nd$sex <- as.factor(df_nd$sex)

## 2b. verify the conversion worked as expected
levels(df_nd$sex)              # should be "Female" "Male" - if NULL, something's wrong
length(levels(df_nd$sex))      # should be 2 — if 0, something's wrong

## 2c. count observations per category — two ways, compare their NA behavior
# table(): simple, but silently DROPS NA by default
table(df_nd$sex)
table(df_nd$sex, useNA = "ifany") # always check this version too

# summary(): SAFER default — automatically shows an NA count if any exist
# BUT this only works because sex is now a FACTOR (see 2a). summary() on
# a plain character vector doesn't count categories at all — it just
# reports length/class/mode, since summary() behaves differently
# depending on the column's type
summary(df_nd$sex)                 # counts per category (+ NA's, if any)
summary(as.character(df_nd$sex))   # compare: length/class/mode — NOT counts

## 2d. proportions instead of raw counts
prop.table(table(df_nd$sex))
prop.table(summary(df_nd$sex)) # prop.table() works the same on either

## 2e. dplyr: counts and shares in one pipeline
df_nd %>%
  group_by(sex) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n))



# ============================================================
# 3. EXPLORE CONTINUOUS VARIABLES
# ============================================================

# always start with a GRAPHICAL inspection of continuous data
# using histograms.

## one at a time, in base R
hist(df_nd$height)
hist(df_nd$weight)
hist(df_nd$bmi)

## or apply hist() across several NUMERIC columns at once with sapply()
sapply(df_nd[c("height", "weight", "bmi")], hist)

# note hist() both DRAWS a plot (the side effect we want) and RETURNS a
# "histogram" object; sapply() collects those returns too, so this also
# dumps a cluttered matrix to the console after the plots. wrap in
# invisible() if you only want the plots
invisible( sapply(df_nd[c("height", "weight", "bmi")], hist) )

## you might expect hist(df_nd) to just plot every numeric column of the
# whole data frame at once — it doesn't, in base R. hist() is a
# "generic" function: it looks at the CLASS of its input and dispatches
# to a more specific version behind the scenes. base R only ships
# hist.default(), which handles a single numeric vector (what actually
# ran above) — there is no hist.data.frame() in base R.

## the Hmisc package adds exactly that missing method — which is why
# hist(df_nd) below already works: 00_setup.R already loaded Hmisc at
# the top of this script, so the SAME hist() call now dispatches to
# Hmisc's hist.data.frame() instead, plotting every numeric column at once
# install.packages("Hmisc")
# library(Hmisc)
# this is how you'd install/load a package inline in a script, but since
# we expect to reuse Hmisc a lot, we install/load it "globally" in 00_setup.R instead
hist(df_nd)


# ==== 
# EXERCISE: Interpret the histograms
# ====


### GGPLOT + DPLYR

