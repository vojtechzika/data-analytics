# ====
# OBJECTIVES
# 1) learn how to explore categorical variables with table / prop.table (with the use of factors) and dplyr alternatives
# 2) learn to explore continuous variables with histograms, boxplots, and violin plots
# 3) learn how to pivot wide table into a long table
# ====


# ====
# LOAD DEPENDENCIES
source("scripts/00_setup.R")  


# ============================================================
# 1. LOAD NAD CHECK THE CLEANED DATA
# ============================================================
# This is the file we exported at the end of the cleaning script —
# it already has sex (Male/Female), height (cm), weight (kg), 
# and a bmi column
df_hwb <- read.csv(file.path(dir_dat, "height-weight-bmi.csv"), sep = ",", header = TRUE)

str(df_hwb) # base R
glimpse(df_hwb) # dplyr

summary(df_hwb) # quick look at values


# ============================================================
# 2. EXPLORE CATEGORICAL VARIABLES
# ============================================================

## 2a. convert sex to a factor
# sex is currently character — factorizing it now means every table(),
# summary(), group_by(), and later ggplot() call downstream in this
# script can just use df_hwb$sex directly, with readable labels
df_hwb$sex <- as.factor(df_hwb$sex)

## 2b. verify the conversion worked as expected
levels(df_hwb$sex)              # should be "Female" "Male" - if NULL, something's wrong
length(levels(df_hwb$sex))      # should be 2 — if 0, something's wrong

## 2c. count observations per category — two ways, compare their NA behavior
# table(): simple, but silently DROPS NA by default
table(df_hwb$sex)
table(df_hwb$sex, useNA = "ifany") # always check this version too

# summary(): SAFER default — automatically shows an NA count if any exist
# BUT this only works because sex is now a FACTOR (see 2a). summary() on
# a plain character vector doesn't count categories at all — it just
# reports length/class/mode, since summary() behaves differently
# depending on the column's type
summary(df_hwb$sex)                 # counts per category (+ NA's, if any)
summary(as.character(df_hwb$sex))   # compare: length/class/mode — NOT counts

## 2d. proportions instead of raw counts
prop.table(table(df_hwb$sex))
prop.table(summary(df_hwb$sex)) # prop.table() works the same on either

## 2e. dplyr: counts and shares in one pipeline
df_hwb %>%
  group_by(sex) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n))


# ============================================================
# 3. EXPLORE CONTINUOUS VARIABLES
# ============================================================


### DISTRIBUTIONS
# always start with a GRAPHICAL inspection of continuous data
# using histograms.

## one at a time, in base R
hist(df_hwb$height)
hist(df_hwb$weight)
hist(df_hwb$bmi)

## or apply hist() across several NUMERIC columns at once with sapply()
sapply(df_hwb[c("height", "weight", "bmi")], hist)

# note hist() both DRAWS a plot (the side effect we want) and RETURNS a
# "histogram" object; sapply() collects those returns too, so this also
# dumps a cluttered matrix to the console after the plots. wrap in
# invisible() if you only want the plots
invisible( sapply(df_hwb[c("height", "weight", "bmi")], hist) )

## you might expect hist(df_hwb) to just plot every numeric column of the
# whole data frame at once — it doesn't, in base R. hist() is a
# "generic" function: it looks at the CLASS of its input and dispatches
# to a more specific version behind the scenes. base R only ships
# hist.default(), which handles a single numeric vector (what actually
# ran above) — there is no hist.data.frame() in base R.

## the Hmisc package adds exactly that missing method — which is why
# hist(df_hwb) below already works: 00_setup.R already loaded Hmisc at
# the top of this script, so the SAME hist() call now dispatches to
# Hmisc's hist.data.frame() instead, plotting every numeric column at once
# install.packages("Hmisc")
# library(Hmisc)
# this is how you'd install/load a package inline in a script, but since
# we expect to reuse Hmisc a lot, we install/load it "globally" in 00_setup.R instead
hist(df_hwb)


# ==== 
# EXERCISE: Interpret the histograms
# ====

# you can also use ggplot to make a histogram
# PLUS: ggplot is part of tidyverse so it works with dplyr syntax

df_hwb %>% ggplot( aes(x = weight) ) + 
          geom_histogram( bins = 120 ) # increase granularity by increasing the number of bins
                                        # the more granular you make it, the clearer it is that
                                        # there are two distinct groups of participants

df_hwb %>% ggplot( aes(x = weight, fill = sex) ) + 
  geom_histogram( bins = 60 , 
                  position = "identity", # set distributions to overlap, not "stack" (default)
                  alpha = 0.7 , # too see the overlaps, set increase transparency
                  ) 



### SUMMARIES

boxplot(df_hwb$height)
boxplot(df_hwb$weight)
boxplot(df_hwb$bmi)

invisible( sapply(df_hwb[c("height", "weight", "bmi")], boxplot) ) # one by one with sapply

boxplot(df_hwb[c("height", "weight", "bmi")])  # all three at once, base R -- no Hmisc needed
# bmi shows why this may not be the best view you want to see

#@@ However!!! We know that at least weight differs by sex

# you can make a quick boxplot by sex in base R
boxplot(height ~ sex, data = df_hwb) 

# but ggplot is more flexible
df_hwb %>% ggplot(aes(x = sex, y = weight)) + 
  geom_boxplot() +
  theme_calc() # ggthemes theme


### VIOLIN PLOT: DISTIBUTION PLUS SUMMARIES
# this is probably the most telling diagnostic plot

ggplot(df_hwb, aes(x = sex, y = weight)) +
  geom_violin() +
  geom_boxplot(width = 0.2)  #combined with boxplot in ggplot

  
  
## OPTION A: A FUNCTION
# ====
# aes() normally expects a bare column name (e.g. y = height) -- inside a
# function, you're passing a column NAME AS A STRING instead, so you need
# .data[[var]] to tell ggplot "look up this column dynamically"
# ====

plot_by_sex <- function(var) {
  ggplot(df_hwb, aes(x = sex, y = .data[[var]])) +
    geom_violin() +
    geom_boxplot(width = 0.2)
}

plot_by_sex("height")
plot_by_sex("weight")
plot_by_sex("bmi")


## OPTION B: WIDE TO LONG + FACETS
# ====
# pivot_longer() (tidyr) collapses several columns into two: one holding
# the ORIGINAL COLUMN NAMES (here: "variable"), one holding their VALUES
# -- this reshapes 3 separate numeric columns into 1 column you can facet on
# ====

df_hwb_long <- df_hwb %>%
  select(sex, height, weight, bmi) %>%
  pivot_longer(cols = c(height, weight, bmi), names_to = "variable", values_to = "value")

write.csv(df_hwb_long, file.path(dir_dat, "height-weight-bmi-long.csv"), row.names = FALSE)

glimpse(df_hwb_long)

ggplot(df_hwb_long, aes(x = sex, y = value)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  facet_wrap(~ variable, scales = "free_y")
# scales = "free_y" is NOT optional here -- height, weight, and bmi are on
# completely different scales (cm vs kg vs an index), so a shared y-axis
# would flatten two of the three panels into nothing




# ====
# WHERE TO GO NEXT
# the boxplots above showed height/weight/bmi LOOK different between sexes --
# but "looks different" isn't enough to act on. is the difference big
# enough that it's unlikely to be due to chance alone? that's exactly
# what formal statistical tests (t-tests, ANOVA, ...) are for.
#
# but WHICH test is valid depends on whether your data behaves normally
# (or your sample is large enough for the Central Limit Theorem (CLT)
# to rescue it (about that later in the course. that's why, before 
# running any test, we check normality first. 
# ====





