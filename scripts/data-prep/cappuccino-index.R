source("scripts/00_setup.R")

# load Cappucino Index Dataset
df_cap <- read.csv(file.path(dir_src, "cappuccino-index-2026.csv"), sep = ",", header = TRUE)

# glimpse
glimpse(df_cap)

# NOTE that both Hourly.wage and Price.per.small.cappuccino are stored as characters
# it suggests that at least some values are non-numeric, otherwise R would store them
# as such. The usual suspects are decimal commas instead of decimal points.


## DIAGNOSTICS

# check NAs
df_cap %>% summarise( across( everything(), ~ sum(is.na(.)) ) ) 

# check duplicities
# duplicated() flags a row TRUE only if an earlier row is identical to it
# across every column -- the first occurrence of each row is never flagged,
# only the repeats, so this sum counts "extra" copies, not affected rows.
df_cap %>% summarise( n_duplicates = sum( duplicated(df_cap) ) )

# 93 duplicates found, list them
# duplicated(., fromLast = TRUE) flags the same rows from the bottom up.
# Combining it with duplicated(.) (top-down) catches the first occurrence
# too, not just its repeats -- otherwise the "original" row looks unique
# and gets left out of the printout.
df_cap %>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE)) %>%
  arrange(across(everything()))

# Before deleting anything, check whether these are genuine duplicate rows
# or just different observations that happen to share the same wage/price.
# count(across(everything())) groups by EVERY column at once, so two rows
# only land in the same group if they match on all of them -- currency,
# wage, price, country, city AND urban classification, not just price.
df_cap %>%
  count(across(everything()), name = "n_copies") %>%
  count(n_copies)

# 2,428 rows are unique, but 73 distinct rows appear 2-5 times each,
# accounting for the 93 extra rows. Matching on every single column --
# including city and urban classification -- up to five times over isn't
# something that happens by coincidence; it's the signature of an upstream
# data issue (e.g. the source got merged or scraped more than once), so we
# drop the repeats and keep one copy of each.
df_cap <- df_cap %>% distinct()

#check the duplicates again
df_cap %>% summarise( n_duplicates = sum( duplicated(df_cap) ) )
glimpse(df_cap)



# transform categorical columns to factors and check their levels -- looking
# at the actual level names (not just a count) is how you catch typos,
# inconsistent spellings, or messy compound categories before they cause
# problems downstream (e.g. a country spelled two ways, or a classification
# like "Urban, Suburban" that's really two categories in one string).
df_cap <- df_cap %>%
  mutate(across(c(Currency, Country, City, Urban.classification), as.factor))

# quick overview: how many distinct levels does each one have
df_cap %>% summarise(across(c(Currency, Country, City, Urban.classification), n_distinct))


# and the levels themselves, to actually look through
df_cap %>% select(Currency, Country, City, Urban.classification) %>% lapply(levels)


# Urban.classification is a bit messy:
#
# $Urban.classification
# [1] "Rural"                  "Suburban"               "Suburban, Rural"        "Urban"                  "Urban, Rural"           "Urban, Suburban"       
# [7] "Urban, Suburban, Rural"
#
# There is no single "correct" fix here -- this is an EXPERTISE CALL, not a
# formula. A lot of real data analysis is like this: the data doesn't tell
# you what to do, and no textbook gives a universal rule for it. You have
# to make a reasoned decision, be explicit about what you chose and why,
# and be ready to defend it if someone questions it later. Every time you
# make a call like this, WRITE IT DOWN -- in your paper, report, or
# methods section -- and come back to it when you interpret your results.
# These decisions are not cosmetic: a different reasonable choice here can
# change your numbers, sometimes substantially, so the reader (and future
# you) needs to know it was made and what the alternative would have been.
# (We already made calls like this earlier in this script too -- e.g.
# deciding the repeated rows were true duplicates and not coincidence, or
# trusting that decimal commas were the only issue only after actually
# looking at the flagged values, rather than assuming it.)
#
# Here, because we don't actually know what the ambiguous values mean, we
# replace them with NA rather than guess. You could just as reasonably take
# the first listed category instead -- there isn't a "right" answer, only a
# defensible, documented one.
df_cap <- df_cap %>%
  mutate(Urban.classification = if_else(grepl(",", Urban.classification), NA_character_, as.character(Urban.classification)))


# City has ~1000 distinct values 
#
# checking it properly (fuzzy-matching
# near-duplicate spellings, normalizing whitespace/case, cross-checking
# against Country, etc.) is real data-cleaning work, but it's beyond what
# we'll cover in this course. Just be aware it's there: with a categorical
# variable this granular, some entries are almost certainly the same place
# spelled two different ways, and we're not correcting for that here.



## CONVENIENCE
df_cap <- df_cap %>% 
  rename( # give all columns more convenient column names
    currency         = Currency,
    wage             = Hourly.wage,
    cappuccino       = Price.per.small.cappuccino,
    country          = Country,
    city             = City,
    urban_class      = Urban.classification
  ) %>%
  mutate( # and split the currency into code and name
    cur_code = sub(" -.*", "", currency),   # "DKK"
    cur_name = sub("^[A-Z]{3} - ", "", currency)  # "Danish Krone"
  ) 


## NUMERIC COLUMNS INTO NUMERIC
# Now lets get back to the two columns we want as numeric and first
# check all the values that has something other than a decimal point
#
# the problem is that in some regions, decimal comma is commonly used
# to separate decimals (instead of a decimal point). So we cannot blindly
# replace them (that would cause problems), but we first need to confirm
# no such case occurs.
# 
# Use a regex that matches anything that isn't a digit or a period, with grepl():
# [^0-9.] means "any character that is not 0–9 or .
df_cap %>%
  filter(grepl("[^0-9.]", wage) | grepl("[^0-9.]", cappuccino)) %>%
  select(cur_code, wage, cappuccino)

# We can now visually confirm that the only problem are the decimal commas, which
# is a good news (no spaces or other characters). We can now simply replace the
# comma with "nothing" to get rid of it.
# NOTE that this procedure is OK for a smaller dataset, where we can do such
# visual check. For big-data, you would need to propose a more elaborate and robust
# procedure.

# Remove the comma and convert to numeric.
# A good practice is to create new columns to check the conversion went right.
df_cap <- df_cap %>%
  mutate( # remove the comma (thousands separator) with gsub()
    wage_num      = gsub(",", "", wage),
    cappuccino_num = gsub(",", "", cappuccino)
  ) %>% 
  mutate( # and make columns numeric
    wage_num       = as.numeric(wage_num),
    cappuccino_num = as.numeric(cappuccino_num)
  )

glimpse(df_cap)

# CAUTION! This conversion is a very sensitive step: if you mess it up, all
# the upcoming analysis will be wrong. So as a sanity check, print again the
# "problematic" rows and check visually that the conversion was done right.
# Again, this can be done for a smaller dataset, big data need more robust solution.

df_cap %>%
  filter(grepl("[^0-9.]", wage) | grepl("[^0-9.]", cappuccino)) %>%
  select(wage, wage_num, cappuccino, cappuccino_num) %>%
  View()

# ==============================================================================
# HOMEWORK
#
# In the dataset folder, find a file messy_numbers.csv. It contains a
# column with messy numbers (representing price in US dollars) - 
# all the gotchas you can encounter in real life
# (decimal comma vs. point is only one of them). Can you find them all?
#
# You can use AI to help you write general cleaning code (ask about regex
# patterns, ask it to explain a function, etc.), but don't paste the actual
# data into it. Work out your approach on a few example values you type
# yourself, then apply it to the whole column -- that's the point of the
# exercise, not something an AI tool can shortcut for you.
#
# Here is the summary of the column if all the numbers are correctly read:
#
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   292.0   818.9   969.2   999.9  1146.9  3259.1
# ==============================================================================


## INDICES

# FIRST, we want to compute the cappuccino index - how long do you need to work
# as a barista to buy one cappuccino you do

df_cap <- df_cap %>%
  mutate( 
    cap_index      = cappuccino_num/wage_num
  ) 


# SECOND, we need to recompute all the different currencies into one (EUR), 
# so that we can meaningfully analyze prices and wages accross countries
# this can be done in R, but these packages usually require API keys and payment.
# We can bypass this and use Google Sheets, its inbuilt function 
# GOOGLEFINANCE("CURRENCY: xxxEUR"), where xxx is the currency code we want to use.

# Rather than sending the whole dataset through Google Sheets, we
# only need one exchange rate PER CURRENCY, not per row. There are only a
# couple dozen currencies in the data, so we export just those, convert them
# once, and join the rate back onto df_cap. 

# STEP 1: export the distinct currency codes used in the dataset
df_cap %>%
  distinct(cur_code) %>%
  arrange(cur_code) %>%
  write.csv(file.path(dir_dat, "currency-codes.csv"), row.names = FALSE)

# STEP 2: open google sheets, upload the csv, add a column "eur_rate" with
# the exchange rate to EUR, export as csv, and add it to datasets as exchange-rates-eur.csv
# the formula will be something like (adjust the column letter):
# =IF(A2<>"EUR", GOOGLEFINANCE("CURRENCY:" & A2 & "EUR"), 1)
# (EUR->EUR is set to 1 directly -- GOOGLEFINANCE doesn't reliably return
# a rate for a currency converted to itself; and if any other currency
# comes back #N/A

# STEP 3: read the converted rate table back in
cur_rates <- read.csv(file.path(dir_src, "exchange-rates-eur.csv"), sep = ",", header = TRUE)
glimpse(cur_rates)

glimpse(df_cap)

# STEP 4: join the rate onto every row by currency code, and convert
df_cap <- df_cap %>%
  left_join(cur_rates, by = "cur_code") %>%
  mutate(
    wage_eur = wage_num * eur_rate,
    cap_eur  = cappuccino_num * eur_rate
  ) 

# STEP 5: sanity-check the transformation, per currency (not pooled --
# currencies with very different face-value scales can make a pooled
# correlation misleading even when every conversion is correct)
df_cap %>%
  group_by(cur_code) %>%
  summarise(
    n = n(),
    wage_cor = cor(wage_num, wage_eur, use = "complete.obs"),
    cap_cor  = cor(cappuccino_num, cap_eur, use = "complete.obs")
  ) %>%
  filter(n > 1, wage_cor < 0.99 | cap_cor < 0.99 | is.na(wage_cor) | is.na(cap_cor))
# an empty result means we did it good; NA/warnings just mean too little
# variation in that group to compute a correlation, not a failed conversion.


# THIRD, attach milk prices (since this can be a confound) and join on df_cap
milk_eur <- read.csv(file.path(dir_src, "milk_price_sep26.csv"), sep = ",", header = TRUE)

milk_eur <- milk_eur  %>% rename(milk_eur = milk_price_eur_per_liter)

df_cap <- df_cap %>%
  left_join(milk_eur, by = "country")

# FOURTH, select columns (ion the order we want) for the final csv file
# and export
df_cap_export <- df_cap %>% select(country, cur_name, cur_code, city, urban_class, wage_eur, cap_eur, cap_index, milk_eur)
glimpse(df_cap_export) # check before exporting
write.csv(df_cap_export, file.path(dir_dat, "cappuccino-index-complete.csv"), row.names = FALSE)


