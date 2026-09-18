source("scripts/00_setup.R")

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)
glimpse(df_cap)

alpha  <- 0.05    # significance level for every Shapiro-Wilk test below
n_boot <- 2000    # number of bootstrap resamples for the CLT check

# shapiro.test() needs >= 3 non-missing, non-constant values -- this safe
# version returns just the p-value, or NA if there isn't enough data
safe_shapiro <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(unique(x)) < 3) return(NA_real_)
  shapiro.test(x)$p.value
}

# resample the MEAN n_boot times from x -- the building block for every
# CLT check below.
#
# WHAT BOOTSTRAPPING ACTUALLY DOES: we don't have access to the true
# population, so we can't directly see what "many different samples of this
# size" would look like. Bootstrapping fakes this by treating our one
# sample as a stand-in for the population, then drawing new samples of the
# SAME size FROM IT, WITH replacement (sample(..., replace = TRUE) -- so a
# given observation can be picked more than once in a resample, or not at
# all). Each resample gives one new mean; repeating that n_boot times
# builds up an empirical approximation of the sampling distribution of the
# mean -- the very thing the CLT makes claims about -- using only the data
# we actually have, instead of a formula that just assumes normality.
#
# Reseeding here (rather than once at the top of the script) means every
# bootstrap call, wherever it appears, is independently reproducible --
# rerun any single line and you'll always get the same answer, instead of
# it depending on how much other random code ran before it.
bootstrap_means <- function(x) {
  x <- x[!is.na(x)]
  set.seed(123)
  replicate(n_boot, mean(sample(x, length(x), replace = TRUE)))
}
bootstrap_means <- function(x) {
  x <- x[!is.na(x)]
  set.seed(123)
  replicate(n_boot, mean(sample(x, length(x), replace = TRUE)))
}

# and the CLT check itself: is that resampled distribution of means normal?
bootstrap_clt_p <- function(x) {
  safe_shapiro(bootstrap_means(x))
}


# ====
## STEP 1: MILK_EUR (no subgroups) ###############################

# milk_eur was joined onto df_cap by country, so every city row within a
# country repeats the exact same value. Before looking at its distribution
# we collapse it to one value per country -- otherwise a country with many
# cities (the USA, 589 rows) would count hundreds of times more than a
# country with just one.
milk_vals <- df_cap %>% distinct(country, milk_eur) %>% pull(milk_eur)
milk_vals <- milk_vals[!is.na(milk_vals)]
length(milk_vals)   # true sample size: one value per country

hist(milk_vals, breaks = 50, main = "milk_eur (one value per country)")

shapiro.test(milk_vals)
# p < 0.05 -> we reject normality: the raw data isn't normal

# non-normal raw data can still be safely used with parametric methods
# (t-test, ANOVA, ...) if the sample is large enough for the Central Limit
# Theorem to kick in for the MEAN. Check this directly: resample the mean
# many times from the real data, and test whether THAT distribution is
# normal, rather than trusting a rule of thumb like "n >= 30".
milk_boot_means <- bootstrap_means(milk_vals)
hist(milk_boot_means, breaks = 30, main = "bootstrapped means of milk_eur")
shapiro.test(milk_boot_means)
# p > 0.05 -> the sampling distribution of the mean IS normal -> the CLT
# saves us, parametric methods are fine for milk_eur

# milk_eur doesn't vary by city or urban_class (it's a country-level
# value), so there's no meaningful "by group" breakdown to do for it --
# unlike the next three variables.

# VERDICT: milk_eur is well-behaved under the CLT at its true sample size
# (~76 countries, not the row-inflated 2,447). No subgroup breakdown
# exists for it (country-level variable), so this is the whole story.
#
# milk_eur -> PARAMETRIC POSSIBLE
# ====



# ====
## STEP 2: CAP_EUR (overall, then by urban_class) ################

cap_eur_vals <- df_cap$cap_eur[!is.na(df_cap$cap_eur)]
length(cap_eur_vals)

hist(cap_eur_vals, breaks = 50, main = "cap_eur (overall)")
shapiro.test(cap_eur_vals)
# p < 0.05 -> not normal

cap_eur_boot_means <- bootstrap_means(cap_eur_vals)
hist(cap_eur_boot_means, breaks = 50, main = "bootstrapped means of cap_eur")
shapiro.test(cap_eur_boot_means)
# p > 0.05 -> the sampling distribution of the mean IS normal -> CLT saved us

# Now that we know the overall picture, repeat the SAME check separately
# for each urban_class -- the overall result can hide a subgroup that
# behaves differently. (NA urban_class rows are the ambiguous "Urban,
# Suburban"-type ones we already blanked out during cleaning -- we don't
# know their real category, so we leave them out here too.)
cap_eur_by_urban <- df_cap %>%
  filter(!is.na(urban_class)) %>%
  group_by(urban_class) %>%
  summarise(
    n         = n(),
    shapiro_p = safe_shapiro(cap_eur),
    clt_p     = bootstrap_clt_p(cap_eur),
    .groups   = "drop"
  )
cap_eur_by_urban
# read shapiro_p first: if it's > 0.05, we're done, normal is fine.
# if it's < 0.05, check clt_p -- if THAT'S > 0.05, parametric is still ok.

# VERDICT: cap_eur is well-behaved under the CLT, overall and in every
# urban_class subgroup (Rural, Suburban, Urban all pass comfortably).
#
# cap_eur -> PARAMETRIC POSSIBLE, overall and in every urban_class subgroup
# ====



# ====
## STEP 3: WAGE_EUR (exactly the same recipe as cap_eur) #########

wage_eur_vals <- df_cap$wage_eur[!is.na(df_cap$wage_eur)]
length(wage_eur_vals)

hist(wage_eur_vals, breaks = 50, main = "wage_eur (overall)")
shapiro.test(wage_eur_vals)
# p < 0.05 -> not normal

wage_eur_boot_means <- bootstrap_means(wage_eur_vals)
hist(wage_eur_boot_means, breaks = 50, main = "bootstrapped means of wage_eur")
shapiro.test(wage_eur_boot_means)
# p > 0.05 -> CLT saves us overall -> parametric OK

wage_eur_by_urban <- df_cap %>%
  filter(!is.na(urban_class)) %>%
  group_by(urban_class) %>%
  summarise(
    n         = n(),
    shapiro_p = safe_shapiro(wage_eur),
    clt_p     = bootstrap_clt_p(wage_eur),
    .groups   = "drop"
  )
wage_eur_by_urban

# VERDICT: wage_eur is well-behaved (parametric OK) overall, and within
# Rural and Suburban alone. It fails the CLT check specifically within
# Urban (clt_p=0.033, n=1717 -- not a small-sample fluke, and not because
# Urban itself is a "bad" subgroup: cap_eur was fine in Urban, so this is
# a property of wage_eur there, not of Urban in general).
#
# Practically:
#   - A standalone estimate/CI for wage_eur in Urban: don't use a
#     normal-theory CI (mean +/- 1.96*SE) -- use a bootstrap percentile CI
#     instead, since that's exactly the assumption that failed.
#   - Any comparison of wage_eur ACROSS urban_class (Rural vs Suburban vs
#     Urban): use Kruskal-Wallis for the whole comparison, not ANOVA --
#     Urban being one of the compared groups rules out ANOVA even though
#     Rural and Suburban individually would have been fine with it.
#   - Rural or Suburban analyzed on their own, without Urban: parametric
#     methods (t-test, ANOVA) are fine.
#
# wage_eur -> PARAMETRIC POSSIBLE overall, Rural, Suburban (standalone)
#             NON-PARAMETRIC required for Urban (standalone) and for any
#             comparison across urban_class
# ====





# ====
## STEP 4: CAP_INDEX (same recipe -- but the CLT won't save this one) ###
cap_index_boot_means <- bootstrap_means(cap_index_vals)
hist(cap_index_boot_means, breaks = 50, main = "bootstrapped means of cap_index")
shapiro.test(cap_index_boot_means)
# p = 0.588 -- the CLT DOES rescue cap_index overall, despite the raw data
# being the most extreme deviation from normal of anything in this script
# (W=0.428). Don't stop at the pooled result though -- it can hide
# subgroup-level problems, so check by urban_class too.

cap_index_by_urban <- df_cap %>%
  filter(!is.na(urban_class)) %>%
  group_by(urban_class) %>%
  summarise(
    n         = n(),
    shapiro_p = safe_shapiro(cap_index),
    clt_p     = bootstrap_clt_p(cap_index),
    .groups   = "drop"
  )
cap_index_by_urban
# unlike the overall result, EVERY subgroup fails clearly (Rural clt_p=2e-8,
# Suburban=1.8e-6, Urban=0.0026) -- not borderline, not a fluke. This is
# the OPPOSITE pattern from pooling: cap_index looks fine in aggregate but
# isn't fine within any single urban_class category. Any analysis split by
# urban_class needs non-parametric methods for raw cap_index, even though
# the pooled version is fine.

## STEP 4b: DOES A LOG-TRANSFORM RESCUE CAP_INDEX? ################

# cap_index is a ratio bounded at 0 and stretching out to the right -- a
# classic shape for a log-normal variable. Check log(cap_index) with the
# exact same recipe, rather than assuming the transform fixes it.

log_cap_index_vals <- log(cap_index_vals[cap_index_vals > 0])

hist(log_cap_index_vals, breaks = 50, main = "log(cap_index) (overall)")
shapiro.test(log_cap_index_vals)
# still p < 0.05 on the raw distribution

log_cap_index_boot_means <- bootstrap_means(log_cap_index_vals)
shapiro.test(log_cap_index_boot_means)
# p = 0.103 -- also passes overall, though this doesn't add much on its
# own since raw cap_index already passed overall via CLT. The real
# question is whether logging fixes the subgroups that failed:

log_cap_index_by_urban <- df_cap %>%
  filter(!is.na(urban_class), cap_index > 0) %>%
  mutate(log_cap_index = log(cap_index)) %>%
  group_by(urban_class) %>%
  summarise(
    n         = n(),
    shapiro_p = safe_shapiro(log_cap_index),
    clt_p     = bootstrap_clt_p(log_cap_index),
    .groups   = "drop"
  )
log_cap_index_by_urban
# the log-transform helps, partially: Rural (clt_p=0.487) and Urban
# (clt_p=0.486) both pass cleanly after logging, even though their RAW
# cap_index failed badly. Suburban is the holdout -- it still fails after
# logging (clt_p=0.021), though less dramatically than its raw version did
# (1.8e-6 vs 0.021).



## BONUS: WHY DOES A LOG DIFFERENCE MEAN A PERCENTAGE DIFFERENCE? #####
# Quick numeric demonstration, since the CAUTION below leans on this fact:
# a difference in logs is (almost) a relative/percentage difference, not an
# absolute one. Here's why, and where the shortcut breaks down.

# log(a) - log(b) is EXACTLY the same thing as log(a / b) -- always, no
# approximation involved:
a <- 120
b <- 100
log(a) - log(b)
log(a / b)
# identical

# and turning that log-difference back into a % change is also EXACT, as
# long as you exponentiate it properly:
exp(log(a) - log(b)) - 1   # exact relative change, from the log scale
(a - b) / b                 # exact relative change, computed directly
# identical again -- exp(logdiff) - 1 IS the true % change, always, exactly.

# the "log-difference IS roughly a percentage" shortcut people actually mean
# is different: it's reading the log-difference itself, with no exp() step,
# as if it were already a percentage. That only works for SMALL differences:

small_diff <- log(101) - log(100)   # a is 1% bigger than b
small_diff              # ~ 0.00995
(101 - 100) / 100       # exact relative change: 0.01
# close enough -- reading 0.00995 as "about 1%" costs you almost nothing

big_diff <- log(200) - log(100)     # a is 100% bigger than b (double)
big_diff                 # ~ 0.693
(200 - 100) / 100        # exact relative change: 1.00, i.e. 100%
# NOT close -- reading 0.693 as "69%" would be flatly wrong; the true
# difference is 100%. Rule of thumb: the direct-reading shortcut is fine
# for relative differences up to roughly 10-15%; beyond that, always
# exponentiate properly (exp(diff) - 1) rather than reading the raw
# log-difference as a percentage.

# ===
# VERDICT: cap_index is the most complicated variable in this dataset, and
# the right answer depends on what you're doing with it.
#
#   - Pooled across the whole dataset (ignoring urban_class): parametric is
#     fine via CLT, raw or logged -- you don't even need the transform here.
#
#   - Split by urban_class, RAW cap_index fails in every single group --
#     never use parametric methods on cap_index broken down by urban_class
#     without transforming it first.
#
#   - Split by urban_class, LOG-transformed cap_index passes in Rural and
#     Urban, but still fails in Suburban. So log(cap_index) can be treated
#     parametrically within Rural alone or Urban alone, but NOT within
#     Suburban, and NOT as a comparison across all three urban_class groups
#     (Suburban being one of the compared groups rules out ANOVA for the
#     whole comparison -- same logic as wage_eur and Urban earlier).
#
# CAUTION when using log(cap_index): every parametric result you get from
# it -- a mean, a confidence interval, an ANOVA/t-test comparison -- is in
# LOG units, not in cap_index's original units. You cannot interpret them
# as if they were computed on the raw scale:
#   - mean(log(cap_index)) is NOT log(mean(cap_index)). Exponentiating the
#     mean of the logs, exp(mean(log(x))), gives you the GEOMETRIC mean of
#     cap_index, not the ordinary (arithmetic) mean -- they are different
#     numbers, and the geometric mean is always <= the arithmetic mean for
#     skewed data like this.
#   - a difference between two groups' means on the log scale corresponds
#     to a PROPORTIONAL (percentage) difference on the original scale, not
#     an absolute one. "Urban's log(cap_index) mean is 0.2 higher than
#     Rural's" means Urban's cap_index is about exp(0.2) ~ 22% higher, not
#     "0.2 units higher".
#   - a confidence interval built on log(cap_index) and then exponentiated
#     back is a CI for the geometric mean / median of cap_index, not for
#     its arithmetic mean -- don't report it as if it were the latter.
# If you need the actual arithmetic mean back on the original scale, this
# whole toolkit doesn't give it to you directly -- report results in log
# terms explicitly (e.g. "median cap_index" or "percentage difference"),
# or use a method built for the original scale (like the non-parametric
# route, which is exactly why it stays the recommended default here).
#
# EXPERTISE NOTE: whether to transform cap_index at all -- and if so, when
# -- is a judgment call, not a mechanical rule. It depends on what you're
# going to do with the number afterwards, not just on which version passes
# more Shapiro tests. A few considerations:
#   - If your audience needs to read "the cappuccino index" as a familiar,
#     directly interpretable ratio (e.g. cappuccinos-per-hour-of-wage), the
#     raw scale communicates that meaning and log(cap_index) does not --
#     even where log passes more checks above, that's a reason to stay on
#     the raw scale and use non-parametric methods instead.
#   - If your analysis is really about RELATIVE differences ("is Rural 20%
#     more expensive than Urban?") rather than absolute ones, the log scale
#     isn't a workaround, it's the more natural scale for that question, and
#     the CAUTION above about proportional differences becomes an advantage
#     rather than a complication.
#
# There's also a more radical option worth naming explicitly: cap_index
# isn't a directly measured physical quantity like a wage or a price -- it's
# an index WE constructed (cappuccino_num / wage_num). Nothing says the raw
# ratio is the "true" definition and the log is a derived fix applied to it
# after the fact. You could just as legitimately DEFINE the index from the
# start as log(cappuccino_num / wage_num) -- at which point there's no
# "raw vs. transformed" question left to resolve, because the log version
# simply IS the index, not a patched-up version of some other, more
# original one. That's a defensible move precisely because cap_index is our
# own construction rather than a directly observed measurement with a fixed,
# pre-existing meaning -- it would NOT be defensible to silently redefine
# wage_eur this way, since wages are a directly measured quantity with a
# real-world unit (EUR/hour) that a reader expects to see on its original
# scale.
#
# As with the urban_class NA-ing decision earlier: there is no single
# correct answer here, and the choice can materially change what a reader
# takes away ("20% higher" vs. "2.3 points higher" are both defensible but
# say different things). Document whichever choice you make and why, so it
# can be revisited at the interpretation stage rather than silently baked in.
#
# cap_index -> PARAMETRIC POSSIBLE when pooled overall (raw or log), or
#              within Rural/Urban alone using log(cap_index) -- but read
#              the CAUTION and EXPERTISE NOTE above before reporting any
#              log-scale result as if it were in cap_index's original units.
#              NON-PARAMETRIC required for Suburban (raw or log), for any
#              raw cap_index subgroup analysis, and for any comparison
#              across all three urban_class groups.
# ====






# ====
## FINAL SUMMARY TABLE #############################################

final_table <- bind_rows(
  tibble(variable = "milk_eur",  group = "overall", n = length(milk_vals),
         shapiro_p = safe_shapiro(milk_vals), clt_p = safe_shapiro(milk_boot_means)),
  
  tibble(variable = "cap_eur",   group = "overall", n = length(cap_eur_vals),
         shapiro_p = safe_shapiro(cap_eur_vals), clt_p = safe_shapiro(cap_eur_boot_means)),
  cap_eur_by_urban %>% transmute(variable = "cap_eur", group = urban_class, n, shapiro_p, clt_p),
  
  tibble(variable = "wage_eur",  group = "overall", n = length(wage_eur_vals),
         shapiro_p = safe_shapiro(wage_eur_vals), clt_p = safe_shapiro(wage_eur_boot_means)),
  wage_eur_by_urban %>% transmute(variable = "wage_eur", group = urban_class, n, shapiro_p, clt_p),
  
  tibble(variable = "cap_index", group = "overall", n = length(cap_index_vals),
         shapiro_p = safe_shapiro(cap_index_vals), clt_p = safe_shapiro(cap_index_boot_means)),
  cap_index_by_urban %>% transmute(variable = "cap_index", group = urban_class, n, shapiro_p, clt_p),
  
  tibble(variable = "log_cap_index", group = "overall", n = length(log_cap_index_vals),
         shapiro_p = safe_shapiro(log_cap_index_vals), clt_p = safe_shapiro(log_cap_index_boot_means)),
  log_cap_index_by_urban %>% transmute(variable = "log_cap_index", group = urban_class, n, shapiro_p, clt_p)
) %>%
  mutate(decision = case_when(
    shapiro_p > alpha ~ "parametric OK (normal)",
    clt_p     > alpha ~ "parametric OK (CLT verified)",
    TRUE               ~ "use non-parametric"
  ))

final_table

#save as csv
write.csv(final_table, file.path(dir_dia, "cappuccino-check.csv"), row.names = FALSE)

#and also as pdf so we can use it as a cheat sheet later
pdf(file.path(dir_dia, "cappuccino-check.pdf"), width = 9, height = 6); gridExtra::grid.table(final_table, rows = NULL); dev.off()
# ====