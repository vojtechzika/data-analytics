# ===================================================================
# 03_two-groups-comparison.R — comparing means with t-tests: one
# sample against a fixed value, two independent samples, and paired
# samples
# ===================================================================

# ============================================================
# 1. LOAD DEPENDENCIES AND DATA
# ============================================================
source("scripts/00_setup.R")

df_hwb <- read.csv(file.path(dir_dat, "height-weight-bmi.csv"), sep = ",", header = TRUE)

male_bmi   <- df_hwb$bmi[df_hwb$sex == "Male"]
female_bmi <- df_hwb$bmi[df_hwb$sex == "Female"]


# ============================================================
# 2. ASSUMPTIONS BEHIND THE T-TEST
# ============================================================
# ====
# a t-test isn't valid for just any data -- it relies on a few
# assumptions. worth checking each one against our own bmi data below
# ====

## INDEPENDENCE
# ====
# each observation should be unrelated to the others (aside from the
# deliberate pairing used in a paired test). this isn't something you
# test statistically -- it comes from how the data was COLLECTED. our
# rows are unrelated individuals from a cross-sectional survey, so this
# one is satisfied by design, not by a formula
# ====


## NORMALITY
# ====
# for a small sample, the DATA itself should be roughly normal. for a
# larger sample, that requirement relaxes -- only the SAMPLING
# DISTRIBUTION of the mean needs to be approximately normal, which it
# tends to become as sample size grows (the central limit theorem),
# regardless of how the raw data looks
#
# for a TWO-SAMPLE test, this applies to EACH group separately, not to
# the pooled/whole dataset. each group gets its own sample mean and its
# own sampling distribution, so each one needs to satisfy this on its
# own -- checking normality on the combined male + female column tells
# you nothing about whether male_bmi or female_bmi individually meet it
#
# this normality is exactly what the t-test relies on to get its
# p-value: once the sample mean is converted into a t-statistic, R
# places that t-statistic on a (near-)normal curve -- the t-distribution,
# which converges to the normal distribution as df grows -- and the
# p-value is simply the AREA under that curve beyond your observed t
# (both tails, for a two-sided test). a bigger |t| pushes further into
# the tail, leaving less area beyond it, hence a smaller p-value. this
# is the exact calculation the critical-values tables further below
# used to do by hand: the critical t is just the point past which a
# fixed amount of area (your alpha) remains
#
# if the sample is small AND the raw data is non-normal, the CLT hasn't
# kicked in yet -- so the true sampling distribution of the mean may not
# actually match the t-distribution R assumes, and the p-value it
# reports can be distorted (either too conservative or too liberal,
# depending on the skew)
#
# shapiro.test() formally tests this (H0: the data IS normal) -- but it
# caps out at 5000 observations, so a sample of this size needs a slice
# rather than the whole column. it's also VERY sensitive at large n:
# even a mild departure from normality will often get rejected
#
# taking the first 1000 rows would be a mistake here, since the data
# is grouped by sex -- that slice would silently become male-only. the
# fix is to take evenly-spaced rows across the whole dataset instead,
# so both groups stay represented regardless of how the rows are ordered
# ====

even_slice <- floor(seq(1, nrow(df_hwb), length.out = 1000))

shapiro.test(df_hwb$bmi[even_slice])
# small p-value -- H1 IS supported here, i.e. the data does not look
# exactly normal at this sample size. this is the sensitivity called
# out above: bmi has a real, if mild, skew, and n = 1000 is enough for
# shapiro.test() to catch it (at a much smaller n, like 100, it
# typically stops rejecting -- not because the data changed, but
# because the test has less power to detect the same departure)
#
# either way, this doesn't threaten the ONE-SAMPLE test below: with
# n = 10000, the CENTRAL LIMIT THEOREM alone guarantees the sampling
# distribution of the mean is normal, regardless of whether the raw
# data is

shapiro.test(male_bmi[1:1000])
shapiro.test(female_bmi[1:1000])


## EQUAL VARIANCES (TWO-SAMPLE CASE ONLY)
# ====
# do the two groups have equal population variances? the classic
# ("Student's") t-test assumes they do; Welch's version doesn't require
# that assumption at all, at only a small cost in precision when
# variances genuinely ARE equal. R defaults to Welch's version for
# exactly this reason -- it's the safer choice when you're not sure,
# which is most of the time. var.equal = TRUE switches to the classic
# version if you have good reason to assume equal variances
#
# var.test() checks this formally (H0: the two variances are equal) --
# check the p-value it gives you: below your significance level means
# the variances genuinely differ, so Welch's version (the default used
# throughout this script) is the right call, not just the safe one
# ====

var.test(male_bmi, female_bmi)


## WHAT IF THE ASSUMPTIONS DON'T HOLD?
# ====
# every t-test above has a NONPARAMETRIC counterpart that drops the
# normality assumption entirely, working with the RANKS of the data
# instead of its raw values: wilcox.test() replaces one-sample,
# two-sample, and paired t-tests alike (same function, same
# paired = TRUE argument). more on when and why to reach for this later
# in the course -- for now, just note it exists as the fallback
# wilcox.test(male_bmi, female_bmi)
# ====




# ============================================================
# 3. ONE-SAMPLE T-TEST
# ============================================================
# ====
# tests whether the population mean could plausibly BE some specific
# hypothesized value -- here, whether average bmi differs from 25.5.
# mu = ... is what sets that hypothesized value; the default (if you
# leave it out) is 0, which would rarely be the comparison you
# actually want
# ====

hypothesized_bmi = 25.5

one_sample <- t.test(df_hwb$bmi, mu = hypothesized_bmi)
one_sample

# ====
# reading the output:

## T-statistics (t)
# - t: how many standard errors the sample mean sits away from mu --
#   t = (sample mean - mu) / standard error, where the standard error
#   is sd / sqrt(n). the further from 0 (in either
#   direction), the more the data disagrees with mu = 25.5

standard_error <- sd(df_hwb$bmi) / sqrt(length(df_hwb$bmi))
( mean(df_hwb$bmi) - hypothesized_bmi ) / standard_error

one_sample$statistic
# matches the manual calculation above -- this is exactly what t.test()
# computes internally

## DEGREES OF FREEDOM (df)
# - df: degrees of freedom, n - 1 for a one-sample test -- together
#   with t, this is what gets converted into the p-value below
#
#   before software did this conversion for us, t and df were looked up
#   by hand in a critical values table: find your df down the side,
#   your chosen alpha across the top, and read off the critical t at
#   that intersection. if your computed |t| exceeds it, you find
#   support for H1 -- the same decision t.test() makes internally, just
#   via a p-value instead of a table lookup
#   https://www.scribbr.com/statistics/students-t-table/
#   https://www.stat.purdue.edu/~lfindsen/stat503/t-Dist.pdf

## ALTERNATIVE HYPOTHESIS
# - "alternative hypothesis: true mean is not equal to 25.5" -- this
#   line is just restating the SETUP (what H1 is), not the RESULT. it
#   always prints, whether or not the test actually found support for
#   it -- don't read it as a conclusion

## P-VALUE
# - p-value: the probability of seeing a sample mean this far from 25.5
#   if the TRUE mean bmi really were 25.5. THIS is where the
#   actual verdict comes from -- e.g. p = 0.39 here is nowhere near
#   significant, so we do NOT find support for H1: this data is
#   perfectly consistent with a true mean bmi of 25.5

## CONFIDENCE INTERVAL
# - 95 percent confidence interval: the plausible range for the true
#   mean bmi -- 25.5 falls right inside it here, which lines up with
#   that high p-value

## SAMPLE ESTIMATES
# - sample estimates: the actual sample mean bmi
# ====


# ============================================================
# 4. TWO-SAMPLE T-TEST (TWO-SIDED)
# ============================================================
# ====
# tests whether two INDEPENDENT groups have the same population mean --
# here, whether bmi differs by sex. two-sided because, taken at face
# value, we're only asking IF the two differ, not in which direction
# ====

two_sample <- t.test(male_bmi, female_bmi)
two_sample

# ====
# same output shape as the one-sample test above, with a few differences:
# - "Welch Two Sample t-test" -- the unequal-variance version from the
#   assumptions section above, used by default
# - "alternative hypothesis: true difference in means is not equal to
#   0" -- again, this is just the SETUP printing back at you, not the
#   result. don't read this line as a conclusion
# - p-value: here, p < 2.2e-16 -- about as small as R will print --
#   strong evidence the two population means genuinely differ
# - the confidence interval is now for the DIFFERENCE in means
#   (male - female): [4.04, 4.17] here -- entirely above 0, consistent
#   with that tiny p-value
# - sample estimates lists both group means side by side instead of
#   just one (27.5 for males, 23.4 for females here)
# ====


# ============================================================
# 5. THE GOTCHA: DO WE ACTUALLY HAVE A TWO-SIDED HYPOTHESIS?
# ============================================================
# ====
# two-sided is the safe DEFAULT, but it's not always the right choice.
# say prior research already gives us a specific DIRECTIONAL hypothesis --
# that male bmi is HIGHER than female bmi, not just "different". then
# the honest test is one-sided: alternative = "greater" puts the entire
# rejection region on one side, which makes the test more POWERFUL for
# that specific direction (a smaller p-value for the same data)
#
# the catch: the direction has to be decided BEFORE looking at the
# data. picking "greater" or "less" after peeking at which way the
# sample means already lean is a shortcut to more false positives
# ====

two_sample_one_sided <- t.test(male_bmi, female_bmi, alternative = "greater")
two_sample_one_sided

two_sample$p.value
two_sample_one_sided$p.value
# both print as 0 here -- not because the halving relationship broke,
# but because the effect is so large (n close to 5000 per group) that
# BOTH p-values are far too small for R to store as anything but 0
#
# to actually see the halving, try it on a much smaller slice of the
# same two groups instead, where the p-values aren't so tiny:

small_male   <- male_bmi[1:20]
small_female <- female_bmi[1:20]

t.test(small_male, small_female)$p.value
t.test(small_male, small_female, alternative = "greater")$p.value
# the second is (approximately) half the first -- exactly what the
# two-sided-vs-one-sided logic above predicts

# ====
# note what changed: the p-value is roughly half of the two-sided
# version above (for a difference in the expected direction), and the
# confidence interval is now one-sided too -- a lower bound only, with
# no upper limit
# ====


# ============================================================
# 6. PAIRED T-TEST
# ============================================================
# ====
# a paired test compares two measurements taken on the SAME subjects --
# e.g. before/after a treatment -- by testing whether their average
# DIFFERENCE is zero. paired = TRUE tells R to compute that per-row
# difference instead of treating the two columns as independent groups
#
# this dataset doesn't have a genuine repeated measurement (bmi and
# weight are different quantities in different units, so pairing them
# wouldn't mean anything). to still see paired = TRUE do something real,
# imagine our participants attended a diet experiment: bmi_after_diet
# takes each person's starting bmi and subtracts a randomly drawn
# amount, standing in for however much bmi each person lost on the
# diet. we draw that amount from a NORMAL distribution -- realistically, 
# some participants lose a lot, some barely anything, and some may even 
# gain a little, rather than everyone
# losing some guaranteed positive amount. now both columns are the SAME
# quantity, in the SAME units, on the SAME people, measured at two
# points in time -- exactly what paired = TRUE is for
# ====

bmi_after_diet <- df_hwb$bmi - rnorm(nrow(df_hwb), mean = 1, sd = 0.5)

paired_test <- t.test(df_hwb$bmi, bmi_after_diet, paired = TRUE)
paired_test

# ====
# reading the output:
# - "Paired t-test" -- confirms R computed the per-row difference
#   (bmi before - bmi after) and ran a one-sample test on those
#   differences against 0
# - t, df: same idea as before, now describing how far the MEAN
#   DIFFERENCE sits from 0, in standard errors, with df = n - 1
# - "alternative hypothesis: true mean difference is not equal to 0" --
#   again, this just restates the setup, not the result
# - p-value: here, essentially 0 -- with n = 10000 participants, even
#   this modest, noisy average effect is estimated precisely enough to
#   be far from 0, giving overwhelming evidence the diet had SOME
#   effect on average (individual participants can still show no
#   effect, or even a small gain, without changing that conclusion)
# - the confidence interval is for the MEAN DIFFERENCE this time, not
#   two separate group means -- expect it to sit close to 1, the mean
#   we drew the simulated weight loss from
# - sample estimates: "mean of the differences" -- a single number
#   here, unlike the two-sample case, which lists two separate group
#   means
# ====


# ============================================================
# 7. PULLING OUT INDIVIDUAL VALUES
# ============================================================
# ====
# t.test() returns a list, not just printed text -- saving it lets you
# grab any one piece of it instead of reading it off the console
# (we already did some of it)
# ====

two_sample$statistic   # the t-statistic
two_sample$parameter   # degrees of freedom
two_sample$p.value
two_sample$conf.int
two_sample$estimate    # the two group means

# ====
# conf.int is a vector of length 2 (lower, upper) -- index into it to
# pull just one bound instead of reading both off the printed pair
# ====

two_sample$conf.int[1]   # lower bound
two_sample$conf.int[2]   # upper bound


# ============================================================
# 8. THE TIDYVERSE ALTERNATIVE
# ============================================================
# ====
# rstatix::t_test() runs the same test but takes a data.frame and
# formula (outcome ~ group), pipe-friendly, and returns a tidy tibble
# instead of the printed object above -- convenient for feeding into
# further dplyr steps or ggplot. needs its own package, not part of
# core tidyverse
# ====

library(rstatix) # already added to 00_setup

df_hwb %>% t_test(bmi ~ sex)

# ====
# the tibble above is compact but leaves out the confidence interval --
# detailed = TRUE adds that back, along with the two group means
# ====

df_hwb %>% t_test(bmi ~ sex, detailed = TRUE)

# ====
# this is a tibble (a data.frame), not a list like base t.test() -- so
# individual values come out as COLUMNS, by name, instead of with $
# ====

tidy_result <- df_hwb %>% t_test(bmi ~ sex, detailed = TRUE)

tidy_result$statistic   # the t-statistic
tidy_result$df          # degrees of freedom
tidy_result$p           # p-value
tidy_result$conf.low    # lower bound of the confidence interval
tidy_result$conf.high   # upper bound
tidy_result$estimate1   # group 1 mean
tidy_result$estimate2   # group 2 mean