## THE MOMENTS OF A NORMAL DISTRIBUTION (THEORY, ON SIMULATED DATA)
# ====
# before checking whether OUR data is normal, let's see what these
# numbers look like for data we KNOW is normal (simulated) -- mean,
# variance, skewness (~0), kurtosis (~3) for a true normal distribution
#
# all four are really the SAME formula, just with a different power k:
#   mean( (x - mean(x))^k )
# k=1 -> always 0 (that's WHY the mean isn't itself called "a moment" in
#        this sense -- it's the reference point everything else deviates
#        FROM, not a deviation itself)
# k=2 -> variance                     (spread)
# k=3 -> (standardized) skewness      (asymmetry)
# k=4 -> (standardized) kurtosis      (tail weight)
# "standardized" = divided by sd(x)^k, so the result doesn't depend on
# the units/scale of x -- that's what lets you compare skewness/kurtosis
# across variables measured in completely different units
# ====

set.seed(1)
sim_normal <- rnorm(10000, mean = 0, sd = 1)   # large n so the numbers converge close to theory
n <- length(sim_normal)

# show graphically
hist(sim_normal)
qqnorm(sim_normal); qqline(sim_normal) 

## k=1: MEAN
# ====
# the reference point everything below is measured AS A DEVIATION FROM
# -- not itself a "moment" in the central-moment sense
# ====
m <- mean(sim_normal)
m


## k=2: VARIANCE -- FORMULA VS SHORTCUT
# ====
# var() isn't magic -- it's the SAME formula, mean((x-m)^2), just with
# one twist: it divides by (n-1), not n (Bessel's correction), because
# we're ESTIMATING variance from a SAMPLE, not observing an entire
# population. dividing by n alone slightly underestimates it
# ====
v_formula   <- sum((sim_normal - m)^2) / n         # dividing by n (population variance)
v_corrected <- sum((sim_normal - m)^2) / (n - 1)   # same formula, dividing by (n-1) instead
v           <- var(sim_normal)                     # R's shortcut

v_formula; v_corrected; v

v_corrected == v

# v_corrected and v match almost exactly -- with n=10000 the gap between
# v_formula and v_corrected is tiny, but it's not zero, and it grows
# more noticeable on small samples

sdev_formula <- sqrt(v_corrected)

sdev <- sd(sim_normal)   # sqrt(var()) -- same (n-1) correction, same units as x

sdev_formula == sdev # check they are the same

## k=3: SKEWNESS
# ====
# no base R shortcut exists for this -- but it's just k=3 of the same
# formula, standardized by dividing by sd^3 so it doesn't depend on the
# units of x. a TRUE normal distribution has skewness = 0 (symmetric):
# positive = right-skewed (long tail to the right), negative = left-skewed
# ====
skewness <- mean( (sim_normal - m)^3 ) / sdev^3
skewness   # should land close to 0


## k=4: KURTOSIS
# ====
# same idea, k=4, standardized by sd^4. a TRUE normal distribution has
# kurtosis = 3 -- not "optimal", just the reference value for normal-
# shaped tails:
#   kurtosis = 3  -> mesokurtic  (normal-like tails)
#   kurtosis > 3  -> leptokurtic (heavier tails, more extreme outliers)
#   kurtosis < 3  -> platykurtic (lighter tails, fewer outliers)
# this is why people often report EXCESS kurtosis (kurtosis - 3) instead
# -- it reframes normal as 0, matching how skewness already works, so
# you don't have to remember "3 is the target" as a special case
# ====

kurtosis        <- mean((sim_normal - m)^4) / sdev^4

excess_kurtosis <- kurtosis - 3

kurtosis; excess_kurtosis   # kurtosis should land close to 3, excess close to 0


## SUMMARY
m; sdev; skewness; excess_kurtosis
# for a genuine normal distribution: mean ~ 0, skewness ~ 0, excess
# kurtosis ~ 0 -- three numbers that should all read close to zero once
# properly centered/standardized. keep these in mind as the "textbook"
# reference before we compute the same four numbers for the real
# height/weight/bmi data next




# ============================================================
# 1. LOAD DEPENDENCIES AND DATA
# ============================================================
source("scripts/00_setup.R")  

df_hwb_l <- read.csv(file.path(dir_dat, "height-weight-bmi-long.csv"), sep = ",", header = TRUE)
glimpse(df_hwb_l)




## THE SAME FOUR NUMBERS, ON THE REAL DATA
# ====
# same formulas as the theory section above, just grouped by sex and
# variable now that the data's in long format -- compare these against
# the simulated-normal benchmarks: skewness ~ 0, excess kurtosis ~ 0
# ====

df_hwb_l %>%
  group_by(sex, variable) %>%
  summarise(
    mean            = mean(value, na.rm = TRUE),
    sd              = sd(value, na.rm = TRUE),
    skewness        = mean((value - mean(value))^3, na.rm = TRUE) / sd(value, na.rm = TRUE)^3,
    excess_kurtosis = mean((value - mean(value))^4, na.rm = TRUE) / sd(value, na.rm = TRUE)^4 - 3,
    .groups = "drop"
  )


## FORMAL VERDICT: SHAPIRO-WILK
# ====
# eyeballing "close to 0" isn't a statistical conclusion -- shapiro.test()
# gives an actual p-value per group. H0 = "this looks like it came from a
# normal distribution"; a small p-value (< 0.05) rejects that
# ====

df_hwb_l %>%
  group_by(sex, variable) %>%
  summarise(
    p_value = shapiro.test(value)$p.value,
    .groups = "drop"
  )


# ====
# VERDICT
# weight: normal, both informally (moments near 0) and formally (p > .05)
# bmi:    genuinely non-normal -- largest moment deviations AND rejected
# height: formally rejected (p ~ 1e-6), but moments are essentially
#         indistinguishable from the simulated-normal benchmark -- this
#         is large-n oversensitivity, not a real departure from normal.
#         treated as practically normal for what follows
# ====




