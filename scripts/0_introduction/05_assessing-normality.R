# ===================================================================
# 05_assessing-normality.R — visual and quantitative checks for
# whether a variable is "normal enough" to justify mean-based
# (parametric) methods later in the course
# ===================================================================

# ============================================================
# 1. LOAD DEPENDENCIES AND DATA
# ============================================================
source("scripts/00_setup.R")

# wide format -- one row per person, needed for the per-variable QQ-plots below
df_hwb <- read.csv(file.path(dir_dat, "height-weight-bmi.csv"), sep = ",", header = TRUE)

# long format (built in 04_exploration.R) -- one row per person PER variable,
# needed for the grouped moments/Shapiro-Wilk checks further down
df_hwb_long <- read.csv(file.path(dir_dat, "height-weight-bmi-long.csv"), sep = ",", header = TRUE)

glimpse(df_hwb)
glimpse(df_hwb_long)


# ============================================================
# 2. VISUAL CHECK: QQ-PLOTS
# ============================================================
# ====
# a QQ-plot ("quantile-quantile") checks normality by comparing your
# data against what a perfectly normal distribution would look like
#
# how it's built: values are first sorted from lowest to highest, so
# each one gets a rank (1st smallest, 2nd smallest, ..., largest). Each
# ranked value is then plotted against the value we'd EXPECT at that
# exact rank if the data were perfectly normal
#
# x-axis = expected value for that rank, under a perfect normal curve
#          (theoretical -- always centered on 0)
# y-axis = the actual sorted data value at that rank
#
# if the data really is normal, actual values track their expected
# rank closely, so points fall on the diagonal line (qqline()).
# systematic deviations -- curving away at the ends, S-shapes, etc. --
# reveal HOW the data differs from normal (e.g. heavy tails, skew)
# ====

## 2a. how it should look: simulated NORMAL data
# ====
# set.seed() fixes R's random number generator to a specific starting
# point -- R's "random" numbers are actually PSEUDO-random (a fixed,
# deterministic sequence that just looks random), so the same seed
# always produces the exact same draws, on any machine. without this,
# rnorm() below would give a different sample every run, and nobody
# could reproduce anybody else's numbers. the seed value itself (1) is
# arbitrary -- what matters is fixing SOME value, not which one
#
# it gets called again before each later simulation (2b, 3, ...) rather
# than just once at the top, so each block stays reproducible on its
# own, no matter what code runs before it
# ====
set.seed(1)

# normal data are defined by its first two moments -- mean and standard
# deviation, more on this in section 3 below
sim_normal <- rnorm(500, mean = 0, sd = 1)

hist(sim_normal)
qqnorm(sim_normal); qqline(sim_normal)     # points hug the line closely -- this is the reference case


## 2b. how it should NOT look: simulated LOG-NORMAL data
# ====
# common for things like income, prices, reaction times -- this is what a
# CLEARLY non-normal histogram/QQ-plot looks like, to calibrate your eye
# before we look at the more ambiguous real data below
# ====
set.seed(1)
sim_lognormal <- rlnorm(500, meanlog = 0, sdlog = 1)

hist(sim_lognormal)                          # strong right skew
qqnorm(sim_lognormal); qqline(sim_lognormal) # clear upward curve -- deviates from the line


## 2c. now the real data, pooled across sex
qqnorm(df_hwb$height); qqline(df_hwb$height)  # curves off -- same sex-mixture issue as the histograms in 04_exploration.R
qqnorm(df_hwb$weight); qqline(df_hwb$weight)  # same story
qqnorm(df_hwb$bmi);    qqline(df_hwb$bmi)     # same story


## 2d. same check, but within each sex group
# ====
# height/weight/bmi were pooled across sex above -- like the histograms in
# 04_exploration.R, check whether they look normal WITHIN each sex group
# instead. same idea as plot_by_sex() there: .data[[var]] looks up a column
# by name, and facet_wrap() reuses the layering system from 02_visualizations.R
# ====

qq_by_sex <- function(var) {
  ggplot(df_hwb, aes(sample = .data[[var]])) +
    stat_qq() + stat_qq_line() +
    facet_wrap(~ sex)
}

qq_by_sex("height")
qq_by_sex("weight")
qq_by_sex("bmi")

# ====
# pooled height, weight and bmi looked off (both the histograms in
# 04_exploration.R and the QQ-plots above show the sex-mixture pattern) --
# but split by sex, all three variables look reasonably normal. visual
# inspection is a good start, but "looks normal" isn't a number -- section 4
# below checks the same thing quantitatively, via moments and a formal test
# ====


# ============================================================
# 3. THE MOMENTS OF A NORMAL DISTRIBUTION (THEORY, ON SIMULATED DATA)
# ============================================================
# ====
# before checking whether OUR data is normal, let's see what a family of
# summary numbers called MOMENTS look like for data we KNOW is normal
# (simulated): mean, variance, skewness, kurtosis.
#
# all four are the SAME formula, just with a different power k:
#   mean( (x - mean(x))^k )
# k=1 -> the mean itself       k=2 -> variance
# k=3 -> skewness (asymmetry)  k=4 -> kurtosis (tail weight)
# ====

set.seed(1)
# a larger n than section 2's sim_normal -- so the moments below converge
# close to their theoretical values instead of just looking visually normal
sim_normal_large <- rnorm(10000, mean = 0, sd = 1)

hist(sim_normal_large)
qqnorm(sim_normal_large); qqline(sim_normal_large)


## HISTOGRAM WITH A DENSITY CURVE
# ====
# a histogram bins the data and counts how many values fall in each bin
# -- by default the y-axis is just these raw counts
#
# a density curve is a SMOOTHED estimate of the underlying distribution
# shape -- instead of discrete bins, it estimates how likely any given
# x-value is, and the curve is scaled so the total area underneath = 1
#
# because of that "area = 1" rule, density is not on the same scale as
# raw counts -- that's why we rescale the histogram with
# aes(y = after_stat(density)) before overlaying the two: it converts
# counts into the same "area = 1" scale so the curve actually matches
# the shape of the bars, rather than being invisible at the bottom
# ====
ggplot(data.frame(x = sim_normal_large), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), fill = "gray") +
  geom_density()


## ONE FORMULA, FOUR MOMENTS
# ====
# don't take "same formula" on faith -- write it ONCE, as a function,
# and call it four times. every number below comes out of the SAME
# central_moment() call, just with a different k each time:
# ====

central_moment <- function(x, k) mean((x - mean(x))^k)

moments <- data.frame(k = 1:4) %>%
  rowwise() %>%   # rowwise() -- from dplyr -- makes mutate() treat each ROW as
  # its own group, so central_moment() runs once PER ROW with
  # that row's own k, instead of getting the whole k column
  # (1,2,3,4) at once. that matters here because central_moment()
  # isn't built to accept a vector of k's -- without rowwise(),
  # mutate() would silently RECYCLE 1,2,3,4 across all 10000
  # values of sim_normal_large instead of computing 4 separate
  # results, giving a wrong answer with no error or warning
  mutate(moment = round(central_moment(sim_normal_large, k), 4))
# round(..., 4) is purely for DISPLAY: the k=1 result is a
# floating-point residual like -1.4e-16 (see the
# x <- c(2,4,6,8,10) example below for why), and tibbles
# print numbers that small in scientific notation --
# rounding turns that into a clean 0.0000 instead

moments
#   k=1   k=2   k=3   k=4
#   ~0    ~1    ~0    ~3
# same function, called four times via rowwise(), four different k --
# THAT's the whole trick. everything below is just explaining what each
# of these four numbers MEANS, and one adjustment ("standardizing") that
# two of them need and two of them don't


## k=1: MEAN
# ====
# central_moment(x, 1) is always ~0, by construction -- deviations from
# the mean average out to zero. that's WHY the mean isn't itself called
# "a moment" in this sense: it's the reference point everything else is
# measured AS A DEVIATION FROM, not a deviation itself
# ====
m <- mean(sim_normal_large)
m

# see for yourself, on a tiny example:
x <- c(2, 4, 6, 8, 10)
x - mean(x)          # the individual deviations: -4 -2 0 2 4
mean(x - mean(x))    # their mean: 0 (or something like -1e-16 due to floating point rounding)


## k=2: VARIANCE -- WHY IT ISN'T STANDARDIZED
# ====
# central_moment(x, 2) IS the variance -- used on its own scale, no
# further adjustment needed. contrast with k=3/k=4 below, which DO get
# divided by a power of sd ("standardized").
#
# standardizing k=2 the same way would tell you nothing: dividing the
# second moment by sd(x)^2 always gives variance / variance = 1, no
# matter what the data looks like. see for yourself:
# ====
central_moment(sim_normal_large, 2) / sd(sim_normal_large)^2   # ~1, always -- carries no information

# that's exactly why variance is reported raw, and exactly why skewness
# and kurtosis (below) aren't -- they don't have this problem, so
# standardizing them actually buys you something: comparability

v   <- central_moment(sim_normal_large, 2)   # our formula
v_r <- var(sim_normal_large)                 # R's shortcut

v; v_r   # close, not identical

# ====
# for the curious: why "close, not identical"? central_moment() divides
# by n (the number of observations) implicitly, through mean(). var()
# instead divides by (n-1) -- Bessel's correction -- because it's
# ESTIMATING variance from a SAMPLE rather than observing an entire
# population: dividing by n alone would slightly UNDERESTIMATE the true
# variance, so (n-1) corrects for that. the gap shrinks as n grows, and
# is already tiny here (n = 10000)
# ====

# ====
# sdev matters for what comes next: sd() is the SQUARE ROOT of variance,
# which puts it back in the ORIGINAL units of x (variance itself is in
# units^2 -- e.g. cm^2, meaningless on its own). that's exactly why
# skewness/kurtosis divide by sd^3 / sd^4 below: sd^3 is in the same
# units as (x - mean(x))^3, so the division cancels the units out
# entirely, leaving a plain, comparable number -- the same idea as the
# "standardizing k=2 does nothing" check above, just applied where it
# actually changes something
# ====
sdev_var <- sqrt(v_r)          # sd, built from variance by hand
sdev     <- sd(sim_normal_large)   # R's shortcut

sdev_var; sdev   # identical -- sd() IS just sqrt(variance), nothing more


## k=3: SKEWNESS -- WHY IT IS STANDARDIZED
# ====
# central_moment(x, 3) alone would be in the units of x, CUBED -- not
# something you can compare across variables measured in different
# units (cm vs kg vs an index, say). dividing by sd(x)^3 removes the
# units, giving a scale-free number: a TRUE normal distribution has
# standardized skewness = 0 (symmetric); positive = right-skewed (long
# tail to the right), negative = left-skewed
#
# symmetric data like sim_normal_large can't actually show why this
# division matters -- raw or standardized, its third moment is ~0
# either way. sim_lognormal (from section 2b) is skewed, so it's the
# one that makes the point: rescale it, as if you'd measured it in
# different units, and watch what happens to each version
# ====

skewness <- central_moment(sim_normal_large, 3) / sdev^3
skewness   # should land close to 0

raw_skew_original <- central_moment(sim_lognormal, 3)
std_skew_original <- central_moment(sim_lognormal, 3) / sd(sim_lognormal)^3

sim_lognormal_rescaled <- sim_lognormal * 1000   # pretend we'd measured in different units
raw_skew_rescaled <- central_moment(sim_lognormal_rescaled, 3)
std_skew_rescaled <- central_moment(sim_lognormal_rescaled, 3) / sd(sim_lognormal_rescaled)^3

raw_skew_original; raw_skew_rescaled   # wildly different -- the raw moment inherited the units
std_skew_original; std_skew_rescaled   # identical (up to floating-point rounding) -- standardizing removed them

# THIS is what dividing by sd^k buys you: it's what lets you say
# "skewness of 6" and have that mean the same thing regardless of
# whether x was measured in dollars, cents, or anything else


## k=4: KURTOSIS
# ====
# same logic as k=3: central_moment(x, 4) gets divided by sd^4, for the
# same scale-free reason. a TRUE normal distribution has STANDARDIZED
# kurtosis = 3 -- not "optimal", just the reference value for normal-
# shaped tails:
#   kurtosis = 3  -> mesokurtic  (normal-like tails)
#   kurtosis > 3  -> leptokurtic (heavier tails, more extreme outliers)
#   kurtosis < 3  -> platykurtic (lighter tails, fewer outliers)
# this is why people often report EXCESS kurtosis (kurtosis - 3) instead
# -- it reframes normal as 0, matching how skewness already works, so
# you don't have to remember "3 is the target" as a special case
# ====

kurtosis        <- central_moment(sim_normal_large, 4) / sdev^4
excess_kurtosis <- kurtosis - 3

kurtosis; excess_kurtosis   # kurtosis should land close to 3, excess close to 0


## SUMMARY
m; sdev; skewness; excess_kurtosis
# for a genuine normal distribution: mean ~ 0, skewness ~ 0, excess
# kurtosis ~ 0 -- three numbers that should all read close to zero once
# properly standardized (variance is the odd one out: it's meaningful on
# its OWN scale, which is exactly why section k=2 above showed
# standardizing it is pointless). keep these in mind as the "textbook"
# reference before we compute the same four numbers for the real
# height/weight/bmi data next

# ==== DON'T WANT TO DERIVE SKEWNESS/KURTOSIS BY HAND EVERY TIME?
# two ways to avoid retyping the formula in every script:
#
# (a) YOUR OWN FUNCTION (recommended) -- wrap the "when do we
# standardize" rule from k=2/k=3/k=4 above into ONE function: give it
# the data and which moment you want, it returns the right thing
# already, standardized or not:
#
#   moment <- function(x, k) {
#     n <- length(x)
#     m <- mean((x - mean(x))^k)
#     if (k == 2) m <- m * n / (n - 1)   # match var()'s (n-1) convention
#     if (k >= 3) m <- m / sd(x)^k       # standardize only from k=3 onward
#     m
#   }
#
# put this in your 00_setup.R and every script gets, for free:
#   moment(x, 1)   # mean (~0)
#   moment(x, 2)   # variance
#   moment(x, 3)   # skewness
#   moment(x, 4)   # kurtosis -- RAW, not excess: moment(x, 4) - 3 for that
#
# (b) THE {moments} PACKAGE (install.packages("moments")) -- provides
# skewness() and kurtosis() ready-made:
#   moments::skewness(sim_normal_large)   # matches our skewness
#   moments::kurtosis(sim_normal_large)   # matches our RAW kurtosis (~3) --
#                                          # NOT excess_kurtosis, still
#                                          # subtract 3 yourself for that
#
# IMPORTANT!!!
# The :: reaches directly into a package's own namespace, bypassing the
# normal search path (and .GlobalEnv) entirely -- so moments::skewness()
# works no matter what "skewness" means locally. it's most useful when
# two attached packages export a function with the same name and you
# need to say which one you mean (e.g. dplyr::filter() vs stats::filter())
#
# for something this small -- one line of arithmetic -- writing your
# own is usually the better call: no dependency to install and version,
# no guessing which convention a package uses (raw vs. excess kurtosis,
# as above), and you can see exactly what it computes. save packages for
# methods that are genuinely nontrivial to implement correctly yourself
# ====

# ============================================================
# 4. THE SAME FOUR NUMBERS, ON THE REAL DATA
# ============================================================
# ====
# same formulas as the theory section above, just grouped by sex and
# variable now that the data's in long format -- compare these against
# the simulated-normal benchmarks: skewness ~ 0, excess kurtosis ~ 0
#
# NOTE: mean and sd are computed ONCE per group and then reused inside
# the skewness/kurtosis formulas (dplyr lets later summarise() columns
# reference earlier ones) -- this avoids recomputing them twice AND a
# subtle bug: mean(value) without na.rm = TRUE would return NA for any
# group containing a missing value, silently turning that group's
# skewness/kurtosis into NaN
# ====

moments_by_group <- df_hwb_long %>%
  group_by(sex, variable) %>%
  summarise(
    mean            = mean(value, na.rm = TRUE),
    sd              = sd(value, na.rm = TRUE),
    skewness        = mean((value - mean)^3, na.rm = TRUE) / sd^3,
    excess_kurtosis = mean((value - mean)^4, na.rm = TRUE) / sd^4 - 3,
    .groups = "drop"
  )

moments_by_group

# save as a diagnostic, same idea as imperial-metric-cor.csv in 03_processing.R
write.csv(moments_by_group, file.path(dir_dia, "moments-by-sex.csv"), row.names = FALSE)


# ============================================================
# 5. FORMAL VERDICT: SHAPIRO-WILK
# ============================================================
# ====
# eyeballing "close to 0" isn't a statistical conclusion -- shapiro.test()
# gives an actual p-value per group. H0 = "this looks like it came from a
# normal distribution"; a small p-value (< 0.05) rejects that
#
# two things shapiro.test() is fussy about:
# - it errors on any NA, so wrap the input in na.omit()
# - it only accepts samples between 3 and 5000 observations -- a group
#   bigger than that needs to be down-sampled first (e.g. slice_sample())
#   or you skip the test and lean on the CLT argument from
#   06_central-limit-theorem.R instead
# ====

normality_pvalues <- df_hwb_long %>%
  group_by(sex, variable) %>%
  summarise(
    n       = sum(!is.na(value)),
    p_value = shapiro.test(na.omit(value))$p.value,
    .groups = "drop"
  )

normality_pvalues

write.csv(normality_pvalues, file.path(dir_dia, "shapiro-by-sex.csv"), row.names = FALSE)


# ============================================================
# 6. VERDICT
# ============================================================
# weight: normal, both informally (moments near 0) and formally (p > .05)
# bmi:    genuinely non-normal -- largest moment deviations AND rejected
# height: formally rejected (p ~ 1e-6), but moments are essentially
#         indistinguishable from the simulated-normal benchmark -- this
#         is large-n oversensitivity, not a real departure from normal.
#         treated as practically normal for what follows
#
# next: 06_central-limit-theorem.R asks a related but different question --
# even when a variable itself isn't perfectly normal, is the MEAN of a
# sample from it normal enough to use anyway?