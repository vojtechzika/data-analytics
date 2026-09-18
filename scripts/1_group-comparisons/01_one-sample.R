# ===================================================================
# 01_one-sample.R — the one-sample toolkit: z-score, CI by hand,
# bootstrapped CI, one-sample t-test, one-sample Wilcoxon
# ===================================================================

source("scripts/00_setup.R")

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)
glimpse(df_cap)

# working vector for this whole script -- cap_eur has a handful of NAs,
# so we strip them once here rather than re-filtering in every section
cap_eur_vals <- df_cap$cap_eur[!is.na(df_cap$cap_eur)]
length(cap_eur_vals)


## Z-SCORES
# ====
# a z-score tells you how many standard deviations a value is from the
# mean:
#   z = (x - mean) / sd
#
# this is the exact same idea used behind the scenes in a QQ-plot's
# x-axis (05_assessing-normality.R) -- there we compared ranks against
# EXPECTED z-scores for a normal distribution; here we compute an
# ACTUAL z-score for one real cappuccino price
# ====

cap_mean <- mean(cap_eur_vals)
cap_sd   <- sd(cap_eur_vals)

# take one city's cappuccino price and see how "unusual" it is
one_cap <- cap_eur_vals[100]
one_cap

z <- (one_cap - cap_mean) / cap_sd
z   # e.g. z = 1.4 means this city's cappuccino is 1.4 SDs pricier than average


## WHY Z-SCORES (AND THE DISTRIBUTION) MATTER FOR CONFIDENCE INTERVALS
# ====
# for a normal distribution, we know EXACTLY what share of values falls
# within any given number of SDs of the mean. R can compute this
# directly with pnorm() -- the proportion of a standard normal
# distribution BELOW a given z:
# ====

pnorm(1.96) - pnorm(-1.96)   # proportion of values within +/- 1.96 SDs of the mean

# ~0.95 -- about 95% of a normal distribution falls within +/- 1.96 SDs.
# that 1.96 is exactly where the "95%" in a 95% confidence interval
# comes from

z_95 <- 1.96

## THE NUMBER TO REMEMBER: 1.96
# ====
# in practice, "how extreme is too extreme" almost always gets anchored
# to roughly 2 SDs from the mean -- that's the everyday shorthand behind
# words like "unusual" or "an outlier" in statistics. the PRECISE cutoff
# for the middle 95% is z = 1.96 (not exactly 2 -- 2 SDs is actually
# closer to 95.4%, the rounded version of the rule) -- but 1.96 is the
# one value worth memorizing, because it's what almost every 95%
# confidence interval in this course will be built on
# ====


## VISUALIZING THE NORMAL CURVE + THE EMPIRICAL RULE (WITH COLOR)
# ====
# the classic "68-95-99.7 rule": for ANY normal distribution, about 68%
# of values fall within 1 SD of the mean, ~95% within 2 SDs, ~99.7%
# within 3 SDs -- here it's drawn for the STANDARD normal (mean=0, sd=1),
# not for cap_eur specifically -- the rule describes the SHAPE, and
# applies to any variable once you've converted it to z-scores
# ====

x <- seq(-4, 4, length.out = 1000)
df_norm <- data.frame(x = x, y = dnorm(x))
df_norm$band <- cut(abs(df_norm$x), breaks = c(0, 1, 2, 3, Inf),
                    labels = c("within 1 SD", "within 2 SD", "within 3 SD", "beyond 3 SD"),
                    include.lowest = TRUE)
df_norm$side <- sign(df_norm$x)   # needed so left/right tails don't get connected into one shape

band_colors <- c("within 1 SD" = "#256abf",
                 "within 2 SD" = "#6da7ec",
                 "within 3 SD" = "#b7d3f6",
                 "beyond 3 SD" = "#e1e0d9")

ggplot(df_norm, aes(x, y)) +
  geom_area(aes(fill = band, group = interaction(band, side)), position = "identity") +
  geom_vline(xintercept = c(-3, -2, -1, 1, 2, 3), linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = band_colors) +
  annotate("text", x = 0,   y = 0.42, label = "68%") +
  annotate("text", x = 2.5, y = 0.08, label = "95%") +
  annotate("text", x = 3.6, y = 0.02, label = "99.7%") +
  labs(x = "z-score", y = "density", fill = "Region",
       title = "The standard normal distribution") +
  theme_minimal() +
  theme(legend.position = "bottom")


## BUILDING A CONFIDENCE INTERVAL, BY HAND
# ====
# one important shift from the z-score section above: there, z measured
# how far a single VALUE was from the mean, using sd. here, we're
# measuring how far a sample MEAN could plausibly be from the true
# population mean -- and sample means don't vary as much as individual
# values do (06_central-limit-theorem.R showed this directly: bootstrapped
# means cluster much tighter than the raw values).
#
# that tighter spread is called the STANDARD ERROR:
#   se = sd / sqrt(n)
# it's still "how much does this bounce around", just for a MEAN of n
# observations instead of a single observation
#
# a 95% confidence interval is then:
#   mean +/- z_95 * se
#
# THIS SHIFT ALSO CHANGES WHETHER NORMALITY MATTERS. an individual
# z-score's "usual" reading -- z=2 means roughly the 97.5th percentile,
# via pnorm() -- genuinely assumes the raw data IS normal; cap_eur itself
# isn't (05_assessing-normality.R: Shapiro rejects it clearly). but the CI
# here isn't about a single value's percentile -- it's about the MEAN's
# sampling distribution, and THAT'S approximately normal for large n
# regardless of what the raw data looks like (the CLT argument from 06;
# with n=2501 cities, CLT has already kicked in for the mean, even though
# it hasn't -- and doesn't need to -- for individual cappuccino prices).
#
# this isn't unconditional, though: it depends on n being large enough.
# for small samples of non-normal data, the sampling distribution of the
# mean might not be normal enough yet, and a z- (or even t-) based CI can
# be unreliable -- exactly the case the BOOTSTRAPPED interval below, and
# the Wilcoxon test at the end of this script, are built for
# ====

ci_bounds <- function(x, z = z_95) {
  m  <- mean(x)
  se <- sd(x) / sqrt(length(x))
  c(mean = m, lower = m - z * se, upper = m + z * se)
}

ci_bounds(cap_eur_vals)   # the whole sample, one CI


## COMPARING TO A FIXED HYPOTHESIS: IS THE AVERAGE CAPPUCCINO €3.50?
# ====
# a "point + whisker" plot: the dot is the mean, the whiskers are the
# 95% CI. suppose someone claims "the average cappuccino in this dataset
# costs €3.50" -- does our data support or refute that? here the
# comparison is against a fixed HYPOTHESIZED value rather than another
# group -- the simplest version of "does the data rule this value out?"
# ====

hypothesized_cap <- 3.50

cap_ci_overall <- as.data.frame(t(ci_bounds(cap_eur_vals)))
cap_ci_overall

ggplot(cap_ci_overall, aes(x = "cap_eur", y = mean)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05, linewidth = 1, color = "#2a78d6") +
  geom_point(size = 2, color = "#2a78d6") +
  geom_hline(yintercept = hypothesized_cap, linetype = "dashed", color = "#e34948") +
  labs(x = NULL, y = "cap_eur", title = "Mean cappuccino price (95% CI) vs. hypothesized €3.50") +
  theme_minimal()

# if the dashed line (3.50) falls OUTSIDE the whisker (the CI), that's a
# strong hint the TRUE population mean isn't 3.50. if it falls INSIDE, the
# data doesn't rule out 3.50 as a plausible mean -- and, as the formal
# test below will confirm, that's not just a preview of a "real" answer,
# it already IS one


## A BOOTSTRAPPED INTERVAL, FOR WHEN NORMALITY IS IN DOUBT
# ====
# the CI above leaned on the CLT: at n=2501, the MEAN's sampling
# distribution is close enough to normal for the z-based formula to be
# trustworthy, even though the raw cap_eur values aren't normal
# themselves. bootstrapping (07_bootstrapping.R) sidesteps that
# assumption entirely -- it builds an approximate sampling distribution
# of any statistic directly from the data, by resampling with
# replacement, rather than assuming its shape. written generally here
# (stat = mean by default) so the exact same function also gives us a
# bootstrap interval for the MEDIAN later in this script, not just the
# mean
# ====

n_boot <- 2000

bootstrap_stat <- function(x, stat = mean, seed = 123) {
  x <- x[!is.na(x)]
  set.seed(seed)
  replicate(n_boot, stat(sample(x, length(x), replace = TRUE)))
}

cap_boot_means <- bootstrap_stat(cap_eur_vals)   # stat = mean by default

# a 95% bootstrap PERCENTILE interval: just the 2.5th and 97.5th
# percentiles of the resampled means themselves -- no formula, no
# normality assumption anywhere in this calculation
cap_boot_ci <- quantile(cap_boot_means, c(0.025, 0.975))
cap_boot_ci

ci_bounds(cap_eur_vals)   # compare: the hand-built z-based interval
cap_boot_ci                # against: the bootstrapped interval
# expect these to be close -- that closeness IS the empirical confirmation
# that the CLT has already rescued this variable at this sample size


## THE FORMAL VERSION: ONE-SAMPLE T-TEST
# ====
# t.test() automates everything above (mean, SE, interval, AND a formal
# p-value against the hypothesized value) in one call. it uses the
# t-distribution rather than a fixed z = 1.96 -- slightly wider, to
# account for also estimating sd from the sample -- but converges to the
# z-based version as n grows, which is why it should closely match our
# by-hand interval here
# ====

t.test(cap_eur_vals, mu = hypothesized_cap)

# read the output: "95 percent confidence interval" should closely match
# ci_bounds(cap_eur_vals) above, and the p-value answers the same
# question the plot did -- is 3.50 a plausible value for the true mean?
# p < 0.05 means the data is not consistent with a true mean of 3.50
# (matches "3.50 falls outside the CI" from the plot); p > 0.05 means
# the data can't rule 3.50 out


## THE NON-PARAMETRIC VERSION: ONE-SAMPLE WILCOXON
# ====
# wilcox.test() is the rank-based analogue of the one-sample t-test --
# and it's worth being precise about what it actually tests, because it's
# NOT the same target as the t-test above:
#
#   t.test()      tests the MEAN against hypothesized_cap
#   wilcox.test() tests the MEDIAN (technically the "pseudomedian", a
#                 rank-based stand-in for it) against hypothesized_cap
#
# for a symmetric variable those two targets are close together, but
# cap_eur is right-skewed -- so its mean and median are genuinely
# different numbers, and the two tests are quietly answering two
# different questions even when run against the same hypothesized value
# ====

wilcox.test(cap_eur_vals, mu = hypothesized_cap)

median(cap_eur_vals)   # compare this to cap_mean above -- for a skewed
# variable like cap_eur, expect these to differ

# same reading as the t-test: p < 0.05 means the data is not consistent
# with a true median of 3.50; p > 0.05 means it can't rule that out --
# just remember it's making that statement about the median, not the mean


## A CONFIDENCE INTERVAL FROM THE WILCOXON TEST -- BUT READ THE FINE PRINT
# ====
# wilcox.test() can also return an interval directly, with conf.int =
# TRUE. worth being precise about what it's an interval FOR, though:
# it's NOT a confidence interval for median(x) itself -- it's for the
# HODGES-LEHMANN ESTIMATOR, the median of all pairwise averages of the
# data ("Walsh averages"). that's a location estimate closely related to
# the median, and it's what the rank-sum math behind the test naturally
# produces an interval for -- but it's not numerically identical to
# median(x), so don't report it as "the CI for the median" without that
# caveat
# ====

wilcox.test(cap_eur_vals, mu = hypothesized_cap, conf.int = TRUE)

# compare the "95 percent confidence interval" in that output to:
median(cap_eur_vals)
# the Hodges-Lehmann estimate ("difference in location" in the output)
# should sit close to the median, but don't expect it to match exactly


## A BOOTSTRAPPED INTERVAL FOR THE MEDIAN ITSELF
# ====
# for an interval around the ACTUAL median (rather than the
# Hodges-Lehmann stand-in above), reuse bootstrap_stat() with
# stat = median instead of the mean default -- same resampling logic,
# different statistic recomputed on each resample
# ====

cap_boot_medians   <- bootstrap_stat(cap_eur_vals, stat = median)
cap_boot_median_ci <- quantile(cap_boot_medians, c(0.025, 0.975))

median(cap_eur_vals)   # point estimate
cap_boot_median_ci      # bootstrap 95% interval around it

# compare this to the Wilcoxon interval just above -- similar in spirit
# (both assumption-free, both about a "typical value" rather than the
# mean), built two different ways, and technically targeting two
# different quantities (the median itself here, vs. the
# Hodges-Lehmann estimator there)


## RECAP: SEVEN TOOLS, THE SAME UNDERLYING QUESTION
# ====
# every section above answered some version of "is a single number a
# plausible summary of this one sample?" -- but with different targets
# and different assumptions:
#
#   z-score                how unusual is ONE observation          assumes raw data is normal
#   CI by hand (z-based)   plausible values for the MEAN           assumes CLT has kicked in
#   bootstrapped CI        same target as above                    assumption-free
#   one-sample t-test      formal version of the CI, for the MEAN     assumes CLT has kicked in
#   one-sample Wilcoxon    formal test, for the MEDIAN             assumption-free
#   Wilcoxon CI            interval for the Hodges-Lehmann location   assumption-free
#   bootstrapped median CI interval for the MEDIAN itself          assumption-free
#
# for cap_eur specifically, CLT rescues the mean-based tools even though
# the raw data itself isn't normal -- so all of these should tell a
# broadly consistent story here. that won't always be true: variables
# like cap_index broken down by urban_class, later in this course, are
# exactly where the mean- and median-based answers genuinely diverge
# ====
