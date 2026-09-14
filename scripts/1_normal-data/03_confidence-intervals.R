source("scripts/00_setup.R")

df_hwb <- read.csv(file.path(dir_dat, "height-weight-bmi.csv"), sep = ",", header = TRUE)


## OBJECTIVES: GOING BEYOND THE MEAN
# ====
# based on our exploratory analysis, we suspect there might be a sex
# difference in bmi -- and in business, that's often where the analysis
# stops: "group A averages 24.1, group B averages 23.4, so B is lower."
# job done.
#
# but a difference between two sample MEANS isn't automatically a real
# difference -- it could easily just be sampling noise 
# (Central Limit Theorem just showed sample means bounce around even when 
# drawn from the exact same population). 
# comparing raw means alone can't tell the two apart.
#
# to go beyond that, we'll build a CONFIDENCE INTERVAL for each group's
# mean and see whether they overlap -- a useful, intuitive first check,
# though not yet a formal answer (that's a proper test, coming in
# later). to build a confidence interval, we first
# need to understand what a Z-SCORE is -- that's where we start
# ====


## Z-SCORES
# ====
# a z-score tells you how many standard deviations a value is from the
# mean:
#   z = (x - mean) / sd
#
# this is the exact same idea used behind the scenes in the QQ-plot's
# x-axis earlier (01_moments-and-normality.R) -- there we compared
# ranks against EXPECTED z-scores for a normal distribution; here we
# compute an ACTUAL z-score for a real value
# ====

bmi_mean <- mean(df_hwb$bmi)
bmi_sd   <- sd(df_hwb$bmi)

# take one person's bmi and see how "unusual" it is
one_bmi <- df_hwb$bmi[1]
one_bmi

z <- (one_bmi - bmi_mean) / bmi_sd
z   # e.g. z = 2.05 means this person's bmi is 2.05 SDs above the average


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
# within 3 SDs -- here it's drawn for the STANDARD normal (mean=0, sd=1)
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
# values do (02_clt.R showed this directly: means_n30 was a much
# tighter spread than the raw bmi values).
#
# that tighter spread is called the STANDARD ERROR:
#   se = sd / sqrt(n)
# it's still "how much does this bounce around", just for a MEAN of n
# observations instead of a single observation
#
# a 95% confidence interval is then:
#   mean +/- z_95 * se
# ====

male_bmi   <- df_hwb$bmi[df_hwb$sex == "Male"]
female_bmi <- df_hwb$bmi[df_hwb$sex == "Female"]

ci_bounds <- function(x, z = z_95) {
  m  <- mean(x)
  se <- sd(x) / sqrt(length(x))
  c(mean = m, lower = m - z * se, upper = m + z * se)
}

ci_male   <- ci_bounds(male_bmi)
ci_female <- ci_bounds(female_bmi)

ci_male
ci_female


# SAME THING, WITH DPLYR (MUCH FASTER, NOW YOU SEE ADVANTAGES OF DPLYR)
# ====
# same formula, but now computed for both groups at once via group_by()
# -- useful once you want this for multiple groups (or variables)
# without repeating the by-hand version each time
# ====

bmi_ci <- df_hwb %>%
  group_by(sex) %>%
 # slice_sample(n = 10) %>% 
  summarise(
    mean  = mean(bmi),
    se    = sd(bmi) / sqrt(n()),
    lower = mean - z_95 * se,
    upper = mean + z_95 * se,
    .groups = "drop"
  )

bmi_ci

## COMPARING THE TWO GROUPS: POINT ESTIMATE + CI ("X-WING PLOT")
# ====
# a "point + whisker" plot: the dot is the mean, the whiskers are the
# 95% CI 
# ====

ggplot(bmi_ci, aes(x = sex, y = mean, color = sex)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Male" = "#2a78d6", "Female" = "#eb6834")) +
  labs(x = NULL, y = "bmi", title = "Mean bmi by sex, with 95% CI") +
  theme_minimal() +
  theme(legend.position = "none")


## HOW TO READ THE CI TABLE AND PLOT
# ====
# if one group's mean falls OUTSIDE the other group's CI, that's a
# strong hint the two means differ
#
# if the two CIs don't overlap AT ALL, it's close to certain the means
# differ -- but the reverse isn't true: some overlap does NOT guarantee
# the means are the same. the proper comparison uses the standard error
# of the DIFFERENCE between the two means, which is narrower than what
# eyeballing two separate CIs suggests -- that's exactly what a formal
# test gives us, coming up next.
# ====



### EXERCISE
# ====
# do the same CI comparison for all three variables at once (height,
# weight, bmi) -- one long table, one CI computation, one faceted plot,
# connected by %>% rather than repeating the bmi code three times
# ====



## WHAT DOES "95% CONFIDENCE" ACTUALLY MEAN?
# and FREQUENTIST stat in general
# ====
# it does NOT mean "95% chance the true mean is in this interval" -- the
# true population mean isn't random, it's a fixed (if unknown) number.
# what's random is the INTERVAL -- a new sample gives a new interval.
#
# once you've drawn your sample and computed actual numbers, say
# [22.1, 23.4], there's nothing left to be random -- the true mean is
# either in that range or it isn't, full stop, not "95% true." it's like
# a coin that's already been flipped and covered by your hand: there's
# no more "50% chance of heads" -- it already landed. you just don't
# know which. your UNCERTAINTY doesn't put probability back into
# something that's already fixed.
#
# this is really the core idea behind FREQUENTIST statistics (what
# we're doing in this whole course): probability only describes
# something that could vary across repeated trials -- NOT a single,
# already-realized event. that's why "95% confidence" describes the
# PROCEDURE, evaluated across many hypothetical repeats, not any one
# interval you've already computed. let's check that directly, reusing
# the resampling idea from the Central Limit Theorem script
# ====

set.seed(1)
true_mean   <- mean(df_hwb$bmi)   # here we KNOW the true mean, since we're treating the full dataset as the "population"

n_reps      <- 100   # how many "fresh samples" we'll simulate
sample_size <- 30    # size of each simulated sample

# draw n_reps samples, build a 95% CI for each one -- replicate() collects
# all the results; since ci_bounds() returns 3 numbers (mean/lower/upper),
# the result is a matrix, one COLUMN per rep
ci_reps <- replicate(n_reps, ci_bounds(sample(df_hwb$bmi, size = sample_size, replace = TRUE)))

# transpose so each rep is a ROW instead of a column, then convert to a
# normal data frame we can work with
df_coverage <- as.data.frame(t(ci_reps))

df_coverage$rep <- 1:n_reps   # just an index/ID for each simulated sample, for plotting

# does THIS interval happen to contain the true mean? TRUE/FALSE per rep
df_coverage$contains_true <- df_coverage$lower <= true_mean & df_coverage$upper >= true_mean

mean(df_coverage$contains_true)   # should land close to 0.95

ggplot(df_coverage, aes(x = rep, y = mean, color = contains_true)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept = true_mean, linetype = "dashed", color = "grey40") +
  coord_flip() +
  scale_color_manual(values = c("TRUE" = "#2a78d6", "FALSE" = "#e34948")) +
  labs(x = "sample #", y = "bmi", color = "contains true mean?",
       title = "100 resampled 95% CIs -- most contain the true mean, a few don't") +
  theme_minimal() +
  theme(legend.position = "bottom")


## COMPARING TO R'S BUILT-IN TOOLS
# ====
# in practice, you won't build CIs by hand -- t.test() (among others)
# does it for you. it's worth seeing that it gives (almost) the same
# answer as our formula:
# ====

ci_male     # our hand-built version
t.test(male_bmi)$conf.int   # R's built-in version

# they're close but not IDENTICAL -- t.test() uses the t-distribution
# rather than a fixed z = 1.96. the t-distribution has slightly heavier
# tails, to account for the extra uncertainty of also estimating sd from
# the sample -- it matters most for SMALL samples, and converges to the
# normal/z version as n grows (which is exactly why our large-n hand
# calculation nearly matches it here)



