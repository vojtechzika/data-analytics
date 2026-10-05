# ===================================================================
# 02_two-sample.R — comparing two independent groups: CIs for each
# group, CI for the DIFFERENCE, two-tailed Welch t-test, then the
# one-tailed version for a directional question, Wilcoxon rank-sum,
# effect sizes, bootstrapped difference, and (at the end) the PAIRED
# versions of both tests on a simulated price increase
# question: are URBAN cappuccino prices different from, and then HIGHER
# than, RURAL ones?
# ===================================================================

source("scripts/00_setup.R")


df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

# keep only the two groups we are comparing (drops "Suburban") and strip NAs.
# the factor LEVEL ORDER MATTERS here: R treats the first level as "group 1",
# and every directional test below is "group 1 vs group 2". we want
# "Urban > Rural", so Urban must come first (default is alphabetical,
# which would put Rural first and silently flip the direction of the test!)
df_two <- df_cap %>%
  filter(urban_class %in% c("Urban", "Rural"), !is.na(cap_eur)) %>%
  mutate(urban_class = factor(urban_class, levels = c("Urban", "Rural")))

df_two %>% count(urban_class)   # group sizes -- two-sample tests do NOT need equal n

cap_urban <- df_two$cap_eur[df_two$urban_class == "Urban"]
cap_rural <- df_two$cap_eur[df_two$urban_class == "Rural"]

n_boot <- 2000   # same as in the one-sample script


## STEP 0: LOOK AT THE DATA FIRST
# ====
# before any test, look at both distributions side by side. this is where
# you spot skew, outliers and very different spreads -- which decide
# whether mean-based (t) or rank-based (Wilcoxon) tools are more sensible
# ====

ggplot(df_two, aes(x = urban_class, y = cap_eur, fill = urban_class)) +
  geom_boxplot(alpha = 0.6, show.legend = FALSE) +
  labs(x = NULL, y = "cap_eur", title = "Cappuccino price by urban class") +
  theme_minimal()

df_two %>%
  group_by(urban_class) %>%
  summarise(n = n(), mean = mean(cap_eur), median = median(cap_eur), sd = sd(cap_eur))


## CONFIDENCE INTERVAL FOR EACH GROUP
# ====
# same z-based recipe as in the one-sample script (mean +/- 1.96 * se),
# just computed separately inside each group via group_by() + summarise()
# ====

cap_ci_groups <- df_two %>%
  group_by(urban_class) %>%
  summarise(n_obs = n(),
            m     = mean(cap_eur),
            se    = sd(cap_eur) / sqrt(n_obs),
            lower = m - 1.96 * se,
            upper = m + 1.96 * se)
cap_ci_groups

ggplot(cap_ci_groups, aes(x = urban_class, y = m)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05, linewidth = 1, color = "#2a78d6") +
  geom_point(size = 2, color = "#2a78d6") +
  labs(x = NULL, y = "cap_eur", title = "Mean cappuccino price (95% CI) by urban class") +
  theme_minimal()

# HOW TO READ TWO X-WINGS, AND THE TRAP IN DOING SO
# ====
# - whiskers that DON'T overlap  -> the groups are clearly different
# - whiskers that DO overlap     -> inconclusive, NOT automatically
#   "no difference". the uncertainty of a DIFFERENCE is
#   sqrt(se1^2 + se2^2), which is smaller than se1 + se2 (what you
#   implicitly add up when you compare whiskers by eye). so two intervals
#   can overlap a little and the difference can still be significant.
#   the reliable answer comes from the CI OF THE DIFFERENCE itself:
# ====


## CONFIDENCE INTERVAL FOR THE DIFFERENCE, BY HAND
# ====
# the quantity of interest is now ONE number: mean(Urban) - mean(Rural).
#   se_diff = sqrt(se_urban^2 + se_rural^2)
#   CI      = diff +/- z * se_diff
# if the interval excludes 0, "no difference" is not a plausible value.
# (this unpooled se is exactly what the Welch test below uses)
# ====

m_urban  <- cap_ci_groups$m[cap_ci_groups$urban_class == "Urban"]
m_rural  <- cap_ci_groups$m[cap_ci_groups$urban_class == "Rural"]
se_urban <- cap_ci_groups$se[cap_ci_groups$urban_class == "Urban"]
se_rural <- cap_ci_groups$se[cap_ci_groups$urban_class == "Rural"]

diff_means <- m_urban - m_rural
se_diff    <- sqrt(se_urban^2 + se_rural^2)

c(diff  = diff_means,
  lower = diff_means - 1.96 * se_diff,
  upper = diff_means + 1.96 * se_diff)   # two-sided 95% CI


## THE FIRST QUESTION: IS THERE A DIFFERENCE AT ALL? (TWO-TAILED)
# ====
#   H0: mean(Urban) =  mean(Rural)    (no difference)
#   H1: mean(Urban) != mean(Rural)    (a difference, in EITHER direction)
#
# this is the default setup of every test in R: a two-tailed test splits
# its 5% error budget between both tails, because it is prepared to be
# surprised in either direction
# ====


## THE PARAMETRIC TEST: TWO-SAMPLE (WELCH) T-TEST
# ====
# base R's t.test() uses the Welch version by default: it does NOT assume
# equal variances in the two groups (var.equal = FALSE), and adjusts the
# degrees of freedom accordingly. that's the safe default -- the classic
# "pooled" Student version only helps if variances are truly equal, and
# costs you when they aren't.
# formula interface:  outcome ~ grouping variable
# ====

t.test(cap_eur ~ urban_class, data = df_two)

# reading the output (only what's NEW compared to the one-sample test):
# - "t": (mean_urban - mean_rural) / se_diff -- how many standard errors
#   the observed difference sits away from 0 (the sign shows the direction)
# - "df": a non-integer value is normal -- that's the Welch correction
# - "alternative hypothesis: true difference in means between group Urban
#   and group Rural is not equal to 0" -- the two-tailed H1
# - "95 percent confidence interval": for the DIFFERENCE, the same
#   interval we built by hand above (t- instead of z-based)
# - "sample estimates": the two group means; the difference is their gap
# - p-value: the probability of seeing a difference at least this large,
#   IN EITHER DIRECTION, if the true difference were 0. p < 0.05 -> reject
#   H0, found support for "the groups differ"; p > 0.05 -> no support

# same test with rstatix (Welch by default)
df_two %>%
  t_test(cap_eur ~ urban_class, detailed = TRUE)


## BUT WE ASKED A MORE SPECIFIC QUESTION: IS URBAN *HIGHER*? (ONE-TAILED)
# ====
# the test above only answers "is there a difference?" -- it would be
# equally excited about Urban being much pricier or much cheaper. our
# actual question has a direction (urban rents and wages are higher, so we
# expect urban cappuccinos to cost MORE), and a directional question gets
# a directional hypothesis:
#
#   H0: mean(Urban) <= mean(Rural)    (urban is NOT pricier)
#   H1: mean(Urban) >  mean(Rural)    (urban IS pricier)
#
# in R:  alternative = "greater"  means "group 1 > group 2", where group 1
# is the FIRST factor level (Urban, thanks to the releveling above).
# if the levels were the other way round, you'd need "less" -- always check
# the "alternative hypothesis:" line in the output to confirm the direction.
#
# a one-tailed test is only legitimate when the direction is chosen BEFORE
# looking at the data (here: from theory). it's more powerful for that one
# direction -- but it can never detect an effect in the opposite direction,
# however large
# ====

t.test(cap_eur ~ urban_class, data = df_two, alternative = "greater")

df_two %>%
  t_test(cap_eur ~ urban_class, alternative = "greater", detailed = TRUE)

# what changed compared to the two-tailed output:
# - the "alternative hypothesis" line now says "greater than 0"
# - the confidence interval is one-sided, [lower, Inf]: see below
# - the p-value: if the observed difference is in the predicted direction,
#   the one-sided p-value is exactly HALF the two-sided one (all the
#   rejection area sits in one tail instead of being split between two).
#   that's the entire "advantage" of a one-tailed test -- and why the
#   direction must be fixed in advance. if the difference went the OTHER
#   way (negative t), the one-sided p would be above 0.5 (precisely:
#   1 - two-sided p / 2). check the SIGN of t before celebrating a halved p

# the opposite direction, for comparison (H1: Urban is LOWER than Rural).
# the two one-sided p-values always add up to 1. this is a demonstration
# of the mechanics only -- picking the direction AFTER seeing the data
# would break the rule above
df_two %>%
  t_test(cap_eur ~ urban_class, alternative = "less", detailed = TRUE)

# THE ONE-TAILED INTERVAL: ONLY A LOWER BOUND
# ====
# a one-tailed test only cares about the lower end of the plausible
# differences. all 5% of the "error budget" goes into ONE tail, so the
# critical value shrinks from 1.96 to qnorm(0.95) = 1.645, and the interval
# becomes [lower, Inf) -- open-ended on the other side
# ====

qnorm(0.95)   # 1.645 -- compare with qnorm(0.975) = 1.96 for the two-sided case
c(diff = diff_means, lower_one_sided = diff_means - qnorm(0.95) * se_diff)
# if this lower bound is above 0, the one-tailed test will find support for H1


## HOW BIG IS THE DIFFERENCE? EFFECT SIZE
# ====
# as in the one-sample case, p-values depend on n, effect sizes don't. here
# the two groups have very different sizes, which is exactly when a
# non-significant p-value can mean "too little data" rather than "no
# difference" -- the effect size shows how big the gap is either way.
#
# the raw gap (mean_urban - mean_rural, in euros) is already an effect size,
# and the most interpretable one. Cohen's d standardizes it:
#
#   d = (mean_urban - mean_rural) / sd
#
# the only new question is WHICH sd. there are two common choices:
#   - pooled sd:    weights each group's variance by its size,
#                   sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2) )
#   - average sd:   treats the groups equally, sqrt( (s1^2 + s2^2) / 2 )
# with equal group sizes they coincide; with very different sizes they don't
# ====

s_urban <- sd(cap_urban)
s_rural <- sd(cap_rural)
n_urban <- length(cap_urban)
n_rural <- length(cap_rural)

sd_pooled <- sqrt(((n_urban - 1) * s_urban^2 + (n_rural - 1) * s_rural^2) /
                    (n_urban + n_rural - 2))
sd_avg    <- sqrt((s_urban^2 + s_rural^2) / 2)

d_pooled <- (mean(cap_urban) - mean(cap_rural)) / sd_pooled
d_avg    <- (mean(cap_urban) - mean(cap_rural)) / sd_avg
c(d_pooled = d_pooled, d_avg = d_avg)

# the same numbers straight from rstatix. the default (var.equal = FALSE)
# is the AVERAGE sd, matching the Welch t-test's no-equal-variances
# philosophy; var.equal = TRUE gives the pooled version
df_two %>% cohens_d(cap_eur ~ urban_class)                    # matches d_avg
df_two %>% cohens_d(cap_eur ~ urban_class, var.equal = TRUE)  # matches d_pooled

# the sign follows the order of the groups (Urban - Rural): negative means
# Urban is LOWER. the "magnitude" column ignores the sign and applies the
# rough conventions (|d| ~ 0.2 small, ~ 0.5 medium, ~ 0.8 large)

## d AND t ARE STILL LINKED
# ====
# same logic as in the one-sample script: t grows with sample size, d doesn't.
# for the pooled (Student) version of the test the link is exact:
#
#   t = d_pooled * sqrt( n1 * n2 / (n1 + n2) )
# ====

d_pooled * sqrt(n_urban * n_rural / (n_urban + n_rural))
t.test(cap_eur ~ urban_class, data = df_two, var.equal = TRUE)$statistic   # same number

# the Welch t-statistic printed earlier is close to this, but not identical
# (it uses the unpooled se). what carries over is the lesson: the same d
# gives a bigger |t| and a smaller p-value as n1 and n2 grow
# ====


## A BOOTSTRAPPED INTERVAL FOR THE DIFFERENCE
# ====
# assumption-free alternative to the by-hand interval. the one new rule
# compared to the one-sample bootstrap: resample WITHIN EACH GROUP
# separately (so the group sizes stay what they are), compute the statistic
# in each, and take the difference. repeat n_boot times. written with
# stat = so the same function also handles the difference in medians.
# ====

bootstrap_diff <- function(x, y, stat = mean, seed = 123) {
  set.seed(seed)
  replicate(n_boot, stat(sample(x, length(x), replace = TRUE)) -
              stat(sample(y, length(y), replace = TRUE)))
}

boot_diff_means <- bootstrap_diff(cap_urban, cap_rural)

quantile(boot_diff_means, c(0.025, 0.975))   # two-sided 95% percentile interval
quantile(boot_diff_means, 0.05)              # one-sided: lower bound only (5th percentile)

# informal one-sided check: the share of bootstrap differences that are
# <= 0. if almost none are, "Urban is not higher" is hard to believe
mean(boot_diff_means <= 0)

# compare with the by-hand interval above -- with groups this large they
# should be close, for the same CLT reason as in the one-sample script


## THE NON-PARAMETRIC VERSION: WILCOXON RANK-SUM (MANN-WHITNEY) TEST
# ====
# the two-sample Wilcoxon pools all observations, ranks them, and checks
# whether one group's observations tend to sit at higher ranks. it does NOT
# compare means, and strictly it doesn't compare medians either:
#   - in general it tests whether a random Urban price tends to be higher
#     than a random Rural price (P(Urban > Rural) > 0.5)
#   - if the two distributions have the SAME SHAPE and differ only by a
#     shift, that reads as "the medians/locations differ"
# so with skewed, differently-spread groups, interpret it as "tends to be
# higher", not "the median is higher by X".
#
# with this many observations R uses a normal approximation, and because
# prices have many exact ties it will warn that it "cannot compute exact
# p-value with ties" -- harmless at this sample size
#
# same two-step logic as with the t-test: first the two-tailed version
# (is there a difference?), then the one-tailed one (is Urban higher?)
# ====

wilcox.test(cap_eur ~ urban_class, data = df_two, conf.int = TRUE)

# new in the output:
# - "W": the rank-sum statistic (counts pairs where Urban > Rural) -- not
#   interpretable on its own, only through the p-value
# - "difference in location" under sample estimates: the Hodges-Lehmann
#   estimate -- the MEDIAN OF ALL PAIRWISE DIFFERENCES (every Urban price
#   minus every Rural price). like the one-sample case, it is close to --
#   but not the same as -- the difference of the two medians
# - the confidence interval is for that estimate

median(cap_urban) - median(cap_rural)   # compare to the Hodges-Lehmann estimate

# the one-tailed version: same alternative = argument as in the t-test
wilcox.test(cap_eur ~ urban_class, data = df_two, alternative = "greater", conf.int = TRUE)
# the confidence interval is now one-sided ([lower, Inf]) and the
# p-value follows the same halving rule as for the t-test

# the pipe-friendly version:
df_two %>%
  wilcox_test(cap_eur ~ urban_class, alternative = "greater", detailed = TRUE)


## EFFECT SIZE FOR THE WILCOXON
# ====
# Cohen's d is built from means and sds, so it belongs to the t-test. the
# rank-based tools have their own effect sizes, and the most natural one
# here answers the question the test is actually asking:
#
#   probability of superiority = P(a random Urban price > a random Rural price)
#
# 0.5 means "no tendency either way", 1 means Urban always higher. it comes
# straight from the W statistic: W counts the Urban-Rural pairs where Urban
# is higher (ties count as half), and there are n1 * n2 pairs in total
# ====

w_stat <- wilcox.test(cap_eur ~ urban_class, data = df_two)$statistic
w_stat / (n_urban * n_rural)   # probability of superiority for Urban

# a related version rescales this to -1..1 (the rank-biserial correlation):
2 * (w_stat / (n_urban * n_rural)) - 1   # 0 = no effect, negative = Urban tends to be LOWER

# and the standard "r" from rstatix: the test's z-statistic scaled by
# sample size, r = |z| / sqrt(n). it ranges from 0 to 1 (rough guide:
# 0.1 small, 0.3 medium, 0.5 large). it has no sign, so the DIRECTION
# comes from the test's estimate or the probability of superiority above
df_two %>% wilcox_effsize(cap_eur ~ urban_class)

# same lesson as with d: the p-value shrinks with n, the effect size doesn't.
# if the probability of superiority is close to 0.5 and r is tiny, the groups
# overlap almost completely, whatever the p-value says


## A BOOTSTRAPPED INTERVAL FOR THE DIFFERENCE IN MEDIANS
# ====
# for an interval around the actual difference in medians (rather than the
# Hodges-Lehmann stand-in), reuse bootstrap_diff() with stat = median
# ====

boot_diff_medians <- bootstrap_diff(cap_urban, cap_rural, stat = median)

median(cap_urban) - median(cap_rural)               # point estimate
quantile(boot_diff_medians, c(0.025, 0.975))        # two-sided
quantile(boot_diff_medians, 0.05)                   # one-sided lower bound


## A NOTE ON INDEPENDENCE
# ====
# every test above assumes the observations are INDEPENDENT. the data glimpse
# shows repeated cities (e.g. Copenhagen appears many times, with the same
# wage_eur) -- observations from the same city or country are more alike than
# random pairs, which makes the effective sample size smaller than n and the
# p-values somewhat too optimistic. worth keeping in mind when interpreting
# very small p-values; the models later in the course (e.g. with country as
# a grouping factor) handle this properly
# ====


## PAIRED DATA: THE SAME CAPPUCCINOS, MEASURED TWICE
# ====
# everything above compared two DIFFERENT sets of observations (urban cafes
# vs rural cafes). a paired design is different: the SAME unit is measured
# twice (before/after, left/right, treatment/control on one subject), so
# every value in one column has a natural partner in the other.
#
# our dataset has no second measurement, so we SIMULATE one: suppose every
# cappuccino in the data gets a random price increase of 0-2% (a "slightly
# higher" price list next year). set.seed() makes the "random" numbers
# reproducible, so everyone gets the same simulated data
# ====

set.seed(123)

df_pair <- df_cap %>%
  filter(!is.na(cap_eur)) %>%
  mutate(cap_eur_new = cap_eur * (1 + runif(n(), min = 0, max = 0.02)),
         cap_diff    = cap_eur_new - cap_eur)

df_pair %>% select(city, cap_eur, cap_eur_new, cap_diff) %>% head()

n_pair <- nrow(df_pair)


## WHY PAIRING MATTERS: TWO VERY DIFFERENT SPREADS
# ====
# prices differ enormously BETWEEN cappuccinos (Copenhagen vs a rural town),
# but the CHANGE within each cappuccino is small and consistent. compare:
# ====

sd(df_pair$cap_eur)    # spread between cappuccinos: large
sd(df_pair$cap_diff)   # spread of the changes: tiny

# an unpaired test (like the Welch test above) has to fight through the
# large between-cappuccino spread to see a small shift. a paired test
# works with the DIFFERENCES only, so all the stable "this city is just
# expensive" variation cancels out. in fact:
#
#   a paired t-test IS a one-sample t-test on the differences, against 0
#
# so everything from 01_one-sample.R carries over: the null is
# H0: mean difference = 0, the se is sd(diff) / sqrt(n), and so on


## CONFIDENCE INTERVAL FOR THE MEAN DIFFERENCE, BY HAND
# ====
# exactly the one-sample recipe, applied to the differences
# ====

m_diff  <- mean(df_pair$cap_diff)
se_pair <- sd(df_pair$cap_diff) / sqrt(n_pair)

c(mean_diff = m_diff,
  lower     = m_diff - 1.96 * se_pair,
  upper     = m_diff + 1.96 * se_pair)

# does the interval exclude 0? that is the paired test in one line.


## THE PAIRED T-TEST
# ====
# the direction is known in advance (we simulated an INCREASE, and prices
# are expected to rise), so this is a one-tailed test like the Urban >
# Rural one: H1 is "after > before", i.e. alternative = "greater"
#   H0: mean(after - before) <= 0
#   H1: mean(after - before) >  0
# in base R, paired = TRUE with two vectors: the DIFFERENCE is always the
# first argument minus the second, so put "after" first
# (note: newer R versions no longer accept paired = TRUE in the formula
# interface, so the two-vector form is the safe one)
# ====

t.test(df_pair$cap_eur_new, df_pair$cap_eur, paired = TRUE, alternative = "greater")

## PROOF: PAIRED TEST = ONE-SAMPLE TEST ON THE DIFFERENCES
# ====
# run both versions, store them, and put the numbers side by side
# ====

paired_t   <- t.test(df_pair$cap_eur_new, df_pair$cap_eur, paired = TRUE, alternative = "greater")
one_samp_t <- t.test(df_pair$cap_diff, mu = 0, alternative = "greater")

data.frame(test     = c("paired", "one-sample on differences"),
           t        = c(paired_t$statistic,  one_samp_t$statistic),
           df       = c(paired_t$parameter,  one_samp_t$parameter),
           p        = c(paired_t$p.value,    one_samp_t$p.value),
           estimate = c(paired_t$estimate,   one_samp_t$estimate),   # mean difference
           ci_lower = c(paired_t$conf.int[1], one_samp_t$conf.int[1]))

# the rows are identical. all.equal() makes it explicit (TRUE = same
# numbers up to floating-point precision):
all.equal(unname(paired_t$statistic), unname(one_samp_t$statistic))
all.equal(paired_t$p.value, one_samp_t$p.value)

# and both are just the by-hand formula from the one-sample script,
# t = mean / se, with the DIFFERENCES as the data:
m_diff / se_pair   # same t-statistic as above

# the same holds in rstatix (formula "~ 1" = one sample, as in 01):
df_pair %>%
  t_test(cap_diff ~ 1, mu = 0, alternative = "greater", detailed = TRUE)

# new in the output: "mean difference" (the mean of the paired differences)
# replaces the two group means, and df = n - 1 (pairs, not observations)

# for contrast: the UNPAIRED test on the very same numbers, which ignores
# which cappuccino belongs to which
t.test(df_pair$cap_eur_new, df_pair$cap_eur, alternative = "greater")
# expect a far smaller t-statistic and a much larger p-value here, even
# though the data are identical: the unpaired test is blind to the pairing
# and drowns the small shift in the large between-cappuccino spread

# rstatix wants the data in LONG format for paired tests: one row per
# measurement, with an id linking the pairs. factor order again decides
# the direction ("after" first -> after - before)
df_long <- df_pair %>%
  mutate(id = row_number()) %>%
  select(id, before = cap_eur, after = cap_eur_new) %>%
  pivot_longer(c(before, after), names_to = "time", values_to = "price") %>%
  mutate(time = factor(time, levels = c("after", "before")))

df_long %>%
  t_test(price ~ time, paired = TRUE, alternative = "greater", detailed = TRUE)


## THE NON-PARAMETRIC VERSION: WILCOXON SIGNED-RANK TEST
# ====
# the rank-based paired test. it ranks the absolute DIFFERENCES and checks
# whether the positive ones carry more rank than the negative ones. like
# the t-test, it is the one-sample test applied to the differences, so
# the one-sample Wilcoxon logic applies: it targets the (pseudo)median of
# the differences rather than their mean, and the confidence interval is
# for the Hodges-Lehmann estimate. here "V" is the sum of the ranks of the
# positive differences (instead of W)
# ====

wilcox.test(df_pair$cap_eur_new, df_pair$cap_eur, paired = TRUE,
            alternative = "greater", conf.int = TRUE)

median(df_pair$cap_diff)   # compare to the Hodges-Lehmann estimate

# the same equivalence holds here: the signed-rank test on pairs is the
# one-sample Wilcoxon on the differences
paired_w   <- wilcox.test(df_pair$cap_eur_new, df_pair$cap_eur, paired = TRUE, alternative = "greater")
one_samp_w <- wilcox.test(df_pair$cap_diff, mu = 0, alternative = "greater")
all.equal(unname(paired_w$statistic), unname(one_samp_w$statistic))   # same V
all.equal(paired_w$p.value, one_samp_w$p.value)                       # same p

df_long %>%
  wilcox_test(price ~ time, paired = TRUE, alternative = "greater", detailed = TRUE)


## EFFECT SIZE FOR PAIRED DATA
# ====
# the raw effect size is the mean difference in euros, as always. the
# standardized version, d_z, divides it by the sd of the DIFFERENCES:
#
#   d_z = mean(diff) / sd(diff)         (and t = d_z * sqrt(n), as before)
#
# careful with the interpretation: because sd(diff) is small here, d_z
# measures how CONSISTENT the change is, not how big it is. expect a huge
# d_z even though the average increase is only a few cents -- a tiny but
# very regular shift. always report the raw difference next to it
# ====

d_z <- mean(df_pair$cap_diff) / sd(df_pair$cap_diff)
d_z
d_z * sqrt(n_pair)   # equals the paired t-statistic

df_long %>% cohens_d(price ~ time, paired = TRUE)        # matches d_z (sign: after - before)
df_long %>% wilcox_effsize(price ~ time, paired = TRUE)  # rank-based r

# the independence note from above still applies, but now to the PAIRS:
# the pairs must be independent of each other, while the two values
# WITHIN a pair are deliberately dependent -- that's what the paired test
# exploits


## RECAP: TWO GROUPS, THE SAME UNDERLYING QUESTION
# ====
# every section answered some version of "do Urban and Rural differ, and is
# Urban higher?":
#
#   CI per group (X-wings)       eyeball check; overlap is inconclusive, not "no difference"
#   CI of the difference         plausible values of mean_U - mean_R             assumes CLT
#   Welch t-test (two-tailed)    is there a difference in MEANS, either way?     assumes CLT
#   Welch t-test (one-tailed)    is the mean HIGHER in Urban?                    assumes CLT
#   Cohen's d                    size of the mean difference, in SDs             goes with the t-test
#   bootstrapped diff (mean)     same target as the CI of the difference         assumption-free
#   Wilcoxon rank-sum            "Urban tends to be higher" (ranks, not means)   assumption-free
#   Wilcoxon CI                  interval for the Hodges-Lehmann difference      assumption-free
#   prob. of superiority / r     size of the rank-based difference               goes with the Wilcoxon
#   bootstrapped diff (median)   interval for the difference in MEDIANS          assumption-free
#
# and for PAIRED data (the same units measured twice):
#
#   paired t-test                one-sample t-test on the differences            assumes CLT
#   paired Wilcoxon (signed-rank) one-sample Wilcoxon on the differences         assumption-free
#   d_z / paired r               size of the change, in SDs of the differences   consistency, not size
#
# two-tailed answers "is there a difference?", one-tailed answers "is it in
# THIS direction?". the one-tailed set-up halves the p-value (when the
# effect goes the predicted way), turns the CI into [lower, Inf), and only
# makes sense if the direction was fixed BEFORE looking at the data.
# effect sizes don't depend on the tail -- they describe the data
# ====