# ===================================================================
# 02_two-continuous.R -- ASSOCIATION BETWEEN TWO CONTINUOUS VARIABLES
# (hourly wage and cappuccino price): scatterplots, covariance and
# Pearson's r (by hand and with cor.test), confidence intervals (Fisher's
# z and bootstrap), a permutation test, what r can and cannot see
# (Anscombe), rank correlations (Spearman, Kendall), the trap of
# correlating a ratio with its own component, pooled vs within-group
# correlations, repeated cities, and the bridge to regression
# ===================================================================

source("scripts/00_setup.R")

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

n_boot <- 2000   # same as in the previous scripts

# the question: are cities with higher wages also cities with pricier
# cappuccinos? both variables are now NUMERIC:
#   wage_eur   the hourly wage in euros
#   cap_eur    the price of a cappuccino in euros
# we keep the three urban classes (as in 03) for the group section later,
# and only rows where both variables exist and are positive (we will take
# logarithms, which need positive numbers)
df_cont <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"),
         !is.na(cap_eur), !is.na(wage_eur),
         cap_eur > 0, wage_eur > 0) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")))

nrow(df_cont)

x <- df_cont$wage_eur   # the two variables as plain vectors, for the by-hand parts
y <- df_cont$cap_eur
n <- length(x)


## STEP 0: LOOK AT THE DATA FIRST -- THE SCATTERPLOT
# ====
# with two numeric variables the picture is a SCATTERPLOT: one point per
# observation, one variable on each axis. ALWAYS plot before you compute a
# correlation -- a single number can hide almost anything (see Anscombe,
# below). look for: the overall direction (up / down), the shape (a
# straight line? a curve?), the spread (does it fan out?), and unusual
# points far from the rest
# ====

ggplot(df_cont, aes(x = wage_eur, y = cap_eur)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#e34948") +
  labs(x = "wage_eur (EUR per hour)", y = "cap_eur (EUR)",
       title = "Cappuccino price vs hourly wage") +
  theme_minimal()



## THE QUESTION AND THE HYPOTHESES
# ====
#   H0: the population correlation is zero    (rho = 0, no LINEAR association)
#   H1: it is not zero                        (rho != 0)
#
# the test is two-tailed: it asks about ANY linear association, positive or
# negative. as always, "no association" is the null, and a small p-value says
# only that the data are hard to reconcile with rho = 0 -- not that rho is
# large, and not that wages CAUSE prices (or the reverse): correlation is
# symmetric, it has no "outcome" and no "predictor"
# ====


## PEARSON'S r: THE LOGIC, BY HAND
# ====
# step 1, COVARIANCE: do x and y tend to be above their means TOGETHER?
#
#   cov(x, y) = sum( (x - mean(x)) * (y - mean(y)) ) / (n - 1)
#
# each point contributes the product of its two deviations: positive when
# both are above (or both below) their means, negative when one is above
# and the other below. the sign of the covariance is the direction of the
# association. but its SIZE depends on the units (euros vs cents), so it
# cannot be compared across variables
#
# step 2, STANDARDIZE: divide by both standard deviations. the result is
# Pearson's correlation coefficient, always between -1 and +1:
#
#   r = cov(x, y) / (sd(x) * sd(y))
#
# equivalently: r is the AVERAGE PRODUCT OF z-SCORES, sum(zx * zy) / (n - 1).
# -1 = perfect downhill line, 0 = no LINEAR relationship, +1 = perfect
# uphill line. r does not change if we change units (euros -> cents) or
# swap the two variables
# ====

cov_xy <- sum((x - mean(x)) * (y - mean(y))) / (n - 1)
c(by_hand = cov_xy, cov = cov(x, y))

r_hand <- cov_xy / (sd(x) * sd(y))
r_z    <- sum(scale(x) * scale(y)) / (n - 1)
c(covariance_over_sds = r_hand, mean_product_of_z = r_z, cor = cor(x, y))   # all the same number

# the test: if H0 (rho = 0) is true, then
#
#   t = r * sqrt( (n - 2) / (1 - r^2) )
#
# follows a t-distribution with n - 2 degrees of freedom. notice what the
# formula says: for a given r, a LARGER n means a larger t and a smaller
# p-value. with thousands of observations even a small r is "significant"
t_stat <- r_hand * sqrt((n - 2) / (1 - r_hand^2))
p_r    <- 2 * pt(-abs(t_stat), df = n - 2)   # two-tailed
c(r = r_hand, t = t_stat, df = n - 2, p = p_r)


## THE SAME THING WITH cor.test()
# ====
# cor.test() does the test and adds a 95% CI for the correlation. the output
# should match the by-hand numbers above
# ====

cor.test(x, y)   # Pearson is the default

# the pipe-friendly version (rstatix)
df_cont %>% cor_test(wage_eur, cap_eur)

# result: r = 0.476, t = 26.7 on 2436 df -- the by-hand numbers match. the
# by-hand p-value is about 4e-138; cor.test() prints "< 2.2e-16", which is
# only the smallest number R is willing to display, so never report
# "p = 0": write p < 0.001. the 95% CI is 0.445 to 0.506 (rstatix rounds
# the same numbers to 0.48 and 0.445-0.506). wage and price clearly move
# together, with a moderate-to-strong positive correlation, and the CI
# excludes 0 by a mile, so H0 is firmly rejected
#
# reading: "cor" is the sample r, "t" and "df" the test statistic, p the
# p-value, and the CI says which values of the population correlation are
# compatible with our data. p < 0.05 -> reject H0: there is a linear
# association. p > 0.05 -> no support for one. in either case look at r and
# its CI, not only at p


## THE CONFIDENCE INTERVAL FOR r: FISHER'S z
# ====
# the sampling distribution of r is skewed (it cannot leave the -1 to +1
# range), so a plain "r +/- 1.96 se" would be wrong. Fisher's trick: the
# transformation z = atanh(r) is approximately normal with standard error
# 1 / sqrt(n - 3). build the CI on the z-scale, then transform back with
# tanh(). this is what cor.test() does internally
# ====

z_r  <- atanh(r_hand)
se_z <- 1 / sqrt(n - 3)
tanh(z_r + c(-1, 1) * qnorm(0.975) * se_z)   # same as the CI in cor.test()

# result: 0.445 to 0.506, identical to cor.test(). the interval is slightly
# longer towards zero (0.031 below r, 0.030 above) -- the skewness that the
# transformation corrects for. here it is tiny because r is far from +-1
# and n is large; to see the asymmetry clearly, compute the interval for
# r = 0.9 with n = 20


## EFFECT SIZE: r IS THE EFFECT SIZE, r^2 IS ITS SQUARE
# ====
# unlike the t-test or chi-square, the correlation coefficient is already
# an effect size -- it lives on a standard 0-1 (in absolute value) scale
# whatever the units. rough conventions: |r| 0.1 small, 0.3 medium, 0.5 large
#
# its square, r^2, is the share of the variance of one variable that is
# "accounted for" by a straight line through the other. the squaring is
# humbling: r = 0.3 sounds medium, but r^2 = 0.09 means 91% of the variation
# is NOT explained by the linear relationship. as always, with thousands of
# observations a tiny r can have a tiny p-value
# ====

r_hand^2

# result: r^2 = 0.227. a straight line through the wage "accounts for" about
# 23% of the variation in prices, and the remaining 77% is something else
# (the cafe, the neighbourhood, tourism, noise...). r = 0.48 sits between
# the conventional "medium" (0.3) and "large" (0.5), yet three quarters of
# the variance is unexplained -- which is the humbling side of r


## WHAT r CAN AND CANNOT SEE: ANSCOMBE'S QUARTET
# ====
# four small datasets, built by the statistician Francis Anscombe in 1973
# (they ship with R as `anscombe`). they have IDENTICAL means, variances,
# correlation and regression line -- and four completely different
# pictures. the numbers below are the same in all four; only the plot tells
# the truth
# ====

anscombe_long <- bind_rows(lapply(1:4, function(i) {
  data.frame(set = paste("dataset", i), x = anscombe[[paste0("x", i)]], y = anscombe[[paste0("y", i)]])
}))

anscombe_long %>%
  group_by(set) %>%
  summarise(mean_x = mean(x), mean_y = mean(y), sd_x = sd(x), sd_y = sd(y), r = cor(x, y))

# result: the four datasets have the same means (9 and 7.5), the same sds
# (3.32 and 2.03) and the same r (0.816, with 0.817 for dataset 4) -- to
# three decimals, nothing in the table distinguishes them. the plot does

ggplot(anscombe_long, aes(x = x, y = y)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#e34948") +
  facet_wrap(~ set) +
  labs(title = "Anscombe's quartet: the same r, four different stories") +
  theme_minimal()

# the four stories: (1) an ordinary linear cloud -- the r is a fair summary;
# (2) a perfect CURVE -- r understates a relationship that is exact but not
# linear; (3) a perfect line with ONE OUTLIER that drags the fitted line
# away; (4) no relationship at all, except ONE extreme point that creates
# the whole correlation. the lesson: r measures LINEAR association only,
# is sensitive to outliers, and must be read together with the scatterplot


## ASSUMPTIONS: WHAT CAN GO WRONG?
# ====
# Pearson's r (and its test) assume (1) the relationship is roughly LINEAR,
# (2) no extreme outliers or heavy skew driving the result, (3) independent
# observations, and, for the test and CI in small samples, (4) roughly
# normal variables (bivariate normality). with large samples (4) matters
# less, for the same CLT reason as in the earlier scripts -- but (1) and (2)
# matter at ANY sample size, and Anscombe shows why
#
# for our data: the scatterplots above are the check for (1) and (2), and
# the log-log version told us whether a transformation helps. (3) gets its
# own section at the end -- it is the one that is violated here
# ====


## LOGARITHMS: CHANGING THE QUESTION
# ====
# if wages are skewed and the relationship is closer to a straight line on
# the log-log plot, Pearson's r on the LOGGED variables is the better
# summary: it measures the linear association between PERCENTAGE
# differences. but note that this is a different question from the raw one:
# the correlation of the logs is not the correlation of the raw values, and
# neither is "the" correlation. state which one you report
# ====

cor(x, y)                # the raw scale
cor(log(x), log(y))      # the log-log scale
cor.test(log(x), log(y))

# a big difference between the two tells you that extreme values (or the
# curvature that a log transformation removes) drive the raw correlation
#
# result: r = 0.476 on the raw scale and 0.571 on the log-log scale. the
# logs RAISE the correlation, as expected if wages and prices are related
# more in percentage terms than in euros. but before celebrating, recall
# the summary: the minimum wage is 0.20 euros per hour. a log transformation
# STRETCHES the low end (log(0.2) is far below log(14)), so very low wages
# become high-leverage points that can pull the log-scale r around -- the
# transformation that tames the high extremes can create low ones.
# a robustness check: how many wages are tiny, and what happens without them?

sum(x < 2)
keep <- x >= 2
c(raw_all = cor(x, y), raw_trimmed = cor(x[keep], y[keep]),
  log_all = cor(log(x), log(y)), log_trimmed = cor(log(x[keep]), log(y[keep])))

# if the trimmed values are close to the full-data ones, the extreme wages
# are not driving the result; if they move a lot, report both and look at
# those rows (they may be errors). the threshold of 2 euros per hour is
# arbitrary -- try others, and decide it on substantive grounds, never by
# which one gives the nicest r


## RANK CORRELATIONS: SPEARMAN AND KENDALL
# ====
# the non-parametric versions, in the same spirit as the rank-sum and
# Kruskal-Wallis tests in 02 and 03: replace the values by their RANKS and
# work with those. an extreme value becomes just "the highest rank".
#
#   Spearman's rho    = Pearson's r computed on the ranks. measures how well
#                       the relationship is MONOTONE (always going the same
#                       direction), whether straight or curved
#   Kendall's tau     = based on PAIRS of observations: a pair is CONCORDANT
#                       if the one with the higher x also has the higher y,
#                       DISCORDANT otherwise. tau = (concordant - discordant)
#                       / all pairs. smaller than rho in absolute value for
#                       the same data, but has a direct reading: "the
#                       probability of a concordant pair minus the
#                       probability of a discordant one"
#
# both ignore the shape of the distributions, are robust to outliers, and
# find any monotone association, which is why they are good defaults when in
# doubt. the price: they say nothing about the SLOPE of a straight line
# ====

# Spearman by hand: it is just Pearson on ranks
c(by_hand = cor(rank(x), rank(y)), spearman = cor(x, y, method = "spearman"))

# a nice property: ranks do not change under any monotone transformation,
# so Spearman on the raw data and on the log data is exactly the same --
# the transformation question that troubled Pearson simply does not arise
c(raw = cor(x, y, method = "spearman"),
  log = cor(log(x), log(y), method = "spearman"))

# the tests. both variables have many tied values (rounded prices, one wage
# per city), and R then cannot compute exact p-values -- we ask for the
# normal approximation, which is fine at this sample size
cor.test(x, y, method = "spearman", exact = FALSE)
cor.test(x, y, method = "kendall",  exact = FALSE)

# the pipe-friendly version for Spearman
df_cont %>% cor_test(wage_eur, cap_eur, method = "spearman")

# compare the three coefficients for the same data: Pearson (raw), Spearman,
# Kendall. when Pearson and Spearman agree, the straight-line summary is
# safe; when Spearman is much larger, the relationship is monotone but
# curved or driven by a few extreme points. Kendall's tau is usually
# smaller than both -- compare tau with rho only by their SIGNIFICANCE or
# with their own conventions, never by size
#
# result: Spearman's rho = 0.438 (S = 1.36e9) and Kendall's tau = 0.315
# (z = 23.1), both with p < 2.2e-16 (rstatix shows the p of rho as about
# 6e-115 -- the same verdict). tau < rho, as expected. now line up the
# coefficients for the same data: Pearson 0.476 (raw), Pearson 0.571
# (log-log), Spearman 0.438. Spearman is LOWER than raw Pearson and well
# below log-log Pearson. for a clean, bivariate-normal-looking cloud they
# would be roughly equal, so the gap says that a few extreme points inflate
# the Pearson values -- consistent with the extreme maxima of wage and price
# and, for the log-log version, with the very low wages. when in doubt, the
# rank-based 0.44 is the number that does not depend on them


## CONFIDENCE INTERVALS BY BOOTSTRAP
# ====
# the same percentile bootstrap as in 01-03, with one thing to get right:
# resample ROWS (pairs), never x and y separately -- shuffling them
# independently would destroy the very association we are measuring. a
# bootstrap CI works for Pearson and for Spearman alike, with no
# normality assumption
# ====

boot_cor <- function(d, method = "pearson", seed = 123) {
  set.seed(seed)
  b <- replicate(n_boot, {
    i <- sample(nrow(d), replace = TRUE)           # resample whole rows
    cor(d$wage_eur[i], d$cap_eur[i], method = method)
  })
  data.frame(method   = method,
             estimate = cor(d$wage_eur, d$cap_eur, method = method),
             lower    = unname(quantile(b, 0.025)),
             upper    = unname(quantile(b, 0.975)))
}

bind_rows(boot_cor(df_cont, "pearson"), boot_cor(df_cont, "spearman"))

# compare the Pearson bootstrap interval with the Fisher-z interval from
# cor.test(): with a large sample and a well-behaved relationship they are
# close. if they differ a lot, the normality behind Fisher's z is suspect
#
# result: Pearson 0.476 with a bootstrap CI of 0.429 to 0.516, against
# 0.445 to 0.506 from Fisher's z. the bootstrap interval is wider on both
# sides (width 0.087 vs 0.061): the sampling variability of r is larger
# than the normal-theory formula says, as one would expect with extreme
# points in the data. for Spearman the bootstrap CI is 0.401 to 0.473. when
# the two methods disagree, prefer the wider, resampling-based one


## A PERMUTATION TEST: THE NULL DISTRIBUTION YOU CAN SEE
# ====
# what would r look like if wages and prices were UNRELATED? we can find
# out directly: keep every wage where it is, SHUFFLE the prices among the
# rows (which breaks any link between the two columns while keeping both
# distributions intact), compute r, and repeat many times. the shuffled r
# values ARE the null distribution. the p-value is the share of shuffles
# whose |r| is at least as large as the one we observed
# ====

set.seed(123)
r_obs  <- cor(x, y)
r_perm <- replicate(n_boot, cor(x, sample(y)))

# the +1 counts the observed sample itself as one of the permutations; it
# keeps the p-value from being exactly 0 (the smallest possible value is
# 1 / (n_boot + 1))
(sum(abs(r_perm) >= abs(r_obs)) + 1) / (n_boot + 1)

ggplot(data.frame(r = r_perm), aes(x = r)) +
  geom_histogram(bins = 40, fill = "#2a78d6", alpha = 0.6) +
  geom_vline(xintercept = r_obs, color = "#e34948", linewidth = 1) +
  geom_vline(xintercept = -r_obs, color = "#e34948", linewidth = 1, linetype = "dashed") +
  labs(x = "r after shuffling the prices", y = "count",
       title = "Permutation distribution of r under H0 (red: our r and its mirror image)") +
  theme_minimal()

# reading: the histogram is what pure chance produces -- a bell centered at
# 0 whose width is about 1 / sqrt(n). if our r (the solid red line) lies far
# in the tail, chance alone is a poor explanation. this is the same logic
# as the t-test formula above, without any formula. the permutation
# approach assumes the rows are exchangeable -- which, as the last section
# shows, is not quite true for our data
#
# result: p = 0.0005, which is exactly 1 / 2001: NONE of the 2000 shuffles
# produced an |r| as large as ours. the histogram is a bell centered at 0
# with a width of about 1 / sqrt(n) = 0.02, and our r = 0.476 is more than
# 20 such widths away. a permutation test can never give p = 0, and cannot
# resolve anything below 1 / (n_boot + 1), so report p < 0.0005 (or run
# more shuffles). the tiny 4e-138 from the t-test is not something the
# permutation test could confirm -- nor does it need to: the verdict is the
# same


## A TRAP: CORRELATING A RATIO WITH ITS OWN COMPONENT
# ====
# the cappuccino index is cap_eur / wage_eur. what happens if we correlate
# the INDEX with the wage? the wage sits in the denominator, so the index
# falls whenever the wage rises -- a negative correlation that can arise
# even when price and wage are completely unrelated. this is a classic
# trap (Karl Pearson called it "spurious correlation", in 1897). to see it,
# simulate prices and wages INDEPENDENTLY, then build the index
# ====

set.seed(123)
sim_wage <- rlnorm(2000, meanlog = log(15), sdlog = 0.4)   # wages
sim_cap  <- rlnorm(2000, meanlog = log(3),  sdlog = 0.2)   # prices, drawn INDEPENDENTLY of the wages
sim_idx  <- sim_cap / sim_wage

c(price_vs_wage        = cor(sim_cap, sim_wage),
  index_vs_wage        = cor(sim_idx, sim_wage),
  index_vs_wage_spearman = cor(sim_idx, sim_wage, method = "spearman"))

# result: price and wage are unrelated by construction (r = -0.01), yet the
# index and the wage correlate at -0.76 (Pearson) and -0.89 (Spearman).
# the arithmetic:
# on a log scale, log(index) = log(price) - log(wage), and the wage part
# has twice the spread of the price part (0.4 vs 0.2), so the index is
# mostly "minus the wage". the more variable the denominator, the stronger
# the artifact
#
# now our real data: the correlation of the index with the wage, next to the
# correlation of the price with the wage
df_idx2 <- df_cont %>% filter(!is.na(cap_index))
c(price_vs_wage = cor(df_idx2$cap_eur,   df_idx2$wage_eur, method = "spearman"),
  index_vs_wage = cor(df_idx2$cap_index, df_idx2$wage_eur, method = "spearman"))

# result: in the real data the price-wage rho is 0.44, but the index-wage rho
# is -0.80 -- almost as strong as the -0.89 in the simulation where prices
# and wages had NOTHING to do with each other. two things are true at once:
# (1) a large part of this negative value is an artifact of putting the
# wage in the denominator; (2) it is not ONLY an artifact, because prices
# do rise with wages (0.44), but evidently less than in proportion -- if
# the price were exactly proportional to the wage, the index would be
# unrelated to it. so the sign carries a hint, but the SIZE (-0.80) says
# almost nothing about how prices relate to wages and should not be reported
# as a finding. notice also how little the index says about the price it is
# built from: their correlation is 0.04 (see the correlation matrix below)
#
# the rule: never read a correlation between a ratio and one of its parts
# as a finding. correlate the components themselves (price and wage), or
# model the price with the wage as a predictor -- which is where this
# course is heading


## POOLED VS WITHIN-GROUP CORRELATIONS
# ====
# the correlation above pools all rows. but the rows come from three kinds
# of places, and the groups may differ in BOTH variables. a pooled
# correlation mixes two things: how wage and price move together WITHIN a
# group, and how the group averages differ between groups. these can
# disagree -- in the extreme case (Simpson's paradox) the pooled correlation
# is positive while within every group it is negative
# ====

df_cont %>%
  group_by(urban_class) %>%
  summarise(n = n(),
            r = cor(wage_eur, cap_eur),
            rho = cor(wage_eur, cap_eur, method = "spearman"))

c(pooled_r = cor(x, y))

ggplot(df_cont, aes(x = wage_eur, y = cap_eur, color = urban_class)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  scale_x_log10() +
  labs(x = "wage_eur (log scale)", y = "cap_eur", color = NULL,
       title = "The wage-price relationship, group by group") +
  theme_minimal()

# result: the within-group Pearson r is 0.39 (Rural, n = 140), 0.31 (Suburban,
# 581) and 0.53 (Urban, 1717), against a pooled 0.48. the pooled value sits
# INSIDE the range of the group values, not above all of them or with the
# opposite sign: there is no Simpson's paradox here, and no sign that
# between-group differences manufacture the correlation -- wage and price
# move together within every kind of place. but compare Pearson with
# Spearman: in Urban they are identical (0.53 and 0.53), in Rural 0.39 vs
# 0.20 and in Suburban 0.31 vs 0.17. a big gap between r and rho in the
# smaller groups means a few extreme points carry the Pearson correlation
# there -- the Anscombe lesson in real data (with 140 rows, a handful of
# points is enough). the within-group associations in Rural and Suburban
# are probably closer to the Spearman values
#
# reading: compare the pooled r with the three within-group values, and
# look at whether the three fitted lines share a direction and a slope. if
# the groups differ in their averages, the pooled r partly measures the
# difference BETWEEN groups. the tool for separating the two is a
# regression that includes the group -- the next folder in this course


## A NOTE ON CORRELATION AND CAUSATION
# ====
# a correlation of wages and prices tells you that they move together, not
# why. wages could push prices up (businesses pass costs on), prices and
# wages could both follow the cost of living, a third factor (tourism, the
# size of the city) could drive both, or part of it could be coincidence.
# nothing in r, in its p-value or in its CI distinguishes these stories.
# to argue for a cause you need a design (an experiment, a natural
# experiment) or at the very least a model that takes the competing
# explanations seriously
# ====


## SEVERAL CORRELATIONS AT ONCE: A CORRELATION MATRIX AND MULTIPLE TESTING
# ====
# with more than two variables people like to compute all pairwise
# correlations. with k variables there are choose(k, 2) tests, and the
# family-wise error problem from 03 is back: with 10 variables, 45 tests, a
# couple of "significant" correlations appear by chance. so adjust the
# p-values (Holm), and read the matrix as a map, not as a list of findings
# ====

vars_num <- df_cap %>% select(cap_eur, wage_eur, cap_index)

round(cor(vars_num, use = "pairwise.complete.obs", method = "spearman"), 2)

# cor_test() lists EVERY ordered pair, including each variable with itself
# and each pair twice (9 rows for 3 variables). we keep one row per distinct
# pair BEFORE adjusting, otherwise the Holm correction multiplies by the
# wrong number of tests: only choose(3, 2) = 3 of them are real
vars_num %>%
  cor_test(method = "spearman") %>%
  filter(var1 < var2) %>%
  mutate(p.adj = p.adjust(p, method = "holm"))

# note the third variable is the ratio of the first two, so its correlations
# with them are exactly the kind the ratio section warned about
#
# result: price-wage rho = 0.44 and index-wage rho = -0.80 (both with
# p < 2.2e-16), and price-index rho = 0.04 with p = 0.065: not significant
# even before any correction (the Holm adjustment leaves the largest of the
# three p-values unchanged). so the index is almost uncorrelated with the
# price it is built from and almost a mirror image of the wage: the ratio
# section in action


## THE BRIDGE TO REGRESSION
# ====
# correlation is symmetric: r(wage, price) = r(price, wage). regression
# asks the DIRECTIONAL question "by how much does the price change when the
# wage changes by one unit?", and gives a SLOPE. the two are tightly linked:
#
#   slope = r * sd(y) / sd(x)         and        R^2 = r^2
#
# so the correlation is the slope you get after standardizing both
# variables (so that each has an sd of 1)
# ====

fit <- lm(cap_eur ~ wage_eur, data = df_cont)
coef(fit)["wage_eur"]
cor(x, y) * sd(y) / sd(x)                 # the same slope

summary(fit)$r.squared
cor(x, y)^2                               # the same R^2

coef(lm(scale(y) ~ scale(x)))[2]          # the slope of the standardized variables is r itself
cor(x, y)

# result: the slope is 0.0858 -- rows whose hourly wage is one euro higher
# have cappuccinos that are on average about 8.6 cents more expensive (a
# description of the pattern, not a causal effect). R^2 = 0.227 equals r^2,
# and the slope of the standardized variables is 0.476 = r. the SLOPE
# answers "how much", in euros; r answers "how tightly", on a unit-free
# scale -- the same r can come with a steep or a shallow slope
#
# the F or t test of the slope in summary(fit) gives the same p-value as
# the correlation test above. so everything in this script is a special case
# of what comes next: regression adds the units (a slope in euros per euro),
# a prediction, and -- crucially -- room for more than one predictor


## A NOTE ON INDEPENDENCE -- AND HERE IT REALLY MATTERS
# ====
# as in 02 and 03, the data contain repeated cities: the wage is a property
# of the CITY, so every row from the same city carries exactly the same
# wage_eur. for a correlation with the wage this is serious. we do not have
# n independent pairs, but a smaller number of cities with several price
# observations each, and the p-values, the CIs and the permutation test
# above all treat the rows as if they were independent -- they are
# overly optimistic. below we see how much
#
# the number of different wages is (nearly) the number of cities
# ====

n_distinct(df_cont$wage_eur)   # vs n rows

# approach 1: collapse to one observation per city. rows with the same wage
# come from the same city, so we average the price within each wage value
# (if your data have a city column, group by that instead)
df_city <- df_cont %>%
  group_by(wage_eur) %>%
  summarise(n_rows = n(), cap_mean = mean(cap_eur), .groups = "drop")

nrow(df_city)
cor.test(df_city$wage_eur, df_city$cap_mean)
cor.test(df_city$wage_eur, df_city$cap_mean, method = "spearman", exact = FALSE)

# approach 2: the CLUSTER BOOTSTRAP. resample whole CITIES (with all their
# rows), not single rows. the interval then reflects the true amount of
# independent information
city_list <- split(df_cont, df_cont$wage_eur)

set.seed(123)
r_cluster <- replicate(n_boot, {
  d <- do.call(rbind, city_list[sample(length(city_list), replace = TRUE)])
  cor(d$wage_eur, d$cap_eur)
})

# the percentile CI that accounts for the clustering, next to the naive
# one from cor.test() on rows
rbind(naive_fisher_z = cor.test(x, y)$conf.int,
      cluster_bootstrap = quantile(r_cluster, c(0.025, 0.975)))

# result so far: there are only 869 different wages for 2438 rows -- about
# 2.8 rows per city -- so we have far fewer independent units than rows. at
# the city level (one averaged observation per city) the Pearson correlation
# is 0.55 (95% CI 0.50 to 0.59) and Spearman's rho is 0.57, BOTH HIGHER than
# the row-level 0.48 and 0.44. this is not a standard-error correction: it
# is a different quantity. averaging the prices within a city removes the
# noise of individual observations (different cafes, different days), so
# the city means follow the wage more closely than single rows do -- a
# general property of correlations of aggregates, which tend to exceed the
# correlations of the units they are made of. so the two analyses answer
# different questions: "do cities with higher wages have pricier
# cappuccinos on average?" (city level, r = 0.55) versus "how well does a
# city's wage go with the price of a single observation?" (row level,
# r = 0.48). report the one that matches your question, and never mix them
#
# the cluster bootstrap resamples cities but still computes the correlation
# of ROWS, so it targets the row-level r (0.48): compare its interval with
# the naive 0.445 to 0.506 -- it should be WIDER, because rows from the same
# city carry less independent information than their count suggests (the
# city-level CI of 0.50 to 0.59 is not comparable: it belongs to another
# quantity). the models later in the course (mixed models, or regression
# with clustered standard errors) handle this properly while keeping the
# rows
# ====


## WHAT WE FOUND
# ====
# - wage and price are positively correlated: Pearson r = 0.48 (95% CI 0.45
#   to 0.51; bootstrap 0.43 to 0.52), r^2 = 0.23, Spearman rho = 0.44,
#   Kendall tau = 0.32; the permutation p-value is below 0.0005
# - Spearman is below Pearson, and the log-log Pearson (0.57) is above both:
#   extreme points (very low and very high wages, a few very high prices)
#   push the Pearson values around -- plot first, report the rank-based
#   value alongside, and check the robustness of the log-log result
# - the slope is about 8.6 cents per euro of hourly wage; R^2 = r^2 = 0.23
# - the index-wage correlation (-0.80) is largely produced by the ratio
#   itself; the index is almost uncorrelated with the price (0.04)
# - the positive association holds within every urban class (0.39, 0.31,
#   0.53), with no Simpson's paradox; in Rural and Suburban the Pearson
#   values are about twice the Spearman ones, a sign of influential points
# - only 869 different wages sit behind 2438 rows: at the city level the
#   correlation is higher (0.55), a different quantity, and the row-level
#   intervals are too narrow by the clustering
# ====


## RECAP: TWO CONTINUOUS VARIABLES
# ====
# "are the two variables linearly (or monotonically) related?" -- always
# start with the scatterplot:
#
#   what we ask                        parametric                     non-parametric
#   association?                       Pearson's r                    Spearman's rho, Kendall's tau
#   test of H0: no association         t-test on r (n - 2 df)         test on rho / tau
#   assumptions                        linear, no big outliers,       monotone relationship
#                                      (normality for small n)
#   confidence interval                Fisher's z, or bootstrap       bootstrap
#   effect size                        r, and r^2                     rho, tau
#   transformations                    change the answer (log)        do not matter (ranks)
#   without any distribution formula   permutation test               permutation test
#   many correlations                  Holm correction                Holm correction
#   picture                            scatterplot + line             scatterplot (+ ranks)
#
# r tells you the DIRECTION and STRENGTH of a linear association -- not the
# slope, not the shape, not the cause. look at the plot first, report the
# effect size with its CI, and ask whether the rows are really independent
# ====


## WHICH METHOD DO I USE? A DECISION GUIDE
# ====
#   1. two numeric variables -> the tools in this script. (a categorical and
#      a numeric variable: 01-03. two categorical variables: the previous
#      script)
#   2. plot first. is the relationship roughly a straight line, without
#      extreme points or a strong skew?
#        yes -> Pearson's r (on the raw or on the log scale: say which)
#        no, but monotone (curved, or outliers) -> Spearman or Kendall
#        no, and not monotone (a U-shape) -> neither: no correlation
#        coefficient summarizes it; model it instead
#   3. is one of the variables a ratio or a sum that contains the other?
#      -> do not correlate them
#   4. are the observations independent? (same city, same person, same
#      time series repeated?)
#        no -> aggregate to the independent unit, or use a cluster
#              bootstrap / the models later in the course
#   5. are the data pooled over groups? -> look within groups too
#   6. always report r (or rho) with a confidence interval, not only the
#      p-value
# ====


## TRY IT YOURSELF
# ====
# 1. repeat the Pearson / Spearman / Kendall comparison with cap_eur and
#    wage_eur on the log scale, and then in each urban class. which
#    coefficients change, and which do not?
# 2. add the single most extreme wage to a copy of the data with a
#    cap_eur of 0.01 (an outlier, by construction). how much does Pearson's
#    r move, and how much does Spearman's rho? what does this tell you
#    about when to use which?
# 3. in the ratio section, change the sdlog of the wage from 0.4 to 0.1
#    (or to 0.8) and watch the correlation of the index with the wage.
#    can you predict the sign of the change before running it?
# 4. in the permutation test, shuffle the prices WITHIN each urban class
#    instead of across all rows (use group_by() and sample()). why is this
#    a more sensible null hypothesis when the groups differ in wages?
# 5. compute the cluster bootstrap for Spearman's rho instead of Pearson's
#    r. does clustering change the conclusion?
# 6. fit lm(cap_eur ~ wage_eur + urban_class) and compare the wage slope
#    with the one from the simple regression. what happened, and which
#    section of this script predicted it?
# ====