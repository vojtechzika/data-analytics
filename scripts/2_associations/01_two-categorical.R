# ===================================================================
# 01_two-categorical.R -- ASSOCIATION BETWEEN TWO CATEGORICAL VARIABLES:
# contingency tables, the chi-square test of independence (by hand and
# with chisq.test), expected counts and when the test is trustworthy,
# effect size (Cramer's V), standardized residuals as a post hoc,
# Fisher's exact test for small samples (and a simulation of why it
# exists), and pairwise post hoc tests with odds ratios and a CI graph --
# including when NOT to run them
# ===================================================================

source("scripts/00_setup.R")

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

n_boot <- 2000   # same as in the previous scripts

# we need two CATEGORICAL variables. we already have one, urban_class; the
# second one we create by cutting the price into categories:
#   price_level  low / mid / high -- tertiles of cap_eur (three categories)
#   expensive    above / not above the median price      (two categories)
#   mid_price    mid / not mid (low and high merged)     (two categories)
# cutting a numeric variable throws information away -- we do it here only
# because we want to practice tests for categorical data on variables we
# already know. mid_price is special: it is defined from price_level, and
# we will only USE it in the last section, after the residuals have shown
# us where to look -- see the warning there about data-driven categories
df_cat <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"), !is.na(cap_eur)) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")),
         price_level = cut(cap_eur,
                           breaks = quantile(cap_eur, c(0, 1/3, 2/3, 1)),
                           include.lowest = TRUE,
                           labels = c("low", "mid", "high")),
         expensive   = factor(cap_eur > median(cap_eur),
                              levels = c(FALSE, TRUE),
                              labels = c("not expensive", "expensive")),
         mid_price   = factor(price_level == "mid",
                              levels = c(FALSE, TRUE),
                              labels = c("not mid", "mid")))

# (if quantile() returns repeated values because of many identical prices,
# cut() will complain about non-unique breaks -- then use fewer categories)
#
# note that the three price levels will NOT be exactly equal thirds (the
# column totals below are 813 / 874 / 751): prices are rounded, so many
# observations share the same value and a tie at a breakpoint has to land
# in one category. the median split (expensive) comes out exactly 50/50


## STEP 0: LOOK AT THE DATA FIRST -- THE CONTINGENCY TABLE
# ====
# two categorical variables are summarized in a CONTINGENCY TABLE (a
# cross-tab): rows = categories of one variable, columns = categories of the
# other, each cell = the number of observations with that combination.
# there is no mean or sd here -- counts and proportions are all we have
# ====

tab_cat <- with(df_cat, table(urban_class, price_level))
tab_cat

addmargins(tab_cat)   # with row and column totals

# row proportions: within each urban class, what share falls in each price
# level? this is the version to READ -- it compares the groups on the same
# scale even though the groups have different sizes
round(prop.table(tab_cat, margin = 1), 3)

ggplot(df_cat, aes(x = urban_class, fill = price_level)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "share of observations", fill = "price level",
       title = "Price level by urban class") +
  theme_minimal()

# if the three bars looked identical, the variables would be unrelated
# (independent). the test below asks whether the differences we see are
# larger than what chance alone would produce
#
# what we see in our data: Urban is spread almost evenly over the three
# levels (34% / 34% / 32%), while Rural concentrates in the middle (45% mid,
# only 26% low) and Suburban sits in between. the differences are visible
# but modest -- and note that Rural has only 140 observations, so its
# percentages are the least precise


## THE QUESTION AND THE HYPOTHESES
# ====
#   H0: urban_class and price_level are INDEPENDENT
#       (the distribution of price levels is the same in every urban class)
#   H1: they are NOT independent (the distribution differs somewhere)
#
# same logic as the omnibus ANOVA: the test is NOT directional and does not
# say WHICH cells or categories are responsible. for that we need a post hoc
# (standardized residuals, below). and as with correlation, "association"
# is not causation -- the test is symmetric, neither variable is the outcome
# ====


## THE CHI-SQUARE TEST OF INDEPENDENCE: THE LOGIC, BY HAND
# ====
# if the two variables were independent, what counts would we EXPECT in each
# cell? the share of a row times the share of a column times n:
#
#   expected = (row total * column total) / n
#
# the test then compares OBSERVED (O) and EXPECTED (E) counts:
#
#   chi^2 = sum over all cells of (O - E)^2 / E
#
# every cell contributes how far it is from the independence scenario,
# scaled by the expected count (a gap of 10 matters more when we expected 20
# than when we expected 200). if H0 is true, chi^2 is small. it is compared
# against the chi-square distribution with (r - 1) * (c - 1) degrees of
# freedom (r rows, c columns). like the F-test, only LARGE values are
# evidence against H0, so the p-value is the upper tail
# ====

n_obs    <- sum(tab_cat)
expected <- outer(rowSums(tab_cat), colSums(tab_cat)) / n_obs
round(expected, 1)

chi_stat <- sum((tab_cat - expected)^2 / expected)
df_chi   <- (nrow(tab_cat) - 1) * (ncol(tab_cat) - 1)
p_chi    <- pchisq(chi_stat, df_chi, lower.tail = FALSE)
c(chi_sq = chi_stat, df = df_chi, p = p_chi)

# result: chi^2 = 11.42, df = 4, p = 0.022. the expected counts (46.7 / 50.2
# / 43.1 for Rural, etc.) are what a table with the same row and column
# totals would look like if price level had nothing to do with urban class.
# the biggest gaps between observed and expected are in the "mid" column:
# Rural 63 observed vs 50.2 expected, Urban 584 vs 615.5


## THE SAME THING WITH chisq.test()
# ====
# chisq.test() on a table does the test of independence. it should match
# the by-hand numbers above (for tables larger than 2x2 -- see the note on
# the continuity correction below)
# ====

chi_cat <- chisq.test(tab_cat)
chi_cat

chi_cat$expected   # the same expected counts as above
chi_cat$observed

# the pipe-friendly version (rstatix) -- takes the table as well
chisq_test(tab_cat)

# reading: p < 0.05 -> reject H0: the two variables are associated (found
# support for H1). p > 0.05 -> no support for an association, and we stop
# here: no post hoc
#
# here: chisq.test() reproduces the by-hand numbers (X-squared = 11.42,
# df = 4, p = 0.0222), and the rstatix version adds n = 2438. p < 0.05, so we
# reject independence: price level and urban class are associated, and we
# may go on to the post hoc. (note: rstatix::chisq_test() has no `detailed`
# argument -- for expected counts and residuals use the base R object,
# chi_cat$expected, chi_cat$stdres)


## ASSUMPTIONS: WHEN CAN WE TRUST THE CHI-SQUARE TEST?
# ====
# the p-value comes from the chi-square DISTRIBUTION, which is only an
# approximation of the real distribution of the statistic. it works well
# when the expected counts are not too small:
#
#   rule of thumb: all expected counts >= 5
#   (a looser version: no expected count below 1, and at most 20% of cells
#   below 5)
#
# note: it is the EXPECTED counts that matter, not the observed ones. R
# warns you ("Chi-squared approximation may be incorrect") when the rule
# is violated. other assumptions: (1) observations are independent, (2) each
# observation falls in exactly one cell (categories are mutually exclusive),
# (3) we analyze COUNTS, never percentages
# ====

min(chi_cat$expected)   # the smallest expected count
mean(chi_cat$expected < 5)   # share of cells below 5

# result: the smallest expected count is 43.1 (Rural / high) and 0% of the
# cells are below 5 -- the rule is met with a wide margin, even for the
# small Rural group, so the chi-square approximation is fine here (and the
# simulated p-values at the end of the script confirm it)

# with thousands of observations, all expected counts are large and the
# approximation is excellent -- which is also why the test then flags even
# tiny associations as significant. so we also need an effect size


## EFFECT SIZE: CRAMER'S V
# ====
# chi^2 grows with the sample size, so it is not an effect size. Cramer's V
# rescales it to a 0-1 scale:
#
#   V = sqrt( chi^2 / ( n * min(r - 1, c - 1) ) )
#
# 0 = independence, 1 = perfect association (every row has its own
# category). rough conventions DEPEND ON THE TABLE SIZE, via
# df* = min(r - 1, c - 1):
#
#   df* = 1  (2xK table)  0.10 small, 0.30 medium, 0.50 large
#   df* = 2  (3x3 table)  0.07 small, 0.21 medium, 0.35 large
#   df* = 3               0.06 small, 0.17 medium, 0.29 large
#
# for a 2x2 table, V is the same as the phi coefficient (the correlation of
# two binary variables). V has no sign: it measures strength, not direction
# ====

sqrt(unname(chi_cat$statistic) / (n_obs * (min(dim(tab_cat)) - 1)))

cramer_v(tab_cat)   # rstatix, should agree

# result: V = 0.048, and rstatix agrees. with df* = 2 the threshold for a
# "small" effect is 0.07, so this association is below even small: it is
# statistically significant (p = 0.022) but practically very weak. this is
# the same lesson as eta^2 in 03 -- with 2438 observations a tiny departure
# from independence is already detectable, so ALWAYS report the effect size
# next to the p-value


## POST HOC: WHICH CELLS DRIVE THE RESULT? STANDARDIZED RESIDUALS
# ====
# a significant chi-square says "somewhere". to see WHERE, look at each
# cell's STANDARDIZED RESIDUAL: (O - E) scaled by its standard error, so
# that under H0 it behaves roughly like a standard normal variable (z-score).
#
#   > 0  the cell has MORE observations than independence would predict
#   < 0  FEWER than predicted
#   |z| > 2  is the classic rough flag (about 5% of cells will exceed it
#            by chance alone!)
#
# so, as with ANOVA, we face multiple testing: with r*c cells, some will
# cross |z| = 2 by chance. below we turn the residuals into p-values and
# adjust them (Holm, as in 03) over all cells
# ====

round(chi_cat$stdres, 2)

stdres_df <- as.data.frame(as.table(chi_cat$stdres)) %>%
  rename(stdres = Freq) %>%
  mutate(p_raw  = 2 * pnorm(-abs(stdres)),                  # two-tailed
         p_holm = p.adjust(p_raw, method = "holm"))
stdres_df

# reading: a cell with a significant adjusted p is over-represented (positive
# residual) or under-represented (negative) relative to independence. the
# cell-wise approximation is rough and the cells are not independent of one
# another, so treat this as a guide to WHERE to look, not as a final verdict
#
# result: the |z| > 2 rule of thumb flags two cells, Rural / mid (+2.33) and
# Urban / mid (-2.92). after the Holm correction only Urban / mid survives
# (adjusted p = 0.032); Rural / mid goes from p = 0.020 to p = 0.16. so the
# defensible statement is: mid-priced cappuccinos are UNDER-represented in
# Urban areas (584 observed vs 616 expected). the Rural excess of mid prices
# is suggestive, but with only 140 rural observations we cannot separate it
# from chance once we account for testing nine cells. the one surviving cell
# is what we follow up on in the last section of the script

ggplot(stdres_df, aes(x = price_level, y = urban_class, fill = stdres)) +
  geom_tile() +
  geom_text(aes(label = round(stdres, 1))) +
  scale_fill_gradient2(low = "#2a78d6", mid = "white", high = "#e34948", midpoint = 0) +
  labs(x = "price level", y = NULL, fill = "std. residual",
       title = "Standardized residuals: red = more than expected under independence") +
  theme_minimal()


## THE 2x2 TABLE: TWO GROUPS, A BINARY OUTCOME
# ====
# the smallest table: Rural vs Urban, expensive vs not expensive. this is
# the categorical cousin of the two-sample tests from 02: instead of
# comparing means, we compare PROPORTIONS (the share of "expensive" in each
# group). for a 2x2 table the test of independence is the same thing as the
# two-sample test of proportions
# ====

df_two <- df_cat %>%
  filter(urban_class %in% c("Rural", "Urban")) %>%
  droplevels()

tab_2x2 <- with(df_two, table(urban_class, expensive))
tab_2x2
round(prop.table(tab_2x2, margin = 1), 3)   # share of "expensive" in each group

# result: 53.6% of Rural observations are expensive vs 50.0% of Urban ones
# -- a difference of 3.5 percentage points, from 140 vs 1717 observations

# effect size #1: the difference in proportions (in percentage points)
# effect size #2: the ODDS RATIO, (a * d) / (b * c) for a table
#
#                     not expensive   expensive
#       Rural              a              b
#       Urban              c              d
#
# the odds = P(expensive) / P(not expensive) within a group; the odds ratio
# compares the odds in Urban to the odds in Rural. OR = 1 -> independence,
# OR > 1 -> the odds of "expensive" are higher in Urban. careful: an odds
# ratio of 2 does NOT mean "twice as likely" (that would be a risk ratio),
# the two are similar only when the outcome is rare
cells <- as.vector(t(tab_2x2))   # a, b, c, d: row by row
(cells[1] * cells[4]) / (cells[2] * cells[3])   # sample odds ratio

# result: OR = 0.868. the odds of "expensive" in Urban are about 13% LOWER
# than in Rural (an OR below 1 means the second group, Urban, has lower
# odds). direction matters here: the same table with the rows swapped gives
# 1 / 0.868 = 1.15

# the continuity correction (Yates): for 2x2 tables only, chisq.test()
# applies a correction BY DEFAULT that makes the test slightly more
# conservative, to compensate for approximating a discrete distribution by a
# continuous one. many statisticians consider it unnecessary (it is overly
# conservative); with large samples it makes no difference
chisq.test(tab_2x2)                   # with Yates correction (default for 2x2)
chisq.test(tab_2x2, correct = FALSE)  # without -- the plain formula from above

# result: X-squared = 0.52 (p = 0.473) with the correction, 0.65 (p = 0.420)
# without. the correction pulls the statistic down and the p-value up, but
# the verdict is the same: no evidence that Rural and Urban differ in the
# share of expensive cappuccinos

# the test of two proportions gives the same chi-square (the z statistic
# squared), plus a CI for the DIFFERENCE in proportions
prop.test(tab_2x2[, "expensive"], rowSums(tab_2x2), correct = FALSE)

# result: same X-squared (0.65) and p (0.420) as the chi-square test above.
# the estimates are prop 1 = Rural = 53.6% and prop 2 = Urban = 50.0%, so the
# CI (-5.1, +12.1 percentage points) is for Rural MINUS Urban. it contains 0,
# which is the same verdict as the p-value -- but notice how wide it is:
# the data are compatible with anything from Urban being 5 points MORE
# expensive to Rural being 12 points more expensive


## P-VALUES DEPEND ON n, EFFECT SIZES DO NOT: THE SAME PROPORTIONS, MORE DATA
# ====
# what would the 2x2 table look like with the SAME proportions (53.6% vs
# 50.0%) but 4, 10 or 25 times as many observations? multiplying every cell
# by a constant keeps all the proportions, and so the effect size, exactly
# as they are -- only the sample size changes
# ====

mult <- c(1, 4, 10, 25)
data.frame(multiplier = mult,
           n   = sum(tab_2x2) * mult,
           p   = sapply(mult, function(k) chisq.test(tab_2x2 * k, correct = FALSE)$p.value),
           phi = sapply(mult, function(k) unname(sqrt(chisq.test(tab_2x2 * k, correct = FALSE)$statistic /
                                                        (sum(tab_2x2) * k)))))

# result: phi is 0.019 in every row, but p goes from 0.42 (our data) to 0.11
# (4 times as many observations), 0.011 (10 times) and 0.00006 (25 times).
# the chi-square statistic grows in proportion to n while the association
# stays the same, so with enough data ANY difference becomes "significant".
# a p-value answers "is there a difference?", never "does it matter?" --
# that is the job of phi / V / the odds ratio, and of your judgement about
# what size of difference is worth caring about (3.5 percentage points of
# expensive cappuccinos probably isn't)


## FISHER'S EXACT TEST: WHEN THE SAMPLE IS SMALL
# ====
# the chi-square test is an APPROXIMATION and breaks down when expected
# counts are small. Fisher's exact test needs no approximation: it takes the
# row and column totals as FIXED and asks how likely it is to see a table
# as extreme as ours (or more) among ALL tables with the same totals, using
# the hypergeometric distribution. the p-value is exact -- for any sample
# size, small or large.
#
# the price: it is classically a 2x2 tool (R can do larger tables, but with
# heavy computation), and it is conservative. with huge samples it gives the
# same answer as chi-square, so it is only NEEDED for small samples
#
# to see it in action, we draw a SMALL random sample from our data: 8
# observations per group, so that some expected counts in a 2x2 table fall
# below the rule of thumb. results change with the seed -- the point is the
# comparison
# ====

set.seed(123)
df_small <- df_two %>%
  group_by(urban_class) %>%
  slice_sample(n = 8) %>%
  ungroup()

tab_small <- with(df_small, table(urban_class, expensive))
tab_small
chisq.test(tab_small)$expected   # expected counts: some below 5

chisq.test(tab_small)   # R warns -- the approximation is not trustworthy here
chisq.test(tab_small, correct = FALSE)   # the same table without the correction
fisher.test(tab_small)  # exact p-value, plus an odds ratio with a 95% CI

# result (seed 123): the sample table is 5 / 3 (Rural) and 6 / 2 (Urban).
# the expected counts are 5.5 and 2.5, so the "expensive" column is below 5
# and R warns. the p-values are:
#   chi-square with Yates correction    p = 1       (X-squared = 0)
#   chi-square without correction       p = 0.59    (X-squared = 0.29)
#   Fisher's exact test                 p = 1       (exact)
# so the uncorrected approximation is off (0.59 vs the exact 1), and the
# Yates-corrected one happens to land on the exact answer here -- the
# correction is exactly what makes the chi-square test safer for small
# tables. the real message is in the Fisher CI for the odds ratio: 0.03 to
# 7.4. with 16 observations the data are compatible with a huge effect in
# EITHER direction, so "p = 1" means "we learned nothing", not "no
# difference". also notice that Fisher's odds ratio (0.58) differs from the
# sample one, (5 * 2) / (3 * 6) = 0.56

# Fisher's test by hand. with the margins fixed (8 Rural, 8 Urban; 11 "not
# expensive", 5 "expensive"), the 8 Rural observations are like 8 cards
# drawn from a deck of 16 that holds 11 "not expensive" ones. how many "not
# expensive" cards should we expect to draw, and how likely is each count?
# that is the hypergeometric distribution, dhyper():
n_not_exp <- sum(tab_small[, "not expensive"])
n_exp     <- sum(tab_small[, "expensive"])
n_rural   <- sum(tab_small["Rural", ])

fisher_dist <- data.frame(not_expensive_in_rural = 0:n_rural,
                          prob = dhyper(0:n_rural, m = n_not_exp, n = n_exp, k = n_rural))
fisher_dist   # the probability of every possible table with these margins

p_observed <- dhyper(tab_small["Rural", "not expensive"], n_not_exp, n_exp, n_rural)
sum(fisher_dist$prob[fisher_dist$prob <= p_observed + 1e-10])   # the two-sided p-value

# reading the distribution: our table (5 "not expensive" among the Rural 8)
# is the MOST likely one (probability 0.359, tied with 6) -- and the p-value
# adds up the probabilities of all tables that are at most as likely as ours.
# since ours is the most likely, that is ALL of them, so p = 1. this is the
# whole test: no approximation, just counting. the tables that would have
# been surprising (only 3 or 8 "not expensive" among the Rural 8, about 1%
# each) are the ones that would give small p-values

# reading the Fisher output:
# - p-value: exact, two-sided (sums the probabilities of all tables at most
#   as likely as ours, given the totals)
# - odds ratio: NOT the same as the sample odds ratio (a*d)/(b*c) -- it is a
#   conditional maximum-likelihood estimate, which is better behaved in
#   small tables (and finite even when one cell is zero)
# - CI: for the odds ratio. if it contains 1, we cannot reject independence
# - alternative = "greater" / "less" gives one-tailed versions (we stay with
#   two-tailed throughout)

# the same test on the FULL data: with large samples Fisher and chi-square
# agree, so there is no reason to prefer Fisher except small counts
fisher.test(tab_2x2)
chisq.test(tab_2x2)

# result: Fisher p = 0.430, chi-square with correction p = 0.473, without
# correction p = 0.420 (above). all three agree on the verdict, as expected
# with 2x2 counts this large. Fisher's odds ratio is 0.868 (the sample one
# is 0.8677 -- the conditional estimate is nearly identical when counts are
# big), with a 95% CI of 0.60 to 1.24: it contains 1, in line with the
# p-value, and it is much more informative than "p > 0.05" alone -- the
# odds of "expensive" in Urban may be anything from 40% lower to 24% higher

# bigger than 2x2: fisher.test() also accepts r x c tables, but may run out
# of workspace; a Monte Carlo version is then the practical choice
# (resamples tables with the same margins)
set.seed(123)   # simulated p-values change from run to run, so fix the seed
fisher.test(tab_cat, simulate.p.value = TRUE, B = n_boot)

# the same logic works for the chi-square test as well: a Monte Carlo
# p-value that does not rely on the chi-square distribution
chisq.test(tab_cat, simulate.p.value = TRUE, B = n_boot)

# result: simulated p-values of about 0.02 for both (0.024 for Fisher and
# 0.019 for chi-square with set.seed(123)), against 0.022 from the
# chi-square distribution. they will not match to the last digit (each is
# based on 2000 random tables, so expect noise of a few thousandths, and
# your numbers will differ slightly from these), but the
# agreement confirms that the asymptotic p-value is trustworthy for this
# table. with all expected counts above 43 we would not expect otherwise


## WHY THE CHOICE OF TEST MATTERS FOR SMALL SAMPLES: A SIMULATION
# ====
# the same idea as the FWER simulation in 03: make H0 TRUE, so that every
# "significant" result is a false alarm, and count how often each test
# raises one. here: two groups of 8, both with a 50% chance of "expensive",
# so there is truly no association. a test that keeps its promise should
# raise a false alarm in about 5% of the samples
# ====

set.seed(123)
sim_small <- replicate(5000, {
  x1  <- rbinom(1, 8, 0.5)
  x2  <- rbinom(1, 8, 0.5)
  tab <- matrix(c(8 - x1, x1, 8 - x2, x2), nrow = 2, byrow = TRUE)
  # chisq.test() returns NaN when a column is empty; isTRUE() turns that into FALSE
  p_plain <- suppressWarnings(chisq.test(tab, correct = FALSE)$p.value)
  p_yates <- suppressWarnings(chisq.test(tab)$p.value)
  p_fish  <- fisher.test(tab)$p.value
  c(chisq_plain = isTRUE(p_plain < 0.05),
    chisq_yates = isTRUE(p_yates < 0.05),
    fisher      = p_fish < 0.05)
})
rowMeans(sim_small)   # false-alarm rates

# expect roughly 0.08 for the plain chi-square test, and about 0.02 for both
# the Yates-corrected chi-square and Fisher's test. two lessons:
# - the plain chi-square test is too LIBERAL with small counts: it cries
#   wolf in about 8% of samples although we asked for 5%
# - "exact" does not mean "exactly 5%". Fisher's test guarantees AT MOST 5%,
#   and with a small, discrete table it is far below that (about 2%). it
#   never over-promises, but it pays with POWER: it misses real differences
#   more often. the Yates correction behaves almost the same way at this
#   size. so with small counts the question is not which test is "right",
#   but which kind of error you can live with
# (with our large samples none of this matters: the three tests agree)


## WHEN TO STOP AND WHEN TO GO ON: POST HOC FOR TABLES WITH MORE THAN TWO GROUPS
# ====
# with three urban classes the logic is the same as with ANOVA in 03: an
# omnibus test first, and pairwise comparisons ONLY if the omnibus test is
# significant. each pair is a 2x2 table, so Fisher's exact test (or the test
# of proportions) is the tool, and the p-values are adjusted with Holm
# (3 comparisons -> the FWER problem from 03 applies here too).
#
# two examples with OPPOSITE outcomes. first, the binary outcome from the
# 2x2 section, "expensive" (above the median price), now for all three
# urban classes
# ====

tab_3x2 <- with(df_cat, table(urban_class, expensive))
tab_3x2
round(prop.table(tab_3x2, margin = 1), 3)

chisq.test(tab_3x2)   # omnibus first

# result: 53.6% / 49.1% / 50.0% of Rural / Suburban / Urban observations are
# expensive, and the omnibus test gives chi^2 = 0.92, df = 2, p = 0.63. there
# is nothing to follow up, so we STOP here. running the three pairwise tests
# anyway would only give us three more chances of a false alarm (that is the
# whole point of the FWER section in 03). a null omnibus result is a result,
# not a failure of the analysis: the share above the median price does not
# differ between the urban classes


## SECOND EXAMPLE: THE MIDDLE OF THE PRICE DISTRIBUTION
# ====
# the 3x3 table told us the action is in the "mid" column: Urban was under-
# represented there. so we ask a sharper question: is a MID price (rather
# than a low or a high one) equally common in all three urban classes? the
# outcome is now mid_price = mid / not mid
#
# a WARNING about what we just did: we chose to merge "low" and "high" into
# "not mid" AFTER looking at the standardized residuals. category choices
# made after seeing the data inflate the false-positive rate (the "garden
# of forking paths": with enough ways to cut the table, some cut will look
# significant). so treat everything below as EXPLORATORY -- a hypothesis
# worth testing on new data, not a confirmed finding. a comparison fixed
# BEFORE looking at the data would not have this problem.
#
# notice also the contrast with the median split above: the SAME prices,
# cut at the median, showed nothing; cut around the middle, they show
# something. how a numeric variable is cut into categories can change the
# story, which is why cutting is a last resort (later in the course, price
# stays numeric and goes into a regression)
# ====

tab_mid <- with(df_cat, table(urban_class, mid_price))
tab_mid
round(prop.table(tab_mid, margin = 1), 3)   # share of mid prices in each class

chisq.test(tab_mid)   # omnibus first
cramer_v(tab_mid)     # df* = 1 here (a 3x2 table): small = 0.10

# result: 45.0% of Rural, 39.1% of Suburban and 34.0% of Urban observations
# are mid-priced. omnibus: chi^2 = 10.24, df = 2, p = 0.006 -- significant,
# so we may continue. Cramer's V = 0.065 is below the "small" threshold of
# 0.10 for this table size: a real but weak association, which matches the
# 3x3 result. now WHICH classes differ?

pairwise_fisher_test(tab_mid, p.adjust.method = "holm")
pairwise_prop_test(tab_mid, p.adjust.method = "holm")   # chi-square based alternative

# result (Fisher; the proportion test gives very similar numbers):
#
#   pair                raw p     Holm p
#   Rural - Suburban    0.213     0.213
#   Rural - Urban       0.0098    0.029     <- significant
#   Suburban - Urban    0.031     0.062     <- NOT significant after Holm
#
# this is Holm at work, exactly as in 03: sort the raw p-values, multiply the
# smallest by 3 (0.0098 * 3 = 0.029), the next by 2 (0.031 * 2 = 0.062), the
# largest by 1. Suburban vs Urban has a raw p below 0.05 and would have been
# declared "significant" by an uncorrected test, but with three comparisons
# on the table it is not strong enough. use p.adj for the verdict

# the p-values say WHETHER; the picture should say HOW MUCH. the effect size
# for a pair of groups is the odds ratio, and just as Tukey gave simultaneous
# CIs in 03, we make the CIs match the correction: with m = 3 comparisons we
# use a 1 - 0.05 / 3 = 98.3% interval (Bonferroni), so that all three
# intervals together keep 95% coverage. fisher.test() gives the odds ratio
# and its CI for each pair; the odds ratio compares the SECOND group to the
# first, as in the 2x2 section

pairs_mid <- combn(rownames(tab_mid), 2, simplify = FALSE)
m_pairs   <- length(pairs_mid)

or_res <- bind_rows(lapply(pairs_mid, function(pr) {
  ft <- fisher.test(tab_mid[pr, ], conf.level = 1 - 0.05 / m_pairs)
  data.frame(comparison = paste(pr[2], "vs", pr[1]),
             odds_ratio = unname(ft$estimate),
             conf.low   = ft$conf.int[1],
             conf.high  = ft$conf.int[2],
             p_raw      = ft$p.value)
})) %>%
  mutate(p_holm = p.adjust(p_raw, method = "holm"))
or_res

# the same layout as the Tukey plot in 03: comparisons on the vertical axis,
# the effect on the horizontal one, a dashed line at "no effect". for odds
# ratios "no effect" is 1 (not 0), and the axis is logarithmic so that an OR
# of 0.5 and an OR of 2 are the same distance from 1
ggplot(or_res, aes(x = reorder(comparison, odds_ratio), y = odds_ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#e34948") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1, color = "#2a78d6") +
  geom_point(size = 2, color = "#2a78d6") +
  geom_text(aes(y = conf.high, label = paste0("Holm p = ", signif(p_holm, 2))),
            hjust = -0.15, size = 3.2) +
  scale_y_log10(expand = expansion(mult = c(0.05, 0.4))) +
  coord_flip() +
  labs(x = NULL, y = "odds ratio of a mid price (log scale)",
       title = "Odds of a mid price: pairwise odds ratios with simultaneous 95% CIs",
       subtitle = "OR < 1: the second group is less likely to have a mid price. exploratory (see text)") +
  theme_minimal()

# result: Urban vs Rural has an OR of about 0.63 -- the odds of a mid price
# are roughly 37% lower in Urban than in Rural areas -- and its interval
# (about 0.41 to 0.98) stays just below the dashed line. Suburban vs Urban
# (OR about 0.80) has an interval that reaches past 1, in line with its Holm
# p of 0.062, and Suburban vs Rural (OR about 0.78) clearly includes 1. the
# ordering of the three classes is also informative: the more urban the
# class, the lower the share of mid prices, so the pairs line up as a
# gradient rather than as one odd group out. remember: odds ratios are
# odds, not probabilities -- the same result in plain words is "45% vs 34%"


## A NOTE ON INDEPENDENCE
# ====
# as in 02 and 03: the data contain repeated cities (several rows from the
# same city), so the observations are not independent. both the chi-square
# and Fisher's test assume independence, so the p-values here are somewhat
# optimistic. the models later in the course handle this properly.
# (and when the SAME units are measured twice, for example the same person
# answering yes/no before and after a treatment, the observations are
# PAIRED and the right test is McNemar's test, mcnemar.test() -- not the
# ones in this script)
# ====


## WHAT WE FOUND
# ====
# - urban class and price level (3 x 3): associated, chi^2(4) = 11.42,
#   p = 0.022 -- but the effect is tiny, Cramer's V = 0.048 (below "small")
# - the association comes from ONE cell: mid-priced cappuccinos are
#   under-represented in Urban areas (standardized residual -2.92, Holm-
#   adjusted p = 0.032); nothing else survives the correction
# - Rural vs Urban, share of expensive cappuccinos (2 x 2): 53.6% vs 50.0%,
#   no evidence of a difference (Fisher p = 0.43, OR = 0.87, 95% CI 0.60 to
#   1.24) -- and the CI shows how little we can rule out with 140 rural
#   observations
# - all three urban classes vs expensive (3 x 2): omnibus p = 0.63, so we
#   stopped: no post hoc
# - all three urban classes vs mid price (3 x 2, EXPLORATORY): omnibus
#   p = 0.006, V = 0.065; after Holm only Rural vs Urban differs (p = 0.029,
#   OR about 0.63), Suburban vs Urban does not (p = 0.062)
# - small samples: with 8 + 8 observations the tests cannot detect anything
#   (Fisher CI for the OR: 0.03 to 7.4), the plain chi-square is too liberal
#   and Fisher's test is conservative
# - the same proportions with 10 times as many observations would be
#   "significant": p-values depend on n, the effect size does not
# ====


## RECAP: TWO CATEGORICAL VARIABLES
# ====
# "are the two variables independent?" -- one omnibus test, then, only if
# it is significant, a look at where the association comes from:
#
#   what we ask                        large samples                  small samples
#   omnibus: association?              chi-square test (chi^2)        Fisher's exact test
#   assumption to check                expected counts >= 5           (none -- exact)
#   effect size                        Cramer's V; for 2x2: phi,      odds ratio (with CI)
#                                      odds ratio, difference of
#                                      proportions
#   post hoc: which cells?             standardized residuals         --
#                                      (Holm-adjusted)
#   post hoc: which pairs?             pairwise prop. tests (Holm)    pairwise Fisher (Holm)
#   picture                            100% stacked bars; residual    --
#                                      heatmap; odds ratio plot
#
# chi-square tells you THAT the variables are associated, the residuals and
# the pairwise tests tell you WHERE, and Cramer's V / the odds ratio tell
# you HOW MUCH. and, as always, "associated" does not mean "caused"
# ====


## WHICH TEST DO I USE? A DECISION GUIDE
# ====
#   1. two categorical variables, observations counted in a table
#        -> the tools in this script. (a categorical and a NUMERIC variable:
#           back to 01-03. two numeric variables: the next script)
#   2. are the observations independent (different units, not the same
#      units measured twice, not clustered)?
#        no, paired yes/no data -> McNemar's test
#        no, clustered (cities) -> models later in the course
#   3. are all expected counts >= 5?
#        yes -> chi-square test (the Yates correction for 2x2 is optional)
#        no  -> Fisher's exact test (2x2), or a Monte Carlo p-value for
#               larger tables; think about merging sparse categories, but
#               do it for substantive reasons and BEFORE looking at results
#   4. is the table larger than 2x2 and the omnibus test significant?
#        -> standardized residuals (which cells), pairwise 2x2 tests with
#           Holm (which groups). if the omnibus test is not significant, stop
#   5. always report an effect size next to the p-value: Cramer's V for the
#      table, an odds ratio or a difference in proportions for 2x2 -- with CIs
# ====


## TRY IT YOURSELF
# ====
# 1. the pairwise tests in the last section compared the groups on mid_price.
#    redo the omnibus test and the pairwise tests with price_level == "high"
#    as the outcome instead. predict first: which classes should differ,
#    given the heatmap of residuals? were you right?
# 2. cut cap_eur into FOUR or FIVE quantile levels instead of three and
#    repeat the 3 x K test of independence. do the conclusion and Cramer's V
#    survive a different number of categories?
# 3. in the Fisher section, change n in slice_sample() to 20, 50 and 140 and
#    watch what happens to the confidence interval of the odds ratio. how
#    large must the groups be before the interval stops containing 1 for
#    the difference we saw in the full data?
# 4. use prop.table(tab_cat, margin = 2) instead of margin = 1. which
#    question does the column version answer, and why is it the wrong one
#    for "does the price level depend on the urban class?"
# 5. in the odds ratio plot, replace Holm by Bonferroni in the p-values
#    (p.adjust.method = "bonferroni"). does any verdict change? why does
#    Holm never do worse than Bonferroni?
# ====