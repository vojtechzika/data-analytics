# ===================================================================
# 03_three-sample.R — comparing THREE groups (Rural / Suburban / Urban):
# why not many t-tests, one-way ANOVA (cap_eur), Welch ANOVA,
# Kruskal-Wallis (cap_index), post hoc tests with corrections for
# multiple testing, and post hoc comparison via CI graphs
# ===================================================================

source("scripts/00_setup.R")

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

# all three urban classes this time (nothing is dropped). the level order
# is the natural one -- from least to most urban -- so that differences in
# the post hoc tests read as "more urban minus less urban"
# each variable gets its own data frame, since NAs are stripped per variable
df_three <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"), !is.na(cap_eur)) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")))

df_idx <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"), !is.na(cap_index)) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")))

df_three %>% count(urban_class)   # group sizes -- ANOVA does NOT need equal n, but see Levene below

n_boot <- 2000   # same as in the previous scripts


## STEP 0: LOOK AT THE DATA FIRST -- TWO VARIABLES, TWO ANALYSES
# ====
# we will analyze two different variables, because they call for different tools:
#   cap_eur    the price in euros            -> classic (parametric) ANOVA
#   cap_index  cap_eur / wage_eur, i.e. the price of a cappuccino as a share
#              of an hour's wage             -> non-parametric (Kruskal-Wallis)
# a RATIO like cap_index is typically heavily right-skewed with extreme
# values (a low wage in the denominator makes the index explode), which is
# exactly the situation where means -- and so ANOVA -- become unreliable.
# the plots below show this already
# ====

ggplot(df_three, aes(x = urban_class, y = cap_eur, fill = urban_class)) +
  geom_boxplot(alpha = 0.6, show.legend = FALSE) +
  labs(x = NULL, y = "cap_eur", title = "Cappuccino price by urban class") +
  theme_minimal()

ggplot(df_idx, aes(x = urban_class, y = cap_index, fill = urban_class)) +
  geom_boxplot(alpha = 0.6, show.legend = FALSE) +
  labs(x = NULL, y = "cap_index", title = "Cappuccino index by urban class") +
  theme_minimal()

df_three %>%
  group_by(urban_class) %>%
  summarise(n = n(), mean = mean(cap_eur), median = median(cap_eur), sd = sd(cap_eur))

df_idx %>%
  group_by(urban_class) %>%
  summarise(n = n(), mean = mean(cap_index), median = median(cap_index), sd = sd(cap_index))


## THE QUESTION AND THE HYPOTHESES
# ====
#   H0: mean(Rural) = mean(Suburban) = mean(Urban)    (all three equal)
#   H1: at least one mean differs from the others
#
# note what H1 does NOT say: not WHICH group differs, not in WHICH
# direction. ANOVA is an OMNIBUS test -- one answer to "is there any
# difference anywhere?". to find out where, we need post hoc tests (below)
# ====


## WHY NOT JUST RUN SEVERAL T-TESTS?
# FWER
# ====
# with three groups there are choose(3, 2) = 3 pairwise comparisons
# (R-S, R-U, S-U). every test has a 5% chance of a false positive when H0
# is true -- but the chance that AT LEAST ONE of several tests produces
# one is much higher. for m independent tests:
#
#   P(at least one false positive) = 1 - (1 - alpha)^m
#
# this is called the FAMILY-WISE ERROR RATE (FWER). the formula assumes
# independent tests (pairwise comparisons share groups, so it is only an
# approximation), but the message is right:
# ====

data.frame(groups      = 2:6,
           comparisons = choose(2:6, 2),
           fwer        = 1 - (1 - 0.05)^choose(2:6, 2))

# seeing it happen: simulate 3 groups drawn from the SAME distribution, so
# H0 is true and every "significant" result is a false alarm. how often does
# each approach raise at least one?
set.seed(123)
sim <- replicate(2000, {
  y <- rnorm(90)
  g <- factor(rep(c("A", "B", "C"), each = 30))
  p_raw  <- pairwise.t.test(y, g, p.adjust.method = "none")$p.value
  p_holm <- pairwise.t.test(y, g, p.adjust.method = "holm")$p.value
  c(three_t_tests      = any(p_raw  < 0.05, na.rm = TRUE),
    three_t_tests_holm = any(p_holm < 0.05, na.rm = TRUE),
    anova              = summary(aov(y ~ g))[[1]][["Pr(>F)"]][1] < 0.05)
})
rowMeans(sim)   # false-alarm rates

# expect roughly 0.12 for the uncorrected t-tests (not the 0.05 we think we
# are getting), and close to 0.05 for ANOVA and for the corrected tests.
# that is the whole motivation: ANOVA asks ONE question first, and post hoc
# tests then correct their p-values for asking several


## ONE-WAY ANOVA ON cap_eur: THE LOGIC, BY HAND
# ====
# ANOVA = ANalysis Of VAriance. the idea: if the group means are equal,
# the variation BETWEEN the group means should be no bigger than what you
# would expect from the variation WITHIN the groups (random noise).
#
#   total variation  =  between-group variation  +  within-group variation
#   SS_total         =  SS_between               +  SS_within
#
#   MS = SS / df           (a mean square is a variance estimate)
#   F  = MS_between / MS_within
#
# if H0 is true, both mean squares estimate the same noise and F is near 1.
# the more the group means spread out relative to the noise, the larger F.
# F is compared against the F-distribution with (k - 1, n - k) degrees of
# freedom. the F-test is inherently one-directional (large F = evidence
# against H0) but NOT directional in the sense of the groups: it cannot say
# whether Urban is higher or lower
# ====

k          <- nlevels(df_three$urban_class)
n_total    <- nrow(df_three)
grand_mean <- mean(df_three$cap_eur)

group_stats <- df_three %>%
  group_by(urban_class) %>%
  summarise(n_g = n(), mean_g = mean(cap_eur))

ss_between <- sum(group_stats$n_g * (group_stats$mean_g - grand_mean)^2)
ss_within  <- sum((df_three$cap_eur - ave(df_three$cap_eur, df_three$urban_class))^2)
ss_total   <- sum((df_three$cap_eur - grand_mean)^2)
c(ss_between + ss_within, ss_total)   # the decomposition adds up

df_between <- k - 1
df_within  <- n_total - k
ms_between <- ss_between / df_between
ms_within  <- ss_within / df_within
f_stat     <- ms_between / ms_within
p_val      <- pf(f_stat, df_between, df_within, lower.tail = FALSE)   # upper tail: large F is extreme
c(F = f_stat, p = p_val)


## THE SAME THING WITH aov()
# ====
# aov() fits the model, summary() prints the classic ANOVA table. the
# formula is the same  outcome ~ grouping variable  as in the t-tests
# ====

aov_eur <- aov(cap_eur ~ urban_class, data = df_three)
summary(aov_eur)

# reading the table (it should match the by-hand numbers above):
# - urban_class row  = BETWEEN groups: Df = k - 1, Sum Sq, Mean Sq = MS_between
# - Residuals row    = WITHIN groups:  Df = n - k, Sum Sq, Mean Sq = MS_within
# - F value, Pr(>F)  = the test statistic and its p-value
# p < 0.05 -> reject H0: at least one mean differs (found support for H1).
# p > 0.05 -> no support for a difference, and we stop here: no post hoc

# the pipe-friendly version (needs the car package installed). the "ges"
# column is the effect size, see below
df_three %>% anova_test(cap_eur ~ urban_class, detailed = TRUE)


## EFFECT SIZE FOR ANOVA: ETA SQUARED
# ====
# the share of the total variation in prices that is explained by the
# group membership:
#
#   eta^2 = SS_between / SS_total          (0 to 1)
#
# rough conventions: 0.01 small, 0.06 medium, 0.14 large. as always, with
# thousands of observations a tiny eta^2 can still give a tiny p-value
# ====

ss_between / ss_total   # eta squared; equals "ges" in the anova_test() output above


## ASSUMPTIONS: WHAT CAN GO WRONG?
# ====
# ANOVA assumes (1) independent observations, (2) roughly normal residuals
# within each group, (3) equal variances across groups ("homoscedasticity").
#
# (2) matters less with large groups, for the same CLT reason as in the
#     earlier scripts -- but only if the data aren't extremely skewed.
# (3) matters a lot when group sizes are UNEQUAL (as here): if the biggest
#     group has the smaller variance, F becomes too liberal, and vice versa.
#
# first, look at the residuals of both models. a QQ-plot of residuals (see
# 05_assessing-normality.R) should hug the line. the cap_index model is
# fitted ONLY to look at its diagnostics -- we won't interpret that ANOVA
# ====

aov_idx <- aov(cap_index ~ urban_class, data = df_idx)

df_res <- bind_rows(data.frame(variable = "cap_eur",   res = residuals(aov_eur)),
                    data.frame(variable = "cap_index", res = residuals(aov_idx)))

ggplot(df_res, aes(sample = res)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line(color = "#e34948") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "QQ-plots of ANOVA residuals") +
  theme_minimal()

# Levene's test checks equal variances: H0 = all group variances equal, so
# a SMALL p-value is the BAD news here (variances differ)
df_three %>% levene_test(cap_eur ~ urban_class)
df_idx   %>% levene_test(cap_index ~ urban_class)

# like Shapiro-Wilk, Levene's test gets very sensitive with thousands of
# observations -- read it together with the sd column in the summary above,
# not instead of it


## WHEN VARIANCES DIFFER: WELCH'S ANOVA
# ====
# the same idea as the Welch t-test in 02: don't assume equal variances,
# and adjust the degrees of freedom. base R's oneway.test() does this by
# default (var.equal = FALSE). 
# WHEN IN DOUBT, REPORT THIS ONE: if the
# variances turn out equal it costs almost nothing
# ====

oneway.test(cap_eur ~ urban_class, data = df_three)

df_three %>% welch_anova_test(cap_eur ~ urban_class)

# compare to the classic F-test above: if the two agree on the verdict,
# the unequal variances don't change the story


## THE NON-PARAMETRIC VERSION: KRUSKAL-WALLIS ON cap_index
# ====
# the Kruskal-Wallis test is the multi-group version of the Wilcoxon
# rank-sum test from 02: it works on RANKS instead of raw values. all
# observations are pooled and ranked from 1 to N, and then the AVERAGE RANK
# of each group is compared. if the groups don't differ, every group's
# average rank should be close to (N + 1) / 2:
# ====

df_idx %>%
  mutate(rank_idx = rank(cap_index)) %>%
  group_by(urban_class) %>%
  summarise(n = n(), mean_rank = mean(rank_idx))

(nrow(df_idx) + 1) / 2   # the mean rank every group would have if H0 were true

# the H statistic measures how far the group mean ranks are from this value,
# and is compared against a chi-square distribution with k - 1 df.
# as with the rank-sum test, be careful about WHAT it tests: strictly, that
# a random observation from one group is equally likely to be higher or lower
# than one from another group; if the groups have the same SHAPE, this reads
# as "the medians differ". extreme values (the reason we are here) barely
# matter, because a huge value is just "the highest rank"
# ====

kruskal.test(cap_index ~ urban_class, data = df_idx)

# the pipe-friendly version
df_idx %>% kruskal_test(cap_index ~ urban_class)

# effect size: eta^2 based on H (rstatix), same 0-1 scale and conventions
# as eta^2 in ANOVA (0.01 small, 0.06 moderate, 0.14 large)
df_idx %>% kruskal_effsize(cap_index ~ urban_class)

# reading: p < 0.05 means at least one group tends to have higher or lower
# values than another -- again an OMNIBUS answer, no "which" yet


## POST HOC TESTS: WHICH GROUPS DIFFER?
# ====
# a significant omnibus test says "somewhere", so we compare the groups
# pairwise -- but now with corrections for multiple testing (see the FWER
# problem above). only do this when the omnibus test is significant.
# the corrections ADJUST THE P-VALUES (or the CIs) upward to account for the
# number of comparisons. all comparisons below are two-tailed
#
# first: how the corrections work, with plain pairwise t-tests
# (pairwise_t_test() from rstatix; pool.sd = TRUE by default, i.e. it uses
# the same pooled MS_within as ANOVA)
# ====

df_three %>% pairwise_t_test(cap_eur ~ urban_class, p.adjust.method = "none")   # raw: NOT corrected

df_three %>% pairwise_t_test(cap_eur ~ urban_class, p.adjust.method = "bonferroni")
df_three %>% pairwise_t_test(cap_eur ~ urban_class, p.adjust.method = "holm")

# the "p" column is the raw p-value, "p.adj" the corrected one -- use p.adj
# for the verdict. the two corrections, by hand with p.adjust():
#   - Bonferroni: multiply every p by the number of comparisons m (cap at 1).
#                 simple, valid always, but conservative (loses power)
#   - Holm:       sort the p-values; multiply the smallest by m, the next by
#                 m - 1, and so on. ALWAYS at least as powerful as Bonferroni
#                 with the same guarantee, so there is no reason to prefer
#                 Bonferroni except simplicity -- Holm is a good default

raw_p <- df_three %>%
  pairwise_t_test(cap_eur ~ urban_class, p.adjust.method = "none") %>%
  pull(p)

data.frame(raw        = raw_p,
           bonferroni = p.adjust(raw_p, method = "bonferroni"),
           holm       = p.adjust(raw_p, method = "holm"))

# these tests all just give corrected p-values. next, the two ANOVA-specific
# post hoc tests, which also give confidence intervals for the differences


## TUKEY'S HSD: THE CLASSIC POST HOC FOR ANOVA
# ====
# Honestly Significant Difference: compares ALL pairs at once using the
# studentized range distribution, so the FAMILY of intervals has 95%
# coverage jointly (not each separately). it needs the ANOVA assumptions
# (including equal variances; R's version handles unequal group sizes).
# output per pair:
#   estimate           difference in means, group2 - group1
#   conf.low/conf.high simultaneous 95% CI for that difference
#   p.adj              corrected p-value
# a pair differs significantly if its CI excludes 0 (equivalently p.adj < 0.05)
# ====

df_three %>% tukey_hsd(cap_eur ~ urban_class)

TukeyHSD(aov_eur)   # the base R version, same numbers


## GAMES-HOWELL: THE POST HOC FOR UNEQUAL VARIANCES
# ====
# the post hoc partner of the Welch ANOVA: no equal-variance assumption,
# Welch-style degrees of freedom for every pair. if Levene's test and the
# sd column suggest unequal variances, use this instead of Tukey
# ====

df_three %>% games_howell_test(cap_eur ~ urban_class)

# if Tukey and Games-Howell tell the same story, the variance issue doesn't
# matter. if they disagree, trust Games-Howell

# effect size for each pair (Cohen's d, as in 02)
df_three %>% cohens_d(cap_eur ~ urban_class)




## POST HOC FOR THE NON-PARAMETRIC CASE: DUNN'S TEST
# ====
# after a significant Kruskal-Wallis, the matching post hoc is Dunn's test:
# it compares the mean ranks of each pair, using the SAME overall ranking
# as Kruskal-Wallis, and adjusts the p-values for multiple testing.
# we STORE the result: the same Holm-adjusted p-values go onto the median
# plot at the end of the script
# ====

dunn_idx <- df_idx %>% dunn_test(cap_index ~ urban_class, p.adjust.method = "holm")
dunn_idx

# an alternative: separate Wilcoxon rank-sum tests for every pair, with the
# same correction. here each pair is re-ranked on its own, so the answers
# can differ slightly from Dunn's
df_idx %>% pairwise_wilcox_test(cap_index ~ urban_class, p.adjust.method = "holm")

# (both warn about ties at times -- harmless at this sample size)


## POST HOC VIA CONFIDENCE INTERVALS: THE VERTICAL GRAPH
# ====
# as in 02, we can compare groups by plotting each group's mean with its 95%
# CI (point + whisker, or "X-wing") -- this time with three of them:
# ====

cap_ci_groups <- df_three %>%
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

# reading: whiskers that DON'T overlap -> those two groups clearly differ.
# overlap is inconclusive, not "no difference" (the trap from 02). with
# THREE groups there's a second catch: every interval is a separate 95%
# statement, with no correction for the number of groups compared. so this
# plot is a quick eyeball check, and the rigorous version plots the
# DIFFERENCES with the corrected (simultaneous) intervals:

tukey_res <- df_three %>%
  tukey_hsd(cap_eur ~ urban_class) %>%
  mutate(comparison = paste(group2, "-", group1))   # estimate = group2 - group1

# values on the x-axis, comparisons on the y-axis: the standard layout for
# post hoc plots, because comparison labels ("Suburban - Rural") are long and
# there are choose(k, 2) of them -- with many groups a vertical axis of
# labels stays readable where a horizontal one would overlap.
# reorder() sorts the comparisons by their estimate, so the biggest
# difference sits on top and the plot reads like a ranking.
# (coord_flip() swaps the axes of the finished plot, which is why the
# aesthetics below are still written as x = category, y = value)
ggplot(tukey_res, aes(x = reorder(comparison, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#e34948") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 1, color = "#2a78d6") +
  geom_point(size = 2, color = "#2a78d6") +
  coord_flip() +
  labs(x = NULL, y = "difference in mean cap_eur (EUR)",
       title = "Tukey HSD: pairwise differences with simultaneous 95% CIs") +
  theme_minimal()

# the post hoc verdict, at a glance: an interval that does NOT contain the
# dashed zero line is a significant pair; an interval that contains 0 is not.
# (positive = the more urban group is pricier). the position of each interval
# also shows HOW BIG the difference is, not just whether it exists -- which
# a table of p-values can't. for unequal variances, plot the
# games_howell_test() output instead: it has the same columns


## THE SAME PICTURE FOR THE NON-PARAMETRIC CASE: BOOTSTRAPPED MEDIANS
# ====
# for cap_index we shouldn't plot means: skew and extreme values make them
# unstable, and the test is about ranks. instead, plot each group's MEDIAN
# with a bootstrapped 95% CI (the same percentile bootstrap as in 01 and
# 02, applied per group -- resample WITHIN each group)
# ====

boot_median_ci <- function(x, seed = 123) {
  set.seed(seed)
  b <- replicate(n_boot, median(sample(x, length(x), replace = TRUE)))
  data.frame(med = median(x),
             lower = unname(quantile(b, 0.025)),
             upper = unname(quantile(b, 0.975)))
}

cap_idx_ci <- bind_rows(lapply(levels(df_idx$urban_class), function(g) {
  cbind(urban_class = g, boot_median_ci(df_idx$cap_index[df_idx$urban_class == g]))
})) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")))
cap_idx_ci

ggplot(cap_idx_ci, aes(x = urban_class, y = med)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05, linewidth = 1, color = "#2a78d6") +
  geom_point(size = 2, color = "#2a78d6") +
  labs(x = NULL, y = "cap_index", title = "Median cappuccino index (bootstrapped 95% CI) by urban class") +
  theme_minimal()

# same reading and same caveat as for the means: non-overlapping whiskers
# are a strong hint, but these intervals are NOT corrected for multiple
# comparisons. the formal verdict comes from Dunn's test above, so we now
# put ITS p-values on the plot: the CIs show HOW BIG the differences are,
# the brackets show WHETHER they are significant. (the two come from
# different machinery: bootstrap on the medians vs. a test on the mean
# ranks -- they usually agree, but if they don't, trust the bracket)

# library(ggpubr)   # already in 00_setup.R

# add two columns to the stored Dunn result:
#   y.position   where each bracket sits: 10% above the highest CI limit,
#                stacked, so brackets collide neither with whiskers nor
#                with each other (change the 0.10 to loosen or tighten)
#   p.label      the ADJUSTED p (Holm), never the raw one. tiny values are
#                printed as "p < 0.001"
dunn_plot <- dunn_idx %>%
  mutate(y.position = max(cap_idx_ci$upper) * (1 + 0.10 * seq_len(n())),
         p.label    = ifelse(p.adj < 0.001, "p < 0.001", paste0("p = ", signif(p.adj, 2))))

ggplot(cap_idx_ci, aes(x = urban_class, y = med)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05, linewidth = 1, color = "#2a78d6") +
  geom_point(size = 2, color = "#2a78d6") +
  # dunn_plot has its own columns (group1, group2), hence inherit.aes = FALSE
  stat_pvalue_manual(dunn_plot, label = "p.label", y.position = "y.position",
                     tip.length = 0.01, inherit.aes = FALSE) +
  labs(x = NULL, y = "cap_index",
       title = "Median cappuccino index (bootstrapped 95% CI) by urban class",
       subtitle = "Pairwise p-values: Dunn's test, Holm-adjusted") +
  theme_minimal()


# tweaks:
# - stars instead of p-values: label = "p.adj.signif" (already in the output)
# - brackets too tight or too loose: change the 0.10 in y.position
# - unequal variances / a different post hoc: swap dunn_test() for
#   pairwise_wilcox_test() -- the columns group1, group2 and p.adj are the same
# ====




## A NOTE ON INDEPENDENCE
# ====
# as in 02: the data contain repeated cities (the same wage for many
# Copenhagen rows), so observations within a city are more alike than random
# pairs, and all p-values here are somewhat optimistic. the models later in
# the course handle this properly
# ====


## RECAP: THREE GROUPS, THE SAME UNDERLYING QUESTION
# ====
# "do Rural, Suburban and Urban differ?" -- first the omnibus test, then, only
# if it is significant, the post hoc tests:
#
#   what we ask                        parametric (cap_eur)             non-parametric (cap_index)
#   omnibus: any difference?           one-way ANOVA (F)                Kruskal-Wallis (H)
#     ... if variances differ          Welch ANOVA                      (rank-based: not needed)
#   effect size                        eta^2                            eta^2[H]
#   post hoc: which pairs?             Tukey HSD                        Dunn's test (Holm)
#     ... if variances differ          Games-Howell                     pairwise Wilcoxon (Holm)
#   post hoc picture                   mean + 95% CI / Tukey CI plot    median + bootstrapped CI
#
# corrections for multiple testing (Bonferroni, Holm, Tukey's built-in one)
# keep the family-wise error rate at 5% instead of letting it creep up with
# every extra comparison. ANOVA tells you THAT groups differ, the post hoc
# tests and CI graphs tell you WHICH ones, and the effect sizes tell you
# HOW MUCH
# ====