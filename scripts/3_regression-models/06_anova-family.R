# ===================================================================
# 06_anova-family.R -- THE OTHER VERSIONS OF ANOVA, AS LINEAR MODELS
# two-way ANOVA (two factors, main effects, interaction, unbalanced
# groups), why cutting a number into groups is wasteful, ANCOVA (a
# factor plus a numeric covariate), adjusted group means, interaction
# with a numeric variable (do the slopes differ?), robust versions, and
# a pointer to the ANOVA for clustered data (mixed models, in
# 4_regression-advanced)
# ===================================================================

source("scripts/00_setup.R")
load_packages(c("car", "emmeans", "lmtest", "sandwich"))

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

df_cont <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"),
         !is.na(cap_eur), !is.na(wage_eur),
         cap_eur > 0, wage_eur > 0) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")),
         country     = gsub(" ", " ", country),
         city_id     = paste(country, city, sep = " / "))

nrow(df_cont)


## WHERE WE ARE: THE ANOVA FAMILY IN ONE PICTURE
# ====
# in 1_group-comparisons we met ONE-way ANOVA: one outcome, one factor. in 03
# of this folder we saw that it is lm() with a categorical predictor. the
# "other versions of ANOVA" are the same model with more on the right:
#
#   one-way ANOVA                  y ~ factor
#   two-way ANOVA                  y ~ factor1 * factor2     (two factors + their interaction)
#   ANCOVA                         y ~ factor + numeric      (a factor and a covariate)
#   ANCOVA with different slopes   y ~ factor * numeric
#   repeated measures / mixed      y ~ ... + (1 | unit)      (the same unit measured many times)
#   Welch / robust ANOVA           the same models with robust standard errors
#
# so there is not a zoo of tests but ONE tool (the linear model) and a few
# questions that differ in what is on the right-hand side. this script goes
# through them in that order
# ====


## TWO FACTORS: THE TWO-WAY ANOVA
# ====
# a two-way ANOVA asks about TWO categorical predictors at once. we already
# have one, the urban class. make the second by cutting the wage into three
# equally populated bands (low / mid / high: terciles). for a moment we PRETEND
# that the wage is a category -- we will see in a moment that this is not a
# good idea, but it is the cleanest way to show a two-way design:
# ====

df_two_way <- df_cont %>%
  mutate(wage_band = cut(wage_eur,
                         breaks = quantile(wage_eur, c(0, 1/3, 2/3, 1)),
                         labels = c("low", "mid", "high"),
                         include.lowest = TRUE))

table(df_two_way$urban_class, df_two_way$wage_band)

# the table of counts is a "design": every combination of the two factors is
# a CELL. note that the cells are very unequal: from 34 rows (Rural, low wage)
# to 643 (Urban, low wage). a design with unequal cell sizes is called
# UNBALANCED, and it makes the main-effect tests depend on the order of the terms
# (more in 4_regression-advanced/03)
#
# the model: each of the 9 cell means is allowed to differ, and is decomposed
# into pieces:
#
#   y = grand mean + effect of factor 1 + effect of factor 2 + INTERACTION + e
#
#   main effect of urban class   do the urban classes differ (averaged over the wage bands)?
#   main effect of wage band     do the bands differ (averaged over the classes)?
#   interaction                  does the effect of one factor DEPEND on the level of the other?
#
# three F-tests, three hypotheses. in R the formula is  y ~ A * B
# (= A + B + A:B)
# ====

cell_means <- df_two_way %>%
  group_by(urban_class, wage_band) %>%
  summarise(n = n(), mean_price = mean(cap_eur), .groups = "drop")
cell_means

ggplot(cell_means, aes(x = wage_band, y = mean_price, color = urban_class, group = urban_class)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = n)) +
  labs(x = "wage band", y = "mean cap_eur", color = NULL, size = "rows",
       title = "Interaction plot: mean price by wage band, for each urban class") +
  theme_minimal()

# the INTERACTION PLOT is the picture of a two-way design. parallel lines
# = no interaction (the effect of the wage band is the same in every class);
# lines that fan out or cross = interaction
#
# result: in all three classes the price rises from the low to the mid
# band (about 3.4 -> 4.1 euros), but then the lines part: in the Urban places
# the mean keeps rising to 4.44 euros in the high band, in Suburban places it
# falls back (4.10 -> 4.01), in Rural places it stays flat (4.10 -> 4.19).
# the lines are not parallel: expect an interaction

fit_2way <- lm(cap_eur ~ urban_class * wage_band, data = df_two_way)
anova(fit_2way)
summary(fit_2way)$r.squared

# result: the wage band has a huge main effect (F = 283), the interaction is
# also clearly there (F = 8.3, p = 1e-6) and the urban class main effect is
# p = 0.46. R^2 = 0.198. a warning about the row of the urban class: with unequal
# cell sizes anova() tests the terms in the order of the formula, so that row
# changes when the order changes (4_regression-advanced/03 shows by how much)
#
# and an important rule when an interaction is present: do NOT interpret the
# main effects on their own ("the effect of the wage band") -- the interaction
# says there is no single effect of the wage band, it depends on the class.
# describe the cell means, or the effect of one factor within the levels of
# the other


## WHY NOT CUT A NUMBER INTO GROUPS? THE ANCOVA
# ====
# turning the wage into three bands threw information away: all wages in a band
# are treated as equal, and two nearly identical wages on either side of a
# boundary are treated as completely different. the wage is a NUMBER, so keep
# it a number. a model with a factor AND a numeric predictor is the
# ANALYSIS OF COVARIANCE (ANCOVA); the numeric variable is the COVARIATE.
# we have fitted it already (04): it is simply
#
#   cap_eur = b0 + b1 * wage + b2 * Suburban + b3 * Urban + e
#
# a straight line in the wage with a separate HEIGHT for every class. the
# classes are compared at the SAME wage: the vertical distance between
# parallel lines
# ====

fit_ancova <- lm(cap_eur ~ wage_eur + urban_class, data = df_cont)
summary(fit_ancova)$coefficients
# Anova(type = 2) tests each term after the other terms (the types of sums of
# squares are explained in 4_regression-advanced/03)
Anova(fit_ancova, type = 2)

c(R2_one_way = summary(lm(cap_eur ~ urban_class, data = df_cont))$r.squared,
  R2_two_way_bands = summary(lm(cap_eur ~ urban_class + wage_band, data = df_two_way))$r.squared,
  R2_ancova = summary(fit_ancova)$r.squared)

# result: the urban class, which did NOTHING on its own (03: F = 0.63, p = 0.53),
# is significant once the wage is accounted for: F = 8.2, p = 0.0003. and
# the ANCOVA (R^2 = 0.232, 4 parameters) explains more than the additive model
# with 3 wage bands (R^2 = 0.187, 5 parameters) and even more than the two-way
# model with the interaction (R^2 = 0.198, 9 parameters). the lesson: a numeric variable should stay numeric. the first reason to
# include a covariate in an ANOVA is that it removes variation that has
# nothing to do with the factor, so the factor's effect can show: reduces the
# "noise" (the residual variance) the F-test compares against. here it works
# even more strongly: the covariate also corrects a DIFFERENCE between the
# groups (their wages differ), which is a confounding adjustment

# the group means ADJUSTED for the covariate: the predicted price of each
# class at the SAME wage (by default: the average wage of all rows)
emmeans(fit_ancova, ~ urban_class)
pairs(emmeans(fit_ancova, ~ urban_class), adjust = "tukey")

# the same table without the adjustment is the raw group means of 03
df_cont %>% group_by(urban_class) %>% summarise(raw_mean = mean(cap_eur), mean_wage = mean(wage_eur))

# result: at the average wage (13.1 euros per hour) the expected prices are
# Rural 3.94, Suburban 3.82 and Urban 3.98 -- raw means were 4.01, 3.91 and
# 3.95. the adjustment lowers Rural and Suburban because their rows have
# above-average wages (13.9 and 14.2) and raises Urban, which has the lowest
# (12.7). Tukey: Suburban is lower than Urban by 0.17 euros (p = 0.0002), and
# the other two pairs do not differ. a cappuccino in a suburb is cheaper than one in a city with
# the same wage. mind the unit: these are WITHIN-the-model comparisons, and
# the model is not perfect (see the next two sections)


## ASSUMPTIONS OF ANCOVA: ARE THE SLOPES REALLY PARALLEL?
# ====
# the ANCOVA above assumes that the wage slope is the same in all classes
# ("homogeneity of slopes"): parallel lines. that is an assumption we can test.
# let the slope differ by adding the INTERACTION wage x class and compare the
# nested models (the F-test of 04):
# ====

fit_ancova_int <- lm(cap_eur ~ wage_eur * urban_class, data = df_cont)
anova(fit_ancova, fit_ancova_int)

# the separate slopes, with their standard errors, from the interaction model
emtrends(fit_ancova_int, ~ urban_class, var = "wage_eur")

ggplot(df_cont, aes(x = wage_eur, y = cap_eur, color = urban_class)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "wage_eur", y = "cap_eur", color = NULL,
       title = "One line per class: are they parallel?") +
  theme_minimal()

# result: the F-test of the interaction is F = 10.8, p = 2e-5: the slopes
# differ. they are 0.089 per euro of wage in Rural, 0.057 in Suburban and 0.095
# in Urban places (standard errors 0.018, 0.007 and 0.004): the Suburban line
# is clearly flatter. the "parallel lines" ANCOVA is therefore an approximation, and
# a statement like "Urban is 0.17 euros dearer than Suburban" is only true
# at the AVERAGE wage; at 5 euros per hour or at 25 it is a different
# number. check it with emmeans at chosen wages:

emmeans(fit_ancova_int, ~ urban_class | wage_eur, at = list(wage_eur = c(5, 15, 25)))

# result: at a wage of 5 euros the expected prices are Rural 3.22, Suburban 3.39
# and Urban 3.22 (Suburban is the DEAREST); at 15 euros 4.11, 3.96 and 4.17
# (Suburban the cheapest); at 25 euros 4.99, 4.53 and 5.11 (Suburban cheaper by
# more than half a euro). the Suburban line CROSSES the other two. an interaction means that a single "difference between
# the groups" does not exist: the answer depends on the covariate. the
# practical advice: always PLOT the lines before reporting adjusted means


## THE ROBUST AND NON-PARAMETRIC VERSIONS
# ====
# the unequal variances that call for the Welch ANOVA also call for robust
# standard errors in the larger models; the F-tests of the ANCOVA have a
# robust version in car:
# ====

Anova(fit_ancova, type = 2)                          # classic F-tests
Anova(fit_ancova, type = 2, white.adjust = "hc3")    # without equal variances
Anova(fit_ancova, type = 2, vcov. = vcovCL(fit_ancova, cluster = df_cont$country))   # countries as clusters

# result: the robust F for the class is 8.3 (p = 0.0003): the same as the
# classic one -- unequal variance is not the problem here. the cluster-robust
# test is another story: with countries as the independent units the F of the
# class drops to 1.26 (p = 0.28), NOT significant, while the wage stays
# overwhelming (F = 18, p = 2e-5). the class "effect" of the classic ANCOVA
# disappears once the standard errors admit that rows of the same country
# are alike. (a mixed model gives yet another answer: 4_regression-advanced/03)
#
# there is no standard non-parametric two-way ANOVA. the usual workaround is
# the rank transformation of 03 (fit the same model to rank(y)), keeping in
# mind that it loses the clean interpretation of an interaction. with
# ties, skew and clusters like these, the better tool is a model that
# respects the clustering: a mixed model (4_regression-advanced/03)


## DEPENDENT OBSERVATIONS: WHEN ROWS BELONG TOGETHER
# ====
# the last version of ANOVA is the one for data in which rows belong together:
# the same person (or city) measured several times (REPEATED MEASURES; the paired
# t-test of 02_two-sample.R is its simplest form), or rows nested in groups, as
# here (cappuccinos in cities in countries). the tool is the MIXED MODEL, a
# regression with a separate random intercept for each unit, written
# lmer(y ~ x + (1 | unit)) in the lme4 package.
#
# in this data the rows of one country are very alike: about 80% of the variation
# in prices lies BETWEEN countries (an intraclass correlation of 0.80). that is the
# reason for the cluster-robust standard errors of 02 and 04, and the mixed model
# is the other way to respect it. it needs some extra theory, so it lives in
# 4_regression-advanced/03_mixed-models-and-sums-of-squares.R
# ====


## WHICH ANOVA DO I USE? A DECISION GUIDE
# ====
#   what is on the right-hand side?                   model in R                    old name
#   one categorical predictor                         lm(y ~ A)                     one-way ANOVA
#   two categorical predictors                        lm(y ~ A * B)                 two-way ANOVA
#     ... with an unbalanced design                   Anova(fit, type = 2 or 3)     Type II / III SS
#   categorical + numeric                             lm(y ~ A + x)                 ANCOVA
#     ... slopes differ by group                      lm(y ~ A * x)                 ANCOVA with interaction
#   same unit measured several times                  lmer(y ~ A + (1 | unit))      repeated measures ANOVA
#   rows nested in groups (cities in countries)       lmer(y ~ x + (1 | g1 / g2))   multilevel / mixed ANOVA
#   unequal variances                                 add white.adjust = "hc3"      Welch ANOVA
#   skewed outcome, outliers                          transform (log) or ranks      Kruskal-Wallis
#
# (the lmer() rows are explained in 4_regression-advanced/03)
#
# the questions to ask in this order: (1) what is the OUTCOME? (2) which
# predictors, categorical or numeric? (3) are the rows independent? if not,
# what are the units? (4) is the design balanced? (5) do the effects depend
# on each other (interaction)? (6) the residual plots (02)
# ====


## WHAT WE FOUND
# ====
# - a two-way ANOVA of price by urban class and wage band shows a significant
#   interaction (F = 8.3): the effect of the wage band depends on the class
# - with unequal cell sizes the F-test of the urban class depends on the order
#   of the terms / the type of sum of squares (p from 0.46 to 0.002) -- the
#   factors overlap (4_regression-advanced/03)
# - cutting the wage into bands wastes information: the ANCOVA with the
#   numeric wage explains more (R^2 0.232 vs 0.187 / 0.198) with fewer parameters
# - the urban class, which does not matter alone, matters once the wage is
#   in the model (F = 8.2); Suburban places are cheaper than Urban places at
#   the same wage -- but the slopes differ by class (interaction F = 10.8), so
#   there is no single difference, and the class effect is not significant
#   with country-clustered standard errors (p = 0.28); a mixed model
#   (4_regression-advanced/03) gives a different answer, so the conclusion
#   depends on how dependence between rows is handled
# - rows of the same country are very alike (about 80% of the variance of
#   prices lies between countries), which is why the clustered tests matter
# ====


## TRY IT YOURSELF
# ====
# 1. cut the wage into FIVE bands instead of three and refit the two-way
#    model. what happens to the interaction? and to R^2?
# 2. fit the ANCOVA on log(cap_eur) with log(wage_eur) as the covariate. do the
#    class differences and the interaction change?
# ====
