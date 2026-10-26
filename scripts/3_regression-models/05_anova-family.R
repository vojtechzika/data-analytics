# ===================================================================
# 05_anova-family.R -- THE OTHER VERSIONS OF ANOVA, AS LINEAR MODELS
# two-way ANOVA (two factors, main effects, interaction, unbalanced
# groups), Type I / II / III sums of squares, why cutting a number into
# groups is wasteful, ANCOVA (a factor plus a numeric covariate),
# adjusted group means, interaction with a numeric variable (do the
# slopes differ?), robust versions, and the ANOVA for clustered data:
# random effects / mixed models (the ANOVA cousin of the repeated
# measures designs and of the paired t-test)
# ===================================================================

source("scripts/00_setup.R")
load_packages(c("car", "emmeans", "lmtest", "sandwich", "lme4"))

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
# UNBALANCED, and it is the reason the next-but-one section exists
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
# p = 0.46. R^2 = 0.198. HOLD ON before reading the row of the urban class: it
# changes when we change the order of the factors in the formula (next section)
#
# and an important rule when an interaction is present: do NOT interpret the
# main effects on their own ("the effect of the wage band") -- the interaction
# says there is no single effect of the wage band, it depends on the class.
# describe the cell means, or the effect of one factor within the levels of
# the other


## UNBALANCED DESIGNS: TYPE I, II AND III SUMS OF SQUARES
# ====
# when the cell sizes are unequal the factors are partly CONFOUNDED with each
# other (here: more low-wage rows are Urban), and "the sum of squares of the
# urban class" is no longer a single well-defined number -- it depends on
# what we take into account FIRST. the three conventions:
#
#   Type I    SEQUENTIAL: each term is tested after those ABOVE it in the formula
#             (anova() and summary(aov()) in base R). the answer depends on the ORDER
#   Type II   each term is tested after all the other terms that do NOT contain
#             it (so: main effects after each other, but BEFORE their interaction).
#             a good default when the interaction is absent or not interpreted.
#             car::Anova(fit, type = 2)
#   Type III  each term is tested after ALL other terms, interaction included.
#             needs sum-to-zero ("effect") coding of the factors to be meaningful
#             (see 03) -- SPSS and SAS use it by default. car::Anova(fit, type = 3)
#
# with EQUAL cell sizes (a balanced design) the three types give the same
# numbers; with unbalanced data they do not, and which one is "right" is a
# question about what you want to test, not a statistical fact
# ====

# Type I, in both orders: the order of the two factors changes the first one
anova(lm(cap_eur ~ urban_class + wage_band, data = df_two_way))    # urban class FIRST
anova(lm(cap_eur ~ wage_band + urban_class, data = df_two_way))    # urban class SECOND

# Type II and Type III for the model with the interaction
Anova(fit_2way, type = 2)

fit_2way_sum <- lm(cap_eur ~ urban_class * wage_band, data = df_two_way,
                   contrasts = list(urban_class = "contr.sum", wage_band = "contr.sum"))
Anova(fit_2way_sum, type = 3)

# result: the F-test of the urban class changes with the convention: Type I
# with the class first: F = 0.77, p = 0.46; with the class second (after the
# wage band): F = 6.2, p = 0.002; Type II in the interaction model: F = 6.3,
# p = 0.002; Type III: F = 4.6, p = 0.010. the same factor, the same data,
# four p-values from 0.46 to 0.002. the reason is the one in 03: urban places
# have LOWER wages, so the class is mixed up with the wage band, and whether
# its "own" contribution is large depends on whether the wage band is allowed
# to explain its share first. the interaction row (F = 8.3) is the same in
# every convention -- it is the last term in all of them. rule of thumb: make
# the choice BEFORE looking at the p-values, say which one you used, and
# if the verdict depends on it, report the cell means and be careful
#
# (a Type III warning: it is only meaningful with sum-to-zero contrasts. with
# R's default "treatment" coding, Anova(type = 3) tests something else and
# can give misleading answers when there is an interaction)


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
# are alike. (the mixed model below gives yet another answer -- read on)
#
# there is no standard non-parametric two-way ANOVA. the usual workaround is
# the rank transformation of 03 (fit the same model to rank(y)), keeping in
# mind that it loses the clean interpretation of an interaction. with
# ties, skew and clusters like these, the better tool is a model that
# respects the clustering -- next


## DEPENDENT OBSERVATIONS: RANDOM EFFECTS AND THE MIXED MODEL
# ====
# the last version of ANOVA is the one for data in which rows belong together.
# the classic cases:
#   - REPEATED MEASURES: the same person (or city, or cappuccino) measured
#     several times. the paired t-test of 02_two-sample.R is its simplest form
#     (two measurements per unit), the repeated-measures ANOVA the multi-
#     measurement version
#   - NESTED / HIERARCHICAL data: rows inside cities inside countries, as here
#
# the old repeated-measures ANOVA is a special case of a MIXED MODEL: a
# regression with (a) FIXED effects, the usual coefficients we care about, and
# (b) RANDOM effects, a separate intercept for each unit (city, country, person),
# assumed to come from a bell-shaped distribution of unit differences.
# a unit's random intercept absorbs everything that makes ITS rows alike
# (a country's cost of living, a person's baseline), which removes the
# dependence between rows. in R (lme4):
#
#   lmer(y ~ fixed terms + (1 | unit), data)
#
# (1 | unit) = "a random intercept for each unit". nested units: (1 | country/city)
# ====

# step 1: the EMPTY model: only a grand mean and random intercepts for countries.
# its job is to split the variance of the price into "between countries" and
# "within a country"
fit_null <- lmer(cap_eur ~ 1 + (1 | country), data = df_cont)
summary(fit_null)

vc <- as.data.frame(VarCorr(fit_null))
vc[, c("grp", "vcov")]

# the INTRACLASS CORRELATION (ICC): the share of the total variance that lies
# between the units. also the correlation between two rows of the same unit
icc_country <- vc$vcov[vc$grp == "country"] / sum(vc$vcov)
icc_country

# result: the variance between countries is 1.42 and the residual (within a country)
# variance is 0.36: ICC = 0.80. EIGHTY percent of the variation in
# cappuccino prices in this dataset is between countries, only 20% between
# the rows of the same country. that is the single most important fact about
# these data, and the reason why all the standard errors in 01-04 that treat
# rows as independent are too small (two rows from one country are, on
# average, correlated 0.8!). for comparison: ICC = 0 would mean rows from
# a country are no more alike than rows from different countries

# step 2: the wage as a fixed effect, with random intercepts for countries and
# for the cities inside them
fit_mixed <- lmer(cap_eur ~ wage_eur + (1 | country / city_id), data = df_cont)
summary(fit_mixed)

data.frame(model = c("simple lm (01)", "random city intercept", "random country + city", "country dummies (04)"),
           wage_slope = c(coef(lm(cap_eur ~ wage_eur, data = df_cont))["wage_eur"],
                          fixef(lmer(cap_eur ~ wage_eur + (1 | city_id), data = df_cont))["wage_eur"],
                          fixef(fit_mixed)["wage_eur"],
                          coef(lm(cap_eur ~ wage_eur + factor(country), data = df_cont))["wage_eur"]))

# result: the variance components of the mixed model: country 1.21, city
# (within country) 0.07, residual 0.30. so most of the variation is BETWEEN
# countries (77% of the total), 4% is between cities of the same country and
# 19% is left within a city. the wage slope in the mixed model is 0.0247 (se
# 0.0044): under a third of the pooled 0.0858, and close to the country-dummy value
# 0.029 (04). the mixed model takes into account that the wage differs
# mostly between countries, with the random effects "soaking up" the rest of
# the country differences. the slope is still clearly positive (t = 5.6):
# within a country, a city with a 10-euro higher wage has a cappuccino about
# 25 cents dearer. NOTE: no p-values are printed. lme4 does not provide them
# on purpose (the degrees of freedom of a mixed model are debated); use the t-value
# (|t| > 2 is roughly p < 0.05), a likelihood-ratio test (anova(m0, m1)), or
# the lmerTest package

# the mixed ANCOVA: the urban class as a factor, the wage as a covariate, and
# random intercepts for the countries
fit_mixed_ancova <- lmer(cap_eur ~ wage_eur + urban_class + (1 | country), data = df_cont)
summary(fit_mixed_ancova)$coefficients

# a likelihood-ratio test for the urban class (refit with ML, which anova()
# does automatically): the mixed-model version of the F-test of the factor
fit_mixed_noclass <- lmer(cap_eur ~ wage_eur + (1 | country), data = df_cont)
anova(fit_mixed_noclass, fit_mixed_ancova)

# result: the wage slope is 0.032 (t = 7.4) and, relative to Rural, Suburban is
# +0.002 (t = 0.04) and Urban +0.120 (t = 2.2). the likelihood-ratio test of the
# class is chi-square = 17.7 on 2 df, p = 0.0001: significant. so we have three
# answers on the class: classic ANCOVA p = 0.0003, cluster-robust ANCOVA p =
# 0.28, mixed model p = 0.0001. they are not contradictions: they answer
# different questions. the cluster-robust test treats countries as the units
# and compares the classes ACROSS the whole data (where the class mix differs
# country by country). the mixed model gives every country its own baseline
# and so compares Urban with Suburban and Rural places mostly WITHIN a
# country -- a sharper comparison, which finds Urban cappuccinos about 12 cents
# dearer at the same wage. the within-country reading is the more convincing
# one (it cannot be produced by differences between countries), but it rests on
# the mixed model's assumptions (normal random intercepts, a common slope --
# and we saw that the slopes differ by class). report the model, its
# assumptions, and the fact that the conclusion depends on how dependence is handled


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
# the questions to ask in this order: (1) what is the OUTCOME? (2) which
# predictors, categorical or numeric? (3) are the rows independent? if not,
# what are the units? (4) is the design balanced? (5) do the effects depend
# on each other (interaction)? (6) the residual plots (02)
# ====


## WHAT WE FOUND
# ====
# - a two-way ANOVA of price by urban class and wage band shows a significant
#   interaction (F = 8.3): the effect of the wage band depends on the class
# - with unequal cell sizes the F-test of the urban class depends on the type
#   of sum of squares (p from 0.46 to 0.002) -- the factors overlap
# - cutting the wage into bands wastes information: the ANCOVA with the
#   numeric wage explains more (R^2 0.232 vs 0.187 / 0.198) with fewer parameters
# - the urban class, which does not matter alone, matters once the wage is
#   in the model (F = 8.2); Suburban places are cheaper than Urban places at
#   the same wage -- but the slopes differ by class (interaction F = 10.8), so
#   there is no single difference, and the class effect is not significant
#   with country-clustered standard errors (p = 0.28), though it is in the
#   mixed model (p = 0.0001)
# - 80% of the variance of prices lies between countries (ICC = 0.80). once
#   countries are random effects the wage slope falls to about 0.025 from
#   0.086 -- the pooled slope mostly reflects differences between countries
# ====


## TRY IT YOURSELF
# ====
# 1. make the two-way design BALANCED by drawing 34 rows (the size of the
#    smallest cell) from every cell with slice_sample(). do Type I, II and III
#    now agree? why?
# 2. cut the wage into FIVE bands instead of three and refit the two-way
#    model. what happens to the interaction? and to R^2?
# 3. fit the ANCOVA on log(cap_eur) with log(wage_eur) as the covariate. do the
#    class differences and the interaction change?
# 4. add urban_class to fit_mixed (random country + city) and test it with a
#    likelihood-ratio test against the model without it
# 5. add a random SLOPE for the wage within countries: (1 + wage_eur | country).
#    does the average wage slope change? what does the variance of the
#    slopes tell you? (the model may warn about convergence: why?)
# 6. compute the ICC for the city level on its own (null model with (1 |
#    city_id)). is it larger or smaller than the country ICC? why?
# ====
