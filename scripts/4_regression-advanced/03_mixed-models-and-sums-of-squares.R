# ===================================================================
# 03_mixed-models-and-sums-of-squares.R -- TWO ADVANCED TOPICS FROM THE ANOVA FAMILY
# optional material that goes with 3_regression-models/06: Type I / II / III
# sums of squares in unbalanced designs, and random effects / mixed models
# (the ANOVA cousin of repeated measures) with the intraclass correlation
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

# the two-way design of 06 (urban class x three wage bands) for the sums-of-squares part
df_two_way <- df_cont %>%
  mutate(wage_band = cut(wage_eur,
                         breaks = quantile(wage_eur, c(0, 1/3, 2/3, 1)),
                         labels = c("low", "mid", "high"),
                         include.lowest = TRUE))
fit_2way <- lm(cap_eur ~ urban_class * wage_band, data = df_two_way)


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
#             (see advanced 02) -- SPSS and SAS use it by default. car::Anova(fit, type = 3)
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

data.frame(model = c("simple lm (01)", "random city intercept", "random country + city", "country dummies (advanced 01)"),
           wage_slope = c(coef(lm(cap_eur ~ wage_eur, data = df_cont))["wage_eur"],
                          fixef(lmer(cap_eur ~ wage_eur + (1 | city_id), data = df_cont))["wage_eur"],
                          fixef(fit_mixed)["wage_eur"],
                          coef(lm(cap_eur ~ wage_eur + factor(country), data = df_cont))["wage_eur"]))

# result: the variance components of the mixed model: country 1.21, city
# (within country) 0.07, residual 0.30. so most of the variation is BETWEEN
# countries (77% of the total), 4% is between cities of the same country and
# 19% is left within a city. the wage slope in the mixed model is 0.0247 (se
# 0.0044): under a third of the pooled 0.0858, and close to the country-dummy value
# 0.029 (4_regression-advanced/01). the mixed model takes into account that the wage differs
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


## TRY IT YOURSELF
# ====
# 1. make the two-way design BALANCED by drawing 34 rows (the size of the
#    smallest cell) from every cell with slice_sample(). do Type I, II and III
#    now agree? why?
# 2. add urban_class to fit_mixed (random country + city) and test it with a
#    likelihood-ratio test against the model without it
# 3. add a random SLOPE for the wage within countries: (1 + wage_eur | country).
#    does the average wage slope change? what does the variance of the
#    slopes tell you? (the model may warn about convergence: why?)
# 4. compute the ICC for the city level on its own (null model with (1 |
#    city_id)). is it larger or smaller than the country ICC? why?
# ====
