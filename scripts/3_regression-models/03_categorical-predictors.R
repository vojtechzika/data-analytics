# ===================================================================
# 03_categorical-predictors.R -- t-TESTS AND ANOVA ARE REGRESSIONS
# a categorical predictor in lm(): dummy variables, the two-group case
# (= the t-test), the three-group case (= one-way ANOVA), the meaning of
# the coefficients and of the reference group, changing the reference,
# post hoc comparisons from the model, Welch / Kruskal-Wallis in their
# regression form, a case with a real group difference, and independence
# ===================================================================

source("scripts/00_setup.R")

# car (Companion to Applied Regression: robust ANOVA tables), emmeans
# (estimated group means and post hoc comparisons from a fitted model),
# lmtest + sandwich (robust standard errors, from 02)
load_packages(c("car", "emmeans", "lmtest", "sandwich"))

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

# all three urban classes, from least to most urban: Rural is the first level
# and therefore the REFERENCE group (we will see what that means)
df_three <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"),
         !is.na(cap_eur), !is.na(wage_eur), cap_eur > 0, wage_eur > 0) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")),
         country     = gsub(" ", " ", country),
         city_id     = paste(country, city, sep = " / "))

# the two-group data of 1_group-comparisons/02_two-sample.R: Urban first, Rural second
df_two <- df_three %>%
  filter(urban_class %in% c("Urban", "Rural")) %>%
  mutate(urban_class = factor(urban_class, levels = c("Urban", "Rural")))

df_three %>% count(urban_class)


## WHERE WE ARE: THE LINE WAS ALWAYS A "GROUP MEAN" MACHINE
# ====
# in 01 the predictor was a number (the wage). but lm() does not care: put a
# CATEGORICAL variable on the right-hand side and R turns it into numbers,
# fits the same least-squares model, and prints the same summary(). the
# surprise is how much of the previous course is already inside it:
#
#   two groups   -> the (Student) two-sample t-test
#   three groups -> the one-way ANOVA, with the very same F
#   unequal variances -> Welch's tests (via robust standard errors)
#   ranks        -> Kruskal-Wallis (approximately)
#
# so this script is a RE-reading of 1_group-comparisons, not new theory.
# and it prepares the next steps: once groups live INSIDE a regression,
# they can be combined with other predictors (04 and 05)
# ====


## STEP 0: LOOK AT THE DATA FIRST
# ====
# the same picture as in 03_three-sample.R
# ====

ggplot(df_three, aes(x = urban_class, y = cap_eur, fill = urban_class)) +
  geom_boxplot(alpha = 0.6, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3) +   # the x marks the mean
  labs(x = NULL, y = "cap_eur", title = "Cappuccino price by urban class (x = mean)") +
  theme_minimal()

df_three %>% group_by(urban_class) %>% summarise(n = n(), mean = mean(cap_eur), sd = sd(cap_eur))


## TWO GROUPS FIRST: URBAN VS RURAL
# ====
# R cannot multiply a word by a slope, so it creates a DUMMY VARIABLE (also:
# indicator): a column that is 1 for rows in the group and 0 for the rest.
# with two groups ONE dummy is enough: the group that has no dummy is the
# REFERENCE group (the first level of the factor -- here Urban). model.matrix()
# shows what lm() really sees:
# ====

head(model.matrix(~ urban_class, data = df_two))
df_two %>% select(urban_class) %>% head()

# the model:  cap_eur = b0 + b1 * Rural + e,   where Rural = 1 for rural rows, 0 for urban ones
#
#   urban row:  expected price = b0
#   rural row:  expected price = b0 + b1
#
# so b0 is the MEAN OF THE REFERENCE GROUP and b1 the DIFFERENCE between the
# two group means. the "slope" is just a step from one group to the other:
# two points, one line, and the slope is the rise between them

fit_two <- lm(cap_eur ~ urban_class, data = df_two)
summary(fit_two)

df_two %>% group_by(urban_class) %>% summarise(mean = mean(cap_eur))   # compare with the coefficients

# result: the intercept 3.947 is the mean price of the Urban rows, and the
# coefficient of urban_classRural, 0.0596, is the Rural mean (4.007) minus
# the Urban mean: rural cappuccinos are about 6 cents dearer, with a
# standard error of 0.088, t = 0.68 and p = 0.50. the name of the
# coefficient always tells you WHICH group is compared with the reference
# ("urban_classRural" = Rural vs the reference). R^2 = 0.0002: the group
# explains nothing noticeable


## IS THAT THE t-TEST?
# ====
# yes. the classic (Student) t-test assumes equal variances in the groups and
# is the SAME test as the slope in the regression: same t (up to sign, because
# t.test() subtracts the second group from the first, lm() the first from
# the second), same df, same p, same interval:
# ====

t.test(cap_eur ~ urban_class, data = df_two, var.equal = TRUE)

c(t_lm = summary(fit_two)$coefficients[2, "t value"],
  p_lm = summary(fit_two)$coefficients[2, "Pr(>|t|)"])
confint(fit_two)[2, ]   # the CI for Rural - Urban; t.test gives -0.232 to 0.113 for Urban - Rural

# and the Welch t-test of 02_two-sample.R (no equal-variance assumption)? its
# regression twin is the robust standard error from 02: same coefficient, a
# standard error that does not assume equal variances
t.test(cap_eur ~ urban_class, data = df_two)   # Welch: t = -0.756, p = 0.451
coeftest(fit_two, vcov = vcovHC(fit_two, type = "HC3"))

# result: Student t = -0.679 vs lm t = 0.679 (the same number, opposite sign
# because of the direction of the subtraction), p = 0.497 in both. Welch gives
# p = 0.451, the HC3 regression p = 0.451 -- the same verdict again. so
# "which test do I use?" in 1_group-comparisons was always a question about
# which STANDARD ERROR to put under the same difference


## THREE GROUPS: TWO DUMMIES
# ====
# with k groups we need k - 1 dummies. Rural is the reference (first level),
# Suburban and Urban get a dummy each:
#
#   cap_eur = b0 + b1 * Suburban + b2 * Urban + e
#
#   rural row:     b0                  (both dummies are 0)
#   suburban row:  b0 + b1
#   urban row:     b0 + b2
#
# so b0 is the Rural mean, and b1, b2 are the DIFFERENCES from Rural. the
# difference between Suburban and Urban is not a coefficient: it is b1 - b2
# ====

head(model.matrix(~ urban_class, data = df_three))
df_three %>% select(urban_class) %>% head()

fit_three <- lm(cap_eur ~ urban_class, data = df_three)
summary(fit_three)

# the group means from the coefficients, and directly from the data
b <- coef(fit_three)
c(Rural = b[[1]], Suburban = b[[1]] + b[[2]], Urban = b[[1]] + b[[3]])
tapply(df_three$cap_eur, df_three$urban_class, mean)

# result: b0 = 4.007 is the Rural mean; Suburban is 0.096 euros BELOW Rural
# (t = -1.05, p = 0.30) and Urban 0.060 below (t = -0.70, p = 0.49). the group
# means reconstructed from the coefficients (4.007, 3.911, 3.947) are exactly
# the ones in the data: with a single categorical predictor the model fits each
# group's mean and nothing else. note which test is NOT in this table: the
# Suburban-vs-Urban difference (it is b1 - b2 = -0.096 - (-0.060) = -0.036,
# a combination of two coefficients) -- we get it below, from emmeans


## THE F-TEST AT THE BOTTOM IS THE ANOVA
# ====
# the individual rows of summary() are t-tests of single differences against
# the reference. the question "do the groups differ AT ALL?" is the F-test at
# the bottom of summary(): H0: b1 = b2 = 0 (all three means equal) -- exactly
# the omnibus H0 of the one-way ANOVA. anova() prints it as the classic
# table, and aov() is just lm() with ANOVA-flavoured output:
# ====

anova(fit_three)
summary(aov(cap_eur ~ urban_class, data = df_three))   # the same table (03_three-sample.R)

# the same SS decomposition as in 01, now between and within groups:
#   SS between = what the group means explain = the "SSR" of the regression
#   SS within  = the residual sum of squares
# and eta^2 of the ANOVA is simply R^2 of the regression:
c(eta_squared = anova(fit_three)[1, "Sum Sq"] / sum(anova(fit_three)[, "Sum Sq"]),
  R_squared   = summary(fit_three)$r.squared)

# result: F = 0.63 on 2 and 2435 df, p = 0.53, R^2 = eta^2 = 0.0005: knowing
# the urban class explains a twentieth of a percent of the variation in
# prices. H0 is not rejected, as in 03_three-sample.R -- the same number,
# because it is the same model. therefore, as in the ANOVA script, we STOP
# before post hoc tests (below we compute them anyway, to see the machinery)


## A DIFFERENT REFERENCE GROUP, A DIFFERENT TABLE -- THE SAME MODEL
# ====
# the choice of the reference group is arbitrary and changes WHICH differences
# appear as coefficients, not the model itself: the fitted values, the R^2
# and the F-test stay identical. choose the reference that makes the
# comparisons you care about directly readable (a control group; the most
# common group; here perhaps Urban)
# ====

fit_ref_urban <- lm(cap_eur ~ relevel(urban_class, ref = "Urban"), data = df_three)
coef(fit_ref_urban)

all.equal(fitted(fit_three), fitted(fit_ref_urban))   # TRUE: the same model
c(F_rural_reference = summary(fit_three)$fstatistic[1], F_urban_reference = summary(fit_ref_urban)$fstatistic[1])

# two more codings you will meet. (a) NO INTERCEPT, "0 +": each coefficient is
# directly a group mean (handy for a table of means with their standard errors;
# but the F-test and R^2 at the bottom then answer a different question -- do
# NOT use them). (b) SUM-TO-ZERO ("effect") coding: the intercept is the mean of
# the group means and each coefficient is a group's deviation from it. this is
# what the Type III ANOVA of 05 needs
coef(lm(cap_eur ~ 0 + urban_class, data = df_three))
coef(lm(cap_eur ~ urban_class, data = df_three, contrasts = list(urban_class = "contr.sum")))

# result: with Urban as the reference the coefficients become Rural +0.060 and
# Suburban -0.036 (the differences from Urban); the no-intercept model returns
# the three means 4.007, 3.911, 3.947 directly; the sum-to-zero intercept is
# 3.955, the plain average of the three means (NOT the overall mean 3.94: the
# groups are very unequal in size, and this one counts them equally)


## POST HOC COMPARISONS FROM THE MODEL
# ====
# emmeans() computes the "estimated marginal means" (here simply the group means
# with their standard errors, from the pooled residual variance of the model)
# and pairs() compares all pairs with a multiplicity adjustment. with
# adjust = "tukey" it IS Tukey's HSD from 03_three-sample.R, now from any
# fitted model -- including the larger models of the next scripts
# ====

emm <- emmeans(fit_three, ~ urban_class)
emm
pairs(emm, adjust = "tukey")        # all pairs, Tukey-adjusted
confint(pairs(emm, adjust = "tukey"))   # the simultaneous CIs
TukeyHSD(aov(cap_eur ~ urban_class, data = df_three))   # the older base-R version: the same numbers

# result: Rural - Suburban 0.096 (p = 0.55), Rural - Urban 0.060 (p = 0.77),
# Suburban - Urban -0.036 (p = 0.72) -- identical to TukeyHSD(). for a model
# without a significant omnibus test this is what you EXPECT: nothing. the
# Rural estimate is the least precise (se 0.082 against 0.024 for Urban):
# only 140 rural rows


## UNEQUAL VARIANCES AND RANKS, IN REGRESSION FORM
# ====
# Welch ANOVA (the answer to unequal variances, 03_three-sample.R) and the
# Kruskal-Wallis test (the answer to skew and outliers) also have regression
# forms:
#   - ROBUST standard errors: Anova(model, white.adjust = "hc3") gives the
#     omnibus F-test without assuming equal variances
#   - a regression on RANKS: replace the outcome by rank(outcome) and fit
#     the same model -- close to Kruskal-Wallis for large samples (the
#     rank transformation is the idea behind both)
# ====

oneway.test(cap_eur ~ urban_class, data = df_three)       # Welch ANOVA
Anova(fit_three, white.adjust = "hc3")                    # robust F-test from the regression

kruskal.test(cap_eur ~ urban_class, data = df_three)      # Kruskal-Wallis
anova(lm(rank(cap_eur) ~ urban_class, data = df_three))   # ANOVA on the ranks

# result: Welch F = 0.75 (p = 0.4725) and the robust regression F = 0.75 (p =
# 0.4728): the same verdict to the third decimal. Kruskal-Wallis p = 0.5957
# and the rank regression p = 0.5959: the same too. the lesson is not
# "never use the named tests", it is that they are VARIATIONS of one idea --
# fit the group-means model, then choose the standard error and the scale
# (raw or ranks) that suit the data


## A CASE WITH A REAL DIFFERENCE: THE WAGE BY URBAN CLASS
# ====
# the price of a cappuccino does not differ between classes, so every test
# above says "nothing". to see the machinery produce a positive result, ask a
# different question of the same data: does the hourly WAGE differ?
# ====

fit_wage <- lm(wage_eur ~ urban_class, data = df_three)
summary(fit_wage)
df_three %>% group_by(urban_class) %>% summarise(n = n(), mean = mean(wage_eur), sd = sd(wage_eur))

leveneTest(wage_eur ~ urban_class, data = df_three)   # unequal variances? (car; the same test as rstatix::levene_test)
oneway.test(wage_eur ~ urban_class, data = df_three)  # Welch
pairs(emmeans(fit_wage, ~ urban_class), adjust = "tukey")

# result: mean wages are 13.87 (Rural), 14.19 (Suburban) and 12.69 (Urban)
# euros per hour. F = 18.5, p = 1e-8 -- and Levene's test (p = 4e-11) shows
# that the variance grows with the urban class (sd 3.9, 4.9, 5.6), so Welch is
# the safer choice: F = 21.2, p = 2e-9, the same verdict. Tukey: Urban is
# 1.5 euros BELOW Suburban (p < 0.0001) and 1.2 euros below Rural (p = 0.032);
# Rural and Suburban do not differ. yet R^2 = 0.015 -- a clearly "significant"
# but tiny effect: urban class explains 1.5% of the variation of the wage
# (p-values depend on n, effect sizes do not). the interesting thing for the
# next scripts: urban places have LOWER wages but the SAME cappuccino prices
# -- which means that, comparing places with the SAME wage, urban cappuccinos
# are relatively dearer. that is a statement a model with two predictors can
# make (05)


## A NOTE ON INDEPENDENCE -- AGAIN
# ====
# as in 03_three-sample.R and 02: rows from the same city, and cities from the
# same country, resemble each other, so the p-values above are too optimistic.
# the regression form makes the remedy easy: swap in a cluster-robust
# covariance matrix, exactly as in 02. here, for the omnibus F-test of the
# price model, with whole countries as clusters:
# ====

Anova(fit_three, vcov. = vcovCL(fit_three, cluster = df_three$country))

# result: F = 0.38 (p = 0.68): clustering moves the already non-significant
# result even further from significance. for the WAGE model the same
# correction would be much more consequential -- try it in the exercises


## WHAT WE FOUND
# ====
# - a regression with a categorical predictor is a model of group means: the
#   intercept is the reference group's mean, every other coefficient a
#   difference from it
# - with two groups, lm() reproduces the Student t-test (and, with robust
#   standard errors, the Welch test); with three, the F-test of lm() is the
#   one-way ANOVA, and R^2 is eta^2
# - the price of a cappuccino does not differ by urban class (F = 0.63, p = 0.53,
#   R^2 = 0.0005), consistent with 03_three-sample.R; the hourly wage does
#   (F = 18.5, Urban lower than the others), although the effect is small
# - post hoc tests (Tukey via emmeans) and the robust / rank versions give the
#   same answers as the dedicated tests
# ====


## RECAP: THE VOCABULARY TRANSLATED
# ====
#   1_group-comparisons                      regression (this script)
#   two-sample t-test (Student)              lm(y ~ group), t-test of the dummy
#   Welch t-test                             the same, with robust (HC3) standard errors
#   one-way ANOVA, F-test                    lm(y ~ group); anova(fit) or the F at the bottom of summary()
#   eta^2                                    R^2
#   Welch ANOVA                              Anova(fit, white.adjust = "hc3")
#   Tukey HSD                                pairs(emmeans(fit, ~ group), adjust = "tukey")
#   Kruskal-Wallis                           lm(rank(y) ~ group), approximately
#   group means with CIs                     emmeans(fit, ~ group)
#
# in a model with ONE categorical predictor there is nothing a regression can
# do that the ANOVA could not -- the point of learning the regression form is
# that it keeps working when a second predictor appears
# ====


## TRY IT YOURSELF
# ====
# 1. fit lm(cap_eur ~ urban_class) with Suburban as the reference group
#    (relevel). which coefficient answers "Suburban vs Urban"? check that
#    the omnibus F stays the same
# 2. a one-sample t-test is a regression too: fit lm(cap_eur ~ 1, data =
#    df_three) (an intercept-only model) and compare it with
#    t.test(df_three$cap_eur). what are the intercept and its standard error?
# 3. repeat the two-group comparison for the wage (Urban vs Rural). do
#    Student, Welch, lm and lm with HC3 agree? which two are the same pair?
# 4. run Anova(fit_wage, vcov. = vcovCL(fit_wage, cluster = df_three$country)).
#    does the wage difference between the urban classes survive treating
#    countries as the independent units? why might it not?
# 5. fit lm(cap_index ~ urban_class) for the cappuccino index (filter out
#    missing values first) and compare its F-test with the Kruskal-Wallis test
#    of 03_three-sample.R, and the F-test of lm(rank(cap_index) ~ urban_class).
#    which pair agrees better, and why?
# ====
