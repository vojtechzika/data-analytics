# ===================================================================
# 04_multiple-regression.R -- MORE THAN ONE PREDICTOR: ADDING THE PRICE OF MILK
# the milk data (a COUNTRY-level variable), why models must be compared on the
# same rows, milk on its own, the two-predictor model and what "holding the
# other variable fixed" means, why a coefficient changes, comparing nested
# models (R^2, adjusted R^2, F-test), collinearity, the log version, WHO COUNTS
# (rows or countries), adding the urban class, and interactions
# ===================================================================

source("scripts/00_setup.R")
load_packages(c("car", "lmtest", "sandwich"))

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

df_cont <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"),
         !is.na(cap_eur), !is.na(wage_eur),
         cap_eur > 0, wage_eur > 0) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")),
         country     = gsub(" ", " ", country),
         city_id     = paste(country, city, sep = " / "))

nrow(df_cont)   # 2438 rows, as in 01 and 02


## THE NEW VARIABLE: THE PRICE OF MILK (A COUNTRY-LEVEL VARIABLE)
# ====
# in data-prep/cappuccino-index.R we joined a milk price (EUR per litre, Sept
# 2026) onto every row BY COUNTRY, "since this can be a confound". why milk?
# it is an ingredient (a cost), and also a rough signal of how expensive
# the country is -- both could move the price of a cappuccino, and both
# could be related to the wage. a variable that is related to BOTH the
# predictor and the outcome is a candidate CONFOUNDER: leaving it out can
# distort the slope we care about. this script asks what happens to the
# wage-price relationship when milk enters the picture
#
# first, what kind of variable is it? the join was by country, so every row
# of a country carries the SAME milk price:
# ====

df_cont %>%
  group_by(country) %>%
  summarise(n_milk_values = n_distinct(milk_eur, na.rm = TRUE)) %>%
  count(n_milk_values)   # 0 = no milk price for that country, 1 = one value per country

# 76 countries have a milk price, 10 have none (and each country has a
# single value at most). so although we have thousands of rows, milk
# carries only 76 DISTINCT pieces of information. keep that in mind: it
# matters a great deal in the second half of this script
#
# first, the missing values:

df_cont %>% filter(is.na(milk_eur)) %>% count(country, sort = TRUE)

# result: 52 rows have no milk price. 42 of them are New Zealand (34) and South
# Africa (8) -- and those two names are exactly the ones that contain a
# non-breaking space (see the gsub() at the top of this script) -- so the join
# in data-prep very likely failed on them (the other 8 missing countries
# are tiny: 1 or 2 rows each). cleaning the country names BEFORE the join
# would recover those 42 rows. here we work with what we have: only
# rows with a milk price can be used in models that contain it


## THE GOLDEN RULE OF MODEL COMPARISON: THE SAME ROWS
# ====
# lm() silently drops every row with a missing value in ANY variable of the
# formula. the model with milk therefore uses fewer rows than the model without
# it, and then differences between the two models mix "the effect of adding
# milk" with "a different sample". so, before comparing, restrict the data to
# the rows that have ALL the variables of the biggest model, and refit every
# model on that subset
# ====

df_milk <- df_cont %>% filter(!is.na(milk_eur))
c(all_rows = nrow(df_cont), rows_with_milk = nrow(df_milk), countries = n_distinct(df_milk$country))

fit_wage <- lm(cap_eur ~ wage_eur, data = df_milk)   # the model of 01, on the common rows
coef(fit_wage)
coef(lm(cap_eur ~ wage_eur, data = df_cont))         # on all 2438 rows: 0.0858

# result: on the 2386 rows with milk the wage slope is 0.0846 (against 0.0858 on
# all rows) -- almost the same: dropping 52 rows changed nothing important


## MILK ON ITS OWN
# ====
# first the milk-only regression: a simple regression, exactly as in 01
# ====

ggplot(df_milk, aes(x = milk_eur, y = cap_eur)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#e34948") +
  labs(x = "milk_eur (EUR per litre)", y = "cap_eur (EUR)",
       title = "Cappuccino price vs price of milk (each column of points is one country)") +
  theme_minimal()

fit_milk <- lm(cap_eur ~ milk_eur, data = df_milk)
summary(fit_milk)$coefficients
summary(fit_milk)$r.squared

# result: the slope is +0.130 euros per euro of milk (t = 2.67, p = 0.008),
# but R^2 = 0.003: milk alone explains 0.3% of the variation in prices -- very
# little. in countries where a litre of milk is a euro dearer, cappuccinos are
# on average 13 cents dearer. (note the vertical stripes in the plot: all rows
# of a country sit above one milk value.) this is what a milk-only analysis
# would "find". is it milk, though? the next step shows


## THE MODEL WITH TWO PREDICTORS
# ====
# a MULTIPLE regression adds more predictors with a plus sign:
#
#   cap_eur = b0 + b1 * wage_eur + b2 * milk_eur + e
#
# the fitting is still least squares (the plane of best fit now, not a line),
# and everything from 01 carries over: coefficients, standard errors, t-tests,
# R^2, residuals. what CHANGES is the meaning of a slope:
#
#   b1 is the expected change in price for one more euro of WAGE, comparing
#      places with the SAME milk price ("holding milk fixed", "controlling for
#      milk", "adjusting for milk" -- all mean the same)
#   b2 is the same for milk, comparing places with the same wage
#
# a coefficient in a multiple regression is a PARTIAL (or conditional)
# effect: it answers a different question than the same variable's slope in
# a simple regression. that is why the numbers can differ -- see below. the
# phrase "holding fixed" is not an experiment: it is a statistical comparison
# of rows that happen to share the other variables' values
# ====

fit_wm <- lm(cap_eur ~ wage_eur + milk_eur, data = df_milk)
summary(fit_wm)
confint(fit_wm)

# result: wage 0.0853 (t = 26.1, p < 2e-16, 95% CI 0.079 to 0.092) and milk
# -0.058 (t = -1.34, p = 0.18, 95% CI -0.144 to +0.027). R^2 = 0.2251
# against 0.2245 for the wage alone. two stories at once:
#
#   (1) the wage slope hardly moved (0.0846 -> 0.0853): taking milk into
#       account does not change what the wage says
#   (2) the milk coefficient CHANGED SIGN. alone: +0.130 (significant); next
#       to the wage: -0.058, indistinguishable from zero. that is not a
#       contradiction: the two numbers answer different questions. alone,
#       milk "gets credit" for the wage's work; next to the wage there is
#       little left to explain. let us see exactly why


## WHY DID A COEFFICIENT CHANGE?
# ====
# the milk coefficient changed from +0.130 (alone) to -0.058 (next to the wage),
# while the wage coefficient hardly moved (0.0846 -> 0.0853). the rule: a
# coefficient changes when you add a variable only if that variable (1) matters
# for the outcome AND (2) is related to the variable whose coefficient you look at.
#
#   - wage -> price: the wage matters a lot for the price, and richer countries
#     have dearer milk (see below), so milk alone "borrows" the wage's association
#   - milk -> wage coefficient: milk hardly matters once the wage is in, and is
#     only weakly related to the wage, so the wage coefficient barely moves
#
# this is the logic of "controlling for a confounder". the exact formula
# (omitted variable bias) and the residual-on-residual picture are in
# 4_regression-advanced/02_model-theory-extras.R
# ====

coef(lm(wage_eur ~ milk_eur, data = df_milk))["milk_eur"]   # a euro more milk per litre goes with ~2.2 euros more hourly wage

# result: richer countries have dearer milk (about 2.2 euros of hourly wage per euro of
# milk), and the wage raises the price (0.085 per euro). 2.2 * 0.085 = +0.19 is the
# amount of the wage's association that milk alone absorbs, turning -0.058 into
# +0.130. the wage is a textbook confounder of MILK's slope (though not the other way round)


## DOES MILK IMPROVE THE MODEL? COMPARING NESTED MODELS
# ====
# the wage-only model is a special case of the wage+milk model (b2 = 0), so
# the two are NESTED, and we can ask whether the bigger one is worth it.
# several lenses, which usually agree:
#
#   R^2             never decreases when a predictor is added, even a useless
#                   one -- so it cannot judge
#   adjusted R^2    R^2 penalized for the number of predictors; it CAN go down
#   F-test          anova(small, big): H0 = "the added terms are all zero"
#                   (with ONE added variable it is the square of its t-test)
#
# (AIC and BIC, which also compare models, are in the advanced folder)
# ====

anova(fit_wage, fit_wm)

data.frame(model = c("wage", "wage + milk"),
           R2     = c(summary(fit_wage)$r.squared, summary(fit_wm)$r.squared),
           adj_R2 = c(summary(fit_wage)$adj.r.squared, summary(fit_wm)$adj.r.squared))

# result: R^2 rises from 0.2245 to 0.2251 -- by construction. adjusted R^2
# is 0.2242 for the wage model and 0.2244 for wage + milk: a gain of 0.0003
# that is not worth a decimal. the F-test gives F = 1.79, p = 0.18. all the
# lenses say: milk adds nothing once the wage is in


## COLLINEARITY: WHEN THE PREDICTORS ARE TOO ALIKE
# ====
# if two predictors are strongly correlated, the model has trouble telling
# their effects apart: the coefficients are unstable and their standard errors
# explode, although the model as a whole still predicts well. the diagnostic
# is the VARIANCE INFLATION FACTOR: VIF = 1 / (1 - R^2 of regressing that
# predictor on all the others). VIF = 1 means no overlap; above 5 (some say
# 10) is a warning. a VIF of 4 means the standard error is twice (sqrt(4)) as
# large as it would be with uncorrelated predictors
# ====

cor(df_milk$wage_eur, df_milk$milk_eur)
vif(fit_wm)
1 / (1 - summary(lm(wage_eur ~ milk_eur, data = df_milk))$r.squared)   # by hand: the same number

# result: r = 0.165 between wage and milk, VIF = 1.03: no collinearity problem.
# the standard errors are only about 1.4% larger than if the two predictors
# were unrelated. (the opposite example: include BOTH cap_eur / wage_eur
# and wage_eur as predictors -- a ratio and its own component -- and the
# VIF explodes; recall the trap from 2_associations)


## WHAT THE MODEL LOOKS LIKE: ONE LINE FOR EACH MILK PRICE
# ====
# a model with two numeric predictors is a PLANE; to draw it in two
# dimensions, show the wage-price line for a few fixed values of milk. we use the
# 10th, 50th and 90th percentiles. the lines have the same slope by
# construction (that is what "no interaction" means); only their HEIGHT
# differs -- by the milk coefficient
# ====

milk_levels <- quantile(df_milk$milk_eur, c(0.10, 0.50, 0.90))
milk_levels

grid <- expand.grid(wage_eur = seq(min(df_milk$wage_eur), max(df_milk$wage_eur), length.out = 100),
                    milk_eur = unname(milk_levels))
grid$pred <- predict(fit_wm, grid)
grid$milk_label <- factor(paste("milk =", round(grid$milk_eur, 2)))

ggplot(df_milk, aes(x = wage_eur, y = cap_eur)) +
  geom_point(alpha = 0.15) +
  geom_line(data = grid, aes(y = pred, color = milk_label), linewidth = 1) +
  labs(x = "wage_eur", y = "cap_eur", color = "milk price (10th, 50th, 90th pct)",
       title = "Predicted price: three almost indistinguishable lines") +
  theme_minimal()

# result: with milk at 1.06, 1.41 and 2.13 euros per litre the three lines
# are 0.02-0.06 euros apart: invisible next to the spread of the points. the
# 90th-percentile milk price is 1.07 euros dearer than the 10th, and the model
# says that makes the cappuccino 1.07 * 0.058 = 6 cents CHEAPER, not significantly
# different from zero


## THE LOG VERSION: A SIGNIFICANT RESULT APPEARS
# ====
# in 02 the log-log model fitted better. repeat the comparison with logs:
# ====

fit_log_w  <- lm(log(cap_eur) ~ log(wage_eur), data = df_milk)
fit_log_wm <- lm(log(cap_eur) ~ log(wage_eur) + log(milk_eur), data = df_milk)
summary(fit_log_wm)$coefficients
anova(fit_log_w, fit_log_wm)
c(R2_wage = summary(fit_log_w)$r.squared, R2_wage_milk = summary(fit_log_wm)$r.squared)

# result: now milk is "significant": the elasticity is -0.058 (t = -3.45, p =
# 0.0006) and the F-test of the nested models agrees (p = 0.0006), although R^2
# rises by only 0.003 (0.329 -> 0.332). the wage elasticity is 0.236. read
# the sign: a 10% dearer litre of milk goes with a cappuccino about 0.6%
# CHEAPER, holding the wage fixed. that is the opposite of what a cost story
# predicts, and no theory in this course suggests it. before believing it,
# ask the question of 01 and 02: are the p-value and the interval honest?
# and here the honest answer is: no, because of the nature of milk


## WHO COUNTS? THE PROBLEM OF A COUNTRY-LEVEL PREDICTOR
# ====
# milk takes only 76 different values. the 2386 rows are many cappuccinos in
# few countries, and all the rows of a country share the same milk price. for
# a predictor that is constant within a cluster, the effective sample size
# for its coefficient is closer to the number of CLUSTERS (76) than the number
# of rows (2386) -- and the standard errors above, which assume independent
# rows, are too small. the remedy is the same as in 02: cluster-robust
# standard errors, with COUNTRY as the cluster
# ====

compare_se <- function(model, term) {
  data.frame(
    se_type = c("classic", "robust HC3", "cluster: city", "cluster: country"),
    se = c(coeftest(model)[term, 2],
           coeftest(model, vcov = vcovHC(model, "HC3"))[term, 2],
           coeftest(model, vcov = vcovCL(model, cluster = df_milk$city_id))[term, 2],
           coeftest(model, vcov = vcovCL(model, cluster = df_milk$country))[term, 2])) %>%
    mutate(estimate = coef(model)[term], t = estimate / se, p = 2 * pnorm(-abs(t)))
}

compare_se(fit_log_wm, "log(milk_eur)")
compare_se(fit_log_wm, "log(wage_eur)")

# (p-values here use the normal approximation, t / se against N(0, 1). with
# only 76 clusters a t-distribution with 75 df would be slightly more
# conservative: the country p-value would then be 0.58 as well)
#
# result: for milk the standard error grows from 0.017 (classic) to 0.018
# (HC3), 0.041 (cities) and 0.104 (countries): six times larger. the p-value
# goes from 0.0006 to 0.0012, 0.16 and 0.58. THE MILK COEFFICIENT IS NOT
# DISTINGUISHABLE FROM ZERO once the countries are treated as the independent
# units. the wage elasticity is not in doubt (0.236 with se 0.029, t = 8.1
# at the country level) -- it varies within countries too


# two more ways of asking "who counts, rows or countries?" -- a regression of
# countries (weighted or not) and country fixed effects -- are in
# 4_regression-advanced/01_clustered-inference.R. the short version: within countries
# the wage slope is about 0.029, a third of the pooled 0.085, so most of the
# wage-price relationship is between countries; and the milk coefficient flips
# sign (+0.26 or -0.07) depending on whether countries are weighted by their rows


## ADDING A CATEGORICAL PREDICTOR: THE URBAN CLASS
# ====
# exercise 6 of 2_associations: add urban_class to the model. the factor
# enters as the dummies of 03 (Rural is the reference)
#
#   cap_eur = b0 + b1 * wage + b2 * milk + b3 * Suburban + b4 * Urban + e
#
# b3 and b4 are the differences in the expected price from a RURAL place with
# the same wage and milk price
# ====

fit_wmu <- lm(cap_eur ~ wage_eur + milk_eur + urban_class, data = df_milk)
summary(fit_wmu)

# the factor has TWO coefficients, so its overall contribution is tested by
# the F-test of the nested models (a t-test per dummy is not the question):
anova(fit_wm, fit_wmu)

# result: the wage slope is 0.087 (almost the same: 0.0853 before). the two
# class coefficients, Suburban -0.111 (p = 0.17) and Urban +0.046 (p = 0.54),
# are NOT significant individually, yet the joint F-test is: F = 7.1, p =
# 0.0008. how can that be? the dummies are compared with RURAL, which has
# only 140 rows and so a large standard error. the joint test looks at the
# differences among ALL three classes, including Suburban vs Urban: -0.157 euros
# (Suburban cheaper), which is precisely determined. with a factor of three or
# more levels, always read the F-test of the factor first, the single t-tests
# second. and a reminder: in 03 the three classes had the SAME mean price (F = 0.63); now,
# comparing places with the same wage, they do not -- urban places have lower
# wages but equal prices, so at equal wages Urban prices are relatively
# higher. that is the topic of 06


## INTERACTIONS: DOES THE SLOPE DEPEND ON THE OTHER VARIABLE?
# ====
# in the model so far the effect of the wage is the SAME whatever the milk
# price (parallel lines). an INTERACTION lets the slope depend on the other
# variable. in R the formula wage_eur * milk_eur means wage + milk + wage:milk,
# where the product term is the interaction:
#
#   cap_eur = b0 + b1 * wage + b2 * milk + b3 * (wage * milk) + e
#
# the slope of the wage is then b1 + b3 * milk: a different line for each
# milk price. caution: with an interaction, b1 is the wage slope for milk = 0
# (nobody's actual milk price), and the coefficients are hard to read
# directly. center the variables or plot the predictions
# ====

fit_int <- lm(cap_eur ~ wage_eur * milk_eur, data = df_milk)
summary(fit_int)$coefficients
anova(fit_wm, fit_int)

# result: the interaction is -0.002 (t = -0.25, p = 0.80): no sign that the wage
# slope depends on the milk price. we stay with the additive model. (the
# wage x urban_class interaction is another matter, see 06)


## A NOTE ON CAUSATION
# ====
# "controlling for" milk does NOT turn a regression into an experiment. it
# adjusts for the variables we INCLUDED, and says nothing about all the ones
# we did not (tourism, rents, taxes, competition...). a variable that sits
# BETWEEN the predictor and the outcome (if higher wages raised milk prices,
# which then raised cappuccino prices) should NOT be adjusted for: it would
# hide part of the effect we ask about. whether milk is a confounder (a
# common cause of wage and price), a mediator, or neither is a question for
# subject-matter knowledge and a causal diagram, not for the data. all we can
# say here: the wage coefficient is insensitive to adding milk
# ====


## WHAT WE FOUND
# ====
# - milk is a country-level variable: 76 values behind 2386 rows (52 rows have
#   none; 42 of those lose their milk to a non-breaking space in two country
#   names, a data-prep issue)
# - alone, milk looks positively related to price (+0.13 euros per euro of
#   milk, p = 0.008, R^2 = 0.003); next to the wage the milk coefficient is
#   -0.058 (p = 0.18) and the wage slope barely changes (0.0846 -> 0.0853).
#   milk's raw association was borrowed from the wage (richer countries have
#   dearer milk)
# - milk adds nothing by R^2, adjusted R^2 or the F-test
# - in logs milk is "significant" with a NEGATIVE sign (p = 0.0006), but the
#   result vanishes (p = 0.58) once countries are treated as the independent
#   units; the milk effect is not robust, and should not be reported as an
#   effect (4_regression-advanced/01 shows that even its sign flips with the
#   analytic choice, and that most of the wage-price relationship is between
#   countries)
# - the urban class matters jointly (F-test p = 0.0008) once the wage is
#   controlled, even though it did not on its own (03)
# ====


## RECAP: MULTIPLE REGRESSION
# ====
#   question                                tool
#   a model with several predictors         lm(y ~ x1 + x2 + ...)
#   what does a slope mean?                 the change in y per unit of x, other predictors held fixed
#   why does a coefficient change?          the added variable matters for y AND is related to x
#   is the bigger model worth it?           anova(small, big); adjusted R^2 (SAME rows!)
#   several dummies for one factor?         F-test of the factor: anova(small, big)
#   are the predictors too alike?           vif(fit): worry above 5
#   does the slope depend on the other?     interaction: x1 * x2
#   the rows are not independent?           cluster-robust SE at the level where the predictor is constant
#
# a model with 2 predictors is no harder than one with 1 -- but each added
# variable is a decision: why it is in, what changes, and whether the
# conclusion survives its removal. report both versions
# ====


## TRY IT YOURSELF
# ====
# 1. fix the missing New Zealand and South Africa milk prices. in the data,
#    countries are spelled with a non-breaking space ("New Zealand"):
#    clean the country names in data-prep BEFORE the join with the milk table
#    (gsub("\u00a0", " ", country)), rerun it, and see whether the milk
#    coefficient (raw, log, clustered) changes with 42 more rows
# 2. refit the two-predictor model separately for each urban class. does the
#    milk coefficient have the same sign in all three?
# 3. drop the United States (572 rows) and refit the row-level log model. how
#    much does the milk coefficient move? what does that say about "who counts"?
# 4. add log(wage_eur)^2 to the log-log model with milk: does the elasticity of
#    the wage depend on the wage? compare with the squared term from 02
# 5. plot the residuals of fit_wm against milk_eur. why do they show vertical
#    stripes, and what does that say about the independence assumption?
# ====
