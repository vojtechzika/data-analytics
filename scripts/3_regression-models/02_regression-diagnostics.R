# ===================================================================
# 02_regression-diagnostics.R -- CAN WE TRUST THE LINE?
# the assumptions of linear regression (LINE), residual plots (residuals
# vs fitted, QQ, scale-location, leverage and Cook's distance), testing
# for curvature, the log-log model and its interpretation, robust and
# cluster-robust standard errors (the regression versions of Welch and of
# "the rows are not independent"), a cluster bootstrap, and Anscombe's
# quartet once more
# ===================================================================

source("scripts/00_setup.R")

# the new packages of this folder: lmtest (tests for a fitted model) and
# sandwich (robust standard errors). load_packages() is from 00_setup.R
load_packages(c("lmtest", "sandwich"))

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

df_cont <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"),
         !is.na(cap_eur), !is.na(wage_eur),
         cap_eur > 0, wage_eur > 0) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")),
         country     = gsub(" ", " ", country),      # two country names contain a non-breaking space; see the note in 04
         city_id     = paste(country, city, sep = " / ")) # a city name alone is not unique (there are several Springfields)

n_boot <- 2000   # same as in the previous scripts

fit <- lm(cap_eur ~ wage_eur, data = df_cont)   # the model from 01_simple-regression.R
coef(fit)


## WHY CHECK ANYTHING? WHAT EACH ASSUMPTION IS FOR
# ====
# the summary() of a regression always prints numbers -- also when the model
# is a poor description of the data. the assumptions are what makes those
# numbers MEAN what we say they mean. the classic list spells "LINE":
#
#   L  LINEARITY       the average of y really is a straight-line function of
#                      x. if not, the slope describes a line that does not
#                      exist (Anscombe, dataset 2). matters at ANY n
#   I  INDEPENDENCE    the errors of different rows are unrelated. violated by
#                      repeated cities, the same person measured twice, time
#                      series. the standard errors are then too small
#   N  NORMAL ERRORS   the residuals are bell-shaped. needed ONLY for exact
#                      small-sample p-values and for prediction intervals;
#                      for the slope with thousands of rows the CLT rescues us
#                      (same logic as in 0_introduction)
#   E  EQUAL VARIANCE  the spread of the residuals is the same at every x
#                      ("homoscedasticity"). violated -> the standard errors
#                      are wrong (the regression twin of Welch's t-test)
#
# a quick guide to what breaks what: the ESTIMATES (b0, b1) need only
# linearity (and no omitted confounders, see 04); the STANDARD ERRORS, CIs and
# p-values additionally need independence and equal variance; normality
# matters least. none of these are checked by R^2
# ====


## DIAGNOSTIC TABLE: ONE ROW PER OBSERVATION
# ====
# we collect everything the diagnostic plots need in one data frame:
#   fitted      the line's height              resid     observed - fitted
#   std_resid   residual / its own expected sd (so about N(0, 1); beyond +-3 is unusual)
#   leverage    how far x is from the mean of x -- a point far out on x CAN pull the line
#   cooks       how much the whole line moves if this row is dropped (leverage x misfit)
# ====

diag_df <- df_cont %>%
  mutate(fitted    = fitted(fit),
         resid     = residuals(fit),
         std_resid = rstandard(fit),
         leverage  = hatvalues(fit),
         cooks     = cooks.distance(fit))

# the base-R shortcut: four standard plots at once (the same four we build
# below with ggplot)
par(mfrow = c(2, 2)); plot(fit); par(mfrow = c(1, 1))


## PLOT 1: RESIDUALS VS FITTED -- LINEARITY AND EQUAL VARIANCE
# ====
# the single most useful plot. if the model is right, the points form a
# shapeless horizontal band around 0, and the smooth curve through them is
# flat. a CURVE in the smoother means the line misses a bend (linearity); a
# FUNNEL (the band widening or narrowing) means unequal variance
# ====

ggplot(diag_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#e34948") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#2a78d6") +
  labs(x = "fitted price (EUR)", y = "residual (EUR)", title = "Residuals vs fitted") +
  theme_minimal()

# result: three things to notice. (1) the blue smoother is not flat: it is
# slightly above 0 in the middle and sags BELOW 0 on the right -- for the
# highest fitted prices (the high-wage cities) the line over-predicts. that is
# the signature of a bend the straight line misses (we test it below).
# (2) a handful of prices lie 4 to 8 euros ABOVE the line and none are that
# far below: a long upper tail. (3) the spread is not constant: the band is
# narrowest in the middle (residual sd 0.64 in the middle fifth of the fitted
# values) and wider at both ends (0.89 at the left, 1.10 at the right). none
# of this is a disaster, but each point is worth a closer look below


## PLOT 2: QQ-PLOT OF THE RESIDUALS -- NORMALITY
# ====
# the same QQ-plot as in 0_introduction/05 and in the ANOVA script, applied
# to the standardized residuals. points on the line = normal residuals
# ====

ggplot(diag_df, aes(sample = std_resid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line(color = "#e34948") +
  labs(title = "QQ-plot of the standardized residuals") +
  theme_minimal()

# result: the middle follows the line, and both tails leave it -- the upper
# one dramatically (standardized residuals up to 9), the lower one mildly. the
# residuals are heavy-tailed and right-skewed (a few very expensive
# cappuccinos). with 2438 rows the
# slope's t-test is not at risk from this (the CLT), but PREDICTION intervals
# from 01 are -- they assume normal errors, and the real upper tail is
# heavier than they say. Shapiro-Wilk would "reject" immediately, as always
# with this many rows: read the plot, not the p-value (see 0_introduction/05)


## PLOT 3: SCALE-LOCATION -- EQUAL VARIANCE, SECOND LOOK
# ====
# the square root of the absolute standardized residuals against the fitted
# values. a flat smoother = equal variance; a rising one = the spread grows
# with the fitted value
# ====

ggplot(diag_df, aes(x = fitted, y = sqrt(abs(std_resid)))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#2a78d6") +
  labs(x = "fitted price (EUR)", y = "sqrt(|standardized residual|)", title = "Scale-location") +
  theme_minimal()

# a formal test, the Breusch-Pagan test. H0: the variance is constant. a
# SMALL p-value is the bad news (like Levene's test in the ANOVA script), and
# like Levene's test it is very sensitive at this n -- read it together with
# the plots
bptest(fit)

# result: BP = 5.5, p = 0.019 -- mild evidence of unequal variance. the
# smoother in the plot rises on the right (the spread is largest for the
# highest fitted prices) after a dip in the middle. the effect is small
# enough that robust standard errors (below) will change the SE by about a
# quarter, not by an order of magnitude


## PLOT 4: LEVERAGE AND COOK'S DISTANCE -- INFLUENTIAL ROWS
# ====
# a row can be unusual in two ways: far from the other rows in x (HIGH
# LEVERAGE: it has the power to tilt the line) or far from the line in y (a
# large RESIDUAL). it is INFLUENTIAL -- it actually moves the line -- when it
# is both. Cook's distance combines the two. a common flag: Cook's
# distance above 4 / n (here 0.0016), a very rough screen -- many rows exceed it
# in a big sample, so look at the biggest ones rather than at the count
# ====

ggplot(diag_df, aes(x = leverage, y = std_resid, size = cooks)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "#e34948") +
  labs(x = "leverage", y = "standardized residual", size = "Cook's distance",
       title = "Leverage vs residual: influential points are far out AND far from zero") +
  theme_minimal()

sum(diag_df$cooks > 4 / nrow(diag_df))   # how many rows pass the rough flag

diag_df %>%
  arrange(desc(cooks)) %>%
  select(country, city, wage_eur, cap_eur, fitted, std_resid, leverage, cooks) %>%
  head(6)

# result: 163 rows pass the 4 / n flag, which is normal for a skewed outcome.
# the five most influential are exactly the points that stand out on the
# scatterplot: Shenzhen (11.18 euros at a wage of 3.9), St. Moritz (9.51),
# Copenhagen (12.04 at a wage of 18.7), and two high-wage places (Sydney,
# Munich) with LOW prices that pull the line the other way. note that even
# the largest Cook's distance is only 0.07, far below 1: no single row
# controls the line. each of these deserves a check for a data error. the decision rule is not "delete
# influential rows" but "does the conclusion change without them?":

fit_trim <- lm(cap_eur ~ wage_eur, data = diag_df %>% filter(cooks <= 4 / nrow(diag_df)))
rbind(all_rows = coef(fit), without_flagged_rows = coef(fit_trim))

# result: the slope moves from 0.0858 to 0.0887 when all 163 flagged rows are
# dropped -- the story does not depend on them. the conclusion is robust, so we
# keep all rows (dropping 163 rows because they were "inconvenient" would
# itself be a decision to report)


## IS THE RELATIONSHIP REALLY A STRAIGHT LINE? ADD A BEND
# ====
# a simple, honest check for curvature: add the SQUARE of the wage as a
# second predictor, so the model can bend, and ask whether it helps.
#
#   cap_eur = b0 + b1 * wage + b2 * wage^2 + e
#
# (it is still a LINEAR model -- "linear" refers to the coefficients, not to
# the shape of the curve.) a model that is a special case of another is
# "nested" in it, and anova() compares nested models with an F-test of H0:
# "the extra terms add nothing" -- we will use the same tool in 04
# ====

fit_quad <- lm(cap_eur ~ wage_eur + I(wage_eur^2), data = df_cont)
summary(fit_quad)$coefficients
anova(fit, fit_quad)

curve_grid <- data.frame(wage_eur = seq(min(df_cont$wage_eur), max(df_cont$wage_eur), length.out = 200))
curve_grid$line  <- predict(fit, curve_grid)
curve_grid$curve <- predict(fit_quad, curve_grid)

ggplot(df_cont, aes(x = wage_eur, y = cap_eur)) +
  geom_point(alpha = 0.2) +
  geom_line(data = curve_grid, aes(y = line,  color = "straight line"), linewidth = 1) +
  geom_line(data = curve_grid, aes(y = curve, color = "with wage^2"),  linewidth = 1) +
  labs(x = "wage_eur", y = "cap_eur", color = NULL, title = "A straight line and a line that may bend") +
  theme_minimal()

# result: the squared term is -0.0027 (t = -8.0), F = 63 and p = 3e-15: the
# bend is real, and it bends DOWN -- prices rise with the wage at first
# (0.154 euros per euro of wage at a wage near zero) and flatten out for
# high wages. the straight line is a decent but not perfect summary. a bend
# of this kind is easier to describe on a log scale, which is also the usual
# fix for the funnel and the right-skew: next


## THE LOG-LOG MODEL: PERCENTAGES INSTEAD OF EUROS
# ====
# in 2_associations we saw that the log-log correlation (0.57) beats the raw
# one (0.48). the regression version:
#
#   log(cap_eur) = b0 + b1 * log(wage_eur) + e
#
# how to READ b1: a 1% higher wage goes with a b1 % higher price (it is an
# ELASTICITY). exactly: multiplying the wage by a factor k multiplies the
# price by k ^ b1. the model has no intercept in euros -- exp(b0) is the
# price when wage = 1 euro
# ====

fit_log <- lm(log(cap_eur) ~ log(wage_eur), data = df_cont)
summary(fit_log)$coefficients

# in plain words: doubling the wage multiplies the price by 2 ^ b1
c(slope = unname(coef(fit_log)[2]),
  doubling_wage_multiplies_price_by = 2 ^ unname(coef(fit_log)[2]),
  ten_percent_higher_wage = 1.10 ^ unname(coef(fit_log)[2]))

ggplot(df_cont, aes(x = wage_eur, y = cap_eur)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, aes(color = "straight line")) +
  geom_line(data = curve_grid,
            aes(y = exp(predict(fit_log, curve_grid)), color = "log-log model"), linewidth = 1) +
  labs(x = "wage_eur", y = "cap_eur", color = NULL, title = "The log-log model is a curve in euros") +
  theme_minimal()

# result: b1 = 0.232 (t = 34.3): a 10% higher wage goes with a price about
# 2.2% higher, and DOUBLING the wage with a price about 17.5% higher (2 ^ 0.232
# = 1.175). notice the contrast with the straight line, which says 8.6 cents
# per euro EVERYWHERE: the log-log model says the same wage step buys a
# smaller price step at a high wage -- the curve in the plot above.
#
# a warning on comparing models: R^2 = 0.326 for the log model and 0.227 for
# the straight line, but they explain the variation of DIFFERENT outcomes
# (log price vs price), so the two R^2 are not comparable. compare models by
# the residual plots and by what they say, not by R^2 across transformations.
# and the same caution as in 2_associations applies: logs STRETCH the low
# end, so the very low wages are now high-leverage points:

fit_log_trim <- lm(log(cap_eur) ~ log(wage_eur), data = df_cont %>% filter(wage_eur >= 2))
rbind(all_rows = coef(fit_log), wages_at_least_2_eur = coef(fit_log_trim))

# result: the elasticity moves from 0.232 to 0.223 without the wages below
# 2 euros: the conclusion survives. the diagnostics are worth repeating for
# the log model -- redo plots 1-3 with fit_log and see which problems went
# away (the right skew largely did) and which did not (the Breusch-Pagan test
# is now even more significant, BP = 26.3: with this n it flags small
# differences; look at the plots)


## UNEQUAL VARIANCE: ROBUST (HETEROSCEDASTICITY-CONSISTENT) STANDARD ERRORS
# ====
# the regression counterpart of the Welch t-test and Welch ANOVA: keep the
# same coefficients, but compute the standard errors WITHOUT assuming a
# constant residual variance. these are known as "HC" or "sandwich" standard
# errors (HC3 is the recommended flavour for n of a few hundred and up).
# coeftest() prints the usual table with the robust SE substituted:
# ====

coeftest(fit)                                    # classic standard errors
coeftest(fit, vcov = vcovHC(fit, type = "HC3"))  # robust standard errors

# result: the coefficients are identical, the standard error of the slope
# rises from 0.0032 to 0.0041 (+27%): the residual spread really does grow
# with the wage. the slope is still overwhelmingly significant (t = 21.1).
# like Welch's test, robust standard errors cost almost nothing when the
# variances ARE equal and rescue the inference when they are not -- so many
# analysts report them by default


## NON-INDEPENDENT ROWS: CLUSTER-ROBUST STANDARD ERRORS
# ====
# the end of 2_associations warned us: the wage is a property of the CITY, so
# all rows from one city share the same wage and their prices tend to
# resemble each other. the effective sample size is closer to the number of
# CITIES than the number of ROWS, and all standard errors above are too
# small. CLUSTER-ROBUST standard errors allow the errors to be correlated
# within a cluster (any pattern inside, no model for it) while treating
# clusters as independent. the question that matters: WHAT is the cluster?
# the unit that makes rows alike -- here the city, and above it the country
# ====

c(rows = nrow(df_cont),
  different_wages = n_distinct(df_cont$wage_eur),
  cities = n_distinct(df_cont$city_id),
  countries = n_distinct(df_cont$country))

se_table <- bind_rows(
  data.frame(se_type = "classic (rows independent)",   se = coeftest(fit)["wage_eur", 2]),
  data.frame(se_type = "robust HC3",                    se = coeftest(fit, vcov = vcovHC(fit, "HC3"))["wage_eur", 2]),
  data.frame(se_type = "cluster: same wage value",      se = coeftest(fit, vcov = vcovCL(fit, cluster = df_cont$wage_eur))["wage_eur", 2]),
  data.frame(se_type = "cluster: city",                 se = coeftest(fit, vcov = vcovCL(fit, cluster = df_cont$city_id))["wage_eur", 2]),
  data.frame(se_type = "cluster: country",              se = coeftest(fit, vcov = vcovCL(fit, cluster = df_cont$country))["wage_eur", 2])
) %>%
  mutate(slope = coef(fit)["wage_eur"],
         lower = slope - qnorm(0.975) * se,
         upper = slope + qnorm(0.975) * se,
         se_vs_classic = se / se[1])
se_table

ggplot(se_table, aes(x = reorder(se_type, -se), y = slope)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#e34948") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, linewidth = 1, color = "#2a78d6") +
  geom_point(size = 2, color = "#2a78d6") +
  coord_flip() +
  labs(x = NULL, y = "slope of the wage (EUR per EUR), 95% CI",
       title = "The same slope, five ways of being (un)sure about it") +
  theme_minimal()

# result: the slope is always 0.0858; only its uncertainty changes. the classic
# SE is 0.0032; HC3 0.0041; clustering on the wage value 0.0061; on the city
# 0.0096 (3 times the classic); on the COUNTRY 0.0212 (6.6 times the classic).
# the interval for the slope widens from 0.079-0.092 to roughly 0.044-0.127
# with country clusters -- still excluding 0, but now honestly vague about
# the size. the general lesson: the more correlated the rows in a cluster, and
# the more the predictor varies ONLY BETWEEN clusters (here: the wage is
# the same inside a city), the more the naive SE is overconfident. cluster at
# the level where the predictor is constant (city) or higher (country, if
# whole countries behave alike). with only a handful of clusters (say, under
# 30-40) even cluster-robust SEs become unreliable
#
# these are a correction of the STANDARD ERRORS only. a model that
# represents the clustering itself (random intercepts for city / country) is
# the topic of the mixed-model section of 05


## THE CLUSTER BOOTSTRAP FOR THE SLOPE
# ====
# the same idea as in 2_associations, now for the slope: resample whole
# COUNTRIES (with all their rows), refit the line, and keep the slope. the
# percentile interval needs no formula
# ====

country_list <- split(df_cont, df_cont$country)

set.seed(123)
slope_boot <- replicate(n_boot, {
  d <- do.call(rbind, country_list[sample(length(country_list), replace = TRUE)])
  unname(coef(lm(cap_eur ~ wage_eur, data = d))[2])
})

rbind(naive_classic     = confint(fit)["wage_eur", ],
      cluster_robust_se = c(se_table$lower[5], se_table$upper[5]),   # the "country" row of the table
      cluster_bootstrap = quantile(slope_boot, c(0.025, 0.975)))

# result: the country-cluster bootstrap gives 0.045 to 0.127, matching the
# cluster-robust interval almost exactly: two independent routes, the same
# honest answer -- somewhere between about 4 and 13 cents per euro of wage


## ANSCOMBE'S QUARTET, ONCE MORE: THE SAME LINE, FOUR PICTURES
# ====
# in 2_associations the four Anscombe datasets had identical r. the regression
# lines are identical too -- and so are R^2 and the residual standard error.
# only the diagnostics tell them apart, which is exactly why this script
# exists
# ====

anscombe_fits <- lapply(1:4, function(i) {
  d <- data.frame(x = anscombe[[paste0("x", i)]], y = anscombe[[paste0("y", i)]])
  f <- lm(y ~ x, data = d)
  data.frame(set = paste("dataset", i), d,
             intercept = unname(coef(f)[1]), slope = unname(coef(f)[2]), R2 = summary(f)$r.squared,
             sigma = summary(f)$sigma,
             fitted = unname(fitted(f)), resid = unname(residuals(f)),
             leverage = unname(hatvalues(f)), cooks = unname(cooks.distance(f)))
}) %>% bind_rows()

anscombe_fits %>% distinct(set, intercept, slope, R2, sigma)   # four identical rows (up to rounding)

ggplot(anscombe_fits, aes(x = fitted, y = resid)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#e34948") +
  facet_wrap(~ set) +
  labs(x = "fitted", y = "residual", title = "Anscombe's quartet: the residual plots tell the four stories") +
  theme_minimal()

anscombe_fits %>% group_by(set) %>% summarise(max_leverage = max(leverage), max_cooks = max(cooks))

# result: all four fits have intercept 3.0, slope 0.50, R^2 = 0.67 and residual
# SE 1.24. the residual plots are four DIFFERENT pictures: a shapeless band
# (1), a perfect arch (2: the line misses the curve), one outlier standing
# out far above the rest (3) and a vertical stack of points with a single
# point far to the right (4). the influence numbers agree: in dataset 3 the
# outlier has a Cook's distance of 1.39 (above 1: it moves the line a lot).
# dataset 4 is the extreme case: its single distant point has a leverage of
# exactly 1 -- the line passes straight through it, so the entire slope is
# that one row -- and Cook's distance cannot even be computed (NaN)


## WHAT WE FOUND
# ====
# - the straight line is a reasonable first description, but not perfect: a
#   squared term is significant (the price flattens at high wages), the
#   residuals are right-skewed with a heavy upper tail, and the spread is
#   largest for the highest fitted prices
# - the five most influential rows (Shenzhen, St. Moritz, Sydney, Copenhagen,
#   Munich) are visible on the scatterplot; dropping all 163 rows flagged by
#   Cook's rule moves the slope from 0.086 to 0.089 -- the result is robust
# - in logs, a doubling of the wage goes with a price about 17.5% higher
#   (elasticity 0.23); a 10% higher wage with about 2.2% higher price
# - the SLOPE is not in doubt but its PRECISION is: the classic standard
#   error (0.0032) is too small by a factor of 1.3 (unequal variance) and by
#   a factor of 6.6 once countries are allowed to be alike inside (0.0212)
# ====


## RECAP: WHICH PROBLEM, WHICH TOOL
# ====
#   assumption            how we look                      what we do if it fails
#   L  linearity          residuals vs fitted; add x^2     transform (log); add a curve term; model the bend
#   I  independence       think about the design           cluster-robust SE; cluster bootstrap; mixed model (05)
#   N  normal errors      QQ-plot of residuals             matters little for slopes at large n; use bootstrap CIs
#   E  equal variance     residuals vs fitted; scale-loc.  robust (HC3) SE; transform the outcome
#   influential rows      Cook's distance, leverage        check the row; refit without it; REPORT both
#
# the order of the work: (1) plot the data, (2) fit, (3) look at residual
# plots BEFORE reading the p-values, (4) fix or qualify, (5) report
# ====


## TRY IT YOURSELF
# ====
# 1. repeat plots 1-3 for the log-log model (fit_log). which problem got
#    better, which got worse?
# 2. compute the cluster-robust standard errors for the LOG-LOG slope at
#    the city and country level. does the elasticity remain significant?
# 3. remove only the five most influential rows and refit. how much does the
#    slope move? now remove the five rows with the LARGEST positive residuals
#    instead. why is the second change more worrying?
# 4. fit the straight line separately for wages above and below the median
#    wage. do the two slopes agree with the bend found above?
# 5. make a cluster bootstrap that resamples CITIES instead of countries.
#    is its interval between the classic and the country-level one?
# 6. in the Anscombe code, replace the one outlier of dataset 3 by the
#    value predicted by the line through the other points. what happens to
#    R^2 and to the residual standard error?
# ====
