# ===================================================================
# 01_simple-regression.R -- THE STRAIGHT LINE, STEP BY STEP
# (cappuccino price explained by the hourly wage): the model, fitting
# the line by least squares (by hand and with lm), reading summary(),
# the link to the correlation from 2_associations, residuals and R^2,
# standard errors and confidence intervals, the meaning of the
# intercept, units, predictions, and why it is called "regression"
# ===================================================================

source("scripts/00_setup.R")

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

# EXACTLY the same data as in 2_associations/02_two-continuous.R, so that every
# number below can be compared with the correlation we computed there
df_cont <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"),
         !is.na(cap_eur), !is.na(wage_eur),
         cap_eur > 0, wage_eur > 0) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")))

nrow(df_cont)   # 2438 rows, as before

x <- df_cont$wage_eur   # plain vectors, for the by-hand parts
y <- df_cont$cap_eur
n <- length(x)


## WHERE WE ARE: FROM "ARE THEY RELATED?" TO "BY HOW MUCH?"
# ====
# in 2_associations we asked whether wage and cappuccino price move
# together, and answered with a correlation: r = 0.48. r is a good SUMMARY,
# but it leaves three questions open:
#
#   1. by HOW MUCH does the price change when the wage changes by one euro?
#   2. what price would we EXPECT in a city with a wage of, say, 20 euros?
#   3. what happens to the answer if we ALSO take something else into
#      account (the price of milk, the type of place)?
#
# a regression answers all three. the rest of this folder builds it up one
# brick at a time -- this script is the first brick: ONE predictor
# ====


## STEP 0: LOOK AT THE DATA FIRST -- THE SAME SCATTERPLOT
# ====
# the red line that geom_smooth(method = "lm") has been drawing for us since
# 2_associations IS a regression line. today we find out where it comes from
# ====

ggplot(df_cont, aes(x = wage_eur, y = cap_eur)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#e34948") +
  labs(x = "wage_eur (EUR per hour)", y = "cap_eur (EUR)",
       title = "Cappuccino price vs hourly wage") +
  theme_minimal()


## THE MODEL: A STRAIGHT LINE PLUS NOISE
# ====
# a simple linear regression says that the price of a cappuccino is a
# straight-line function of the wage, plus an individual deviation:
#
#   cap_eur  =  b0  +  b1 * wage_eur  +  e
#
#   cap_eur   the OUTCOME (also: response, dependent variable, "y")
#   wage_eur  the PREDICTOR (also: explanatory / independent variable, "x")
#   b0        the INTERCEPT: the line's height at wage = 0
#   b1        the SLOPE: how much the line rises when wage grows by 1 unit
#   e         the ERROR (or disturbance): everything about this particular
#             cappuccino that the wage does not explain -- the cafe, the
#             neighbourhood, tourism, plain noise
#
# b0 and b1 are unknown numbers of the POPULATION -- exactly like a population
# mean or a population correlation. from the sample we compute ESTIMATES,
# written b0-hat and b1-hat (in R scripts: b0 and b1)
#
# unlike correlation, a regression is DIRECTIONAL: it treats one variable as
# the outcome and the other as the predictor. that is a choice of the
# question, not a statement about causes
# ====


## FITTING A LINE: WHAT DOES "BEST" MEAN?
# ====
# many lines go through the cloud. every candidate line gives each row a
# FITTED value (the height of the line above its wage) and a RESIDUAL:
#
#   residual = observed price - fitted price         (the sample version of e)
#
# the line is "best" if the residuals are small overall. positive and
# negative residuals would cancel, so we square them and add them up, the
# SUM OF SQUARED RESIDUALS (SSE, "error sum of squares"). the LEAST-SQUARES
# line is the one with the smallest SSE. let us compare three candidates:
# ====

sse <- function(b0, b1) sum((y - (b0 + b1 * x))^2)

# candidate 1: ignore the wage, a flat line at the mean price
# candidate 2: a guess -- "every euro of wage adds 5 cents"
# candidate 3: the least-squares line (we compute it in a moment)
b1_hand <- cov(x, y) / var(x)
b0_hand <- mean(y) - b1_hand * mean(x)

cand <- data.frame(line = c("flat at the mean", "5 cents per euro (guess)", "least squares"),
                   b0   = c(mean(y), 2.9, b0_hand),
                   b1   = c(0,       0.05, b1_hand))
cand$sse <- mapply(sse, cand$b0, cand$b1)
cand

ggplot(df_cont, aes(x = wage_eur, y = cap_eur)) +
  geom_point(alpha = 0.2) +
  geom_abline(data = cand, aes(intercept = b0, slope = b1, color = line), linewidth = 1) +
  labs(x = "wage_eur", y = "cap_eur", color = NULL,
       title = "Three candidate lines: the least-squares line has the smallest SSE") +
  theme_minimal()

# result: the flat line has SSE = 2302, our guess 2235, and the least-squares
# line 1781 -- smaller than both, and no other line can beat it. the flat line is
# special: its SSE is the TOTAL variation of the prices around their mean,
# which we will meet again below as SST


## THE LEAST-SQUARES LINE, BY HAND
# ====
# calculus gives the minimizing line in closed form, and the formulas are
# ones we already know from 2_associations:
#
#   b1 = cov(x, y) / var(x)  =  r * sd(y) / sd(x)       the slope
#   b0 = mean(y) - b1 * mean(x)                         the intercept
#
# reading: the slope is the covariance rescaled by the spread of the
# PREDICTOR (so it has the units "euros of price per euro of wage"), and the
# intercept forces the line through the point of means (mean(x), mean(y)).
# a computer can also find it by brute force -- optim() searches for the
# b0 and b1 with the smallest SSE and lands on the same numbers:
# ====

c(b1_cov_var = b1_hand, b1_r_sd = cor(x, y) * sd(y) / sd(x))   # two ways to the same slope
b0_hand

optim(c(0, 0), function(b) sse(b[1], b[2]))$par   # brute-force search: same numbers (up to rounding)


## THE SAME THING WITH lm()
# ====
# lm() = "linear model". the formula  outcome ~ predictor  is the same
# notation as in the t-tests and aov() of 1_group-comparisons. we STORE the
# fitted model in an object -- everything else in this script is something
# we ask that object
# ====

fit <- lm(cap_eur ~ wage_eur, data = df_cont)

fit            # just the two coefficients
coef(fit)      # as a vector
c(b0_hand, b1_hand)   # the by-hand numbers match

# result: b0 = 2.82 and b1 = 0.0858. the line is
#
#   expected price = 2.82 + 0.0858 * wage
#
# rows whose wage is one euro per hour higher have, on average, a cappuccino
# that is 8.6 cents more expensive. mind the words: "on average" and "rows
# whose wage is higher" -- a DESCRIPTION of how the data line up, not a
# claim that raising a wage would raise a price


## READING summary(fit)
# ====
# summary() is the standard report. it has four blocks:
#
#   Residuals       the five-number summary of the residuals (a quick look at
#                   whether the misses are balanced around 0)
#   Coefficients    one row per coefficient: Estimate, Std. Error (the
#                   standard error of the estimate), t value = Estimate /
#                   Std. Error, and Pr(>|t|), the two-tailed p-value of
#                   H0: "this coefficient is 0 in the population"
#   Residual standard error   the typical size of a residual, in EUROS
#   Multiple R-squared, F-statistic   how much the model explains, and a test
#                   of the model as a whole (more on both below)
# ====

summary(fit)

# the pipe-friendly, data-frame version of the coefficient table
# (broom is not in 00_setup.R; install.packages("broom") if you want it)
# broom::tidy(fit, conf.int = TRUE)


## THE HYPOTHESES FOR THE SLOPE
# ====
#   H0: b1 = 0    (the line is flat: the wage tells us nothing about the price)
#   H1: b1 != 0
#
# the test of the slope is the t-test in the "wage_eur" row of summary().
# note what it is and is not: it tests whether the slope is ZERO, not
# whether it is large -- always read the estimate and its interval, not only
# the p-value
# ====


## THE LINK TO CORRELATION: NOTHING NEW UNDER THE SUN
# ====
# the bridge at the end of 2_associations told us three things. now we can
# check them:
#   (1) slope = r * sd(y) / sd(x)
#   (2) R^2 = r^2
#   (3) the test of the slope gives the SAME p-value as the correlation test
# ====

r <- cor(x, y)

c(slope = unname(coef(fit)["wage_eur"]), r_sd_ratio = r * sd(y) / sd(x))          # (1)
c(R2 = summary(fit)$r.squared, r_squared = r^2)                                    # (2)

c(t_regression = summary(fit)$coefficients["wage_eur", "t value"],                 # (3)
  t_correlation = unname(cor.test(x, y)$statistic))
c(p_regression = summary(fit)$coefficients["wage_eur", "Pr(>|t|)"],
  p_correlation = cor.test(x, y)$p.value)

# the slope of the STANDARDIZED variables (each divided by its sd) is r itself
coef(lm(scale(y) ~ scale(x)))[2]

# result: identical t (26.7) and p (4e-138): the correlation test and the
# slope test are one test. the standardized slope is 0.476 = r. so r answers
# "how tightly" on a unit-free scale, and the slope answers "how much" in
# euros per euro -- the same r can come with a steep or a shallow line


## FITTED VALUES AND RESIDUALS
# ====
# fitted() gives the height of the line above each row, residuals() the
# vertical miss. both are by-products of the model, and both are worth a
# look -- the residuals are where a model's problems show up
# ====

df_cont$fitted <- fitted(fit)
df_cont$resid  <- residuals(fit)

df_cont %>% select(wage_eur, cap_eur, fitted, resid) %>% head()

# check by hand: fitted = b0 + b1 * x, residual = observed - fitted
all.equal(unname(df_cont$fitted), b0_hand + b1_hand * x)
all.equal(unname(df_cont$resid), y - (b0_hand + b1_hand * x))

# two facts that hold for ANY least-squares line with an intercept:
#   - the residuals add up to zero (the line balances the misses)
#   - the residuals are exactly uncorrelated with the predictor (if they
#     were correlated, a line with a different slope would fit better)
c(sum_resid = sum(df_cont$resid), cor_resid_x = cor(df_cont$resid, x))

# the picture: the vertical segments are the residuals; least squares makes
# the sum of their SQUARES as small as possible
set.seed(123)
df_cont %>%
  slice_sample(n = 60) %>%
  ggplot(aes(x = wage_eur, y = cap_eur)) +
  geom_segment(aes(xend = wage_eur, yend = fitted), color = "#e34948", alpha = 0.6) +
  geom_point() +
  geom_abline(intercept = b0_hand, slope = b1_hand, color = "#2a78d6", linewidth = 1) +
  labs(x = "wage_eur", y = "cap_eur",
       title = "Residuals: the vertical distances from the line (60 random rows)") +
  theme_minimal()


## HOW MUCH DOES THE MODEL EXPLAIN? SST, SSR, SSE AND R^2
# ====
# the same bookkeeping as in the ANOVA of 1_group-comparisons: the total
# variation of the outcome is split into a part the model captures and a part
# it does not:
#
#   SST = sum( (y - mean(y))^2 )          total variation (the flat line's SSE)
#   SSE = sum( (y - fitted)^2 )           left over: the residuals
#   SSR = sum( (fitted - mean(y))^2 )     captured by the line ("regression")
#   SST = SSR + SSE
#
#   R^2 = SSR / SST = 1 - SSE / SST       the SHARE of the variation explained
#
# note: it is the SAME decomposition as SS_total = SS_between + SS_within in
# the one-way ANOVA -- a hint that ANOVA and regression are one family
# (script 03 makes it explicit)
# ====

sst <- sum((y - mean(y))^2)
sse_fit <- sum(df_cont$resid^2)
ssr <- sum((df_cont$fitted - mean(y))^2)

c(SST = sst, SSR_plus_SSE = ssr + sse_fit)
c(R2_by_hand = ssr / sst, R2_lm = summary(fit)$r.squared)

# the RESIDUAL STANDARD ERROR is the typical miss, in the units of the
# outcome (euros). it divides SSE by n - 2 degrees of freedom -- two
# parameters (b0, b1) were estimated from the data
sigma_hat <- sqrt(sse_fit / (n - 2))
c(by_hand = sigma_hat, lm = summary(fit)$sigma)

# result: R^2 = 0.2266 (as r^2 in 2_associations): a straight line through the
# wage captures about 23% of the variation in prices. the residual standard
# error is 0.85 euros: even knowing the wage, a cappuccino is typically about
# 85 cents away from the line -- compare that with the sd of the prices
# themselves
sd(y)

# (sd(y) = 0.97 euros; the model shrinks the typical error from 0.97 to
# 0.85 euros -- not a dramatic gain, even though R^2 sounds respectable. the
# humbling side of R^2 again.) one more note: R^2 is
# descriptive, not a test and not a verdict. a low R^2 is normal when
# the outcome is noisy (prices, behavior), and a high R^2 can be worthless
# (see Anscombe in 02_regression-diagnostics.R)


## HOW SURE ARE WE? THE STANDARD ERROR OF THE SLOPE
# ====
# a different sample of cappuccinos would give a slightly different line.
# the standard error says by how much the slope typically wobbles:
#
#   se(b1) = sigma_hat / sqrt( sum( (x - mean(x))^2 ) )
#
# three things make the slope MORE precise: less noise around the line
# (smaller sigma), more observations, and a wider spread of the predictor
# (a line is easier to pin down when the points cover a long stretch of x).
# then t = b1 / se(b1) is compared with a t-distribution with n - 2 df --
# the same logic as every t-test in the course
# ====

se_b1 <- sigma_hat / sqrt(sum((x - mean(x))^2))
t_b1  <- b1_hand / se_b1
p_b1  <- 2 * pt(-abs(t_b1), df = n - 2)
c(se = se_b1, t = t_b1, p = p_b1)

# the 95% confidence interval: estimate +/- critical t * standard error
b1_hand + c(-1, 1) * qt(0.975, df = n - 2) * se_b1
confint(fit)   # lm's version, for both coefficients: the same interval for the slope

# the F-test at the bottom of summary() asks whether the model as a whole
# beats the flat line. with ONE predictor it is just the squared t-test:
c(F = unname(summary(fit)$fstatistic[1]), t_squared = t_b1^2)

# result: se = 0.0032, t = 26.7, 95% CI for the slope 0.0795 to 0.0920 -- the
# data are compatible with a rise of roughly 8 to 9 cents per euro of wage,
# and firmly incompatible with 0. but remember the warning from the end of
# 2_associations: these standard errors assume independent rows, and we have
# only about 870 different wages for 2438 rows. the intervals are too narrow.
# fixing that is the topic of 02_regression-diagnostics.R


## THE INTERCEPT: OFTEN MEANINGLESS -- AND HOW TO FIX THAT
# ====
# b0 = 2.82 is the expected price at a wage of ZERO euros per hour. no
# real city has that (the lowest wage is 0.20), so the intercept is an
# extrapolation and nobody should read it as "the price of a cappuccino
# when nobody earns anything". a standard remedy is to CENTER the predictor:
# subtract its mean, so that 0 means "an average wage". the slope does not
# change; the intercept becomes the expected price at the average wage
# ====

fit_c <- lm(cap_eur ~ I(wage_eur - mean(wage_eur)), data = df_cont)   # I() = "compute this as written"
coef(fit_c)
c(mean_price = mean(y), mean_wage = mean(x))

# result: the intercept is now 3.94 = the mean price (the line passes through
# the point of means -- see the formula for b0), and the slope is still
# 0.0858. the model has the same R^2 and the same fit: only the question
# "what does 0 mean?" changed


## CHANGING UNITS: WHAT HAPPENS TO THE SLOPE?
# ====
# 8.6 cents per euro is hard to feel -- a euro of hourly wage is a small step.
# measure the wage in TENS of euros and the slope becomes the price
# difference for a 10-euro higher wage. the model is the same, the numbers
# are rescaled; the t-value, p-value and R^2 do not change at all
# ====

fit_10 <- lm(cap_eur ~ I(wage_eur / 10), data = df_cont)
summary(fit_10)$coefficients

# slope x 10 (price in cents instead of euros would multiply it by 100):
c(per_1_euro = unname(coef(fit)[2]), per_10_euro = unname(coef(fit_10)[2]))

# rule: the slope has the units "units of y per unit of x". choose units of
# x that make a one-unit step MEANINGFUL (here: 10 euros -- the sd of the
# wage is about 5.4 euros, so 10 euros is nearly two sds: a big but realistic
# difference. the slope per 10 euros is 0.86: a cappuccino about 86 cents dearer)


## PREDICTION: WHAT PRICE DO WE EXPECT AT A GIVEN WAGE?
# ====
# predict() plugs new wages into the line. there are TWO different
# uncertainties one can attach, and beginners often mix them up:
#
#   confidence interval   for the AVERAGE price of all cappuccinos in places
#                         with this wage -- only the uncertainty about the
#                         LINE (it shrinks as n grows)
#   prediction interval   for ONE new cappuccino in such a place -- the line's
#                         uncertainty PLUS the individual noise e (it never
#                         shrinks below the width of the noise)
# ====

new_wages <- data.frame(wage_eur = c(5, 10, 15, 20, 30))

cbind(new_wages,
      predict(fit, new_wages, interval = "confidence")) # the average price
predict(fit, new_wages, interval = "prediction")        # one new cappuccino

# the picture: both bands around the line. the inner one (confidence) is
# barely visible; the outer one (prediction) contains most of the points
pred_grid <- data.frame(wage_eur = seq(min(x), max(x), length.out = 200))
pred_ci <- cbind(pred_grid, predict(fit, pred_grid, interval = "confidence"))
pred_pi <- cbind(pred_grid, predict(fit, pred_grid, interval = "prediction"))

ggplot(df_cont, aes(x = wage_eur, y = cap_eur)) +
  geom_point(alpha = 0.2) +
  geom_ribbon(data = pred_pi, aes(y = fit, ymin = lwr, ymax = upr), fill = "#2a78d6", alpha = 0.15) +
  geom_ribbon(data = pred_ci, aes(y = fit, ymin = lwr, ymax = upr), fill = "#e34948", alpha = 0.5) +
  geom_line(data = pred_ci, aes(y = fit), color = "#e34948", linewidth = 1) +
  labs(x = "wage_eur", y = "cap_eur",
       title = "The line, its confidence band (red) and its prediction band (blue)") +
  theme_minimal()

# result: at a wage of 15 euros the expected price is 4.10 euros; the
# confidence interval for the AVERAGE price is very narrow (4.07 to 4.14),
# but a prediction interval for ONE cappuccino runs from 2.43 to 5.78. knowing the line precisely does not tell you the price of a
# particular coffee -- that is the 85-cent residual noise again
#
# two warnings on prediction. (1) stay inside the range of the data:
# predicting at a wage of 80 euros is EXTRAPOLATION -- the line keeps going,
# reality may not. (2) the prediction describes the pattern in these rows;
# it is not a forecast of what happens if a particular city changes its wage


## WHY IS IT CALLED "REGRESSION"? A GLANCE AT 1886
# ====
# Francis Galton compared the heights of parents and their adult children
# and found that tall parents have, on average, children who are tall but
# LESS tall than the parents -- and short parents have children closer to
# the average. he called this "regression towards mediocrity". it is the
# slope of the STANDARDIZED line: it equals r, which is below 1, so a
# value that is z standard deviations from the mean is PREDICTED to be only
# r * z standard deviations from the mean on the other variable
#
# here: a city whose wage is 2 sd above the mean is predicted to have a
# price only 2 * 0.476 = 0.95 sd above the mean price
# ====

z_wage <- 2
z_price_pred <- unname(coef(lm(scale(y) ~ scale(x)))[2]) * z_wage
z_price_pred   # 0.95 sd above the mean price, not 2

# the line is not "wrong" -- it is the best guess when the link is not
# perfect. this is also why predicting x from y gives a DIFFERENT line from
# predicting y from x (try lm(wage_eur ~ cap_eur, data = df_cont) and see)


## WHAT WE FOUND
# ====
# - the least-squares line is: expected price = 2.82 + 0.0858 * wage; one more
#   euro of hourly wage goes with a cappuccino about 8.6 cents dearer (95% CI
#   7.9 to 9.2 cents; a naive interval -- see the next script)
# - slope, R^2 and the test of the slope are all the correlation of
#   2_associations in other clothes: R^2 = r^2 = 0.227, t = 26.7
# - the line explains 23% of the variation in prices; a typical residual is
#   85 cents
# - confidence bands describe the average; prediction bands describe one
#   cappuccino, and are far wider
# ====


## RECAP: SIMPLE LINEAR REGRESSION
# ====
#   piece                  what it is                              R
#   model                  y = b0 + b1 * x + e                     lm(y ~ x, data)
#   fit                    least squares: minimize SSE             coef(fit)
#   slope b1               change in y per 1 unit of x             = r * sd(y) / sd(x)
#   intercept b0           expected y at x = 0                     center x to make it meaningful
#   residual               observed - fitted                       residuals(fit)
#   R^2                    share of the variation explained        summary(fit)$r.squared
#   residual std. error    typical miss, in units of y             summary(fit)$sigma
#   test of the slope      H0: b1 = 0, t with n - 2 df             summary(fit); confint(fit)
#   prediction             confidence (average) vs prediction (one)   predict(fit, new, interval = )
#
# the model rests on four assumptions (LINE: Linearity, Independence, Normal
# residuals, Equal variance), which we check in the next script. a good fit
# in the summary() is NOT evidence that they hold
# ====


## TRY IT YOURSELF
# ====
# 1. fit lm(wage_eur ~ cap_eur, data = df_cont). is its slope the inverse of
#    the slope of the price-on-wage line? (no: 2.64, not 1 / 0.0858 = 11.7.)
#    compare its R^2 and its t-value with those of the original model: what
#    stays the same, what changes, and why? (hint: r is symmetric, slopes
#    are not)
# 2. change the second candidate line (b0 = 2.9, b1 = 0.05) to a line you
#    think is better. can you get below the SSE of the least-squares line?
# 3. predict the price at a wage of 80 euros and of 0 euros, and compare
#    with the minimum and maximum wages in the data. which prediction would
#    you trust, and why?
# 4. fit the model separately for each urban class (filter first). does the
#    slope depend on the class? which section of 2_associations predicted
#    what you see?
# 5. take the 60 rows of the residual plot and fit the line to ONLY those
#    rows (set.seed(123); slice_sample(n = 60)). how far are the slope and
#    its standard error from the full-data values? repeat with n = 600
# ====
