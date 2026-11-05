# ===================================================================
# 05_logit-marginal-effects.R -- WHEN THE OUTCOME IS YES / NO
# (is the cappuccino "expensive"?): why a straight line is awkward for
# a 0/1 outcome, the logit model, why its coefficients cannot be read
# directly, MARGINAL EFFECTS -- the way to say "one more euro of wage
# goes with p percentage points higher probability" -- predicted
# probabilities, percentage points vs percent, a factor next to the
# wage, and honest standard errors
# ===================================================================

source("scripts/00_setup.R")

# marginaleffects: turns the coefficients of a model (also of the logit
# below) into effects on the scale we care about. lmtest + sandwich are
# from 02 (cluster-robust standard errors; marginaleffects uses sandwich
# behind the scenes)
load_packages(c("marginaleffects", "lmtest", "sandwich"))

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

# the same rows as in 01-04, and the same yes/no outcome as in
# 2_associations/01: "expensive" = a price above the median. this time we
# store it as a plain 0/1 NUMBER (1 = expensive), because glm() models the
# probability of the 1. (the median is taken over these rows, so it can
# differ a little from the one in 2_associations)
df_cont <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban"),
         !is.na(cap_eur), !is.na(wage_eur),
         cap_eur > 0, wage_eur > 0) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")),
         country     = gsub("\u00a0", " ", country),
         expensive   = as.integer(cap_eur > median(cap_eur)))

nrow(df_cont)
table(df_cont$expensive)
mean(df_cont$expensive)   # the share of expensive cappuccinos: close to 0.5 by construction


## WHERE WE ARE: FROM "HOW MUCH?" TO "HOW LIKELY?"
# ====
# so far the outcome was a number (the price in euros). often the outcome is
# a yes/no: did the person donate, did the patient recover, is the
# cappuccino expensive. the quantity we model is then a PROBABILITY, and the
# question we want to answer keeps the same shape as in 01:
#
#   "when the wage is one euro higher, the probability of an expensive
#    cappuccino is ... PERCENTAGE POINTS higher"
#
# this script shows how to get that sentence out of a logit model, and why
# it takes one extra step compared with lm()
# ====


## STEP 0: LOOK AT THE DATA FIRST
# ====
# a scatterplot of a 0/1 outcome is two rows of dots, so we jitter them
# vertically. the blue curve is the share of expensive cappuccinos at each
# wage (a logit fit, explained below); the red line is the straight line
# of 01 fitted to the 0/1 values
# ====

ggplot(df_cont, aes(x = wage_eur, y = expensive)) +
  geom_jitter(height = 0.05, alpha = 0.15) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#e34948") +
  geom_smooth(method = "glm", method.args = list(family = binomial),
              formula = y ~ x, se = FALSE, color = "#2a78d6") +
  labs(x = "wage_eur (EUR per hour)", y = "expensive (1 = above the median price)",
       title = "A 0/1 outcome: the straight line and the S-shaped curve") +
  theme_minimal()

# result: the red line is straight and keeps rising, so at the highest wages it
# climbs above 1 (an impossible probability); the blue curve bends and flattens
# as it approaches 1 (and, at the low end, 0). in the middle of the wage range
# the two are close


## THE EASY START: A STRAIGHT LINE ON A 0/1 OUTCOME
# ====
# nothing stops us from running lm() on the 0/1 outcome. this is called the
# LINEAR PROBABILITY MODEL, and its slope is already in the units we want:
# the change in the PROBABILITY per unit of x. the fitted value is the
# estimated probability of "expensive"
# ====

fit_lpm <- lm(expensive ~ wage_eur, data = df_cont)
summary(fit_lpm)$coefficients

# but a probability must stay between 0 and 1, and a straight line does not
# know that:
range(fitted(fit_lpm))
sum(fitted(fit_lpm) < 0 | fitted(fit_lpm) > 1)   # how many rows get an impossible "probability"?

# result: the slope is 0.033: one more euro of hourly wage goes with a 3.3
# percentage point higher probability of an expensive cappuccino. but 13 rows
# get a fitted "probability" above 1 (the largest is 1.73). the line is a fine
# first look, but it predicts probabilities above 1 -- and the true curve must
# flatten as it approaches 0 and 1. so we use a curve that stays inside [0, 1]


## THE LOGIT MODEL: AN S-SHAPED CURVE
# ====
# the logit (logistic regression) model keeps the straight line, but puts it
# on a different scale -- the LOG-ODDS -- and then bends it into a
# probability:
#
#   log-odds  =  b0 + b1 * wage_eur                      (a straight line)
#   p         =  1 / (1 + exp(-(b0 + b1 * wage_eur)))    (an S-curve in [0, 1])
#
# the odds are p / (1 - p): odds of 3 mean "3 expensive for every 1
# not expensive" (p = 0.75). the log of the odds can be any number, which is
# why a straight line is allowed there. glm() ("generalized linear model")
# fits it; family = binomial with its default link is the logit
# ====

fit_logit <- glm(expensive ~ wage_eur, data = df_cont, family = binomial)
summary(fit_logit)$coefficients

# the output looks like lm(), but READ IT DIFFERENTLY:
#   - the Estimate is in LOG-ODDS: b1 is the change in the log-odds of
#     "expensive" per euro of wage. its SIGN and its p-value are
#     meaningful, its SIZE is not something you can say out loud
#   - the test column is a z-value (a normal approximation), not a t-value

# the one transformation you will meet in papers: exp(b1) is the ODDS RATIO,
# the factor by which the ODDS are multiplied per euro of wage
exp(cbind(odds_ratio = coef(fit_logit), confint.default(fit_logit)))

# result: the odds ratio of the wage is 1.17 (95% CI 1.15 to 1.19): each euro of
# wage multiplies the odds of an expensive cappuccino by about 1.17, i.e. 17%
# higher odds. above 1 = more likely, below 1 = less likely, exactly 1 = no
# relation. it is correct, but few people have a feeling for odds -- we can do better


## WHY NOT JUST READ THE SLOPE? THE EFFECT DEPENDS ON WHERE YOU ARE
# ====
# in lm() one more euro of wage always moved the price by b1 euros. in the
# logit it does not: the curve is steep in the middle and flat at both ends,
# so the same euro moves the probability a lot where p is around 0.5 and
# hardly at all where p is close to 0 or 1. the slope of the curve at a row
# is
#
#   marginal effect  =  b1 * p * (1 - p)           (p = that row's probability)
#
# there is no single "the effect of the wage" -- there is one for every
# row. a MARGINAL EFFECT is this slope, on the PROBABILITY scale (so: in
# percentage points). to get one number, we summarise the rows' effects
# ====


## THE AVERAGE MARGINAL EFFECT (AME), BY HAND
# ====
# step 1: the predicted probability of every row.
# step 2: its marginal effect, b1 * p * (1 - p).
# step 3: the AVERAGE over all rows -- the average marginal effect.
# ====

b1    <- unname(coef(fit_logit)["wage_eur"])
p_hat <- fitted(fit_logit)               # = predict(fit_logit, type = "response")

me_rows <- b1 * p_hat * (1 - p_hat)      # one marginal effect per row
summary(me_rows)                         # they differ by row...
ame_hand <- mean(me_rows)                # ...and their average is the AME
ame_hand

# a second way to the same idea, without calculus: predict the probability
# for every row at its own wage and at wage + 1, and average the difference
p_up <- predict(fit_logit, newdata = transform(df_cont, wage_eur = wage_eur + 1), type = "response")
ame_step <- mean(p_up - p_hat)
c(derivative_version = ame_hand, one_euro_step_version = ame_step)

# result: the AME is 0.034, i.e. one more euro of hourly wage goes with a
# probability of an expensive cappuccino that is 3.4 percentage points higher,
# on average over the rows. the effects of single rows range from almost 0 to
# 3.9 points. this is very close to the slope of the linear probability model
# above (3.3 points), which is why the LPM is a respectable quick check. the two
# ways of computing the AME differ in the 4th decimal (0.0341 vs 0.0340)


## THE SAME THING WITH marginaleffects
# ====
# you will not do this by hand in practice. marginaleffects::avg_slopes()
# returns the AME with its standard error, z-test and confidence interval
# (by the delta method -- a formula for the standard error of a
# transformation, which we leave to the package)
# ====

avg_slopes(fit_logit, variables = "wage_eur")

# and the step version, which answers "what if the wage is exactly 1 euro
# higher?" -- avg_comparisons() with a chosen step size:
avg_comparisons(fit_logit, variables = list(wage_eur = 1))

# result: both agree with the by-hand numbers above: 0.034 (95% CI 0.031 to
# 0.037), i.e. 3.4 percentage points per euro. the estimate is a change in
# PROBABILITY: multiply by 100 to get percentage points


## OTHER KINDS OF "THE" MARGINAL EFFECT
# ====
# the average marginal effect is the one to report by default. you will see
# two cousins:
#   - marginal effect AT THE MEAN (MEM): the effect for one imaginary "average"
#     row (mean wage). convenient, but nobody lives at the mean of all
#     variables, and with a factor the "mean row" does not exist
#   - marginal effect AT CHOSEN VALUES: the effect at a wage of 5, 15, 25...
#     -- often the most informative of all, see the next section
# ====

slopes(fit_logit, variables = "wage_eur", newdata = "mean")
avg_slopes(fit_logit, variables = "wage_eur")

# result: the effect at the mean wage is 0.039 (3.9 points), the average of the
# effects is 0.034 (3.4 points). they differ because the effect at the mean wage
# is not the average of the effects (the curve is not a straight line). report
# the AME by default


## PREDICTED PROBABILITIES AND THE MARGINAL EFFECT ALONG THE CURVE
# ====
# a marginal effect says how steep the curve is; predicted probabilities say
# where on the curve we are. report both: "at a wage of 10 euros the
# probability is ..., at 20 euros ..."
# ====

wage_values <- c(5, 10, 15, 20, 30)

predictions(fit_logit, newdata = datagrid(wage_eur = wage_values))   # p at chosen wages
slopes(fit_logit, variables = "wage_eur",
       newdata = datagrid(wage_eur = wage_values))                   # the effect of one more euro, there

plot_predictions(fit_logit, condition = "wage_eur") +
  labs(x = "wage_eur (EUR per hour)", y = "probability of an expensive cappuccino",
       title = "The S-curve: predicted probability by wage") +
  theme_minimal()

plot_slopes(fit_logit, variables = "wage_eur", condition = "wage_eur") +
  labs(x = "wage_eur (EUR per hour)", y = "effect of +1 euro (probability points)",
       title = "The marginal effect is not constant: it depends on the wage") +
  theme_minimal()

# result: the probability of an expensive cappuccino is 0.22 at a wage of 5 euros,
# 0.38 at 10, 0.57 at 15, 0.74 at 20 and 0.93 at 30. the effect of one more euro
# is largest in the middle, around 10 to 15 euros, where the probability is near
# 0.5 (3.7-3.8 points); it is smaller at 5 euros (2.7), at 20 euros (3.0) and
# almost gone at 30 euros (1.0 point). the AME above (3.4) is the average of this
# curve over the wages that occur in the data


## PERCENTAGE POINTS ARE NOT PERCENT
# ====
# the single most common mistake in reporting a logit. a change in a
# probability can be said in two ways:
#
#   percentage POINTS   the DIFFERENCE of the probabilities (absolute):
#                       from 40% to 45% is +5 percentage points
#   PERCENT             the RELATIVE change:
#                       from 40% to 45% is +12.5 percent
#
# a marginal effect is in percentage points (when the estimate is multiplied
# by 100). "the probability is 5 percent higher" would be a different,
# wrong claim. let us see both for a wage of 10 vs 15 euros:
# ====

p10 <- predict(fit_logit, data.frame(wage_eur = 10), type = "response")
p15 <- predict(fit_logit, data.frame(wage_eur = 15), type = "response")

c(p_at_10 = unname(p10), p_at_15 = unname(p15))
c(points = unname(100 * (p15 - p10)),            # percentage points
  percent = unname(100 * (p15 - p10) / p10))     # percent (relative)

# result: the probability rises from 38% to 57%, i.e. +19.3 percentage points --
# which is +51 percent in relative terms. these are very different numbers for
# the same change; say it with the unit that the reader can check against the two
# probabilities


## ADDING A FACTOR: THE URBAN CLASS NEXT TO THE WAGE
# ====
# a logit takes more predictors and factors exactly like lm() in 03 and 04:
# the formula is the same. the average marginal effect of a numeric variable is
# its slope on the probability scale; for a FACTOR it is the average
# difference in probability between a level and the reference (Rural), at
# the same other variables
# ====

fit_logit2 <- glm(expensive ~ wage_eur + urban_class, data = df_cont, family = binomial)
summary(fit_logit2)$coefficients   # log-odds again: signs and p-values only

avg_slopes(fit_logit2)             # one line per variable / contrast, in probability points

# a larger, more natural step for the wage: 10 euros (about 2 standard
# deviations of the wage in this data, see 01), averaged over all rows
avg_comparisons(fit_logit2, variables = list(wage_eur = 10))

# result: at the same wage, Suburban places are 5.3 percentage points LESS likely
# and Urban places 0.8 points MORE likely to have an expensive cappuccino than
# Rural ones, but neither difference is distinguishable from 0 (p = 0.21 and
# 0.85; the Rural group has only 140 rows). the wage effect is 3.5 points per
# euro and 30 points per 10 euros (95% CI 28 to 32). note that the class
# differences found for the price itself in 04 and 06 do not show up in this
# yes/no version of the outcome


## HOW SURE ARE WE? THE STANDARD ERRORS AGAIN
# ====
# the same warning as in 02 and 04: the rows are not independent (the wage
# belongs to the city, cities belong to countries), so the classic standard
# errors are too small. marginaleffects accepts the cluster-robust standard
# errors of 02 in one argument: vcov = ~ country
# ====

rbind(classic = avg_slopes(fit_logit2, variables = "wage_eur")[, c("estimate", "std.error", "conf.low", "conf.high")],
      country = avg_slopes(fit_logit2, variables = "wage_eur", vcov = ~country)[, c("estimate", "std.error", "conf.low", "conf.high")])

# result: the AME is the same (0.0347), the standard error is 6.6 times larger
# when countries are the clusters (0.0016 -> 0.0106), and the interval widens from
# 3.2-3.8 to 1.4-5.6 percentage points per euro. it is still clearly away from 0.
# report the clustered one, as in 02 and 04


## WHAT WE LEFT OUT
# ====
# the logit has a theory of its own: how it is fitted (maximum likelihood),
# what the deviance and the pseudo-R^2 mean, how to judge classification
# (ROC curves), separation problems with rare outcomes. none of it is needed
# to fit a logit and to report its effects correctly, so we skip it here. what
# you must keep from this script is the REPORTING: marginal effects, in
# percentage points, with honest standard errors
# ====


## WHAT WE FOUND
# ====
# - a 0/1 outcome is modelled through its probability: lm() on it (the linear
#   probability model) is a quick check, but predicts impossible probabilities
# - the logit coefficients are log-odds: their sign and p-value are
#   meaningful, their size is not directly interpretable; the odds ratio
#   exp(b) is correct but unintuitive
# - the average marginal effect turns the logit into a sentence: "one more unit of
#   x goes with ... percentage points higher probability of y". for a factor it is the
#   difference in probability from the reference level
# - the effect depends on where on the curve you are, so also report predicted
#   probabilities at chosen values
# ====


## RECAP: LOGIT AND MARGINAL EFFECTS
# ====
#   piece                       what it is                                R
#   model                       log-odds = b0 + b1 * x                    glm(y ~ x, data, family = binomial)
#   coefficient                 change in log-odds per unit of x          coef(fit)
#   odds ratio                  factor on the odds per unit of x          exp(coef(fit))
#   predicted probability       p at given x                              predictions(fit, newdata = datagrid(x = ))
#   marginal effect (row)       b1 * p * (1 - p)                          slopes(fit, variables = "x")
#   average marginal effect     mean of the rows' marginal effects        avg_slopes(fit)
#   effect of a chosen step     average change in p for x + step          avg_comparisons(fit, variables = list(x = step))
#   clustered standard errors   rows of a cluster are alike               ... , vcov = ~cluster
#
# to REPORT: "a 10-euro higher hourly wage goes with a ... percentage-point higher
# probability of an expensive cappuccino (95% CI ... to ...; standard errors
# clustered by country)"
# ====


## TRY IT YOURSELF
# ====
# 1. refit the logit on the wage in LOGS (log(wage_eur)). how does the AME change?
#    compare with the elasticity idea from 02
# 2. compute the average marginal effect of the wage separately for Rural,
#    Suburban and Urban rows (hint: avg_slopes(fit_logit2, variables = "wage_eur",
#    by = "urban_class")). do they agree?
# 3. choose a different cut-off: "expensive" = above the 75th percentile of the
#    price. how do the odds ratio and the AME change? which one changes more,
#    and why?
# 4. add the interaction wage_eur * urban_class to the logit and plot the
#    predicted probabilities (plot_predictions(fit, condition = c("wage_eur",
#    "urban_class"))). are the curves parallel on the probability scale?
# 5. use the by-hand formula b1 * p * (1 - p) to find the wage at which the
#    marginal effect is the largest. what is the probability there?
# ====
