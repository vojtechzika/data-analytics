# ===================================================================
# 02_model-theory-extras.R -- EXTRA THEORY FOR THE REGRESSION SCRIPTS (ADVANCED)
# optional material that goes with 3_regression-models/01-04: omitted variable
# bias, Frisch-Waugh, AIC / BIC, standardized coefficients (04); brute-force
# fitting, SST = SSR + SSE, the standard error by hand, regression to the mean (01);
# scale-location, Breusch-Pagan and Anscombe (02); other codings of a factor,
# Welch / Kruskal-Wallis in regression form (03)
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

# the common rows with a milk price, as in 04
df_milk  <- df_cont %>% filter(!is.na(milk_eur))
fit_wage <- lm(cap_eur ~ wage_eur, data = df_milk)
fit_milk <- lm(cap_eur ~ milk_eur, data = df_milk)
fit_wm   <- lm(cap_eur ~ wage_eur + milk_eur, data = df_milk)


## WHY DID A COEFFICIENT CHANGE? OMITTED VARIABLE BIAS
# ====
# there is an exact formula. take the model with both variables (the "long"
# model) and a model that leaves the second one out (the "short" one). then
#
#   slope in the short model  =  its slope in the long model
#                                +  (the other variable's long-model slope)
#                                   * (slope of the other variable regressed
#                                      on this one)
#
# for the wage:    b_wage(short) = b_wage(long) + b_milk(long) * delta,
#                  delta = slope of  milk ~ wage
# for milk:        b_milk(short) = b_milk(long) + b_wage(long) * gamma,
#                  gamma = slope of  wage ~ milk
#
# the second term is the BIAS: it is large only if the left-out variable
# matters for the outcome (b nonzero) AND is related to the included one
# (delta nonzero). this is the whole logic of "controlling for a confounder"
# ====

delta <- coef(lm(milk_eur ~ wage_eur, data = df_milk))["wage_eur"]
gamma <- coef(lm(wage_eur ~ milk_eur, data = df_milk))["milk_eur"]
b <- coef(fit_wm)

rbind(wage = c(short_model = unname(coef(fit_wage)["wage_eur"]),
               long_plus_bias = unname(b["wage_eur"] + b["milk_eur"] * delta)),
      milk = c(short_model = unname(coef(fit_milk)["milk_eur"]),
               long_plus_bias = unname(b["milk_eur"] + b["wage_eur"] * gamma)))

c(delta = unname(delta), gamma = unname(gamma))

# result: both rows match exactly. for the wage, delta is 0.012 (a euro more
# of wage goes with 1.2 cents more milk) and milk's long-model coefficient is
# -0.058, so the bias is -0.058 * 0.012 = -0.0007: negligible. for milk the
# bias is large: gamma = 2.2 (a euro more milk per litre goes with 2.2 euros
# more hourly wage -- countries with dear milk are richer) times the wage
# slope 0.085 gives a bias of +0.19, which turns milk's long-model
# coefficient of -0.058 into the short-model +0.130. the apparently positive
# association of milk with cappuccino prices is borrowed from the wage,
# because wage and milk move together. the wage is a textbook confounder of
# MILK's slope (though not the other way round)


## THE SAME THING AS A PICTURE: "RESIDUAL ON RESIDUAL" (FRISCH-WAUGH)
# ====
# there is a second way to see what "holding milk fixed" does. to get the wage
# coefficient of the two-predictor model:
#   1. regress price on milk, keep the RESIDUALS: the part of the price that
#      milk does not explain
#   2. regress wage on milk, keep the RESIDUALS: the part of the wage that
#      milk does not explain
#   3. regress the first residuals on the second: the slope IS the wage
#      coefficient of the full model
# so a multiple regression coefficient is the simple slope AFTER the other
# predictors' influence has been subtracted from both variables. plotting
# the two sets of residuals is the "added-variable plot" (avPlots() in car)
# ====

res_price <- resid(lm(cap_eur ~ milk_eur, data = df_milk))
res_wage  <- resid(lm(wage_eur ~ milk_eur, data = df_milk))
c(residual_slope = unname(coef(lm(res_price ~ res_wage))[2]), full_model = unname(b["wage_eur"]))

ggplot(data.frame(res_wage, res_price), aes(x = res_wage, y = res_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#e34948") +
  labs(x = "wage, after removing what milk explains", y = "price, after removing what milk explains",
       title = "Added-variable plot: the wage coefficient of the two-predictor model") +
  theme_minimal()

# car's ready-made version, for both predictors (one panel each)
avPlots(fit_wm)


## INFORMATION CRITERIA: AIC AND BIC
# ====
# besides R^2, adjusted R^2 and the F-test (see 04), models fitted on the SAME
# rows can be compared by information criteria: lower is better; BIC penalizes
# size more strongly. unlike the F-test they do not need the models to be nested
# ====

data.frame(model = c("wage", "wage + milk"),
           AIC    = AIC(fit_wage, fit_wm)$AIC,
           BIC    = BIC(fit_wage, fit_wm)$BIC)

# result: AIC 5983.9 (wage) vs 5984.1 (wage + milk) and BIC 6001 vs 6007 -- both in
# favor of the SIMPLER model, like the F-test and adjusted R^2 in 04


## WHICH PREDICTOR MATTERS MORE? STANDARDIZED COEFFICIENTS
# ====
# raw slopes cannot be compared across predictors with different units (euros
# per euro of wage vs euros per euro of milk-per-litre). a standard fix is to
# standardize every variable (subtract the mean, divide by the sd) and
# refit: each coefficient then says "how many SDs of the OUTCOME the price
# moves per 1 SD of the PREDICTOR". with one predictor this is r (01)
# ====

coef(lm(scale(cap_eur) ~ scale(wage_eur) + scale(milk_eur), data = df_milk))

# result: wage 0.478, milk -0.024: one standard deviation more wage (about 5.4
# euros) goes with 0.48 SD higher prices, one SD more milk (0.40 euros per litre)
# with 0.02 SD LOWER prices -- 20 times smaller. standardized coefficients
# compare predictors on a common scale; they do not tell you which would be
# the better lever, and they depend on the spread in THIS sample


## OBJECTS USED BY THE SECTIONS MOVED HERE FROM 01, 02 AND 03
# ====
# the sections below come from the first three scripts of the folder and use
# their objects: the simple regression of price on wage, the diagnostics table
# and the three-class model
# ====

fit <- lm(cap_eur ~ wage_eur, data = df_cont)   # the model of 01, on all rows
x <- df_cont$wage_eur
y <- df_cont$cap_eur
n <- length(x)
sse <- function(b0, b1) sum((y - (b0 + b1 * x))^2)
fit_three <- lm(cap_eur ~ urban_class, data = df_cont)   # the model of 03
diag_df <- df_cont %>%
  mutate(fitted = fitted(fit), resid = residuals(fit), std_resid = rstandard(fit))


## BRUTE-FORCE FITTING: optim() FINDS THE SAME LINE [from 01]
# ====
# a computer can also find the least-squares line by brute force -- optim()
# searches for the b0 and b1 with the smallest SSE and lands on the same numbers
# as the closed-form formulas of 01
# ====

b1_hand <- cov(x, y) / var(x)
b0_hand <- mean(y) - b1_hand * mean(x)
c(b0_hand, b1_hand)
optim(c(0, 0), function(b) sse(b[1], b[2]))$par   # very close (optim() stops at a tolerance)


## OTHER CODINGS OF A FACTOR: NO INTERCEPT AND SUM-TO-ZERO [from 03]
# ====
# (the prelude of this block defines fit_three, as in 03)
# ====

# two more codings you will meet. (a) NO INTERCEPT, "0 +": each coefficient is
# directly a group mean (handy for a table of means with their standard errors;
# but the F-test and R^2 at the bottom then answer a different question -- do
# NOT use them). (b) SUM-TO-ZERO ("effect") coding: the intercept is the mean of
# the group means and each coefficient is a group's deviation from it. this is
# what the Type III ANOVA of advanced 03 needs
coef(lm(cap_eur ~ 0 + urban_class, data = df_cont))
coef(lm(cap_eur ~ urban_class, data = df_cont, contrasts = list(urban_class = "contr.sum")))


## UNEQUAL VARIANCES AND RANKS, IN REGRESSION FORM [from 03]
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

oneway.test(cap_eur ~ urban_class, data = df_cont)       # Welch ANOVA
Anova(fit_three, white.adjust = "hc3")                    # robust F-test from the regression

kruskal.test(cap_eur ~ urban_class, data = df_cont)      # Kruskal-Wallis
anova(lm(rank(cap_eur) ~ urban_class, data = df_cont))   # ANOVA on the ranks

# result: Welch F = 0.75 (p = 0.4725) and the robust regression F = 0.75 (p =
# 0.4728): the same verdict to the third decimal. Kruskal-Wallis p = 0.5957
# and the rank regression p = 0.5959: the same too. the lesson is not
# "never use the named tests", it is that they are VARIATIONS of one idea --
# fit the group-means model, then choose the standard error and the scale
# (raw or ranks) that suit the data


## PLOT 3: SCALE-LOCATION -- EQUAL VARIANCE, SECOND LOOK [from 02]
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
# enough that robust standard errors (in 02) will change the SE by about a
# quarter, not by an order of magnitude


## ANSCOMBE'S QUARTET, ONCE MORE: THE SAME LINE, FOUR PICTURES [from 02]
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


## THE FULL SUM-OF-SQUARES SPLIT: SST = SSR + SSE [from 01]
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

sst     <- sum((y - mean(y))^2)
sse_fit <- sum(residuals(fit)^2)
ssr     <- sum((fitted(fit) - mean(y))^2)

c(SST = sst, SSR_plus_SSE = ssr + sse_fit)
c(R2_by_hand = ssr / sst, R2_lm = summary(fit)$r.squared)


## THE STANDARD ERROR, t, p AND F BY HAND [from 01]
# ====
# se(b1) = sigma_hat / sqrt( sum( (x - mean(x))^2 ) ), t = b1 / se(b1), compared with a
# t-distribution with n - 2 df
# ====

sigma_hat <- summary(fit)$sigma
b1_hand <- cov(x, y) / var(x)

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


## WHY IS IT CALLED "REGRESSION"? A GLANCE AT 1886 [from 01]
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


## TRY IT YOURSELF
# ====
# 1. repeat the omitted variable bias table for the log-log model of 04. does the
#    formula still match to the last digit? (it is an algebraic identity)
# 2. check the Frisch-Waugh result for the milk coefficient: residualize on the
#    wage and regress the residuals on each other
# 3. in the Anscombe code, replace the one outlier of dataset 3 by the value
#    predicted by the line through the other points. what happens to R^2 and
#    to the residual standard error?
# ====
