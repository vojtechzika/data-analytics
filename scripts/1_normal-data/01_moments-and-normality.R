# ====
# LOAD DEPENDENCIES
source("scripts/00_setup.R")  



# ============================================================
# 1. LOAD NAD CHECK THE CLEANED DATA
# ============================================================
# This is the file we exported at the end of the cleaning script —
# it already has sex (Male/Female), height (cm), weight (kg), 
# and a bmi column
df_nd <- read.csv(file.path(dir_dat, "height-weight-bmi.csv"), sep = ",", header = TRUE)

str(df_nd) # base R
glimpse(df_nd) # dplyr

summary(df_nd) # quick look at values





t.test(df_nd$height ~ df_nd$sex)







# Look closely: it isn't one clean bell curve — there's a subtle second
# bump. That's because "height" is really two different populations
# mixed together: males and females each have their own (roughly
# normal) height distribution, but with different means. Pooling them
# creates a "mixture distribution" that can look bimodal even though
# neither subgroup is bimodal on its own.
#
# Split the same variable by sex and the two bell curves become obvious:
# we use ggplot: - an R workhorse for visualizations
library(ggplot2)
ggplot(df_nd, aes(x = height, fill = sex)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(x = "Height", fill = "Sex",
       title = "Height, split by sex — two overlapping bell curves") +
  theme_minimal()
# PRO TIP: whenever a variable looks bimodal, ask "is there a grouping
# variable I haven't accounted for yet?" before assuming the data is
# broken — sex, treatment vs. control, before vs. after, etc. are all
# common hidden groupings that produce exactly this shape.

# --- b. histogram for every numeric column at once, with Hmisc ---
# install.packages("Hmisc")  # only needed once
library(Hmisc)
hist(df) #default call, but!
# Hmisc loads a lot of dependencies and can mask some base/tidyverse
# function names (e.g. summarize) — worth knowing if something behaves
# unexpectedly after loading it alongside dplyr in the same session.
#
# is_male is left out here because it's categorical, not continuous —
# hist() on a logical column errors out. height, weight, and bmi are
# all shown together on purpose: compare their shapes — height and
# weight both show the same subtle two-hump mixture as above, while
# bmi (which largely factors sex out) looks like a single, more
# normal-looking hump.
Hmisc::hist.data.frame(df[, c("height", "weight", "bmi")])

# --- c. boxplot (base R) ---
boxplot(df_nd$height,
        main = "Boxplot of height",
        ylab = "height (cm)")

# grouped boxplot: height by is_male, so you compare distributions
# side by side instead of looking at one variable in isolation
boxplot(height ~ sex, data = df_nd,
        main = "Height by is_male",
        xlab = "is_male", ylab = "height (cm)")

# with ggplot2 

ggplot(df, aes(x = factor(is_male), y = height)) +
  geom_boxplot(width = 0.1, fill = "white") + #combined with boxplot in ggplot
  labs(x = "is_male", y = "height (cm)") +
  theme_minimal()

# --- d. violin plot with ggplot2 ---
# install.packages("ggplot2")  # only needed once
ggplot(df, aes(x = factor(is_male), y = height)) +
  geom_violin(fill = "skyblue") +
  #geom_boxplot(width = 0.1, fill = "white") + #combined with boxplot in ggplot
  labs(x = "is_male", y = "height (cm)") +
  theme_minimal()

# ============================================================
# 4. NORMAL DISTRIBUTION: THE FOUR MOMENTS
# ============================================================
# Height and weight are mixtures of two sex-specific distributions (see
# section 3) — not a clean example of a single normal distribution. BMI
# largely factors sex out (that's the point of the index), so it's a
# better variable to demonstrate the moments of "a" normal distribution
# without the mixture problem getting in the way.
#
# A normal distribution is fully described by its first two moments
# (mean and variance) and is, by definition, symmetric (skewness = 0)
# with "standard" tail heaviness (kurtosis = 3, or "excess kurtosis" = 0).
# Real data is never perfectly normal, but comparing these four numbers
# against those benchmarks tells you how close it gets.

# 1st moment: mean — the center of the distribution
mean(df$bmi, na.rm = TRUE)
median(df$bmi, na.rm = TRUE)  # compare to the mean: close together
# suggests a roughly symmetric distribution

# 2nd moment: variance / standard deviation — how spread out the values are
# variance = average squared distance from the mean:
#   var(x) = sum((x - mean(x))^2) / (n - 1)
# squaring makes every deviation positive (so they don't cancel out) and
# penalizes big deviations more than small ones; dividing by (n - 1)
# instead of n is "Bessel's correction" — it corrects for the fact that
# a sample's own mean is estimated from the same data, which would
# otherwise make the variance a bit too small on average
var(df$bmi, na.rm = TRUE)
sd(df$bmi, na.rm = TRUE)      # sd = sqrt(variance), back in the
# original units (BMI) — usually the
# more useful of the two for that reason

# 3rd and 4th moments: skewness and kurtosis need an extra package —
# base R doesn't ship these
# install.packages("moments")  # only needed once
library(moments)

skewness(df$bmi, na.rm = TRUE)
# skewness ~= 0  -> symmetric (like a normal distribution)
# skewness  > 0  -> long right tail (mean pulled above the median)
# skewness  < 0  -> long left tail (mean pulled below the median)

kurtosis(df$bmi, na.rm = TRUE)
# moments::kurtosis() reports RAW kurtosis, where a normal distribution
# scores ~3 (not 0) — some other packages/textbooks report "excess
# kurtosis" (raw - 3, so normal = 0) instead, so always check which
# convention a function uses before comparing values across sources

# --- visual check: does bmi actually look normal? ---
# the four moments above are useful summaries, but seeing the shape
# directly is more convincing. Overlay a "textbook" normal curve (built
# from this sample's own mean and sd) on top of the real histogram:
hist(df$bmi, freq = FALSE, breaks = 30,
     main = "BMI with a fitted normal curve",
     xlab = "BMI", col = "skyblue")
# freq = FALSE rescales the histogram to density (area = 1) instead of
# raw counts, which is what puts it on the same scale as dnorm() below
curve(dnorm(x, mean = mean(df$bmi, na.rm = TRUE), sd = sd(df$bmi, na.rm = TRUE)),
      add = TRUE, col = "darkred", lwd = 2)

# a Q-Q plot is a more sensitive check than a histogram: it plots your
# data's quantiles against the quantiles a normal distribution would
# produce. The closer the points hug the diagonal line, the closer the
# data is to normal — a curve at one or both ends usually means skew
# or heavier/lighter tails than a normal distribution would have
qqnorm(df$bmi, main = "Q-Q plot of BMI")
qqline(df$bmi, col = "darkred", lwd = 2)

# a formal test, on top of the visual checks above: the Shapiro-Wilk
# test's null hypothesis is "this data comes from a normal
# distribution" — a small p-value (conventionally < 0.05) is evidence
# AGAINST normality
#
# shapiro.test() only accepts sample sizes between 3 and 5000 — with
# more rows than that (we have 10000), take a random subsample first.
# set.seed() makes the random subsample reproducible, so re-running the
# script gives the same result instead of a different one each time
set.seed(1)
shapiro.test(sample(df$bmi, 5000))
# NOTE: with large samples, shapiro.test() gets very sensitive — it can
# flag even tiny, practically irrelevant departures from normality as
# "significant" (the 5000-row cap above is itself part of the same
# story: the test's designers didn't expect it to be run on huge
# samples). Treat it as one more piece of evidence alongside the
# histogram/curve and Q-Q plot above, not as the final word on its own.

shapiro.test(df$weight[df$is_male == TRUE])
shapiro.test(df$weight[df$is_male == FALSE])

shapiro.test(df$height[df$is_male == TRUE])
shapiro.test(df$height[df$is_male == FALSE])

# ============================================================
# 5. STANDARD DEVIATION AND Z-SCORES
# ============================================================
# sd() tells you the "typical" distance of a value from the mean, in
# the same units as the variable — e.g. an sd of ~4 BMI points means
# most people sit within roughly 4 points of the average BMI.
#
# A z-score rescales a value into "how many standard deviations away
# from the mean is this?" — it strips away the original units, which
# is what makes it possible to compare values across different
# variables (e.g. is this person's BMI more unusual than their
# height, relative to everyone else?):
#
#   z = (x - mean) / sd

# manually:
df$bmi_z <- (df$bmi - mean(df$bmi, na.rm = TRUE)) / sd(df$bmi, na.rm = TRUE)
head(df$bmi_z)

# or with the built-in scale() function (does the same thing, returns
# a matrix, so as.vector() turns it back into a plain numeric column)
df$bmi_z_scale <- as.vector(scale(df$bmi))
head(df$bmi_z_scale)

# For a normal distribution, the "empirical rule" (68-95-99.7 rule)
# says about 68% of values fall within 1 sd of the mean, 95% within 2,
# and 99.7% within 3. You can check how close your data comes to that:
mean(abs(df$bmi_z) <= 1, na.rm = TRUE)  # should be close to 0.68
mean(abs(df$bmi_z) <= 2, na.rm = TRUE)  # should be close to 0.95

# ============================================================
# 6. CONFIDENCE INTERVALS
# ============================================================
# A confidence interval gives a range that's likely to contain the
# TRUE population mean, based on what we observed in this sample.
# A "95% CI" doesn't mean "95% chance the true mean is in this range" —
# it means if you repeated the sampling process many times, about 95%
# of the intervals built this way would contain the true mean.

n <- sum(!is.na(df$bmi))
x_bar <- mean(df$bmi, na.rm = TRUE)
s <- sd(df$bmi, na.rm = TRUE)

# manual calculation using the normal approximation (fine for large n):
# CI = mean +/- z * (sd / sqrt(n)), z = 1.96 for 95% confidence
z <- qnorm(0.975)   # 1.96 — the value that cuts off the top 2.5% of a
# normal distribution (2.5% + 2.5% = 5%, leaving 95%)
margin_of_error <- z * (s / sqrt(n))
c(lower = x_bar - margin_of_error, upper = x_bar + margin_of_error)

# in practice, use t.test() instead — it uses the t-distribution rather
# than the normal one, which correctly accounts for the fact that we
# estimated sd from the sample rather than knowing it exactly (matters
# most for small samples; with n this large the two will look almost
# identical)
t.test(df$bmi)$conf.int

# ============================================================
# 7. CONFIDENCE INTERVALS BY SEX
# ============================================================
# So far we've computed one confidence interval for the whole sample.
# Section 3 showed that height and weight differ by sex (and bmi to a
# lesser extent), so a natural next question is: do men and women have
# different average height/weight/bmi? A first, purely descriptive step
# toward that question is to compute a SEPARATE confidence interval for
# each sex and see whether they overlap — if they don't, that's a
# strong visual hint the difference is real, not just noise.
# (The proper way to test this formally — a t-test — is next time.)

# For each variable: get the 95% CI for men, get it for women, put
# both into a small table, then plot it. t.test()$conf.int is the same
# tool we used for the whole sample in section 6 — here we just run it
# twice, once per sex.

# --- height ---
height_male_ci   <- t.test(df$height[df$is_male == TRUE])$conf.int
height_female_ci <- t.test(df$height[df$is_male == FALSE])$conf.int

height_ci_table <- data.frame(
  sex   = c("Male", "Female"),
  mean  = c(mean(df$height[df$is_male == TRUE], na.rm = TRUE),
            mean(df$height[df$is_male == FALSE], na.rm = TRUE)),
  lower = c(height_male_ci[1], height_female_ci[1]),
  upper = c(height_male_ci[2], height_female_ci[2])
)
height_ci_table

ggplot(height_ci_table, aes(x = sex, y = mean, ymin = lower, ymax = upper, color = sex)) +
  geom_errorbar(width = 0.1) +
  geom_point(size = 3) +
  labs(x = NULL, y = "mean height (cm), 95% CI",
       title = "Height: mean and 95% CI by sex") +
  theme_minimal() +
  theme(legend.position = "none")

# --- weight ---
weight_male_ci   <- t.test(df$weight[df$is_male == TRUE])$conf.int
weight_female_ci <- t.test(df$weight[df$is_male == FALSE])$conf.int

weight_ci_table <- data.frame(
  sex   = c("Male", "Female"),
  mean  = c(mean(df$weight[df$is_male == TRUE], na.rm = TRUE),
            mean(df$weight[df$is_male == FALSE], na.rm = TRUE)),
  lower = c(weight_male_ci[1], weight_female_ci[1]),
  upper = c(weight_male_ci[2], weight_female_ci[2])
)
weight_ci_table

ggplot(weight_ci_table, aes(x = sex, y = mean, ymin = lower, ymax = upper, color = sex)) +
  geom_errorbar(width = 0.1) +
  geom_point(size = 3) +
  labs(x = NULL, y = "mean weight (kg), 95% CI",
       title = "Weight: mean and 95% CI by sex") +
  theme_minimal() +
  theme(legend.position = "none")

# --- bmi ---
bmi_male_ci   <- t.test(df$bmi[df$is_male == TRUE])$conf.int
bmi_female_ci <- t.test(df$bmi[df$is_male == FALSE])$conf.int

bmi_ci_table <- data.frame(
  sex   = c("Male", "Female"),
  mean  = c(mean(df$bmi[df$is_male == TRUE], na.rm = TRUE),
            mean(df$bmi[df$is_male == FALSE], na.rm = TRUE)),
  lower = c(bmi_male_ci[1], bmi_female_ci[1]),
  upper = c(bmi_male_ci[2], bmi_female_ci[2])
)
bmi_ci_table

ggplot(bmi_ci_table, aes(x = sex, y = mean, ymin = lower, ymax = upper, color = sex)) +
  geom_errorbar(width = 0.1) +
  geom_point(size = 3) +
  labs(x = NULL, y = "mean BMI, 95% CI",
       title = "BMI: mean and 95% CI by sex") +
  theme_minimal() +
  theme(legend.position = "none")

