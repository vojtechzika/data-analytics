

# ========================================
# 3. EFFECT SIZE
# ========================================

# effect size quantifies the difference between the control and experimental
# group. the formula depends on the type of data -- for comparing two means,
# we use Cohen's d:
#
#   d = (M1 - M2) / sqrt((s1^2 + s2^2) / 2)
#
# benchmark (Cohen, 1988):
#
#  Relative size  |  Effect size (d)  |  % of control below mean of experimental
#  ---------------|--------------------|------------------------------------------
#                 |        0.0         |                50%
#  Small          |        0.2         |                58%
#  Medium         |        0.5         |                69%
#  Large          |        0.8         |                79%
#                 |        1.4         |                92%
# ====


## A WORKED EXAMPLE: THE WORD-RECALL TEST, IN IOTA AND THETA
# ====
# people in two countries take a memory test: study a list of 60 words for
# a minute, then recall as many as possible. a CONTROL group gets no
# special technique; a TREATMENT group is taught a mnemonic technique
# first, then takes the same test.
#
# Theta's technique is genuinely powerful; Iota's is only mildly helpful.
# both countries run their study with the same modest sample size (n=100
# per group) -- the question is whether that sample size is enough to
# pick up on the technique's real effect in EACH country
# ====

set.seed(1)

n <- 100

control       <- rnorm(n, mean = 40, sd = 12)   # words recalled, no technique
iota_treated  <- rnorm(n, mean = 43, sd = 12)   # Iota's technique: a small real effect
theta_treated <- rnorm(n, mean = 58, sd = 12)   # Theta's technique: a large real effect

cohens_d <- function(x, y) {
  pooled_sd <- sqrt((sd(x)^2 + sd(y)^2) / 2)
  (mean(x) - mean(y)) / pooled_sd
}

cohens_d(theta_treated, control)   # large, per the benchmark above
cohens_d(iota_treated, control)    # small

t.test(theta_treated, control)     # comfortably significant
t.test(iota_treated, control)      # a real effect, but likely NOT significant here --
# exactly the "small sample + small effect size"
# combination from the Type II error section above

s