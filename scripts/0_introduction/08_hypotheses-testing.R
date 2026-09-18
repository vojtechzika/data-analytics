# ===================================================================
# 08_hypotheses-testing.R — what a hypothesis is, and the four things
# that can happen when we test one
# ===================================================================


## HYPOTHESES TESTING
# ====
# so far, we have implicitly generated several hypotheses that we did not
# test analytically yet -- bmi is higher for men then women, for instance.
#
# but what is a "hypothesis" in the first place?
#
# science is generally about finding something new about the world around
# us. we formulate a RESEARCH QUESTION and then derive a TESTABLE HYPOTHESIS.
#
# a hypothesis is a relationship between our data and the reality.
# four things can happen:
#
# 1. we FIND SUPPORT for H0, and H0 is indeed true    -- correct
# 2. we FIND SUPPORT for H1, and H1 is indeed true    -- correct (this is POWER)
# 3. we FIND SUPPORT for H1, but H0 is actually true  -- Type I error (false positive)
# 4. we FIND SUPPORT for H0, but H1 is actually true  -- Type II error (false negative)

## ATTENTION (!!!!!!!!!!!!!!!!)
# The possibility of the Type I / II errors means that
# hypotheses cannot be "REJECTED", let alone "CONFIRMED"
# The best we can say is we DID or DID NOT FIND SUPPORT for our hypotheses.

# REALITY (is H0 or H1 actually true) # and our RESEARCH (what our data leads us to support)
# are two different things -- they only agree some of the time.
#
#
#                    |          REALITY
#                    |
#                    |  H0 is true      |  H1 is true
#  ------------------|------------------|--------------------
#                    |                  |  False-negative
#  RESEARCH finds    |    1 - alpha     |   Type II error
#  support for H0    |                  |     (beta)
#  ------------------|------------------|--------------------
#                    | False-positive   |
#  RESEARCH finds    | Type I error     |     1 - beta
#  support for H1    |    (alpha)       |     (= power)
#  ----------------------------------------------------------
#
#

# Because we can't observe the reality (we would not need the research if we could),
# we have to accept some degree of uncertainty around our findings. In particular,
# around:
# 1) We did not commit Type I error (we found something that does not exists, bad for science, hence Type I.)
# 2) We did not commit Type II error (we fail to find something that exists, bad for us, hence Type II.)
#
# each of these has its own number attached to it: the P-VALUE quantifies
# our confidence about (1), and POWER quantifies our confidence about (2)


# ========================================
# 1. TYPE I. ERROR & SIGNIFICANCE LEVEL
# ========================================
# α is called the significance level, and it is a probability of Type I. error.
#
# In social sciences, it is usually 5% (0.05). Some suggest it should be 0.5% for
# experimental economics.
#
# In other words, α represents the maximum risk we are willing to take that our
# results are false-positive.
#
# Other fields, like medicine or aviation, use lower α, typically less than 0.1%.
# Type I. error is often caused by problems with external and internal validity.

## How to think of α?
# Imagine you test a new drug (H0: „The drug works“, H1: „The drug does not work“), which in
# fact, does not work (H0 thus describes the reality).
#
# Imagine you have unlimited resources and can run 100 experimental trials (each trial with
# different subjects). 97 trials showed no difference between control and treatment groups, and
# 3 showed the difference.
#
# Because you do not have unlimited resources, you only run one trial. That is like if you throw
# the 100 virtual trials in the bag, shake it, and pick one trial result.
#       - If you work at α=0.05, you will be willing to play this game (as you are OK with having 5
#         false-positive results in the bag).
#       - If you work at α=0.01, you will be willing to play this game only if there is only one false-positive
#         result in the bag.

bag_A <- c(rep(TRUE, 97), rep(FALSE, 3))
bag_B <- c(rep(TRUE, 93), rep(FALSE, 7))

sample(bag_A, 1) # bag A is ok as it contains <5% of false-positives
sample(bag_B, 1) # bag B is NOT ok as it contains >5% of false-positives

## How do we know what's in the bag?
#
# The share of false positives in the bag is called the P-VALUE, resulting from a statistical test.
#     - If the p-value exceeds the selected significance level, we find support for H0 (non-existence
#       of the effect).
#     - If the p-value is lower than the significance level, we consider the tested differences
#       statistically significant and thus find support for H1 („If the P is low, the Ho must go!“).
#
# P-VALUE IS the probability of collecting our data if the H0 is true.
#
# P-VALUE IS NOT the probability of acquiring the results by chance, a false-positive result,
# or the probability that the H0 is true. It does not make sense to judge the findings' quality
# by comparing p-values between tests. Within one test, however, lower p is better than higher.

mean(!bag_A)   # 0.03 -- the share of false-positives in bag_A, i.e. its "p-value"
mean(!bag_B)   # 0.07 -- same idea for bag_B


# ========================================
# 2. TYPE II. ERROR & STATISTICAL POWER
# ========================================
# β is the probability of Type II. error, i.e., the situation in which we wrongly find
# support for H0 based on the false-negative result.
#
# α and β are both design choices, but they pull against each other AT A GIVEN SAMPLE
# SIZE: tightening α (fewer false positives) makes β worse (more false negatives),
# not better. these commonly cited pairs hold only because a large enough sample was
# also chosen to reach both targets at once -- shrinking β while also shrinking α
# means collecting more data, not just picking a stricter α:
#  --- With α=0.05, β=0.2 (needs a correspondingly large n)
#  --- With α=0.01, β=0.1 (needs an even larger n, since both got stricter)
#
# A related concept is the statistical power, which is 1-β, i.e., 0.8 (80%).
# Type II. error is typically the result of a combination of a small sample and a
# small effect size.


# An unlimited budget would allow us to realize one study 100 times. This study
# seeks to find support for a relatively rare trait in society
# (say, 1 person in 40 has it).
#
# Because we have trouble finding participants, each study has only 10
# participants. Thus, we can expect to find 1 participant with the trait in, on average,
# 1 in 4 studies (25%).

# (39/40) is the chance any ONE participant does NOT have the trait -- raised to
# the 10th power because we need all 10 participants to miss it independently.
# 1 minus that is the chance at least one of the 10 DOES have it, i.e. the chance
# the study finds a participant with the trait at all
1 - (39/40)^10   # ~0.22 -- close to the 25% claimed above, not exact (rounded for the example)

# Once again, we do not have an unlimited budget and can only realize 1 study.
# Also, we have the bag in which 25% of results support H1 and 75% support H0.
# --- If we would play this game, there is a 75% chance of Type II. error.
# --- That is why we only use bags in which 80% of results support H1.

