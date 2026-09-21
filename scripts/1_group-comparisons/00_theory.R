# ====
# GROUP COMPARISONS THEORY
# ====

# In essence, we are trying to figure out whether GROUP 1 differs from GROUP 2.
# To be more precise, we want to find out whether the DIFFERENCE between the
# two groups is DIFFERENT FROM ZERO.
# The simplest example: is 3 different from 2? Yes, because the difference is
# different from zero (3 - 2 = 1, 1 > 0).

# However, in research, the problem is that we have UNCERTAINTY around these
# numbers, because they are POINT ESTIMATES (computed from a sample), rather
# than objective/true values.
# In fact, we want to compare MEANS of the two groups, i.e.,
# mean(GROUP_1) - mean(GROUP_2), and see whether the result is different from zero.

# Remember, each of these means is just one of the many possible means we could
# have gotten, had we drawn a different sample.

# In reality, this difference will almost always be different from zero, but
# what we do not know (and want to know) is whether the difference is big enough
# to call it "statistically significant".

# Consider two examples.
# 1) You measure espresso prices in Prague and Karlovy Vary and find that
#    espresso in Prague, on average, costs 62 CZK and in Karlovy Vary 61 CZK.
#    Because prices vary a lot from cafe to cafe (say, roughly 50 to 70 CZK),
#    a difference of 1 CZK is most likely not statistically significant.
# 2) You measure reaction times of sober and drunk people and find a
#    difference of 1 second. Because reaction times vary very little within
#    each group (differences are measured in milliseconds), this
#    between-group difference is likely to be significant.

# What these examples show is that to judge whether a difference between
# groups is "large" or "small", we cannot just look at the raw difference -
# we need to STANDARDIZE it into units that account for how much natural
# variation there is. In other words, we need a RULER.

# You already know that the spread of datapoints is measured by VARIANCE, and
# that if you take the SQUARE ROOT of the variance, you get the STANDARD
# DEVIATION (sd). (Equivalently: sd squared = variance.)

# The sd on its own still does not account for SAMPLE SIZE: the more
# observations we have, the more precisely we know the mean, even if the sd
# of individual datapoints stays the same. If you divide sd by the SQUARE
# ROOT of n, you get the STANDARD ERROR (SE = sd / sqrt(n)). This is the
# RULER we were looking for - it shrinks as our sample grows, reflecting that
# larger samples give more precise estimates of the mean.

# Now we can standardize our values onto a common scale. Let's start with a
# simple example in which you want to know whether espresso in Prague costs
# 65 CZK (a fixed hypothesis - a value with no uncertainty attached to it).
# You take the difference between your sample mean and the hypothesized
# value, and divide it by the STANDARD ERROR of GROUP_1:
#   t = (mean(GROUP_1) - 65) / SE

# The same logic extends to comparing two groups instead of one group against
# a fixed number - we replace the fixed value with mean(GROUP_2), and use the
# standard error of the DIFFERENCE between the two group means instead of the
# standard error of a single group:
#   t = (mean(GROUP_1) - mean(GROUP_2)) / SE_difference

# The number you get is called the T-STATISTIC - a standardized measure of
# "how many standard errors away from zero" the observed difference is. The
# useful trick is that WE KNOW the shape of the distribution the t-statistic
# would follow if the true difference were actually zero (its exact shape
# depends on the DEGREES OF FREEDOM, roughly the sample size). This lets us
# work out, for a chosen significance level (conventionally 5%), the range of
# t-statistic values we'd expect about 95% of the time under "no true
# difference" - the values between the 2.5th and 97.5th percentile of that
# distribution.

# If our t-statistic falls OUTSIDE that range - i.e., it is more extreme than
# we'd expect by chance alone if the true difference were zero - we call the
# difference STATISTICALLY SIGNIFICANT. If it falls WITHIN that range, the
# data are consistent with there being no real difference, and we do NOT
# call it significant.


# ====
# TWO INDEPENDENT GROUPS
# ====

# Let's get back to our espresso prices in Prague and Karlovy Vary. Until now
# we tested a single group's mean against one fixed, known value (65 CZK).
# But our real question was never about a fixed value - it was whether
# Prague's average price differs from Karlovy Vary's average price. Now we
# are comparing two means, each with its own uncertainty, so the ruler needs
# to change.

# Instead of one SE (for Prague alone), we now need to account for
# uncertainty coming from BOTH cities. We combine them like this:
#   SE_difference = sqrt(SE_Prague^2 + SE_KV^2)
#                 = sqrt( sd(Prague)^2/n_Prague + sd(KV)^2/n_KV )
# and then standardize the difference in means the same way as before:
#   t = (mean(Prague) - mean(KV)) / SE_difference
# The bigger SE_difference is (the more uncertain we are about either city's
# average price), the smaller our t-statistic will be, and the harder it
# becomes to call the difference significant. This is the INDEPENDENT
# SAMPLES T-TEST (specifically Welch's version, which does not assume Prague
# and Karlovy Vary have the same amount of price variation - a reasonable
# thing to relax, since a touristy city center might have far more spread in
# prices than a quieter one).

# ====
# TWO PAIRED GROUPS
# ====

# Now let's go back to our other example: reaction times, sober vs drunk.
# Originally we imagined two separate groups of people - one tested sober,
# another tested drunk (BETWEEN-SUBJECT DESIGN). But suppose instead we test 
# the SAME people twice: once sober, then again after they've had a few drinks 
# (WITHIN-SUBJECT DESIGN).

# This changes things. If we just treated "sober times" and "drunk times" as
# two independent groups again, we'd be throwing away something useful: some
# people are just naturally quick, some naturally slow, regardless of
# alcohol - and that person-to-person variation adds noise that has nothing
# to do with the effect we actually care about.

# Since each "sober" measurement now has a natural partner - the same
# person's "drunk" measurement - we call this PAIRED (or "matched", or
# "repeated measures") data. The fix is to compute the DIFFERENCE for each
# person first (drunk_time - sober_time), which cancels out the
# person-to-person variation and leaves only the effect of alcohol. What's
# left is a single column of numbers, and the question becomes familiar
# again: is the mean of these differences different from zero?
#   t = mean(MEAN OF DIFFERENCES) / SE(MEAN OF DIFFERENCES)
# Because the person-to-person noise has been stripped out, this PAIRED
# T-TEST is usually more powerful - more likely to detect a real effect -
# than treating the same data as two independent groups.

# And we can also do more than 2 groups, by comparing all pairs together.

# ====
# MORE THAN TWO GROUPS
# ====

# Back to espresso prices - suppose we now also collect data from Brno, so we
# have three cities instead of two: Prague, Karlovy Vary, and Brno. We could
# run three separate pairwise t-tests (Prague vs KV, Prague vs Brno, KV vs
# Brno), but this causes a problem: each individual test carries, say, a 5%
# chance of a FALSE POSITIVE, so the more pairwise tests we run, the more
# likely we are to stumble onto a "significant" difference somewhere purely
# by chance (the MULTIPLE COMPARISONS problem).

# Instead, we use ANOVA (ANALYSIS OF VARIANCE), which compares all three
# cities in a single test. Rather than comparing two means, ANOVA compares
# two kinds of VARIANCE:
# - how much the city MEANS vary around the overall mean (BETWEEN-group
#   variance)
# - how much individual prices vary within each city (WITHIN-group variance)
# If the between-city variance is large relative to the within-city
# variance, at least one city's average price is likely genuinely different
# from the rest. This ratio is the F-STATISTIC, and just like the
# t-statistic, we know its distribution (the F-DISTRIBUTION, defined by two
# sets of degrees of freedom) and can check whether it is more extreme than
# chance alone would produce.

# One catch: a significant ANOVA result only tells you that SOME city
# differs from some other city - not WHICH ones. Pinning that down needs a
# follow-up ("post-hoc") test, such as TUKEY'S HSD, which re-compares all
# pairs of cities while correcting for the multiple comparisons problem.

# ====
# WHAT IF THE DATA IS NOT NORMALLY DISTRIBUTED?
# ====

# Everything above - the t-test on our espresso prices, the paired test on
# reaction times, the ANOVA across cities - rests on one assumption: that the
# data is reasonably NORMALLY DISTRIBUTED (or at least that the sample means
# are, which tends to hold up well for larger samples, thanks to the CENTRAL
# LIMIT THEOREM). If that assumption is badly violated - say, our espresso
# prices are skewed because one fancy hotel cafe charges 250 CZK while
# everyone else charges 50-70 - these tests can become unreliable.

# For cases like that there's a NON-PARAMETRIC alternative: the WILCOXON
# TEST. Instead of working with the actual prices and their mean/sd, it
# converts the data to RANKS and compares those - much less sensitive to
# outliers and skew, at the cost of being slightly less powerful when the
# data actually is normal.
# - Two independent groups (Prague vs Karlovy Vary): WILCOXON RANK-SUM TEST
#   (a.k.a. MANN-WHITNEY U TEST)
# - Two paired groups (sober vs drunk, same people): WILCOXON SIGNED-RANK TEST
# - More than two independent groups (Prague, Karlovy Vary, Brno):
#   KRUSKAL-WALLIS TEST (the rank-based analogue of ANOVA)
