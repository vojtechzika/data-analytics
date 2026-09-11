source("scripts/00_setup.R")

df_nd <- read.csv(file.path(dir_dat, "height-weight-bmi.csv"), sep = ",", header = TRUE)

## THE CENTRAL LIMIT THEOREM, DEMONSTRATED ON BMI
# ====
# CLT: no matter how the POPULATION is distributed, the distribution of
# the SAMPLE MEAN becomes approximately normal as sample size grows.
# bmi itself is mildly skewed (see 01_moments-and-normality.R) -- let's
# treat our observed bmi values as a stand-in "population" and watch what
# happens to the MEAN of repeated samples drawn from it
# ====

hist(df_nd$bmi)   # reminder: the individual values are somewhat skewed


## DRAW REPEATED SAMPLES, TRACK THE MEAN EACH TIME
# ====
# sample(..., replace = TRUE) draws a random sample FROM bmi, simulating
# "what if we'd collected a fresh sample of this size". replicate() just
# repeats that expression many times and collects the results into a
# vector -- here, 1000 sample means for each sample size n
# ====

set.seed(1)

draw_sample_means <- function(n, reps = 1000) {
  replicate(reps, mean(sample(df_nd$bmi, size = n, replace = TRUE)))
}

means_n5   <- draw_sample_means(n = 5)
means_n30  <- draw_sample_means(n = 30)
means_n100 <- draw_sample_means(n = 100)

hist(means_n5)     # still somewhat skewed -- n is too small for CLT to fully kick in
hist(means_n30)    # already much more symmetric
hist(means_n100)   # close to a clean bell curve


## SAME THING, SIDE BY SIDE
df_means <- data.frame(
  n    = factor(rep(c(5, 30, 100), each = 1000)),
  mean = c(means_n5, means_n30, means_n100)
)

ggplot(df_means, aes(x = mean)) +
  geom_histogram(bins = 40) +
  facet_wrap(~ n, scales = "free")   # watch the shape tighten and symmetrize as n grows


## FORMAL CHECK: DOES THE SAMPLING DISTRIBUTION ITSELF PASS?
# ====
# same diagnostics as 01_moments-and-normality.R -- but applied to the
# distribution of MEANS, not the raw bmi values
# ====

qqnorm(means_n100); qqline(means_n100)
shapiro.test(means_n100)
# compare this to bmi's own verdict in 01 -- individual bmi values were
# rejected as non-normal, but the MEAN of a sample this size is normal
# enough. this is the CLT rescue in action, not just asserted by name


## WAIT -- DOES POOLING ACROSS SEX MATTER HERE?
# ====
# above, we resampled from ALL of bmi, ignoring sex -- fine for the
# GENERAL principle (pooled bmi is actually a MORE convincing example,
# since it's a two-group mixture, not just mildly skewed).
#
# but if the goal is justifying a t-test comparing MALE vs FEMALE bmi,
# pooling is the wrong check -- what matters there is whether EACH
# group's own sampling distribution of the mean is normal enough,
# answered separately per group:
# ====

male_bmi   <- df_nd$bmi[df_nd$sex == "Male"]
female_bmi <- df_nd$bmi[df_nd$sex == "Female"]

means_male_n30   <- replicate(1000, mean(sample(male_bmi,   size = 30, replace = TRUE)))
means_female_n30 <- replicate(1000, mean(sample(female_bmi, size = 30, replace = TRUE)))

hist(means_male_n30)
hist(means_female_n30)
# both already look reasonably normal at n=30 -- each GROUP has plenty
# of observations, so CLT applies within each one separately


# ====
# WHY THIS MATTERS
# if your sample is large enough, CLT means you can almost always use
# the tools in THIS folder (t-test, ANOVA, ...) on the MEAN, regardless
# of what a formal normality test on the raw data says -- exactly what
# happened with bmi and height in 01_moments-and-normality.R.
#
# the real trigger for needing the different tools you'll see later (in
# 2_other-data) isn't "failed a normality test" -- it's SAMPLE SIZE.
# small samples don't have enough observations for CLT to kick in.
#
# this is why GROUP size matters, not just overall dataset size: if a
# categorical variable had many more categories (say, 50 instead of 2),
# some of those groups might only have a handful of observations each --
# CLT wouldn't rescue THOSE groups even if the full dataset is huge.
#
# two honest caveats:
# 1. CLT needs FINITE VARIANCE -- some distributions (Cauchy) are never
#    rescued, no matter how large n gets
# 2. if you actually care about medians or ranks rather than the mean,
#    the alternative tools aren't a fallback -- they're the correct
#    tool for the question, regardless of sample size
# ====

