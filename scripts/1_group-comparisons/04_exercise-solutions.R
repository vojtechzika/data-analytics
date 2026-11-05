# ===================================================================
# 04_exercise-solutions.R -- possible solutions to the TRY IT YOURSELF
# exercises of 01_one-sample.R, 02_two-sample.R and 03_three-sample.R
# try the exercises first! the code is here, the interpretation is yours:
# each exercise ends with a "look at" line telling you what to read in the output
# ===================================================================

source("scripts/00_setup.R")

df_cap <- read.csv(file.path(dir_dat, "cappuccino-index-complete.csv"), sep = ",", header = TRUE)

n_boot <- 2000

# bootstrapped percentile interval for any statistic of a vector
boot_ci <- function(x, stat = mean, seed = 123) {
  set.seed(seed)
  b <- replicate(n_boot, stat(sample(x, replace = TRUE)))
  quantile(b, c(0.025, 0.975))
}


# ===================================================================
# 01_one-sample.R
# ===================================================================

cap_vals  <- df_cap$cap_eur[!is.na(df_cap$cap_eur)]
wage_vals <- df_cap$wage_eur[!is.na(df_cap$wage_eur)]


## 1. CI of the mean wage: by hand and with t.test()
w_mean <- mean(wage_vals)
w_se   <- sd(wage_vals) / sqrt(length(wage_vals))
c(lower = w_mean - 1.96 * w_se, upper = w_mean + 1.96 * w_se)   # by hand, z = 1.96
t.test(wage_vals)$conf.int                                      # t-based
# look at: the two intervals are nearly identical. the t-quantile is slightly
# larger than 1.96, and the difference shrinks as n grows


## 2. moving the hypothesized value away from the mean
t.test(cap_vals, mu = 3.50)
t.test(cap_vals, mu = 4.30)
(mean(cap_vals) - c(3.50, 4.30)) / sd(cap_vals)   # Cohen's d for both
# look at: the further mu is from the sample mean, the larger |t| and |d| and the
# smaller the p-value. the sign of t shows the direction of the gap


## 3. skewness of wage vs. price, t-based vs. bootstrapped interval
ggplot(data.frame(wage_eur = wage_vals), aes(x = wage_eur)) + geom_histogram(bins = 40) + theme_minimal()
ggplot(data.frame(cap_eur = cap_vals), aes(x = cap_eur)) + geom_histogram(bins = 40) + theme_minimal()
qqnorm(wage_vals); qqline(wage_vals)
qqnorm(cap_vals);  qqline(cap_vals)
t.test(wage_vals)$conf.int
boot_ci(wage_vals)
# look at: how far the QQ-plot bends away from the line, and whether the two
# intervals differ. with this many observations they are close (CLT)


## 4. a small sample
set.seed(1)
small <- sample(cap_vals, 30)
t.test(small, mu = 3.90)
# repeat with set.seed(2), set.seed(3) ...
# look at: the interval is much wider than with all rows, and the p-value
# changes a lot between seeds: small samples are noisy


## 5. one-sample Wilcoxon against the median
wilcox.test(cap_vals, mu = median(cap_vals))
wilcox.test(cap_vals, mu = median(cap_vals) + 0.50)
# look at: against its own median the p-value is large (nothing to detect);
# a median 0.50 away is clearly rejected


# ===================================================================
# 02_two-sample.R
# ===================================================================

df_two <- df_cap %>%
  filter(urban_class %in% c("Urban", "Rural")) %>%
  mutate(urban_class = factor(urban_class, levels = c("Urban", "Rural")))


## 1. milk price, Urban vs Rural
df_milk2 <- df_two %>% filter(!is.na(milk_eur))
t.test(milk_eur ~ urban_class, data = df_milk2)
df_milk2 %>% cohens_d(milk_eur ~ urban_class)
wilcox.test(milk_eur ~ urban_class, data = df_milk2)
# look at: do both tests reach the same conclusion? if yes, the result does not
# hinge on the distributional assumptions


## 2. wrong direction in the one-tailed test
df_two_rev <- df_two %>% filter(!is.na(cap_eur)) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Urban")))
t.test(cap_eur ~ urban_class, data = df_two_rev, alternative = "greater")
# look at: the question became "is Rural HIGHER than Urban?". the data point the
# other way, so the p-value is close to 1. the direction must come from theory
# BEFORE you look at the data, never from the data


## 3. wage, Urban vs Rural: CI of the difference by hand
w_u <- df_two$wage_eur[df_two$urban_class == "Urban" & !is.na(df_two$wage_eur)]
w_r <- df_two$wage_eur[df_two$urban_class == "Rural" & !is.na(df_two$wage_eur)]
diff_mean <- mean(w_u) - mean(w_r)
diff_se   <- sqrt(var(w_u) / length(w_u) + var(w_r) / length(w_r))
c(diff = diff_mean, lower = diff_mean - 1.96 * diff_se, upper = diff_mean + 1.96 * diff_se)
t.test(w_u, w_r)$conf.int
cohens_d(data.frame(wage = c(w_u, w_r), g = rep(c("U", "R"), c(length(w_u), length(w_r)))), wage ~ g)
# look at: whether the interval contains zero and how large d is. a significant
# difference can still be small


## 4. smaller samples
set.seed(1)
df_small <- df_two %>% filter(!is.na(cap_eur)) %>%
  group_by(urban_class) %>% slice_sample(n = 15) %>% ungroup()
t.test(cap_eur ~ urban_class, data = df_small)
df_small %>% cohens_d(cap_eur ~ urban_class)
# look at: the p-value usually grows (less power) while d stays in the same
# region, only noisier. effect sizes do not depend on n, p-values do


# ===================================================================
# 03_three-sample.R
# ===================================================================

df_three <- df_cap %>%
  filter(urban_class %in% c("Rural", "Suburban", "Urban")) %>%
  mutate(urban_class = factor(urban_class, levels = c("Rural", "Suburban", "Urban")))


## 1. ANOVA of the wage
df_w <- df_three %>% filter(!is.na(wage_eur))
fit_w <- aov(wage_eur ~ urban_class, data = df_w)
summary(fit_w)
df_w %>% anova_test(wage_eur ~ urban_class)   # includes the effect size (ges = eta squared)
df_w %>% levene_test(wage_eur ~ urban_class)
oneway.test(wage_eur ~ urban_class, data = df_w, var.equal = FALSE)   # Welch ANOVA
# look at: if Levene's p is small, trust Welch's F, not the classic F


## 2. post hoc for the wage
TukeyHSD(fit_w)
df_w %>% games_howell_test(wage_eur ~ urban_class)
# look at: which pairs have an adjusted p below 0.05 in both tests


## 3. milk price: ANOVA or Kruskal-Wallis?
df_m <- df_three %>% filter(!is.na(milk_eur))
ggplot(df_m, aes(x = urban_class, y = milk_eur)) + geom_boxplot() + theme_minimal()
df_m %>% levene_test(milk_eur ~ urban_class)
kruskal.test(milk_eur ~ urban_class, data = df_m)
oneway.test(milk_eur ~ urban_class, data = df_m)
# look at: skew and outliers in the boxplot, and whether the two tests agree.
# if they do, the choice does not matter for the conclusion


## 4. unadjusted vs. Holm-adjusted pairwise tests
df_c <- df_three %>% filter(!is.na(cap_eur))
pairwise.t.test(df_c$cap_eur, df_c$urban_class, p.adjust.method = "none", pool.sd = FALSE)
pairwise.t.test(df_c$cap_eur, df_c$urban_class, p.adjust.method = "holm", pool.sd = FALSE)
# look at: Holm multiplies the p-values (the smallest by 3, the next by 2, ...)
# so p-values near 0.05 may cross the line. with three tests the family-wise
# error rate would otherwise creep up to about 14%
