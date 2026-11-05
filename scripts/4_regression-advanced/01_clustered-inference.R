# ===================================================================
# 01_clustered-inference.R -- WHEN THE ROWS ARE NOT INDEPENDENT (ADVANCED)
# optional material that goes with 3_regression-models/02, 03 and 04: a regression
# of countries (weighted and unweighted) and country fixed effects --
# two more ways to ask "who counts, rows or countries?" -- plus a cluster
# bootstrap of the slope and a cluster-robust F-test for a factor
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


## SAME QUESTION, DIFFERENT UNIT: A REGRESSION OF COUNTRIES
# ====
# if milk is a country property, why not analyze countries? collapse the data
# to ONE row per country (the mean log price, the mean log wage, the milk
# price) and fit the same model. but there is a choice hidden here: should the
# country of 572 rows (USA) count as much as the country of 1 row (Taiwan)?
# an UNWEIGHTED country regression says yes; a regression WEIGHTED by the number
# of rows says no -- and it comes close to the row-level analysis
# ====

df_country <- df_milk %>%
  group_by(country) %>%
  summarise(n_rows = n(),
            mean_log_price = mean(log(cap_eur)),
            mean_log_wage  = mean(log(wage_eur)),
            milk_eur       = first(milk_eur),
            .groups = "drop")

fit_cty_unw <- lm(mean_log_price ~ mean_log_wage + log(milk_eur), data = df_country)
fit_cty_w   <- lm(mean_log_price ~ mean_log_wage + log(milk_eur), data = df_country, weights = n_rows)

rbind(unweighted = summary(fit_cty_unw)$coefficients["log(milk_eur)", ],
      weighted   = summary(fit_cty_w)$coefficients["log(milk_eur)", ])

ggplot(df_country, aes(x = milk_eur, y = mean_log_price, size = n_rows)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  labs(x = "milk_eur (log scale)", y = "country mean of log(cap_eur)", size = "rows in the data",
       title = "One point per country: cheap milk and cheap coffee go together -- unless you count rows") +
  theme_minimal()

# result: the unweighted country regression gives milk an elasticity of +0.26
# (se 0.114, p = 0.026): positive and "significant". the weighted regression
# gives -0.066 (se 0.067, p = 0.33): negative, like the row-level model. the
# SIGN of the milk coefficient depends on whether a country with 2 rows counts
# as much as one with 572. the cheap-milk, cheap-coffee countries (India,
# Bolivia, Pakistan: 0.65 to 0.83 euros per litre, only 14 rows between them)
# drive the first, the rich many-row countries (USA, UK, Germany) the second.
# this is what fragility looks like: a conclusion that flips with a defensible
# analytic choice should not be reported as a finding. the honest summary is
# "no reliable association of milk with price once the wage is taken into
# account", plus the note that the dataset's rows are not a sample of
# countries (the numbers of rows per country reflect where data were collected)


## MOST OF THE WAGE EFFECT IS ALSO BETWEEN COUNTRIES
# ====
# the same lens on the wage. a model with a separate intercept for every
# country ("country fixed effects") compares cities WITHIN each country only, so
# everything that makes countries different (milk, prices level, taxes, culture)
# is removed:
# ====

fit_fe <- lm(cap_eur ~ wage_eur + factor(country), data = df_milk)
rbind(pooled          = coeftest(fit_wage)["wage_eur", ],
      country_effects = coeftest(fit_fe, vcov = vcovCL(fit_fe, cluster = df_milk$country))["wage_eur", ])

# result: within countries the wage slope is 0.029 (cluster se 0.008, still
# significant at p = 0.0002), about a THIRD of the pooled 0.085. so two thirds
# of what the pooled line calls "the effect of the wage" is a difference
# BETWEEN countries: richer countries have dearer cappuccinos, and the pooled
# line adds that to the comparison of cities inside a country. neither number
# is "the" answer: they describe different comparisons. random-intercept
# (mixed) models, in advanced 03, handle this in a more economical way than 86
# dummy variables


## CLUSTER-ROBUST F-TEST OF THE URBAN CLASS [from 03]
# ====
# the omnibus F-test of the price model (03), with whole countries as clusters
# ====

fit_three <- lm(cap_eur ~ urban_class, data = df_cont)
Anova(fit_three, vcov. = vcovCL(fit_three, cluster = df_cont$country))

# result: F = 0.38 (p = 0.68): clustering moves the already non-significant
# result even further from significance. for the WAGE model the same
# correction would be much more consequential -- try it in the exercises


## THE CLUSTER BOOTSTRAP FOR THE SLOPE [from 02]
# (the model, n_boot and the country-clustered interval are defined here, as in 02)
fit <- lm(cap_eur ~ wage_eur, data = df_cont)
n_boot <- 2000
ci_country <- coef(fit)["wage_eur"] + c(-1, 1) * qnorm(0.975) *
  coeftest(fit, vcov = vcovCL(fit, cluster = df_cont$country))["wage_eur", 2]

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
      cluster_robust_se = ci_country,   # country-cluster-robust interval
      cluster_bootstrap = quantile(slope_boot, c(0.025, 0.975)))

# result: the country-cluster bootstrap gives 0.045 to 0.127, matching the
# cluster-robust interval almost exactly: two independent routes, the same
# honest answer -- somewhere between about 4 and 13 cents per euro of wage


## TRY IT YOURSELF
# ====
# 1. make the cluster bootstrap resample CITIES instead of countries. is its
#    interval between the classic and the country-level one?
# 2. run the cluster-robust F-test for the WAGE by urban class
#    (lm(wage_eur ~ urban_class)). does the difference survive treating countries
#    as the independent units? why might it not?
# ====
