source("scripts/00_setup.R")

## WHAT NEXT?
# ====
# now that the data is clean, what do we actually want
# to DO with it? something like: "does BMI differ by sex?" -- a real
# question with a real answer, not just description.
#
# but answering that requires picking the RIGHT statistical method, and
# which method is "right" depends on how the data is distributed. 
# ====


## MEAN VS MEDIAN
# ====
# most methods you'll learn work by comparing either the MEAN or the
# MEDIAN of your data across groups.
#
# the MEAN uses the actual numeric values -- which makes it sensitive to
# extreme outliers (one huge value can drag it a long way), and it also
# means the math behind confidence intervals and p-values for the mean
# only works cleanly if the data (or, for large samples, the mean's own
# sampling distribution -- more on this soon) behaves like a normal
# distribution.
#
# the MEDIAN (and rank-based methods generally) only cares about the
# ORDER of your values, not their exact numeric distance from each
# other. that makes it robust to outliers, AND it means the math behind
# it doesn't need any assumption about the shape of your data at all
# ====


## THE LINGO
# ====
# this course calls the two upcoming folders "normal-data" and
# "other-data" -- but the standard statistical terms for this exact
# split are PARAMETRIC and NONPARAMETRIC:
#
#   PARAMETRIC     = assumes the data follows a known distribution
#                    (usually normal), described by a small set of
#                    PARAMETERS (mean, sd) -- mean-based methods
#
#   NONPARAMETRIC  = makes no assumption about the underlying
#                    distribution -- "distribution-free" -- median/
#                    rank-based methods
#
# you'll see both names used interchangeably from here on
# ====


## A ROADMAP
# ====
# same question, two tools -- which one you reach for depends on
# whether your data (or your sample size) lets you assume normality.
# this table previews everything the rest of the course will cover
# ====

methods_reference <- data.frame(
  question = c("comparing two groups",
               "comparing 3+ groups",
               "relationship of two continuous variables",
               "association of two categorical variables"),
  parametric    = c("t-test", "ANOVA", "Pearson correlation / regression", "Chi-square"),
  nonparametric = c("Mann-Whitney U", "Kruskal-Wallis", "Spearman correlation", "Fisher's exact")
)

methods_reference


## EXPORTING FOR LATEX
# ====
# this script has been mostly conceptual, so it's a good moment to
# introduce something you'll use throughout the course: exporting a
# table straight to LaTeX, ready to \input{} into a report.
#
# xtable() converts a data.frame into LaTeX table syntax; print() with
# file = ... writes that to a .tex file instead of printing to console
# ====

methods_table <- xtable(methods_reference)

print(methods_table,
      file = file.path(dir_tab, "methods-reference.tex"),
      include.rownames = FALSE,
      size = "footnotesize",
      booktabs = TRUE)   # matches \usepackage{booktabs} in the LaTeX template below

