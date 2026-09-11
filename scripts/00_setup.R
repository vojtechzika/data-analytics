# ---------------------------------------------------------------------------
# 00_setup.R — run this first, every session, before any lesson script
# ---------------------------------------------------------------------------

## CHECK WE ARE IN THE PROJECT ROOT (i.e. RStudio was opened via the .Rproj)
# file.exists() checks for that exact file in the working directory;
# if it's not there, we're in the wrong folder (or RStudio wasn't opened
# via the .Rproj), so stop immediately with a clear error instead of
# letting later code fail confusingly
if (!file.exists("data-analytics.Rproj")) {
  stop("Not in the project root — open the project via data-analytics.Rproj, then run this script again.")
}

## INSTALL / LOAD COMMONLY USED LIBRARIES
# Use only for libraries you plan to use across the project,
# such as dplyr, ggplot, etc.
# Libraries specific for few scripts is better to load in that
# script itself.

## let's create a function that installs packages ONLY IF they are not yet installed
# and load them every time.
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE) # character.only = TRUE tells library() pkg is a variable holding a name, not a literal package name
  }
}

# install and load all packages defined by a vector
load_packages(
  c(
    "dplyr", 
    "ggplot2",
    "tidyr",
    "Hmisc", # makes hist() work with a data.frame
    "xtable" # exports data frames to latex format
))



## SETUP PROJECT DIRECTORIES
dir_src <- "datasets"       # INPUT — read only, never write here
dir_dat <- "output/data"    # output folder for curated datasets
dir_fig <- "output/figures" # output folder for generated figures
dir_tab <- "output/tables"  # output folder for generated tables
dir_dia <- "output/diagnostics"  # output folder for diagnostic files

# create all of them if they don't exist yet
dirs <- c(dir_src, dir_dat, dir_fig, dir_tab, dir_dia) # collect all paths into one vector...
for (d in dirs) {                              # ...and loop through it
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("Created folder:", d, "\n")
  }
}