# ---------------------------------------------------------------------------
# 00_setup.R — run this first, every session, before any lesson script
# ---------------------------------------------------------------------------

## CHECK WE ARE IN THE PROJECT ROOT (i.e. RStudio was opened via the .Rproj)
# file.exists() checks for that exact file in the working directory;
# if it's not there, we're in the wrong folder (or RStudio wasn't opened
# via the .Rproj), so stop immediately with a clear error instead of
# letting later code fail confusingly
if (!file.exists("./data-analytics.Rproj")) {
  stop("Not in the project root — open the project via data-analytics.Rproj, then run this script again.")
}

## INSTALL / LOAD COMMONLY USED LIBRARIES
# Use only for libraries you plan to use across the project,
# such as dplyr, ggplot, etc.
# Libraries specific for few scripts is better to load in that
# script itself.

## let's create a function that installs packages ONLY IF they are not yet installed
# and load them every time.
#
# NOTE: "already installed" is not the same as "loads cleanly". A package can
# be present on disk but broken — e.g. an install got interrupted partway
# (leaving a stale 00LOCK-<package> folder that blocks any future install of
# it), a computer got a new R version and the old binary no longer matches it,
# or antivirus/OneDrive briefly locked a file mid-install. When that happens,
# requireNamespace() reports the package as present, but library() then
# throws an error and the whole script stops — even though the package "was
# already installed". So instead of trusting requireNamespace() alone, we
# actually try to load the package first, and only (re)install it if that
# load fails.
load_packages <- function(packages) {
  for (pkg in packages) {
    loaded <- suppressWarnings(
      tryCatch({
        library(pkg, character.only = TRUE) # character.only = TRUE tells library() pkg is a variable holding a name, not a literal package name
        TRUE
      }, error = function(e) FALSE)
    )
    
    if (!loaded) {
      # remove a leftover lock folder from a previous interrupted install —
      # if it's still there, install.packages() fails immediately with
      # "failed to lock directory ... for modifying"
      lib_path <- .libPaths()[1]
      lock_dir <- file.path(lib_path, paste0("00LOCK-", pkg))
      if (dir.exists(lock_dir)) {
        unlink(lock_dir, recursive = TRUE)
        cat("Removed a stale lock folder for", pkg, "and will reinstall it.\n")
      }
      
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE) # dependencies = TRUE also grabs any of its own dependencies that may be missing/broken
      
      # try loading again now that it's (re)installed; if this still fails,
      # stop here with a clear message pointing at the real package,
      # instead of a confusing downstream error later in the script
      library(pkg, character.only = TRUE)
    }
  }
}

# install and load all packages defined by a vector
load_packages(
  c(
    "dplyr",
    "ggplot2",
    "ggthemes", # extra themes for ggplot
    "tidyr",
    "Hmisc", # makes hist() work with a data.frame
    "xtable", # exports data frames to latex format
    "rstatix" # tidyverse stat tests
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


## Our function to compute moments for any distribution
# Usage:
#   moment(x, 1)   # mean (~0)
#   moment(x, 2)   # variance
#   moment(x, 3)   # skewness
#   moment(x, 4)   # kurtosis -- RAW, not excess: moment(x, 4) - 3 for that
moment <- function(x, k) {
  n <- length(x)
  m <- mean((x - mean(x))^k)
  if (k == 2) m <- m * n / (n - 1)   # match var()'s (n-1) convention
  if (k >= 3) m <- m / sd(x)^k       # standardize only from k=3 onward
  m
}