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

## SETUP PROJECT DIRECTORIES
dir_src <- "datasets"       # INPUT — read only, never write here
dir_dat <- "output/data"    # output
dir_fig <- "output/figures" # output
dir_tab <- "output/tables"  # output

# create all of them if they don't exist yet
dirs <- c(dir_src, dir_dat, dir_fig, dir_tab) # collect all paths into one vector...
for (d in dirs) {                              # ...and loop through it
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    cat("Created folder:", d, "\n")
  }
}