# LOAD DEPENDENCIES
source("scripts/00_setup.R")  

# LOAD Harry Potter data that we created in 01_base-r.R
df_hp <- read.csv(file.path(dir_dat, "harry-potter.csv"), sep = ",", header = TRUE)

## GGPLOT2
# ====
# for visualisations, we will mostly use ggplot() -- the main function
# from the ggplot2 package, R's standard tool for plotting
# library(ggplot2) - already added to the 00_setup.R as we will use it frequently
# ====


## HOW GGPLOT WORKS
# ====
# ggplot builds a plot in LAYERS, combined with +:
#   1. ggplot(data, aes(...))   -- the DATA, and which columns map to which
#      visual property (x, y, color, ...). Nothing is drawn yet.
#   2. + geom_...()             -- HOW to draw that mapping (points, bars,
#      lines, ...). It uses the same data/aes() unless told otherwise.
# because step 1 and step 2 are separate, you can swap the geom and
# everything else stays exactly the same:
# ====

ggplot(df_hp, aes(x = skill, y = height)) + geom_point()
ggplot(df_hp, aes(x = skill, y = height)) + geom_smooth()
# ^ same data, same mapping -- only the geom (the "how") changed

# you can also save the base and build on it step by step, since a
# ggplot object is really just an accumulation of layers
p <- ggplot(df_hp, aes(x = skill, y = height))
p + geom_point()
p + geom_point() + geom_smooth()          # layers just stack with +

# aes() doesn't have to live in ggplot() -- putting it inside a geom
# instead maps it for THAT layer only, not the whole plot
ggplot(df_hp) +
  geom_point(aes(x = skill, y = height, color = movie))

# geom_bar()'s stat argument lets ggplot RUN an operation on the data
# for you, on the fly, instead of just drawing raw values -- here,
# stat = "summary" + fun = "mean" computes mean(y) per x group
# (movie) before drawing, so you never touch dplyr for this
ggplot(df_hp, aes(x = movie, y = skill)) +
  geom_bar(stat = "summary", fun = "mean")

## MORE LAYERS: FACETS, SCALES, LABELS
# ====
# same idea applies beyond geoms -- facets, scales, and labels are also
# just layers added with +, they just don't draw new geometry
# ====

# facets are ALSO just a layer -- splits the same plot into panels by a
# variable, without touching the underlying data or aes() mapping
# NOTE: the ~ is R's formula syntax -- more on this when we get to t.test()
ggplot(df_hp, aes(x = skill, y = height)) +
  geom_point() +
  facet_wrap(~ movie)

# scales control HOW an aesthetic gets displayed -- e.g. override the
# default colors chosen for the color = movie mapping from before
ggplot(df_hp, aes(x = skill, y = height, color = movie)) +
  geom_point() +
  scale_color_manual(values = c("Harry Potter" = "darkred",
                                "Fantastic Beasts" = "darkblue"))

# labs() and theme() are layers too -- they don't touch the data at all,
# only the plot's appearance
ggplot(df_hp, aes(x = skill, y = height, color = movie)) +
  geom_point() +
  labs(title = "Magic skill vs height", x = "Skill (0-100)", y = "Height (m)") +
  theme_minimal()

# ====
# everything above generalizes: ggplot has MANY geoms (histogram, boxplot,
# violin, bar, line, ...) -- each is just a different "how to draw it"
# choice sitting on top of the same data + aes() foundation you just saw
# ====



## PUTTING IT ALL TOGETHER
# ====
# combine several of the layers above into one polished plot, and export
# it -- once you can add ONE layer, adding five is no harder. this is the
# payoff of the layering system you've just learned
# ====

nice_plot <- ggplot(df_hp, aes(x = skill, y = height, color = movie)) +  # data + mapping: x, y, color by movie
  geom_point(size = 3) +                                                 # draw the points, bigger than default
  geom_smooth(method = "lm", se = FALSE) +                               # linear trend line per color group, no ribbon
  scale_color_manual(values = c("Harry Potter" = "darkred",
                                "Fantastic Beasts" = "darkblue")) +      # custom colors instead of ggplot defaults
  labs(title = "Magic Skill vs Height",                                  # custom title
       subtitle = "Harry Potter vs Fantastic Beasts Characters",         # custom subtitle
       x = "Skill (0-100)",                                              # x-axis title
       y = "Height (m)",                                                 # y-axis title
       color = "Movie") +                                                # legend title   
  theme_minimal() +                                                      # cleaner background style
  theme(legend.position = "bottom")                                     # show the legend below the plot rather than on the left (default)

nice_plot   # print the plot

# ^ export the last plot printed -- one line, no device open/close needed
ggsave(file.path(dir_fig, "hp-skill-vs-height.png"), # where to save
       width = 6,                                    # width
       height = 4,                                   # height
       dpi = 300                                     # resolution
       )


