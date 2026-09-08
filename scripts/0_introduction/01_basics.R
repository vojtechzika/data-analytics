## =========================================================
##  INTRO TO R — Basics you'll use constantly
##  Data Analytics Course
## =========================================================
##
## How to use this script:
## - Run it line by line (Ctrl+Enter / Cmd+Enter in RStudio)
## - Read the comments (anything after a #) — they explain what's happening
## - Nothing bad happens if you run it top to bottom, so experiment freely

# ---------------------------------------------------------
# 1. VARIABLES (storing values)
# ---------------------------------------------------------
# Use <- to assign a value to a name (= also works, but <- is the R convention)

x <- 9
y <- 2

# you can use the variables for basic calculation
x + y
x - y
x * y
x / y
x^y        # exponent (2 to the power of 3)
x %/% y   # integer division
x %% y    # modulo — remainder of 17 / 5

(x + y) * x  # Order of operations (PEMDAS) works as expected:


# Once assigned, a variable stays in memory until you overwrite or remove it
x <- x + 1   # reassign x to its old value + 1
x

# Be careful with reassigning variables, it may cause troubles!

# ---------------------------------------------------------
# 2. VARIABLE TYPES
# ---------------------------------------------------------

# find out what is the variable's type using the class() function
class(x) # this variable is of type "numeric"

# to learn more about a function, use ? and function's name
?class

# possible variable types
var_numeric <- 3.14 # numeric: stores decimal or whole numbers
class(var_numeric)

var_integer <- 1L # integer: stores only integers
class(var_integer)

var_character <- "hello world" # character: stores text (always use quotes)
class(var_character)

var_logical <- TRUE # logical: stores logical values TRUE or FALSE (no quotes, all caps)
                    # TRUE is equivalent to 1, FALSE to 0
class(var_logical)

var_na <- NA # NA = "Not Available" — R's marker for a missing value (no quotes, all caps)
class(var_na)

# use == operator to check whether two variables have the same VALUE
var_integer == var_logical # TRUE: both have the same value as 1 is the same as TRUE (which is also 1)
var_character == "Hello World" # FALSE: R is case-sensitive (hello world vs. Hello World)

# use identical() function to check whether two variables are the same TYPE
identical(var_integer, var_logical) # FALSE: they are not of the same type

# use is.na() function to check whether a value is NA
is.na(var_na) # TRUE
is.na(var_integer) # FALSE

# You cannot run math operations on NA
5 + NA # you cannot sum NA: this is like summing infinite
NA == NA # you cannot compare two NAs: this is like comparing one infinite to another


# ---------------------------------------------------------
# 3. CONVERTING BETWEEN TYPES 
# ---------------------------------------------------------

# very often, you get a dataset with messy variable types
var_numeric_messy <- "3.14"
var_logical_messy <- "TRUE"
var_na_messy <- "NA"

var_numeric_messy * 3 # throws error
var_logical_messy * 3 # throws error
is.na(var_na_messy) # FALSE: is not NA

# Why? Because all the variables are of type "character"
class(var_numeric_messy)
class(var_logical_messy)
class(var_na_messy)

# you can convert character variables to the required type
new_numeric <- as.numeric(var_numeric_messy) # use as.numeric() to convert to numeric
is.numeric(new_numeric) #TRUE: is numeric
new_numeric * 3 # now you can do math

new_logical <- as.logical(var_logical_messy) # use as.logical() to convert to logical
is.logical(new_logical) #TRUE: is logical
new_logical * 3 # now you can do math

new_na <- as.logical(var_na_messy) # use as.logical() to convert to logical
is.na(new_na) #TRUE: is NA


# you can also convert numeric to integer: for instance, on the pi constant
pi

as.integer(pi) # 3: this truncates everything after the decimal point, it does not round 

# if you want to round, use
round(pi, digits = 2) # 3.14: rounds to 2 decimals
floor(pi) # 3: nearest lower integer
ceiling(pi) # 4: nearest higher integer


# ---------------------------------------------------------
# 4. VECTORS (a sequence of values)
# ---------------------------------------------------------

c_name <- c("Albus Dumbledoor", "Bellatrix Lestrange", "Credence Barebone", "Dobby", "Ernie Macmillan", "Fred Weasley", "Gellert Grindelwald", "Hermione Granger") # name of a character
c_blood <- c("Half-blood", "Pure-blood", "Unknown", NA, "Pure-blood", "Pure-blood", "Pure-blood", "Muggle-born") # blood status
c_skill <- c(99L, 85L, 55L, 65L, 35L, 60L, 90L, 80L) # magic skill of a character
c_is_hp <- c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE) # is it a Harry Potter character?
c_height <- c(1.78, 1.58, 1.80, 1.06, NA, 1.89, 1.82, 1.65) # height of a character in meters

#check the class
class(c_name)
class(c_blood)
class(c_skill)
class(c_is_hp)
class(c_height)

#check the type
typeof(c_name)
typeof(c_blood)
typeof(c_skill)
typeof(c_is_hp)
typeof(c_height)

# you can compare vector's types and values
identical(c_skill, c_height) # FALSE, different types
c_skill == c_height # compares each pair of elements

#you can select part of a vector by using []
c_name[4]      # only fourth element (first element is always 1, not zero as in some other languages)
c_name[-4]     # everything EXCEPT the fourth element
c_name[5:8]    # elements 2 through 4

# you can change vector's element value
c_name[1] # surname is misspelled
c_name[1] <- "Albus Dumbledore" # lets make it right

#you can find out vector length()
length(c_name)

#you can sort() a vector
sort(c_name, decreasing = TRUE)

#you can summarize a vector
summary(c_skill)

#you can do math operations on a vector
sum(c_is_hp) # 6: number of Harry Potter characters
mean(c_is_hp) # 0.75: the percentage share of Harry Potter characters

#you can transform a vector, using a vectorized math
m_to_ft <- 3.28084 # meters to feet ratio
c_height * m_to_ft #transform the height in meters into a height in feet

# but pay attention: if you try to do math on a vector containing NA, you will get NA 
mean(c_height)
# ...unless you tell R to ignore (remove) the NA first
mean(c_height, na.rm = TRUE)   

# also vectors can be messy
c_height_messy <- c("1.78", "1.58", "1.80", "1.06", "NA", "1.89", "1.82", "1.65") # numeric values were stored as text - VERY COMMON
class(c_height_messy)

c_height_messy * m_to_ft # now you cannot transform it to feet

as.numeric(c_height_messy) * m_to_ft # you must first convert the vector to numeric, as with the variable
# Watch out! If a value genuinely isn't a valid number, R converts it to NA
# and gives you a warning (as the one you can see in the Console) 
# rather than crashing — always check for this!


## FACTORS — a factor stores categorical values with a fixed set of allowed 
# "levels" (here, the four labels above, ignoring NA), rather than free text. 
blood_factor <- factor(c_blood)
blood_factor

class(blood_factor)      # "factor"
typeof(blood_factor)     # "integer"

as.integer(blood_factor) # underlying codes — NA stays NA, doesn't become its own level
levels(blood_factor)     # "Half-blood" "Muggle-born" "Pure-blood" — alphabetical, not input order

blood_factor[8] <- "Mudblood" # if we try to change Hermione's status to "Mudblood" we get a warning -
                              # no other values than defined labels are not allowed, NA is coerced
blood_factor[8] <- "Muggle-born" # let's change it back to "Muggle-born", now it works!


# ---------------------------------------------------------
# 5. DATA FRAMES (a set of vectors)
# ---------------------------------------------------------

# Now we can combine the vectors to create a data frame ("table") called df_hp 
# (use the df_ prefix to signal it is a data frame, not a common variable)
df_hp <- data.frame(
  name = c_name,
  blood = blood_factor,
  skill  = c_skill,
  height = c_height,
  is_hp_character = c_is_hp
)


## Check the data frame's STRUCTURE

# Display the data frame structure at once
str(df_hp) 

# Or individual properties ... may come handy if you do not want the full str() 
dim(df_hp) # how many rows and columns did we load?
nrow(df_hp) # how many rows did we load?
ncol (df_hp) # how many columns did we load?
colnames(df_hp) # what are the column names? (handy for copy-pasting into code)

# Apply Any Function over a List or Vector
sapply(df_hp, class) # check class for each column (vector)
sapply(df_hp, typeof) # check type for each column (vector)


## Check the data frame's VALUES

df_hp # prints df into the console (not convenient with a big data sets)
head(df_hp) # prints first six rows of the df (convenient as a preview)
View(df_hp) # Open df in "Excel-style" spreadsheet view ... if left open, it reflects all changes to df we make
summary(df_hp) # See a summary() for all columns. Note that summary() does not show means for logical variables (but mean() does)
                # and show counts for factors


## Accessing individual values in data frame

# to access any column as a new data frame, use []
df_hp[1] # creates data frame containing only the first column of df_hp
df_hp["name"] # or you can call any column by its name
df_hp[-3] # creates data frame containing all but the third column of df_hp
df_hp[1:3] # creates data frame containing columns 1 to 3 of df_hp
df_hp[-1:-3] # creates data frame NOT containing columns 1 to 3 of df_hp

# to access any column as a vector, use [[]]
df_hp[[1]] # prints first column as a vector
df_hp[["name"]] # you can do the same by column's name
df_hp$name # or using $ operator: MOST CONVENIENT!

# to access a specific value of a data frame, use [row, column]
df_hp[4, "name"] # prints fourth row of the column "name"
df_hp[4, 1] # prints fourth row of the first column
df_hp$name[4] # or using $ operator in combination with []: MOST CONVENIENT!

# you can leave one number out in df[row, column] like so:
df_hp[, 1] # prints ALL rows for the first column
df_hp[4, ] # prints ALL columns for the fourth row
# this is useful for filtering, see below!


## FILTERING VALUES

# logical filtering
df_hp$blood == "Pure-blood" # returns logical for whether each element of the selected 
                            # column satisfies the condition of having a pure blood.

mean(df_hp$blood == "Pure-blood", na.rm = TRUE) # may be used to quickly compute, for instance, the share of 
                                                # characters with a pure blood (ignoring NAs)

# basic filtering
df_hp[df_hp$is_hp_character == TRUE, ] # prints all columns for hp characters
df_hp[df_hp$is_hp_character, ] # since is_hp_character is LOGICAL, == TRUE is reduntand and you can use the shortened form
length(df_hp[df_hp$is_hp_character, ]) # may be used to compute how many instances satisfying this condition are in the dataset

df_hp[df_hp$is_hp_character, "name"] # prints column name for all HP characters
df_hp[df_hp$is_hp_character, 1] # or you can do the same by column number 

# logical operators
df_hp[df_hp$skill == 65, ] # prints all columns where skill does equal to 65
df_hp[df_hp$skill != 65, ] # prints all columns where skill does NOT equal to 65
df_hp[df_hp$skill > 65, ] # prints all columns where skill is greater than 65
df_hp[df_hp$skill < 65, ] # prints all columns where skill is lower than 65
df_hp[df_hp$skill >= 65, ] # prints all columns where skill is greater OR equal than 65
df_hp[df_hp$skill <= 65, ] # prints all columns where skill is lower OR equal than 65


## Adding/deleting rows and columns to the data frame

# adding and deleting columns is straightforward
df_hp$house <- NA # adds an empty column called "house" filled with NA
df_hp$house <- NULL # deletes the empty house column 
df_hp$house <- c("Gryffindor", "Slytherin", NA, NA, "Hufflepuff", "Gryffindor", NA, "Gryffindor") # adds the house column again, this time with values

# adding rows is a bit trickier
# first we need to create a data frame with new rows - the data frame MUST have the same structure as the one we want to expand!
new_rows <- data.frame(
  name = c("Tina Goldstein", "Newt Scamander"),
  blood = c("Unknown", "Pure-blood"),
  skill = c(70, 75),
  height = c(1.65, 1.78),
  is_hp_character = c(FALSE, FALSE),
  house = c(NA, "Hufflepuff")
)

# then we use rbind() to attach the new rows at the end of df_hp
df_hp <- rbind(df_hp, new_rows)

# and we want to keep alphabetical order by name
df_hp <- df_hp[order(df_hp$name), ]

# optionally, you can now delete new_rows
new_rows <- NULL


# ---------------------------------------------------------------------------
# 7. IF-ELSE STATEMENTS (a way to conditionally decide on an action)
# ---------------------------------------------------------------------------

## ifelse(condition, action if true, action if false)
# itterates through each element of a vector
ifelse(df_hp$is_hp_character == TRUE, "Harry Potter", "Fantastic Beasts")

# we can also save it as a new column of the df_hp
df_hp$movie <- ifelse(df_hp$is_hp_character == TRUE, "Harry Potter", "Fantastic Beasts")

# you can NEST ifelse statement
ifelse(df_hp$skill > 90, "Excellent Wizzard", # if > 90, Excellent, if not, continue to the next ifelse
       ifelse(df_hp$skill > 80, "Very Good Wizzard", # if > 80, Very Good, if not, continue to the next ifelse
              ifelse(df_hp$skill > 60, "Good Wizzard", "Bad Wizzard") # if > 60, Good, if not, Bad
              )
       )

## for more complex ifelse, use cut() (returns factors)
cut(
  df_hp$skill,                          # numeric vector to bin
  breaks = c(-Inf, 60, 80, 90, Inf),    # bin boundaries: (-Inf,60], (60,80], (80,90], (90,Inf]
  labels = c("Not a Good Wizzard", "Good Wizzard", "Very Good Wizzard", "Excellent Wizzard"), # one label per bin, in the same order as breaks
  right = TRUE                          # intervals closed on the right: a value equal to a breakpoint falls in the LOWER bin (e.g. 60 -> "Bad Wizzard")
)

## if(condition) {action if true} else {action if false}
# works only for a single value — used for control flow: deciding which
# block of code to run next (e.g. inside a function, a loop iteration,
# or a script), NOT for producing a vector of results element-by-element
# (that's what ifelse()/cut() are for)

if(nrow(df_hp) > ncol(df_hp)){ # compare df_hp's length (nrow) and width (ncol)
  print(paste0("The data frame df_hp has ", nrow(df_hp), " rows and ", ncol(df_hp), " columns, so it is longer than wider."))
} else {
  print(paste0("The data frame df_hp has ", nrow(df_hp), " rows and ", ncol(df_hp), " columns, so it is wider than longer."))
}

# if / else if / else — chain extra conditions with "else if" before the final "else"
# needed here because there are three possible outcomes (>, <, ==), not just two
if(nrow(df_hp) > ncol(df_hp)){
  print(paste0("The data frame df_hp has ", nrow(df_hp), " rows and ", ncol(df_hp), " columns, so it is longer than wider."))
} else if(nrow(df_hp) < ncol(df_hp)){
  print(paste0("The data frame df_hp has ", nrow(df_hp), " rows and ", ncol(df_hp), " columns, so it is wider than longer."))
} else {
  print(paste0("The data frame df_hp has ", nrow(df_hp), " rows and ", ncol(df_hp), " columns, so it has the same number of rows and columns."))
}


# ---------------------------------------------------------------------------
# 8. SEQUENCES and LOOPS (a way to perform specific operations on data frame
# ---------------------------------------------------------------------------

## Generate sequences easily
0:10                    # creates a vector of values from 1:10
seq(0, 100, by = 10)    # creates a vector of values from 0:100 by steps of 10
rep("A", times = 6)     # creates a vector of six "A"
rep( c("A", "B", "C"), times = 2) # creates a vector of two "A" "B" "C"

## SIMPLE LOOPS
# this is a basic structure of a loop
for (i in 0:10) { # i is every value of a vector 0:10
  print(i) # output every value of i
}

## loop through a vector and say Hello to every character
for(n in df_hp$name){ # n is every value of a df_hp$name vector
 print(paste0("Hello, ", n, "!")) # use paste0() to combine strings and variables
}

###
# EXERCISE: find out what happens if you use paste() and cat() instead of paste0()
###


## COMPLEX LOOP
# loop through a data frame to build a custom message per row
for (i in seq_len(nrow(df_hp))) { # i = 1, 2, ..., number of rows in df_hp
  row <- df_hp[i, ] # grab the i-th row
  
  # classify this row's skill using the same cut() bins defined earlier
  rank <- as.character(cut( # as.character() turns the factor cut() returns into a plain string
    row$skill,
    breaks = c(-Inf, 60, 80, 90, Inf),
    labels = c("not a good", "a good", "a very good", "an excellent"),
    right = TRUE
  ))
  
  # only mention the house if it's not NA (e.g. Fantastic Beasts characters with no Hogwarts house)
  if (is.na(row$house)) {
    house_text <- ""
  } else {
    house_text <- paste0(" from house of ", row$house)
  }
  
  # build the sentence once, so it can be both printed and stored
  sentence <- paste0(row$name, " is a ", row$movie, "'s character", house_text, " and is ", rank, " wizzard.")
  
  # cat() prints without quotes and lets us control line breaks with "\n"
  cat(paste0(sentence, " \n"))
  
  # also save it into a new "verbal" column, in the i-th row
  df_hp$verbal[i] <- sentence
}


## LOOPS vs. VECTORIZED CODE
# The loop above works, but R is built around "vectorized" operations —
# functions like cut(), ifelse(), and paste0() already operate on a whole
# column at once, without you writing the loop yourself.
# Vectorized code is usually faster, shorter, and more idiomatic in R.
# Loops are still the right tool when each step depends on the previous
# one, when you need a side effect per row (printing, writing a file, an
# API call), or when no vectorized function exists for what you need.

# classify skill for every row at once (same bins as cut() above)
rank <- as.character(cut(
  df_hp$skill,
  breaks = c(-Inf, 60, 80, 90, Inf),
  labels = c("not a good", "a good", "a very good", "an excellent"),
  right = TRUE
))

# build house_text for every row at once (empty string where house is NA)
house_text <- ifelse(is.na(df_hp$house), "", paste0(" from house of ", df_hp$house))

# combine into one sentence per row, stored directly in the data frame
df_hp$verbal <- paste0(df_hp$name, " is a ", df_hp$movie, "'s character", house_text, " and is ", rank, " wizzard.")

# print them all at once, one sentence per line
cat(df_hp$verbal, sep = "\n")



# ---------------------------------------------------------------------------
# 9. FUNCTIONS (a way to package code you'll reuse, instead of retyping it as
#               we did with the skill classification above).
# ---------------------------------------------------------------------------

## basic structure of a function
# function_name <- function(argument1, argument2) { body of code }
say_hello <- function(name) {
  paste0("Hello, ", name, "!")
}

# for the function to work, you first need to execute the function's code (as any other code in R)
say_hello("Albus Dumbledore") # call the function with one argument

# functions can take multiple arguments, and arguments can have default values
say_hello <- function(name, greeting = "Hello") { # greeting defaults to "Hello" if not supplied
  paste0(greeting, ", ", name, "!")
}
say_hello("Albus Dumbledore")                 # uses the default greeting
say_hello("Albus Dumbledore", "Good morning") # overrides the default

# a function's last evaluated line is what it returns (no need for return(), though it works too)
add_numbers <- function(a, b) {
  a + b
}
add_numbers(2, 3)

## functions are vectorized automatically IF the code inside them is vectorized
# (this is the same idea as ifelse()/cut()/paste0() from before — R doesn't
# care whether a function is "built-in" or one you wrote yourself)
classify_skill <- function(skill) {
  as.character(cut(
    skill,
    breaks = c(-Inf, 60, 80, 90, Inf),
    labels = c("not a good wizzard", "a good wizzard", "a very good wizzard", "an excellent wizzard"),
    right = TRUE
  ))
}
classify_skill(75)             # works on a single value
classify_skill(df_hp$skill)    # works on the whole column at once, no loop needed

## now package the whole "build a sentence" logic from before into one function
build_sentence <- function(name, movie, house, skill) {
  rank <- classify_skill(skill) # reuse the function we just built
  house_text <- ifelse(is.na(house), "", paste0(" from house of ", house))
  paste0(name, " is a ", movie, "'s character", house_text, " and is ", rank, " wizzard.")
}

# call it on the whole data frame at once — no loop, because everything
# inside build_sentence() (cut, ifelse, paste0) is already vectorized
df_hp$verbal <- build_sentence(df_hp$name, df_hp$movie, df_hp$house, df_hp$skill)
cat(df_hp$verbal, sep = "\n")


# ---------------------------------------------------------------------------
# 10. LIBRARIES (packages of predefined functions someone else already wrote)
# ---------------------------------------------------------------------------
# everything we've used so far (paste0, cut, ifelse, mean, ...) comes
# built into "base R" — already loaded, ready to use.
# a library (aka "package") is just more functions, written by someone
# else, that don't come loaded by default. two steps to use one:
#   1. install.packages("name") — downloads the code onto your machine.
#      only needs to be run ONCE, ever (like installing an app)
#   2. library(name) — loads it into your current R session so its
#      functions become available. needs to be run every time you
#      start a new R session (like opening an app you already installed)

install.packages("writexl")   # only needed once, ever, on your machine
library(writexl)              # run this every session, before using write_xlsx()

# we'll use writexl's write_xlsx() function in section 11 to save df_hp
# to an actual .xlsx file — a function we didn't write ourselves, just
# installed and called, exactly like say_hello() or classify_skill()
# from section 9, except someone else wrote it for us


# ---------------------------------------------------------------------------
# 11. SAVING THE DATA (writing df_hp out to a file on your computer)
# ---------------------------------------------------------------------------

## save as .csv — the default choice: base R, no library needed, opens anywhere (Excel, Python, Google Sheets, ...)
write.csv(df_hp, "harry_potter.csv", row.names = FALSE) # row.names = FALSE skips adding an extra "row number" column

## or save as .xlsx — needs the writexl library installed/loaded in section 10,
## but keeps Excel-specific formatting and is nicer to open directly in Excel
write_xlsx(df_hp, "harry_potter.xlsx")

## WHERE did that file actually go? it gets saved in your WORKING DIRECTORY
getwd() # prints the path to it

## .Rproj file
# if you open RStudio via a .Rproj file, the working directory is automatically
# set to the folder containing that .Rproj file. ALWAYS DO THAT — consistent, reproducible!

## you COULD save using an absolute path instead...
write.csv(df_hp, "/Users/vojtechzika/Courses/data-analytics/output/data/harry_potter.csv", row.names = FALSE)
# ...but this is NOT RECOMMENDED: it only works on MY computer, since nobody
# else has a folder at exactly that path — the opposite of reproducible

## instead, organize your project into subfolders — data files into a data
## folder, graphs into a figures folder, tables into a tables folder, etc. —
## and build the path with file.path(), whose first argument is a relative
## path from your working directory and second is the file name
write.csv(df_hp, file.path("output/data", "harry_potter.csv"), row.names = FALSE)

## because programmers are lazy, we automate what we can:
# save the path to a variable...
dir_dat <- "output/data"
# ...and create the folder if it doesn't exist yet
if (!dir.exists(dir_dat)) {
  dir.create(dir_dat, recursive = TRUE) # recursive = TRUE also creates any missing parent folders (e.g. "output")
}

# to avoid repeating this folder-creation step in every script, we'll set up
# ALL project directories once, in one place — 00_setup.R — and load it at
# the start of every script we write from here on:
# source("../00_setup.R")  "../" goes up one folder, e.g. from 0_introduction/ to scripts/, where 00_setup.R lives

# save the file...
write.csv(df_hp, file.path(dir_dat, "harry_potter.csv"), row.names = FALSE)
# ...and print a success message to the console
cat("Saved:", file.path(dir_dat, "harry_potter.csv"), "\n")

###
# EXERCISE: write a function that handles csv saving automatically
###

