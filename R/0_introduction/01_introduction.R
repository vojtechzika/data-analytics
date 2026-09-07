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
# 1. R AS A CALCULATOR
# ---------------------------------------------------------
# You can just type math and run it — R evaluates it immediately.

2 + 2
10 - 3
6 * 7
20 / 4
2^3        # exponent (2 to the power of 3)
17 %% 5    # modulo — remainder of 17 / 5
17 %/% 5   # integer division

# Order of operations (PEMDAS) works as expected:
(2 + 3) * 4


# ---------------------------------------------------------
# 2. VARIABLES (storing values)
# ---------------------------------------------------------
# Use <- to assign a value to a name (= also works, but <- is the R convention)

x <- 5
y <- 10
x + y
x - y
x * y
x / y
x^y

# Once assigned, a variable stays in memory until you overwrite or remove it
x <- x + 1   # reassign x to its old value + 1
x

# See everything currently stored in your environment
ls()

# <- vs = : two ways to write assignment, but they are NOT used the same way.

# At the TOP LEVEL of a script (i.e. not inside a function call), both create
# a variable, and they do the exact same thing:
a <- 5
b = 5
a   # 5
b   # 5

# Even so, R users almost always use <- here. That's because = has a
# different, specific job elsewhere — see below — so using it for plain
# assignment can make code confusing to read.

# INSIDE A FUNCTION CALL, = does something completely different: it names
# an argument. It is NOT assignment, and it does NOT create a variable.
round(pi, digits = 2)   # digits = 2 tells round() which input gets the value 2

# Proof that no variable was created: exists() checks whether a name exists
# in your environment. It's FALSE, because "digits" was only ever used as
# an argument label inside that one function call, not stored anywhere.
exists("digits")   # FALSE

# Bottom line:
# <- : always means assignment.
# =  : means assignment only at the top level (rare in practice) —
#      inside a function call, it means "here is a named argument."


# ---------------------------------------------------------
# 3. BASIC DATA TYPES
# ---------------------------------------------------------

num_val   <- 3.14          # numeric (decimal or whole numbers)
int_val   <- 3L             # integer (the L marks it as an integer, not numeric)
char_val  <- "hello world"  # character (text) — always use quotes
logic_val <- TRUE           # logical (TRUE / FALSE) — no quotes, all caps
na_val    <- NA             # NA = "Not Available" — R's marker for a missing value

# Check the type of anything with class() or typeof()
class(num_val)
class(char_val)
class(logic_val)
class(na_val)     # NA is logical by default, but it can "blend into" any type

# NA is NOT the same as 0, "", or FALSE — it means "unknown."
# That's why any calculation involving NA also becomes NA:
5 + NA

## to check that a variable equals some particular value, use the == operator
num_val == 3.14
char_val  == "HELLO WORLD"   # FALSE — == is case-sensitive! "hello world" != "HELLO WORLD"

# however, this does not work with NA
na_val == NA   # returns NA (not an error!) — see why below

# To check if something is missing, use is.na() — NEVER use == NA (it won't work!)
is.na(na_val)

# == vs identical(): they are NOT the same thing.
# == compares VALUES (converting types if needed) and can return NA.
# identical() compares the WHOLE object — value AND type — and always
# returns a single TRUE or FALSE, never NA. That makes it the safer
# choice when you need one definite answer (e.g. inside an if statement).
NA == NA                      # NA   — "is an unknown equal to an unknown?" is unknowable
identical(NA, NA)              # TRUE — identical() just checks: are these exactly the same? Yes.

1 == 1L                        # TRUE  — a double and an integer, but == coerces to compare
identical(1, 1L)                 # FALSE — same value, but different types, so not identical

# TRUE/FALSE vs 1L/0L: related, but not the same type
class(TRUE)   # "logical"
class(1L)     # "integer"
TRUE == 1     # TRUE — R silently converts logical to numeric to compare them
identical(TRUE, 1L)   # FALSE — same VALUE once converted, but different TYPES


# ---------------------------------------------------------
# 4. CONVERTING BETWEEN TYPES (very useful later on!)
# ---------------------------------------------------------
# Real datasets often load a numeric-looking column as text (e.g. from a CSV
# with stray symbols, or a column that got quoted). R won't let you do math
# on characters, so you need to explicitly convert — this comes up A LOT.

age_char <- "25"        # this is text, not a number
class(age_char)          # "character"

# age_char + 1           # <- this would throw an error, uncomment to see

age_num <- as.numeric(age_char)   # convert character -> numeric
class(age_num)
age_num + 1                        # now math works

# The same idea works the other direction, and with other types:
as.character(25)     # numeric -> character
as.logical("TRUE")   # character -> logical
as.integer(3.9)       # numeric -> integer (this TRUNCATES, it doesn't round! -> 3)

# It also works on a whole vector at once (this is the version you'll use most).
# c() "combines" values into a vector — more on this in the vectors section below.
messy_ages <- c("21", "22", "19", "25", "23")   # imagine this came from a CSV
class(messy_ages)                                 # "character" — can't do mean() on this yet

clean_ages <- as.numeric(messy_ages)
class(clean_ages)
mean(clean_ages)     # now it works

# WATCH OUT: if a value genuinely isn't a valid number, R converts it to NA
# and gives you a warning rather than crashing — always check for this!
tricky <- c("21", "22", "missing", "25")
as.numeric(tricky)   # "missing" becomes NA, with a warning


# ---------------------------------------------------------
# 5. print() vs cat()
# ---------------------------------------------------------
# print() shows R's internal representation of an object (quotes, [1] index, etc.)
# cat()   just writes plain text, no extra formatting — good for messages/labels

print("hello world")
cat("hello world")

print(42)
cat(42)

# cat() glues multiple pieces together, separated by a space by default
cat("x is currently:", x, "\n")   # "\n" = line break, so the next output starts fresh

# paste0() is different: instead of printing, it BUILDS a character string
# and hands it back to you as a value, which you can store in a variable
# (cat() never does this — it only writes to the console and returns nothing useful)
x_val <- paste0("x is currently:", x)
x_val

# Most of the time, just typing an object's name auto-prints it (same as print())
x


# ---------------------------------------------------------
# 6. VECTORS (a sequence of values of the same type)
# ---------------------------------------------------------
# Vectors are the most basic and most-used structure in R.
# Build one with c() — "combine"

ages <- c(21, 22, 19, 25, 23)
names_vec <- c("Ana", "Ben", "Chi", "Dan", "Eli")
passed <- c(TRUE, TRUE, FALSE, TRUE, FALSE)   # logical vector
passed_na <- c(TRUE, TRUE, NA, TRUE, FALSE)   # logical vector with an NA value

ages
names_vec

# Useful functions that work on vectors:
length(ages)     # how many elements
sum(ages)        # add them all up
mean(ages)       # average
min(ages)
max(ages)
sort(ages)

# you can also sum/average logical vectors directly
sum(passed)
mean(passed)

# what if you run operations on a vector that contains NA?
mean(passed_na)                 # this returns NA...
mean(passed_na, na.rm = TRUE)   # ...unless you tell R to ignore (remove) the NA first

# == vs identical() on vectors:
passed == passed_na            # TRUE TRUE NA TRUE TRUE — compares element by element, returns a vector
identical(passed, passed_na)   # FALSE — one single answer for the whole object

# Access elements by position (R starts counting at 1, not 0!)
ages[1]      # first element
ages[2:4]    # elements 2 through 4
ages[-1]     # everything EXCEPT the first element

# Vectorized math — operations apply to every element at once, no loop needed
ages + 1
ages * 2


# ---------------------------------------------------------
# 7. LOGICAL COMPARISONS AND FILTERING
# ---------------------------------------------------------

ages > 21          # returns TRUE/FALSE for each element
ages[ages > 21]     # returns only the values where the condition is TRUE

# logical operators
larger_than       <- ages[ages > 21]
smaller_than      <- ages[ages < 21]
equal_to          <- ages[ages == 21]
other_than        <- ages[ages != 21]
larger_or_equal   <- ages[ages >= 21]
smaller_or_equal  <- ages[ages <= 21]

length(smaller_or_equal)


# ---------------------------------------------------------
# 8. CONDITIONS (if / else)
# ---------------------------------------------------------
# A condition lets your code make a decision: run one block if something
# is TRUE, and (optionally) a different block if it's FALSE.

grade <- 65

if (grade >= 60) {
  print("Pass")
} else {
  print("Fail")
}

# You can chain multiple conditions with else if:
grade <- 85

if (grade >= 90) {
  print("A")
} else if (grade >= 80) {
  print("B")
} else if (grade >= 60) {
  print("C or better")
} else {
  print("Fail")
}

# if/else works on a SINGLE value at a time (not a whole vector).
# For a vector, R has a vectorized shortcut: ifelse()
grades <- c(55, 91, 72, 88, 40)
ifelse(grades >= 60, "Pass", "Fail")   # checks every element at once, no loop needed


# ---------------------------------------------------------
# 9. SEQUENCES AND REPEATS
# ---------------------------------------------------------

1:10                    # a quick sequence from 1 to 10
seq(0, 100, by = 10)    # sequence from 0 to 100, counting by 10
rep("A", times = 5)     # repeat "A" five times


# ---------------------------------------------------------
# 10. LOOPS (for)
# ---------------------------------------------------------
# A loop repeats the same block of code once for each element of a sequence.
# for (variable in sequence) { ...do something with variable... }

for (i in 1:5) {
  print(i)
}

# Looping over an actual vector, not just numbers:
for (student_name in names_vec) {
  cat("Hello,", student_name, "\n")
}

# A common pattern: loop over a vector and do something with each value
for (a in ages) {
  if (a >= 21) {
    cat(a, "is an adult\n")
  } else {
    cat(a, "is a minor\n")
  }
}

# Looping over TWO vectors together: loop by POSITION (index) instead of
# by value, then use that index to pull the matching item from each vector.
# seq_along(x) generates 1, 2, 3, ... up to the length of x.
grades <- c(55, 91, 72, 88, 40)   # lines up with names_vec, position by position

for (i in seq_along(names_vec)) {
  if (grades[i] >= 60) {
    cat("Good job,", names_vec[i], "!\n")
  } else {
    cat("Try harder next time,", names_vec[i], "!\n")
  }
}

# NOTE: in R, you often DON'T need a loop for things like this — vectorized
# code (like ifelse() above, or ages[ages >= 21]) does the same job faster
# and with less code. Loops are important to understand, but in R they are
# usually a last resort, not the first tool you reach for.


# ---------------------------------------------------------
# 11. WRITING YOUR OWN FUNCTION
# ---------------------------------------------------------
# You'll eventually want to package up repeated steps into a function.

square_it <- function(n) {
  result <- n^2
  return(result)
}

square_it(4)
square_it(ages)   # works on a whole vector at once, too


# ---------------------------------------------------------
# 12. A QUICK LOOK AT DATA FRAMES (tables — where we're headed next)
# ---------------------------------------------------------
# A data frame is a table: rows = observations, columns = variables.
# Under the hood, it's just a collection of vectors of equal length.

students <- data.frame(
  name = names_vec,
  age  = ages,
  passed = passed
)

students          # view the whole table
head(students)    # view just the first few rows
str(students)     # see the structure: columns, types, sample values
nrow(students)    # number of rows
ncol(students)    # number of columns
students$age      # access a single column with $


# ---------------------------------------------------------
# 13. GETTING HELP
# ---------------------------------------------------------
# Whenever you don't know what a function does, ask R directly:

?mean          # opens the help page for mean()
# help(mean)   # same thing, alternate syntax


# ---------------------------------------------------------
# 14. SAVING YOUR DATA (CSV and Excel)
# ---------------------------------------------------------
# Once you've built or cleaned a data frame, you'll usually want to save it
# to a file so you (or someone else) can open it again later.

# Step 1: find out where R is currently working. Every path you write
# without a leading "/" is relative to this folder, so it's worth
# checking before you try to save anything.
getwd()
wd <- getwd()

# Step 2: build the path to a data/clean subfolder INSIDE that working
# directory, using paste0() — a nice callback to what we saw in the
# print() vs cat() section. Keeping the save location in one variable
# means you only have to change it in one place, not in every write call.
data_path <- paste0(wd, "/data/clean/")
data_path

# That subfolder needs to actually exist before you can save into it —
# R will error with "cannot open file" otherwise. dir.create() makes it
# for you if it's missing, and does nothing (safely) if it already exists:
dir.create(data_path, recursive = TRUE, showWarnings = FALSE)

# CSV is built into base R — no extra package needed:
write.csv(students, paste0(data_path, "students.csv"), row.names = FALSE)
# row.names = FALSE stops R from adding an extra "1, 2, 3..." column of
# row numbers to the file — usually not something you want to keep.

# Excel (.xlsx) needs an extra package, since it's not a plain-text format.
# writexl is a good beginner choice: lightweight, and unlike some
# alternatives it doesn't require a separate Java installation.
# install.packages("writexl")   # only needed once, ever, on your machine
library(writexl)
write_xlsx(students, paste0(data_path, "students.xlsx"))

# Note: modern R packages (and Excel itself) work with .xlsx, not the older
# .xls format — .xls is a legacy binary format from pre-2007 Excel, and
# writing it directly from R is rarely worth the extra hassle nowadays.

# Not sure what your current working directory is (relevant if you use a
# relative path instead)? Check with:
getwd()

## =========================================================
## End of script — next up: exploring real data frames in depth
## =========================================================