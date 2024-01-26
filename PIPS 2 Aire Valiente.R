## Assignment 2.1 Solutions - 2024 Programming in Psychological Science (PIPS)
#
# Record of Revisions
#
# Date Programmer Descriptions of Change
# ==== ================ ======================
# 19-Jan-24 Aire Valiente Original code
# Q2.1.1 -----------------------------------------------------------------------
best_sport <- "voetbal"
if (best_sport is "voetbal") {
  print(TRUE)
}

#Answer
best_sport <- "voetbal"
if (best_sport == "voetbal") {
  print(TRUE)
}
# Q2.1.2 -----------------------------------------------------------------------
predicted <- "rain"
actual <- "rain"
salary <- 54023
if (predicted != actual) {
  salary <- salary
} else {
  salary <- salary + 2000
}
print(salary)

predicted <- "high wind"
actual <- "rain"
salary <- 54023
if (predicted != actual) {
  salary <- salary
} else {
  salary <- salary + 2000
}
print(salary)

#Answer a
#If I change "predicted" to "high wind", I get the salary ([1] 54023) without 
#the extra because the else block is executed as in this case predicted and
#actual are not the same anymore.

predicted <- "rain"
actual <- "Rain"
salary <- 54023
if (predicted != actual) {
  salary <- salary
} else {
  salary <- salary + 2000
}
print(salary)

#Answer b
#I get the salary ([1] 54023) without the extra again because R is sensitive to
#character size and it counts "Rain" as different from "rain"

# Q2.1.3 -----------------------------------------------------------------------
#proposed alternative in the question
predicted <- "rain"
actual <- "rain"
salary <- 54023
if (predicted == actual) {
  salary <- salary + 2000
}
print(salary)
#Answer: using the ifelse function
predicted <- "rain"
actual <- "rain"
salary <- 54023
salary <- ifelse(predicted == actual, salary + 2000, salary)
print(salary)

# Q2.1.4 -----------------------------------------------------------------------
predicted <- "rain"
actual <- "rain"
salary <- 54023
if (actual == "sun") {
  salary <- salary + 2000
  print("Birthday moved outside!")
} else if (predicted != actual) {
  salary <- salary
  print("Birthday RUINED! No raise for you!")
} else if (predicted == actual) {
  salary <- salary + 2000
  print("Birthday SUCCESS! You should get a raise!")
} else if (actual == "high wind") {
  salary <- salary
  print("Boss blew off his bike into a canal.")
}
print(salary)

#Answer a 
#As in this code the conditions are evaluated in order, "Boss blew off his bike 
#into a canal." will never get printed. In fact, one of the two: 
#  else if (predicted != actual) {
#  salary <- salary
#  print("Birthday RUINED! No raise for you!") 
#or
#  else if (predicted == actual) {
#  salary <- salary + 2000
#  print("Birthday SUCCESS! You should get a raise!")
#is always gonna be true. Predicted can only be either == or != than actual.

#Answer b
predicted <- "rain"
actual <- "high wind"
salary <- 54023

if (actual == "high wind") {
  salary <- salary
  print("Boss blew off his bike into a canal.")
} else if (actual == "sun") {
  salary <- salary + 2000
  print("Birthday moved outside!")
} else if (predicted != actual) {
  salary <- salary
  print("Birthday RUINED! No raise for you!")
} else if (predicted == actual) {
  salary <- salary + 2000
  print("Birthday SUCCESS! You should get a raise!")
}

print(salary)

# Q2.1.5 -----------------------------------------------------------------------
#Answer: No, the code checks whether all the values in these_numbers are less than 0.
#corrected:
these_numbers <- runif(100, min=-0.01, max=100)
they_are_negative <- FALSE
if (any(these_numbers < 1.1)) {
  they_are_negative <- TRUE
}

# Q2.1.6 -----------------------------------------------------------------------
#Answer
install.packages("lubridate")
library(lubridate)
now <- Sys.time()
current_hour <- hour(now)
current_minute <- minute(now)
start_hour <- 0  
start_minute <- 0
end_hour <- 5 
end_minute <- 0
if (
  (current_hour > start_hour || (current_hour == start_hour && current_minute >= start_minute)) &&
  (current_hour < end_hour || (current_hour == end_hour && current_minute <= end_minute))
) {
  print("Go to sleep!")
}

# Q2.1.7 -----------------------------------------------------------------------
install.packages("curl")
library(curl)
set.seed(13084232)
# See https://github.com/mdnunez/encodingN200
pdmdat <- curl("https://tinyurl.com/dataBayesCogMod")
pdm <- read.csv(pdmdat)
head(pdm)
#Answer
pdm$keep_or_exclude <- ifelse(pdm$RT > 2.5 | pdm$RT < 0.15, "outlier", "good")
table(pdm$keep_or_exclude)

# Q2.1.8 -----------------------------------------------------------------------
#Answer a
library(curl)
set.seed(13084232)
pdmdat <- curl("https://tinyurl.com/dataBayesCogMod")
pdm <- read.csv(pdmdat)
head(pdm)
pdm$keep_or_exclude <- ifelse(pdm$RT > 2.5 | pdm$RT < 0.15, "outlier", "good")
for (i in seq_along(pdm$keep_or_exclude)) { 
  if (pdm$keep_or_exclude[i] == "outlier")
    pdm$RT[i] <- NA
}
sum(is.na(pdm$RT))

#Answer b
library(curl)
set.seed(13084232)
pdmdat <- curl("https://tinyurl.com/dataBayesCogMod")
pdm <- read.csv(pdmdat)
head(pdm)
pdm$keep_or_exclude <- ifelse(pdm$RT > 2.5 | pdm$RT < 0.15, "outlier", "good")
pdm$RT[pdm$RT > 2.5 | pdm$RT < 0.15] <- NA

# Q2.1.9 ------------------------------------------------------------------------
#Answer
numeric_vec <- runif(4, min=0, max=100)
weighted_sum <- 0
for(i in numeric_vec){
  if (i %% 2 == 0) {
    weighted_sum <-(numeric_vec[i]*2) + weighted_sum
  } else {
    weighted_sum <- numeric_vec[i] + weighted_sum
  }
}
print(weighted_sum)

weighted_avg <- weighted_sum / (length(numeric_vec)*1.5)
print(weighted_avg)

# Q2.1.10 ------------------------------------------------------------------------
?rnorm
?replace
#Answer a
truncated <- rnorm(1, mean = 10, sd = 5)
for(i in truncated){
  if (i < 0) {
    truncated <- rnorm(1, mean = 10, sd = 5)
  } 
}

#Answer b
try_out <- rnorm(1000, mean = 10, sd = 5)
# I looked at a bigger rnorm and since it centeres itself around its mean, 
# and I am drawing only one random number, the chance of getting a negative value 
# is low because extreme values are less and less probale the further away you  
# move from the mean. Considering that in this case the mean is 10, negative values
# are highly unlikely. Hence, this code will most of the time result in positive 
# numbers.

# Q2.1.11 ------------------------------------------------------------------------
#Answer
truncated <- rnorm(1, mean = 10, sd = 5)
for(i in truncated){
  if (i < 0) {
    while(truncated [1] <  0){
    truncated <- rnorm(1, mean = 10, sd = 5)
    }
  } 
  print(truncated)
}

# The first code replaced the entire vector when any element is negative, 
# while the second code replaces only the first element of the vector as long as 
# it is negative. 

# Q2.1.12 ------------------------------------------------------------------------
#Answer
weird_fibonacci <- c(0, 3)
length <- 10
for (i in 3:length) {
  next_element <- ((weird_fibonacci[i-1]) + (weird_fibonacci[i - 2])) + 2 * weird_fibonacci[i-2] 
  weird_fibonacci <- c(weird_fibonacci, next_element)
}
print(weird_fibonacci)

# Q2.1.13 ------------------------------------------------------------------------
#Answer
dice <- c(0, 0, 0)

while (TRUE) {
  dice <- sample(1:6, 3, replace = TRUE)
  print(dice)
  if (all(dice == dice[1])) {
    break
  }
}

# Q2.1.14 ------------------------------------------------------------------------
experiment_colors <- c("blue", "green", "yellow", "red", "pink")
for (yellow in experiment_colors) {
  print(yellow)
}
# Answer a 
# Yellow in the for loop is supposed to identify each element of the variable 
# experiment_colors over which the loop has to iterate. In this case it is a bad 
# name because it only refers to yellow, when it should be referring to all colors.
# "colors" would be a better name. 

# Answer b 
experiment_colors <- c("blue", "green", "yellow", "red", "pink")
for (color in experiment_colors) {
  print(yellow)
}

# Q2.1.15 ------------------------------------------------------------------------
#Answer
rounds <- 0
wallet <- 0
while (wallet <= 3 & rounds < 300 & wallet >= - 200) {
  rounds <- rounds + 1
  dice <- sample(1:6, 3, replace = TRUE)
  cat('Dice roll number', rounds, '\n')
  print(wallet)
  if (all(dice == dice[1])) {
    wallet <- wallet + 30
  } else {
    wallet <- wallet - 1
  }
}

# Q2.1.16 ------------------------------------------------------------------------
#Answer
maximum <- numeric()
these_numbers <- c(345, 5728, 65, 8, 755)
for (i in 1:length(these_numbers)) {
  if (sum(these_numbers > these_numbers[i]) == 0) {
    maximum <- c(maximum, these_numbers[i])
  }
}
print(maximum)

# Q2.1.17 ------------------------------------------------------------------------
#Answer
library(lubridate)
end_year <- 2040
while (year(Sys.time()) < end_year) {
  print(Sys.time())
  Sys.sleep(30 * 60)
}

# Q2.1.18 ------------------------------------------------------------------------
verhulst <- function(x, rate_in, cap) rate_in*x*(cap-x)/cap
rabbits <- c(.001)
rate <- 2
capacity <- 1000
for (time in 2:50) rabbits[time] <- verhulst(rabbits[time-1], rate, capacity)
plot(rabbits, type='l', xlab='time', bty='n')

#Answer
verhulst <- function(x, rate_in, cap) {
  return(rate_in * x * (cap - x) / cap)
}

rabbits <- c(.001)
rate <- 2
capacity <- 1000

for (time in 2:50) {
  rabbits[time] <- verhulst(rabbits[time - 1], rate, capacity)
}

plot(rabbits, type='l', xlab='time', bty='n')
# Q2.1.19 ------------------------------------------------------------------------
#Answer
my_array <- array(dim = c(4,50,3))
for (i in 1:4) {
  for (j in 1:50) {
    for (k in 1:3) { 
      my_array[i,j,k] <- rnorm(n = 1, mean = j, sd = 1)
    }
  }
}
print(my_array)

# Q2.1.20 -----------------------------------------------------------------------
#Answer
min <- apply(my_array, MARGIN = 1, min(x, na.rm = TRUE))

# Q2.1.21 -----------------------------------------------------------------------
what_do <- switch(caught_fish,
                  large_trout = "eat",
                  small_trout = "return",
                  carp = ,
                  perch = "sell")
print(what_do)

?switch

#Answer a
#In order to answer this question I looked at ?switch and it says that "A warning 
#is signaled if no alternatives are provided, as this is usually a coding error."
#it also says that the value of one of the elements is NULL whenever no element 
#is selected. Hence, in this case, as there is no matching case to shoe, what_do
#would be NULL. 

#Answer b
#caught_fish should be the string "perch" so that what_do returns "sell". This is
#because the only string defined by "sell" in the switch function is perch.

# Q2.1.22 -----------------------------------------------------------------------
dr_brown <- 88
my_list <- list(1, "hello", TRUE, c(2, 3, 5)) #I added a list
mcfly <- function(my_list) {
  print("Great Scott! Erased from existence!")
  rm(list = my_list, pos = 1)
}

mcfly('mcfly')

#Answer 
#"Great Scott! Erased from existence!" gets printed in the console and the 
#remove function of the mcfly function removes the mcfly function from the global 
#environment.Running mcfly('mcfly') only removes the function mcfly from the 
#global environment, not the list. 

# Q2.1.23 -----------------------------------------------------------------------
?apply
#Answer
t_tests <- replicate(100, t.test(rbeta(10, shape1=2, shape2=2), mu=.5), simplify = FALSE)
p_values <- sapply(t_tests, function(test) test$p.value, simplify = TRUE)
print(p_values)

# Q2.1.24 -----------------------------------------------------------------------
back_count <- function(number) {
  if (length(number) > 1) {
    stop("`number` must be of length 1" )
  }
  if (mode(number) != "numeric") {
    stop("`number` must be numeric." )
  }
  if (number > 0) {
    while (number > 0) {
      if (number == 4) {
        print("Unlucky!")
        break
      }
      print(number)
      number <- number - 1
    }
  } else {
    while (number < 0) {
      print(number)
      number <- number + 1
    }
  }
  return(number)
}
back_count(10)

#Answer
#The back_count function  counts back to 0. If the number you input is greater 
#than 1, then it stops and an error is printed. The same happens if the mode of 
#the number is not numeric. If number is positive, it enters a while loop and 
#prints the value of number until it reaches 0. Whenever the condition 
#if (number == 4) is satisfied it prints "Unlucky!" and the break stops the 
#entire while loop that is then exited. Then break lets the next thing in line 
#after the loop run (which in this case is return number).

# Q2.1.25 -----------------------------------------------------------------------
?round
#Answer
back_count <- function(number) { 
  if (length(number) > 1) {
    stop("`number`⁠ must be of length 1" ) 
  }
  if (mode(number) != "numeric") { 
    stop("`number`⁠ must be numeric." )
  }
  if (number != round(number)) {
    stop("`number` must be round.")
  }
  if (number > 0) {
    while (number > 0) { 
      if (number == 4) {
        print("Unlucky!")
      break
    }
      print(number)
      number <- number - 1
    }
  } else {
    while (number < 0) { 
      print(number)
      number <- number + 1
    }
  } 
  return(number)
}

myBC <- back_count(10) 

# In order to make the back_count function return an error if number is not an 
# integer, I added the following if statement:
#   if (number != round(number)) {
#     stop("`number` must be round.")}
# Furthermore, when I assign the return value to a variable, it doesn't print the 
# value by default. The assignment itself doesn't trigger the printing of the value.

# Q2.1.26 -----------------------------------------------------------------------
?debug
#Answer
adding <- function(a, b) {
  result <- a + b
  return(result)
}

example <- adding(2,4)
debug(adding)
result <- adding(5, 2)

# When I run the function with an example, a new environment opens where I can 
# inspect each variable and a Traceback window. I need to click on Stop button to
# exist this debug environment. 

# Q2.1.27 -----------------------------------------------------------------------
#Answer
special <- function(x) {
  unique_values <- c()
  for (value in x) {
    if (!(value %in% unique_values)) {
      unique_values <- c(unique_values, value)
    }
  }
  if (!is.vector(x)) {
    stop("Input must be a vector.")
  }
  if (length(x) == length(unique(x))) {
    warning("All values are special!")
  }
  return(unique_values)
}

my_vector <- c(1, 2, 3, 2, 4, 1, 5, 6, 4)
result <- special(my_vector)
print(result)

# Q2.1.28 -----------------------------------------------------------------------
#Answer
input_vector <- c(2, 4, 3, 2, 8)
quicksort <- function(input_vector) {
  max_value <- max(input_vector)
  min_value <- min(input_vector)
  print("I tried my best to quicksort!")
  input_vector <- c(max_value, input_vector, min_value)
  return(input_vector)
}

result_vector <- quicksort(input_vector)
result_vector

# Q2.1.29 -----------------------------------------------------------------------
grass <- "green"
colorit <- function(color_me, grass_me) {
  grass_me <- grass
  color_me <- "blue"
  grass <- "blue"
  colorful_items = c(color_me, grass_me)
  return(colorful_items)
}
print(grass)

#Answer a 
# A global variable grass with value "green" is created.
# A function colorit with two parameters, color_me and grass_me, is also created.
# grass_me is assigned the value of the global variable grass; color_me is 
# assigned the local value "blue". 
# A vector colorful_items is also created, containing the local variables 
# color_me and grass_me. The function returns the vector colorful_items. 

#Answer b
# After running the code grass is still "green". 
# A local variable grass is assigned the value "blue", but this doesn't affect 
# the global variable, because If you want to modify the global variable
#  within the function, you need to use <<-

# Q2.1.30 -----------------------------------------------------------------------
#Answer
rm(list=c("colorit","grass"))
source("/Users/airevaliente/Desktop/PIPS/week 2/assignment 2/colorit.R")
try_the_function_out <- colorit(color_me, grass_me)

#Answer a
# The function does not work. It gives: Error in colorit(color_me, grass_me : 
# object 'grass' not found
# This is because a function relies on a global variable (grass in this case)
# which is not defined within the function (which has only the local variable
# grass <- "blue").

#Answer b
# The new code in the other R script should be: 
colorit <- function(color_me, grass_me) {
  grass <- "green"  
  {
    grass_me <- grass
    color_me <- "blue"
    grass <- "blue"
    colorful_items = c(color_me, grass_me)
    return(colorful_items)
  }
}

# where a grass <- green global variable gets created first 

# Q2.1.31 -----------------------------------------------------------------------
#Answer
sum <- function(k) {
  if (k == 0) {
    return(0)
  } else {
    return(k + sum(k - 1))
  }
}
result <- sum(5)
result

# When k is 0, the function returns 0. Otherwise, it returns the sum of k and 
# the result of calling itself with k - 1. The recursion continues until it 
# reaches k == 0.

# Q2.1.32 -----------------------------------------------------------------------
?cos
#Answer
triangle <- function(a, b) {
  c <- sqrt(a^2 + b^2)
  angle_a <- atan(b/a) * (180/pi)
  angle_b <- 90
  angle_c <- 90 - angle_a
  result <- list(
    side = c,
    angle_a = angle_a,
    angle_b = angle_b,
    angle_c = angle_c
  )
  return(result)
}
my_triangle <- triangle(3, 4)
print(my_triangle)
