library(dplyr)
mat <- matrix(1:50, nrow = 10, ncol = 5)
str(mat)
for (i in mat) {
  print(rowSums(mat))
}
zero <- 0
for(i in mat) {
  if(i %% 5 == 0) {
    zero = zero+1 {
      print(zero)
    } }
}

sum(mat[5:20]^2) 
prices <- c(12.43, 9.99, 18.22, 7.25, 0.50)
for (i in prices) {
  if (i<10) {
  print(sum(i)) ## ?? non function sum
  }
}
sum(prices[prices<10])

function1 <- function(a,b,c) {
  print(a+b+c)
  print(a*b/2)
}
function1(1,4,3)
function2 <- function(a,b) {
  print(matrix(, nrow = a, ncol = b))
}
function2(4,2)
function3 <- function(a,b) { ## non functional
  for(i in 1:a)
    print(if(a > b & a %% b != 0)
}

function3(100,17)
function4 <- function(a) { ## non functional :(
  for(i in a) {
    print(case_when(
    i > 10 ~ i^2,
    i < 100 ~ i^2,
    TRUE ~ "out of range"
  )
    )
  }
}
function4(11)

fun5 <- function(a) {
  round(a, digits = 2)
}
fun5(14.568)

rev <- c("a", "c", "x", "d", "s", "d", "c", "b", "v", "v", "a", "r", "k")
fun6 <- function(rev) {
  unique(rev, incomparables = FALSE)
}
fun6(rev)
vec <- runif(7, min = 0, max = 100)
fun7 <- function(a,b,c) {
  runif(a, min = b, max = c) {
  print(summary(vec))
  }
}
