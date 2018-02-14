library(purrr)
library(microbenchmark)


Factorial_loop <- function(n) {
  result = n
  if (n == 0) {
    return(1)
  } else if (n > 0) {
    for(i in n:2) {
      result = result * (i-1)
    }
    return(result)
  } else {
    return(0)
  }
}


Factorial_reduce <- function(n) {
  if (n == 0) {
    return(1)
  } else if (n > 0) {
    result = reduce(n:1, function(x, y) {
      x * y
    })
    return(result)
  } else {
    return(0)
  }
}


Factorial_func <- function(n) {
  if (n < 0) {
    return(0)
  }
  
  if (n == 0) {
    return(1)
  }
  
  if (n > 0) {
    return(n * Factorial_func(n-1))
  }
}


mem <- function() {
  
  cache <- 1
  
  Factorial_mem <- function(n) {
    
    if (n < 0) {
      return(0)
    }
    
    if (n == 0) {
      return(1)
    }
    
    if (length(cache) < n){
      cache <<- `length<-`(cache, n)
    }
    
    if (n > 1) {
      cache[n] <<- n * factorial(n-1)
      cache[n]
    }
  }
  Factorial_mem
}

Factorial_mem <- mem()

compare_result5 <- microbenchmark(
  Factorial_loop(5),
  Factorial_reduce(5),
  Factorial_func(5), 
  Factorial_mem(5)
)

compare_result50 <- microbenchmark(
  Factorial_loop(50),
  Factorial_reduce(50),
  Factorial_func(50), 
  Factorial_mem(50)
)


compare_result100 <- microbenchmark(
  Factorial_loop(100),
  Factorial_reduce(100),
  Factorial_func(100), 
  Factorial_mem(100)
)

compare_result5 <- summary(compare_result5)
compare_result50 <- summary(compare_result50)
compare_result100 <- summary(compare_result100)

result_file <- file("~/Downloads/factorial_output.txt", "w")
write.table(compare_result5, result_file, row.names = TRUE, col.names = TRUE)
write.table(compare_result50, result_file, row.names = TRUE, col.names = TRUE)
write.table(compare_result100, result_file, row.names = TRUE, col.names = TRUE)
close(result_file)





