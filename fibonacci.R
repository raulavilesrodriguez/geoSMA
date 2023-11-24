n <- 8
fib <- function(n){
  # case base
  if(n == 0){
    0;
  }
  else if(n == 1){
    1;
  }
  else{
    fib(n-1) + fib(n-2)
  }
}

result <- fib(n)
result



