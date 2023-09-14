printAllUniqueParts <- function(n){
  L <- list()
  p <- vector(mode="numeric",n)
  k <- 1
  p[1] <- n
  
  while(TRUE){
    L <- c(L, list(p[1:k]))
    rem_val <- 0
    while((k >= 1)&&(p[k]==1)){
      rem_val <- rem_val + p[k]
      k <- k - 1
    }
    if(k<1){
      break
    }
    p[k] <- p[k] - 1
    rem_val <- rem_val + 1
    while(rem_val > p[k]){
      p[k + 1] <- p[k]
      rem_val <- rem_val - p[k]
      k <- k + 1
    }
    p[k + 1] <- rem_val
    k <- k + 1
  }
  return(L)
}
L <- printAllUniqueParts(5)
L <- L[length(L):1]

L <- printAllUniqueParts(10)
L <- L[length(L):1]

L[c(12,19,21,30,32,35,36,38,42)]
#### What's the point of that again?
RevLexParts <- function(n){
  L <- list()
  p <- vector(mode="numeric",n)
  k <- 1
  p[1] <- rep(1,n)
  
  while(TRUE){
    L <- c(L, list(p[1:k]))
    rem_val <- n
    # while((k >= 1)&&(p[k]==1)){
    #   rem_val <- rem_val + p[k]
    #   k <- k - 1
    # }
    # if(k<1){
    #   break
    # }
    p[k] <- p[k] + 1
    rem_val <- rem_val - 1
    # while(rem_val > p[k]){
    #   p[k + 1] <- p[k]
    #   rem_val <- rem_val - p[k]
    #   k <- k + 1
    # }
    p[k + 1] <- rem_val
    k <- k + 1
  }
  return(L)
}

#### OK so now need a list of all standard tableaux for list item of L









