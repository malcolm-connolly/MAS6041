
parts <- function(n){
    
}

# a = [0 for i in range(n + 1)]
# k = 1
# y = n - 1
# while k != 0:
#   x = a[k - 1] + 1
# k -= 1
# while 2 * x <= y:
#   a[k] = x
# y -= x
# k += 1
# l = k + 1
# while x <= y:
#   a[k] = x
# a[l] = y
# yield a[:k + 2]
# x += 1
# y -= 1
# a[k] = x + y
# y = x + y - 1
# yield a[:k + 1]

vec <- numeric(0)
for(i in 1:3) vec <- c(vec, 1)
vec

tail(1:4,-1)

#increase the first value and take off 1


seq2 <- c(seq[1]+1,tail(seq,-2))

#https://colab.research.google.com/notebook#create=true&language=r
#https://colab.to/r

seq3 <- c(seq2[1],c(head(seq2,-1)))

#increment the first value of a list
inc <- function(a){
  a_inc <- c(a[1]+1,tail(a,-2))
}


(inc(seq))

#recursively increment
# rec_inc <- function(a){
#   if (    ) {
#     tail(a,-1) <- inc(tail(a,-1))
#   }
# }

a_1 <- rep(1,7)
a_2 <- inc(a_1)
a_3 <- c(a_2[1],inc(tail(a_2,-1)))
a_4 <- c(a_3[1:2], inc(tail(a_3,-2)))
#check?

# a[[i]] <- c(a[[i]][1:i],inc(tail(a[[i]],-i)))
# while ? 
a_5 <- inc(a_2)
a_6 <- c(a_5[1],inc(tail(a_5,-1)))
a_7 <- c(a_6[1:2], inc(tail(a_6,-2)))
#check?
a_8 <- inc(a_5)
a_9 <- c(a_8[1],inc(tail(a_8,-1)))

a_10 <- c(a_9[1], inc(tail(a_9,-1)))
a_11 <- inc(a_8)
a_12 <- c(a_11[1], inc(tail(a_11,-1)))

a_13 <- inc(a_11)
a_14 <- inc(a_13)



#vec <- numeric(0)
#for(i in 1:n) vec <- c(vec, i)

a <- list(a_1,a_2,a_3,a_4,a_5,a_6,a_7,a_8,a_9,a_10,a_11,a_12,a_13,a_14)
a[[2]]
a[[3]]
a[[4]]

s <- a[[2]]
 
lst <- list()
lst[[1]] <- a_1


for(i in 1:(length(lst[[1]])%/%2)){
  if(i == 1){
    lst[[i+1]] <- inc(lst[[1]])
  }else{
    lst[[i+1]] <- c(lst[[i]][1:(i-1)],inc(tail(lst[[i]],-(i-1))))
  }
}




lst[[1]]
lst[[2]]
lst[[3]]
lst[[4]]
  
c(lst[[1]][1:1],inc(tail(lst[[i]],-i)))

n<-7
lst[[5]] <- c(3,rep(1,n-3))

k <- 0
for(i in 1:((n-k*3) %/% 2)){
  if(i == 1){
    lst[[4+i]] <- c(3,rep(1,n-3))
  }else{
    lst[[4+i]] <- c(lst[[4+i-1]][1:(i-1)],inc(tail(lst[[4+i-1]],-(i-1))))
  }
}



lst[[5]]
lst[[6]]
lst[[7]]

k <- 1
for(i in 1:((n-k*3) %/% 2)){
  if(i == 1 && k == 1){
    lst[[4+i+k]] <- c(rep(3,2),rep(1,n-2*3))
  }else{
    lst[[4+i+k]] <- c(lst[[4+i+k-1]][1:(i-1)],inc(tail(lst[[4+i+k-1]],-(i-1))))
  }
}

lst[[5]]
lst[[6]]
lst[[7]]

for(i in 1:(length(lst[[1]])%/%2)){
  if(i == 1){
    lst[[i+1]] <- inc(lst[[1]])
  }else{
    lst[[i+1]] <- c(lst[[i]][1:(i-1)],inc(tail(lst[[i]],-(i-1))))
  }
}



part_list <- function(N){
  L <- list()
  L[[1]] <- rep(1,N) #the first one is just 1,1,...,1
  for(j in 2:N){ #j is the largest part of the partition
    for(k in 1:(N%/%j)){ #k is the number of j's in the partition
      
      }
    }
  }
  L
}


