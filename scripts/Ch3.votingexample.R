library(permutations)
install.packages("permutations")

S4 <- as.word(allperms(4))


#print_word(S4)

s<-  lapply(seq_len(24), function(i) unclass((S4))[i,])

invS4 <- inverse(S4)


sum(f)


f<- c(7,8,17,15,20,20,1,1,0,1,0,1,8,15,1,0,1,4,35,25,30,10,10,10)

#f[c(1,2,3,5,4,6,7,8,13,19,14,20,9,11,15,21,17,23,10,12,16,22,18,24)]



types4 <- shape(S4)

for(i in 1:24){
  types4[i] <- lapply(types4[i], sort, decreasing = TRUE)
}

### Making X
outer <-function(x = permutation(),y = permutation(),i=integer()){
  x*y[i]
} 

X <- list()
for(i in 1:24){
  X[[i]] <- outer(invS4,S4,i)
  #X[[i]] <- lapply(seq_len(24), function(j) unclass(X[[i]])[j,])
}

for(i in 1:24){
  X[[i]] <- lapply(seq_len(24), function(j) unclass(X[[i]])[j,])
}

### Writing in all the characters


c.4     <- c(1,1,1,1,1)
c.31    <- c(3,1,-1,0,-1)
c.22    <- c(2,0,2,-1,0)
c.211   <- c(3,-1,-1,0,1)
c.1111  <- c( 1,-1,1,1,-1)


chi.4 <- vector(length = 24)
chi.4[which(sapply(types4, '[',1) == 1 & is.na(sapply(types4, '[',2)) )] <- c.4[1]
chi.4[which(sapply(types4, '[',1) == 2 & is.na(sapply(types4, '[',2)) )] <- c.4[2]
chi.4[which(sapply(types4, '[',1) == 2 & sapply(types4, '[',2) == 2)] <- c.4[3]
chi.4[which(sapply(types4, '[',1) == 3 & is.na(sapply(types4, '[',2)) )] <- c.4[4]
chi.4[which(sapply(types4, '[',1) == 4 )] <- c.4[5]


chi.31 <- vector(length = 24)
chi.31[which(sapply(types4, '[',1) == 1 & is.na(sapply(types4, '[',2)) )] <- c.31[1]
chi.31[which(sapply(types4, '[',1) == 2 & is.na(sapply(types4, '[',2)) )] <- c.31[2]
chi.31[which(sapply(types4, '[',1) == 2 & sapply(types4, '[',2) == 2)] <- c.31[3]
chi.31[which(sapply(types4, '[',1) == 3 & is.na(sapply(types4, '[',2)) )] <- c.31[4]
chi.31[which(sapply(types4, '[',1) == 4 )] <- c.31[5]

chi.22 <- vector(length = 24)
chi.22[which(sapply(types4, '[',1) == 1 & is.na(sapply(types4, '[',2)) )] <- c.22[1]
chi.22[which(sapply(types4, '[',1) == 2 & is.na(sapply(types4, '[',2)) )] <- c.22[2]
chi.22[which(sapply(types4, '[',1) == 2 & sapply(types4, '[',2) == 2)] <- c.22[3]
chi.22[which(sapply(types4, '[',1) == 3 & is.na(sapply(types4, '[',2)) )] <- c.22[4]
chi.22[which(sapply(types4, '[',1) == 4 )] <- c.22[5]

chi.211 <- vector(length = 24)
chi.211[which(sapply(types4, '[',1) == 1 & is.na(sapply(types4, '[',2)) )] <- c.211[1]
chi.211[which(sapply(types4, '[',1) == 2 & is.na(sapply(types4, '[',2)) )] <- c.211[2]
chi.211[which(sapply(types4, '[',1) == 2 & sapply(types4, '[',2) == 2)] <- c.211[3]
chi.211[which(sapply(types4, '[',1) == 3 & is.na(sapply(types4, '[',2)) )] <- c.211[4]
chi.211[which(sapply(types4, '[',1) == 4 )] <- c.211[5]

chi.1111 <- vector(length = 24)
chi.1111[which(sapply(types4, '[',1) == 1 & is.na(sapply(types4, '[',2)) )] <- c.1111[1]
chi.1111[which(sapply(types4, '[',1) == 2 & is.na(sapply(types4, '[',2)) )] <- c.1111[2]
chi.1111[which(sapply(types4, '[',1) == 2 & sapply(types4, '[',2) == 2)] <- c.1111[3]
chi.1111[which(sapply(types4, '[',1) == 3 & is.na(sapply(types4, '[',2)) )] <- c.1111[4]
chi.1111[which(sapply(types4, '[',1) == 4 )] <- c.1111[5]


##### projections

f_4 <- vector(length = 24)

for(i in 1:24){
  f_4[i] <- (chi.4 %*% f[match(X[[i]],s)])*chi.4[1]/24
}
(f_4 %*% f_4 /24)

#91.84028

f_31 <- vector(length = 24)

for(i in 1:24){
  f_31[i] <- (chi.31 %*% f[match(X[[i]],s)])*chi.31[1]/24
}
(f_31 %*% f_31 /24)
# 76.5625

f_22 <- vector(length = 24)

for(i in 1:24){
  f_22[i] <- (chi.22 %*% f[match(X[[i]],s)])*chi.22[1]/24
}
(f_22 %*% f_22 /24)

#17.36111

f_211 <- vector(length = 24)
for(i in 1:24){
  f_211[i] <- (chi.211 %*% f[match(X[[i]],s)])*chi.211[1]/24
}
(f_211 %*% f_211 /24)

#2.541667

f_1111 <- vector(length = 24)
for(i in 1:24){
  f_1111[i] <- (chi.1111 %*% f[match(X[[i]],s)])*chi.1111[1]/24
}
(f_1111 %*% f_1111 /24)
#0.1111111

# 4   
# 31  
# 22  
# 211 
# 1111

#
dims <- c(1,3,2,3,1)
dims^2*240/(24^2) #he just compares the means...

#####
library(dplyr)
library(combinat)
pairs <- combinat::combn(c(1,2,3,4),2) %>% t()
pairs[,1:2] <- sapply(pairs[,1:2], as.integer)


matchlist <- list(length=4)
for(i in 1:4){
  matchlist[[i]] <- rep(0,4)
}
Effects_2 <- matrix(nrow=6,ncol=6)

i<-1
j<-5
k<-2
l<-2
#ctr <- 0
for(i in 1:nrow(pairs)){
  items <- pairs[i,]
  others <- setdiff(c(1,2,3,4),items)
  for(j in 1:nrow(pairs)){
    ranklist <- pairs[j,] %>% permn()
    fillerlist <- setdiff(c(1,2,3,4),pairs[j,]) %>% permn()
    ctr <- 0
    delta <- rep(0, factorial(4))
    for(k in 1:2){
      ctr <- ctr + 1
      for(l in 1:2){
        matchlist[[ctr]][items[1:2]] <- ranklist[[k]][1:2]
        matchlist[[ctr]][others[1:2]] <- fillerlist[[l]][1:2]
        matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
        ctr <- ctr + 1
      }
      ctr <- ctr - 1
    }
    delta[match(matchlist,s)] <- 1
    Effects_2[i,j] <- delta %*% f_22
  }
  print(i)
  flush.console()
}
Effects_2
pairs


match(matchlist[73:76],s)
s[c(13,19,15,21)]

delta <- rep(0,factorial(4))
delta[c(13,19,15,21)] <-1
delta %*%f_22

#Yes does seem correct...



#            [,1]       [,2]       [,3]       [,4]       [,5]       [,6]
# [1,] -20.333333  0.6666667  19.666667  19.666667  0.6666667 -20.333333
# [2,]  13.666667 -0.3333333 -13.333333 -13.333333 -0.3333333  13.666667
# [3,]   6.666667 -0.3333333  -6.333333  -6.333333 -0.3333333   6.666667
# [4,]   6.666667 -0.3333333  -6.333333  -6.333333 -0.3333333   6.666667
# [5,]  13.666667 -0.3333333 -13.333333 -13.333333 -0.3333333  13.666667
# [6,] -20.333333  0.6666667  19.666667  19.666667  0.6666667 -20.333333



tab11 <- matrix(rep(0,24),ncol=2, byrow = TRUE)

ctr <- 0
i<-1
j<-1
for(i in 1:nrow(pairs)){
  for(j in 2:1){
    ctr <- ctr + 1
    tab11[ctr,1] <- setdiff(pairs[i,],pairs[i,j])
    tab11[ctr,2] <- pairs[i,j]
  }
}


matchlist <- list(length=2)
for(i in 1:2){
  matchlist[[i]] <- rep(0,4)
}
Effects_11 <- matrix(nrow=12,ncol=12)


i<-1
j<-1
k<-1
for(i in 1:nrow(tab11)){
  pairpos <- tab11[i,]
  otherpos <- setdiff(c(1,2,3,4),pairpos)
  for(j in 1:nrow(tab11)){
    pairlist <- tab11[j,]
    fillerlist <- setdiff(c(1,2,3,4),tab11[j,]) %>% permn()
    ctr <- 0
    delta <- rep(0, factorial(4))
      ctr <- ctr + 1
      for(l in 1:length(fillerlist)){
        matchlist[[ctr]][pairpos[1:2]] <- pairlist[1:2]
        matchlist[[ctr]][otherpos[1:2]] <- fillerlist[[l]][1:2]
        matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
        ctr <- ctr + 1
      }
      ctr <- ctr - 1
    
    delta[match(matchlist,s)] <- 1
    Effects_11[i,j] <- delta %*% f_211
  }
  print(i)
  flush.console()
}
Effects_11
max(Effects_11)
tab11[5,] #(C,G) in (3rd,1st)

min(Effects_11)
#negative effects (C,D) in 3rd and 4th
tab11[4,] #(C,D) in 3rd 4th 
tab11[12,] #(D,C) in 4th, 3rd

