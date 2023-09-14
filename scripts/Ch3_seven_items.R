library(tidyverse)


####read in ranked list, saving frequency ####
test <- read_lines("./data/00014-00000001.soc",skip=22)

test.split <- strsplit(test, ": ")

for(i in 1:length(test.split)){
  test.split[[i]][1:2]<-test.split[[i]][2:1]
}

#### exclude certain items ####

ex.list <- test.split

#suppose we wish to exclude item 1 and item 3


for(i in 1:length(ex.list)){
  ex.list[[i]][1] <- ex.list[[i]][1] %>% str_replace(pattern = ",3,", replacement =",") %>%
    str_remove_all(pattern=",3") %>%
    str_remove_all(pattern="3,") %>%
    str_replace(pattern = ",1,", replacement =",") %>%
    str_remove_all(pattern="1,") %>%
    paste(".", sep="") %>%
    str_remove_all(pattern=",1[.]") %>%
    str_remove_all(pattern = "[.]") %>%
    str_replace(pattern = ",10,",replacement =",") %>%
    str_remove_all(pattern="10,") %>%
    str_remove_all(pattern=",10")
}

# character . has meaning in regex but can use square brackets to mean look for it literally
# it is usually a wildcard, could also use p for placeholder

####renumber the items ####

for(i in 1:length(ex.list)){
  ex.list[[i]][1] <- ex.list[[i]][1] %>% str_replace(pattern = "2", replacement = "1") %>%
    str_replace(pattern = "4", replacement = "2") %>%
    str_replace(pattern = "5", replacement = "3") %>%
    str_replace(pattern = "6", replacement = "4") %>%
    str_replace(pattern = "7", replacement = "5") %>%
    str_replace(pattern = "8", replacement = "6") %>%
    str_replace(pattern = "9", replacement = "7") 
}


###### change first list item to ranks #####


for(i in 1:length(ex.list)){
  temp1 <- ex.list[[i]][1] %>% str_split(pattern=",")
  temp2 <- temp1
  for(j in 1:8){
    k <- temp1[[1]][j] %>% as.numeric()
    j.str <- as.character(j)
    temp2[[1]][k] <- j.str
  }
  ex.list[[i]][1] <- toString(temp2[[1]]) %>% str_remove_all(" ")
}


#### match to revlex in first list / deal with duplicates ####


install.packages("permutations")
library(permutations)

S7 <- as.word(allperms(7)) 
#sometimes throws "Error: $ operator is invalid for atomic vectors" on first attempt
#solution: install package again/reload library /restart R session
#clashes with tidyverse(I think stringr) 

s<-  lapply(seq_len(factorial(7)), function(i) unclass((S7))[i,])

# turning comma separated string "1,2,3" in ex.list into integer list 1 2 3

ranks <- list()
for(i in 1:length(ex.list)){
  ranks[i] <- ex.list[[i]][1] %>% str_split(pattern = ",") %>% lapply(function(x) as.integer(x))
}

ind <- match(ranks,s)

freqs <- list()
for(i in 1:length(ex.list)){
  freqs[i] <- ex.list[[i]][2] %>% lapply(function(x) as.integer(x))
}

f <- vector()
for(i in 1:factorial(7)){
  f[i] <- freqs[ind == i] %>% unlist() %>% sum()
}
f[which(is.na(f))] <- 0 

saveRDS(f, file="./data/S7/f_7.RData")
saveRDS(s, file="./data/S7/s.RData")

f<- readRDS("./data/S7/f_7.RData")
s<- readRDS("./data/S7/s.RData")

######## Now have the function of counts f, can begin projections ####

types <- shape(S7)

for(i in 1:factorial(8)){
  types[i] <- lapply(types[i], sort, decreasing = TRUE)
}

saveRDS(types, file="./data/S7/types.RData")
types <- readRDS("./data/S7/types.RData")

invS7 <- inverse(S7)
saveRDS(invS7, file ="./data/S7/invS7.RData" )


chi.7 <- rep(1,factorial(7))

multi <-function(x = permutation(),y = permutation(),i=integer()){
  x*y[i]
}  


X <- list(length=factorial(7))
#X <- matrix(nrow = 120, ncol = 120) #This is not the Cayley table, a large list.
for(i in 1:factorial(7)){
  X[[i]] <- multi(invS7,S7,i)
  #X[[i]] <- lapply(seq_len(factorial(7)), function(j) unclass(X[[i]])[j,])
}

for(i in 1:factorial(7)){
  X[[i]] <- lapply(seq_len(factorial(7)), function(j) unclass(X[[i]])[j,])
}

#

saveRDS(X, file ="./data/S7/X.RData" )
X<- readRDS("./data/S7/X.RData")

f_tilde <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_tilde[i] <- (chi.7 %*% f[match(X[[i]],s)])*chi.7[1]/factorial(7)
}

(f_tilde%*%f_tilde)/factorial(7)

#0.98419
#Notice f_tilde[i] = sum(f)/factorial(7)
# This is correct because best constant approx has SS= (5000/7!)^2 *7!
# and SS/7! is the number above.

#Character table values on conjugacy classes
c.7 <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) 
c.61 <- c(6,4,2,0,3,1,-1,0,2,0,-1,1,-1,0,-1) #checked
c.52 <- c(14,6,2,2,2,0,2,-1,0,0,0,-1,1,-1,0) #
c.511 <- c(15,5,-1,-3,3,-1,-1,0,1,-1,1,0,0,0,1) #
c.43 <- c(14,4,2,0,-1,1,-1,2,-2,0,1,-1,-1,0,0) #
c.421 <- c(35,5,-1,1,-1,-1,-1,-1,-1,1,-1,0,0,1,0) #checked
c.4111 <- c(20,0,-4,0,2,0,2,2,0,0,0,0,0,0,-1) #
c.331 <- c(21,1,1,-3,-3,1,1,0,-1,-1,-1,1,1,0,0) #mistake!
c.322 <- c(21,-1,1,3,-3,-1,1,0,1,-1,1,1,-1,0,0) #
c.3211 <- c(35,-5,-1,-1,-1,1,-1,-1,1,1,1,0,0,-1,0) #checked
c.31111 <- c(15,-5,-1,3,3,1,-1,0,-1,-1,-1,0,0,0,1) #
c.2221 <- c(14,-4,2,0,-1,-1,-1,2,2,0,-1,-1,1,0,0)
c.22111 <- c(14,-6,2,-2,2,0,2,-1,0,0,0,-1,-1,1,0) #checked
c.211111 <- c(6,-4,2,0,3,-1,-1,0,-2,0,1,1,1,0,-1) #
c.1111111 <- c(1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,-1,1) #


chi.61 <- vector(length = factorial(7))

chi.61[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.61[1]
chi.61[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.61[2]
chi.61[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.61[3]
chi.61[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.61[4]
chi.61[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.61[5]
chi.61[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.61[6]
chi.61[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.61[7]
chi.61[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.61[8]
chi.61[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.61[9]
chi.61[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.61[10]
chi.61[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.61[11]
chi.61[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.61[12]
chi.61[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.61[13]
chi.61[which( (sapply(types, '[',1) == "6") )] <- c.61[14]
chi.61[which(sapply(types, '[',1) == "7" )] <- c.61[15]


f_61 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_61[i] <- (chi.61 %*% f[match(X[[i]],s)])*chi.61[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_61%*%f_61)/factorial(7)


chi.52 <- vector(length = factorial(7))

chi.52[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.52[1]
chi.52[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.52[2]
chi.52[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.52[3]
chi.52[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.52[4]
chi.52[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.52[5]
chi.52[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.52[6]
chi.52[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.52[7]
chi.52[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.52[8]
chi.52[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.52[9]
chi.52[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.52[10]
chi.52[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.52[11]
chi.52[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.52[12]
chi.52[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.52[13]
chi.52[which( (sapply(types, '[',1) == "6") )] <- c.52[14]
chi.52[which(sapply(types, '[',1) == "7" )] <- c.52[15]

f_52 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_52[i] <- (chi.52 %*% f[match(X[[i]],s)])*chi.52[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_52%*%f_52)/factorial(7)
saveRDS(f_52,"./data/S7/f_52.RData")

chi.511 <- vector(length = factorial(7))

chi.511[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.511[1]
chi.511[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.511[2]
chi.511[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.511[3]
chi.511[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.511[4]
chi.511[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.511[5]
chi.511[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.511[6]
chi.511[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.511[7]
chi.511[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.511[8]
chi.511[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.511[9]
chi.511[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.511[10]
chi.511[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.511[11]
chi.511[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.511[12]
chi.511[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.511[13]
chi.511[which( (sapply(types, '[',1) == "6") )] <- c.511[14]
chi.511[which(sapply(types, '[',1) == "7" )] <- c.511[15]

f_511 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_511[i] <- (chi.511 %*% f[match(X[[i]],s)])*chi.511[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_511%*%f_511)/factorial(7)


chi.43 <- vector(length = factorial(7))

chi.43[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.43[1]
chi.43[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.43[2]
chi.43[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.43[3]
chi.43[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.43[4]
chi.43[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.43[5]
chi.43[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.43[6]
chi.43[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.43[7]
chi.43[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.43[8]
chi.43[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.43[9]
chi.43[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.43[10]
chi.43[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.43[11]
chi.43[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.43[12]
chi.43[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.43[13]
chi.43[which( (sapply(types, '[',1) == "6") )] <- c.43[14]
chi.43[which(sapply(types, '[',1) == "7" )] <- c.43[15]

f_43 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_43[i] <- (chi.43 %*% f[match(X[[i]],s)])*chi.43[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_43%*%f_43)/factorial(7)



chi.421 <- vector(length = factorial(7))

chi.421[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.421[1]
chi.421[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.421[2]
chi.421[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.421[3]
chi.421[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.421[4]
chi.421[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.421[5]
chi.421[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.421[6]
chi.421[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.421[7]
chi.421[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.421[8]
chi.421[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.421[9]
chi.421[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.421[10]
chi.421[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.421[11]
chi.421[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.421[12]
chi.421[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.421[13]
chi.421[which( (sapply(types, '[',1) == "6") )] <- c.421[14]
chi.421[which(sapply(types, '[',1) == "7" )] <- c.421[15]

f_421 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_421[i] <- (chi.421 %*% f[match(X[[i]],s)])*chi.421[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_421%*%f_421)/factorial(7)
saveRDS(f_421,"./data/S7/f_421.RData")

chi.4111 <- vector(length = factorial(7))

chi.4111[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.4111[1]
chi.4111[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.4111[2]
chi.4111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.4111[3]
chi.4111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.4111[4]
chi.4111[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.4111[5]
chi.4111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.4111[6]
chi.4111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.4111[7]
chi.4111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.4111[8]
chi.4111[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.4111[9]
chi.4111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.4111[10]
chi.4111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.4111[11]
chi.4111[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.4111[12]
chi.4111[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.4111[13]
chi.4111[which( (sapply(types, '[',1) == "6") )] <- c.4111[14]
chi.4111[which(sapply(types, '[',1) == "7" )] <- c.4111[15]

f_4111 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_4111[i] <- (chi.4111 %*% f[match(X[[i]],s)])*chi.4111[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_4111%*%f_4111)/factorial(7)

# types <- readRDS("./data/S7/types.RData")
# X<- readRDS("./data/S7/X.RData")
#f <- readRDS("./data/S7/f_7.RData")

chi.331 <- vector(length = factorial(7))

chi.331[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.331[1]
chi.331[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.331[2]
chi.331[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.331[3]
chi.331[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.331[4]
chi.331[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.331[5]
chi.331[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.331[6]
chi.331[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.331[7]
chi.331[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.331[8]
chi.331[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.331[9]
chi.331[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.331[10]
chi.331[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.331[11]
chi.331[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.331[12]
chi.331[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.331[13]
chi.331[which( (sapply(types, '[',1) == "6") )] <- c.331[14]
chi.331[which(sapply(types, '[',1) == "7" )] <- c.331[15]

f_331 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_331[i] <- (chi.331 %*% f[match(X[[i]],s)])*chi.331[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_331%*%f_331)/factorial(7)
saveRDS(f_331,"./data/S7/f_331.RData")

chi.322 <- vector(length = factorial(7))

chi.322[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.322[1]
chi.322[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.322[2]
chi.322[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.322[3]
chi.322[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.322[4]
chi.322[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.322[5]
chi.322[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.322[6]
chi.322[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.322[7]
chi.322[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.322[8]
chi.322[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.322[9]
chi.322[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.322[10]
chi.322[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.322[11]
chi.322[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.322[12]
chi.322[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.322[13]
chi.322[which( (sapply(types, '[',1) == "6") )] <- c.322[14]
chi.322[which(sapply(types, '[',1) == "7" )] <- c.322[15]

f_322 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_322[i] <- (chi.322 %*% f[match(X[[i]],s)])*chi.322[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_322%*%f_322)/factorial(7)


chi.3211 <- vector(length = factorial(7))

chi.3211[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.3211[1]
chi.3211[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.3211[2]
chi.3211[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.3211[3]
chi.3211[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.3211[4]
chi.3211[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.3211[5]
chi.3211[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.3211[6]
chi.3211[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.3211[7]
chi.3211[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.3211[8]
chi.3211[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.3211[9]
chi.3211[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.3211[10]
chi.3211[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.3211[11]
chi.3211[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.3211[12]
chi.3211[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.3211[13]
chi.3211[which( (sapply(types, '[',1) == "6")  )] <- c.3211[14]
chi.3211[which(sapply(types, '[',1) == "7" )] <- c.3211[15]

f_3211 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_3211[i] <- (chi.3211 %*% f[match(X[[i]],s)])*chi.3211[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_3211%*%f_3211)/factorial(7)
saveRDS(f_3211,"./data/S7/f_3211.RData")

chi.31111 <- vector(length = factorial(7))

chi.31111[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.31111[1]
chi.31111[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.31111[2]
chi.31111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.31111[3]
chi.31111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.31111[4]
chi.31111[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.31111[5]
chi.31111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.31111[6]
chi.31111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.31111[7]
chi.31111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.31111[8]
chi.31111[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.31111[9]
chi.31111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.31111[10]
chi.31111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.31111[11]
chi.31111[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.31111[12]
chi.31111[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.31111[13]
chi.31111[which( (sapply(types, '[',1) == "6") )] <- c.31111[14]
chi.31111[which(sapply(types, '[',1) == "7" )] <- c.31111[15]

f_31111 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_31111[i] <- (chi.31111 %*% f[match(X[[i]],s)])*chi.31111[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_31111%*%f_31111)/factorial(7)


chi.2221 <- vector(length = factorial(7))

chi.2221[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.2221[1]
chi.2221[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.2221[2]
chi.2221[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.2221[3]
chi.2221[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.2221[4]
chi.2221[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.2221[5]
chi.2221[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.2221[6]
chi.2221[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.2221[7]
chi.2221[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.2221[8]
chi.2221[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.2221[9]
chi.2221[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.2221[10]
chi.2221[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.2221[11]
chi.2221[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.2221[12]
chi.2221[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.2221[13]
chi.2221[which( (sapply(types, '[',1) == "6") )] <- c.2221[14]
chi.2221[which(sapply(types, '[',1) == "7" )] <- c.2221[15]

f_2221 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_2221[i] <- (chi.2221 %*% f[match(X[[i]],s)])*chi.2221[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_2221%*%f_2221)/factorial(7)


chi.22111 <- vector(length = factorial(7))

chi.22111[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.22111[1]
chi.22111[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.22111[2]
chi.22111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.22111[3]
chi.22111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.22111[4]
chi.22111[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.22111[5]
chi.22111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.22111[6]
chi.22111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.22111[7]
chi.22111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.22111[8]
chi.22111[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.22111[9]
chi.22111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.22111[10]
chi.22111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.22111[11]
chi.22111[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.22111[12]
chi.22111[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.22111[13]
chi.22111[which( (sapply(types, '[',1) == "6") )] <- c.22111[14]
chi.22111[which(sapply(types, '[',1) == "7" )] <- c.22111[15]

f_22111 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_22111[i] <- (chi.22111 %*% f[match(X[[i]],s)])*chi.22111[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_22111%*%f_22111)/factorial(7)



chi.211111 <- vector(length = factorial(7))

chi.211111[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.211111[1]
chi.211111[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.211111[2]
chi.211111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.211111[3]
chi.211111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.211111[4]
chi.211111[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.211111[5]
chi.211111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.211111[6]
chi.211111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.211111[7]
chi.211111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.211111[8]
chi.211111[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.211111[9]
chi.211111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.211111[10]
chi.211111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.211111[11]
chi.211111[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.211111[12]
chi.211111[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.211111[13]
chi.211111[which( (sapply(types, '[',1) == "6") )] <- c.211111[14]
chi.211111[which(sapply(types, '[',1) == "7" )] <- c.211111[15]

f_211111 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_211111[i] <- (chi.211111 %*% f[match(X[[i]],s)])*chi.211111[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_211111%*%f_211111)/factorial(7)

chi.1111111 <- vector(length = factorial(7))

chi.1111111[which(sapply(types, '[',1) == "1" & is.na(sapply(types, '[',2)) )] <- c.1111111[1]
chi.1111111[which(sapply(types, '[',1) == "2" & is.na(sapply(types, '[',2)) )] <- c.1111111[2]
chi.1111111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.1111111[3]
chi.1111111[which( (sapply(types, '[',1) == "2") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.1111111[4]
chi.1111111[which( (sapply(types, '[',1) == "3") & is.na(sapply(types, '[',2)) )] <- c.1111111[5]
chi.1111111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & is.na(sapply(types, '[',3)) )] <- c.1111111[6]
chi.1111111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "2") & (sapply(types, '[',3) == "2") )] <- c.1111111[7]
chi.1111111[which( (sapply(types, '[',1) == "3") & (sapply(types, '[',2) == "3") )] <- c.1111111[8]
chi.1111111[which(sapply(types, '[',1) == "4" & is.na(sapply(types, '[',2)) )] <- c.1111111[9]
chi.1111111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "2") )] <- c.1111111[10]
chi.1111111[which( (sapply(types, '[',1) == "4") & (sapply(types, '[',2) == "3") )] <- c.1111111[11]
chi.1111111[which(sapply(types, '[',1) == "5" & is.na(sapply(types, '[',2)) )] <- c.1111111[12]
chi.1111111[which( (sapply(types, '[',1) == "5") & (sapply(types, '[',2) == "2") )] <- c.1111111[13]
chi.1111111[which( (sapply(types, '[',1) == "6")  )] <- c.1111111[14]
chi.1111111[which(sapply(types, '[',1) == "7" )] <- c.1111111[15]

f_1111111 <- vector(length = factorial(7))
for(i in 1:factorial(7)){
  f_1111111[i] <- (chi.1111111 %*% f[match(X[[i]],s)])*chi.1111111[1]/factorial(7)
  if( i %in% seq(500,5000,500)){
    print(i)
    flush.console()
  }
}

(f_1111111%*%f_1111111)/factorial(7)


saveRDS(f_61,"./data/S7/f_61.RData")
saveRDS(f_52,"./data/S7/f_52.RData")
saveRDS(f_511,"./data/S7/f_511.RData")
saveRDS(f_421,"./data/S7/f_421.RData")

 #### basic inference ####

#dimensions
d <- c(c.7[1],c.61[1],c.52[1],c.511[1],c.43[1],c.421[1],c.4111[1],c.331[1],c.322[1],
       c.3211[1],c.31111[1],c.2221[1],c.22111[1],c.211111[1],c.1111111[1])

N<- 5000

qchisq(pchisq(d,d),d)*N/(factorial(7)^2)
d*N/(factorial(7)^2)
SS_scaled <- c(0.984,2.184,7.0958,1.1401,0.9299,35.55178,0.2390416,27.6791,0.420129,34.31915,0.0591376,0.067264411,5.479836,0.01324594,0.02778295)

# Can compare the SS to the expected SS from the chisq distribution
SS_scaled <- c(0.98,2.18 ,2.48,1.40,0.93,2.31 , 0.24 , 0.50, 0.42, 0.46 , 0.06 , 0.07 , 0.06, 0.01 ,0.00)
qchisq(SS_scaled,d)*N/(factorial(7)^2)

qchisq(SS_scaled,d)

(d^2)*N/(factorial(7)^2)

SS_scaled*factorial(7)/N
pchisq(SS_scaled*factorial(7)/N,d^2)

#Diaconis does...
dims <- c(4,5,6,5,4,1)
dims^2*5738/(120^2) #he just compares the means...

##### Computing projections with f - second order deltas #####

#require appropriate deltas

library(combinat)

#threes <- combinat::combn(c(1,2,3,4,5,6,7),3) 

#pairs <- combinat::combn(c(1,2,3,4,5,6,7),2) %>% t()
#pairs[,1:2] <- sapply(pairs[,1:2], as.integer)

#pick a triple and then a pair
#setdiff(c(1,2,3,4,5,6,7),threes[,1]) %>% combn(2) %>% t()

#s[1]

#pairs are items (positions) and ranks (numbers in that position)

#make a delta that is 1 when item 3&5 are ranked 2nd&4th in either order, zero elsewhere

#for the rank order this means there should be a 2/4 in the 3rd and 5th positions
#rank order should be _,_,2,_,4,_,_

#pairs[13,] # 3&5 items
#pairs[8,] # 2&4 ranks

#idea is to create a list of all strings and match these to s, 
# putting a 1 in vector delta for those matched values




#fillerlist <- setdiff(c(1,2,3,4,5,6,7),pairs[8,]) %>% permn()
#pairlist <- pairs[8,] %>% permn()
#pairlist[[1]][1]


#pairpos <- pairs[13,]
#pairpos
#otherpos <- setdiff(c(1,2,3,4,5,6,7),pairs[13,])
#otherpos[1:5]


#matchlist <- list(length=240)
#for(i in 1:240){
#  matchlist[[i]] <- rep(0,7)
#}
#ctr <- 0
#for(i in 1:length(pairlist)){
#  ctr <- ctr + 1
#  for(j in 1:length(fillerlist)){
#    matchlist[[ctr]][pairpos[1:2]] <- pairlist[[i]][1:2]
#    matchlist[[ctr]][otherpos[1:5]] <- fillerlist[[j]][1:5]
#    matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
#    ctr <- ctr + 1
#  }
#  ctr <- ctr - 1
#}



#match(matchlist[1],s)
#s[match(matchlist,s)] #corrected :)

# delta.35_24 <- vector(length = factorial(7))
# delta.35_24 <- rep(0,factorial(7))
# for(i in 1:length(matchlist)){
#   delta.35_24[match(matchlist[i],s)] <- 1
# }
# 
# f_52 %*% delta.35_24 #546

#ok generalise this so that I can get all 21^2 deltas in one big list.
#then I will calculate the inner products later


pairs <- combinat::combn(c(1,2,3,4,5,6,7),2) %>% t()
pairs[,1:2] <- sapply(pairs[,1:2], as.integer)

#Delta <- list(length=nrow(pairs)^2)

matchlist <- list(length=240)
for(i in 1:240){
  matchlist[[i]] <- rep(0,7)
}
Effects_52 <- matrix(nrow=21,ncol=21)

for(i in 1:nrow(pairs)){
  pairpos <- pairs[i,]
  otherpos <- setdiff(c(1,2,3,4,5,6,7),pairpos)
  for(j in 1:nrow(pairs)){
    pairlist <- pairs[j,] %>% permn()
    fillerlist <- setdiff(c(1,2,3,4,5,6,7),pairs[j,]) %>% permn()
    ctr <- 0
    delta <- rep(0, factorial(7))
    for(k in 1:length(pairlist)){
      ctr <- ctr + 1
      for(l in 1:length(fillerlist)){
        matchlist[[ctr]][pairpos[1:2]] <- pairlist[[k]][1:2]
        matchlist[[ctr]][otherpos[1:5]] <- fillerlist[[l]][1:5]
        matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
        ctr <- ctr + 1
      }
      ctr <- ctr - 1
    }
    delta[match(matchlist,s)] <- 1
    Effects_52[i,j] <- delta %*% f_52
  }
  print(i)
  flush.console()
}

#which(Delta[[260]] != delta.35_24) #all equal, checked :)

#saveRDS(Delta,"./data/S7/Delta_corrected.RData")
saveRDS(f_52,"./data/S7/f_52.RData")

Effects_52 <- matrix(nrow=21,ncol=21)

ctr.Delta <- 0
for(i in 1:21){
  for(j in 1:21){
    ctr.Delta <- ctr.Delta + 1
    Effects_52[i,j] <- Delta[[ctr.Delta]]%*%f_52
  }
}

max(Effects_52)


min(Effects_52)

write.csv(Effects_52,"./data/S7/Effects_52.csv")





##### Attempting the higher order effects ######

library(combinat)

#making the equiv of the pairs from before but its now like a pair and a single
#a bit like a tableau so call it tab21
threes <- combn(c(1,2,3,4,5,6,7),3) %>% t() 
threes[,1:3] <- sapply(threes[,1:3], as.integer)

tab21 <- matrix(data= rep(0,nrow(threes)*9), nrow = nrow(threes)*3, ncol=3, byrow=TRUE)

ctr <- 0
i<-1
for(i in 1:nrow(threes)){
  for(j in 1:choose(3,1)){
    ctr <- ctr + 1
    tab21[ctr,1] <- setdiff(threes[i,],threes[i,j])[1]
    tab21[ctr,2] <- setdiff(threes[i,],threes[i,j])[2]
    tab21[ctr,3] <- threes[i,j]
  }
}

# OK suppose I want the items {5,1} and (7) and ranks {1,2} (7)
# means I want 1 and 2 in position 1 or 5 and 7 in 7
# 1,_,_,_,2,_,7 or 2,_,_,_,1,_,7

items <- tab21[42,] 
i<-1
ranks <- tab21[15,] %>% setdiff(tab21[15,3]) %>% permn()
for(i in 1:length(ranks)){
  ranks[[i]] <- union(ranks[[i]],tab21[15,3])
}

filler <- setdiff(c(1,2,3,4,5,6,7),tab21[15,]) %>% permn()

vacant <- setdiff(c(1,2,3,4,5,6,7),items)

pairpos <- items[1:2]
sgtnpos <- items[3]


matchlist <- list(length=length(ranks)*length(filler))
for(i in 1:48){
  matchlist[[i]] <- rep(0,7)
}
ctr <- 0
i<-1
j <- 1
for(i in 1:length(ranks)){
  ctr <- ctr + 1
  for(j in 1:length(filler)){
    matchlist[[ctr]][pairpos[1:2]] <- ranks[[i]][1:2]
    matchlist[[ctr]][sgtnpos[1]] <- ranks[[i]][3]
    matchlist[[ctr]][vacant[1:4]] <- filler[[j]][1:4]
    matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
    ctr <- ctr + 1
  }
  ctr <- ctr - 1
}

#OK that works
match(matchlist[1:48],s)
s[match(matchlist,s)]

f_421[match(matchlist[1:48],s)] %>% sum()

delta.15_7.12_7 <- vector(length = factorial(7))
delta.15_7.12_7 <- rep(0,factorial(7))
for(i in 1:length(matchlist)){
  delta.15_7.12_7[match(matchlist[i],s)] <- 1
}

f_421 %*% delta.15_7.12_7 #-218.1806

#OK great need to generalise this, could try a few more first...

# OK suppose I want the items {4,7} and (5) and ranks {6,7} (1)
# means I want ...
# _,_,_,6,1,_,7 or _,_,_,7,1,_,6

items <- tab21[98,] 
i<-1
ranks <- tab21[43,] %>% setdiff(tab21[43,3]) %>% permn()
for(i in 1:length(ranks)){
  ranks[[i]] <- union(ranks[[i]],tab21[43,3])
}

filler <- setdiff(c(1,2,3,4,5,6,7),tab21[43,]) %>% permn()

vacant <- setdiff(c(1,2,3,4,5,6,7),items)

pairpos <- items[1:2]
sgtnpos <- items[3]


matchlist <- list(length=length(ranks)*length(filler))
for(i in 1:48){
  matchlist[[i]] <- rep(0,7)
}
ctr <- 0
i<-1
j <- 1
for(i in 1:length(ranks)){
  ctr <- ctr + 1
  for(j in 1:length(filler)){
    matchlist[[ctr]][pairpos[1:2]] <- ranks[[i]][1:2]
    matchlist[[ctr]][sgtnpos[1]] <- ranks[[i]][3]
    matchlist[[ctr]][vacant[1:4]] <- filler[[j]][1:4]
    matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
    ctr <- ctr + 1
  }
  ctr <- ctr - 1
}

#OK that works
match(matchlist[1:48],s)
s[match(matchlist,s)]

f_421[match(matchlist[1:48],s)] %>% sum()

delta.47_5.67_1 <- vector(length = factorial(7))
delta.47_5.67_1 <- rep(0,factorial(7))
for(i in 1:length(matchlist)){
  delta.47_5.67_1[match(matchlist[i],s)] <- 1
}

f_421 %*% delta.47_5.67_1 #-69.09722

##### Create all interpretable deltas for V_421 #####


#Adapt this :
#Delta_421 <- list(length=nrow(tab21)^2)

matchlist <- list(length=48)
for(i in 1:48){
  matchlist[[i]] <- rep(0,7)
}
ctr <- 0
#ctr.Delta <- 0
Effects_421 <- matrix(nrow=105,ncol=105)

i<- 1
for(i in 1:nrow(tab21)){
  items <- tab21[i,]
  pairpos <- items[1:2]
  sgtnpos <- items[3]
  vacant <- setdiff(c(1,2,3,4,5,6,7),items)
  for(j in 1:nrow(tab21)){
    ranks <- tab21[j,] %>% setdiff(tab21[j,3]) %>% permn()
    for(m in 1:length(ranks)){
      ranks[[m]] <- union(ranks[[m]],tab21[j,3])
    }
    filler <- setdiff(c(1,2,3,4,5,6,7),tab21[j,]) %>% permn()
    ctr <- 0
    delta <- rep(0, factorial(7))
    for(k in 1:length(ranks)){
      ctr <- ctr + 1
      for(l in 1:length(filler)){
        matchlist[[ctr]][pairpos[1:2]] <- ranks[[k]][1:2]
        matchlist[[ctr]][sgtnpos[1]] <- ranks[[k]][3]
        matchlist[[ctr]][vacant[1:4]] <- filler[[l]][1:4]
        matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
        ctr <- ctr + 1
      }
      ctr <- ctr - 1
    }
    delta[match(matchlist,s)] <- 1
    Effects_421[i,j] <- delta %*% f_421
  }
  print(i)
  flush.console()
}

#which(Delta_421[[260]] != delta.35_24) #all equal, checked :)
#saveRDS(Delta_421,"./data/S7/Delta_421.RDS")
#Check...

tab21[(tab21[,1]==1 & tab21[,2]==5)|(tab21[,1]==5 & tab21[,2]==1),]
which((tab21[,1]==1 & tab21[,2]==5)|(tab21[,1]==5 & tab21[,2]==1))
tab21[42,]

tab21[(tab21[,1]==1 & tab21[,2]==2)|(tab21[,1]==2 & tab21[,2]==1),]
which((tab21[,1]==1 & tab21[,2]==2)|(tab21[,1]==2 & tab21[,2]==1))
tab21[15,]

#42nd row, 15th column
#41*105 +15 = 4320

#which(Delta_421[[4320]] != delta.15_7.12_7)

# Now computing the inner products

# Effects_421 <- matrix(nrow=105,ncol=105)
# 
# ctr.Delta <- 0
# for(i in 1:105){
#   for(j in 1:105){
#     ctr.Delta <- ctr.Delta + 1
#     Effects_421[i,j] <- Delta_421[[ctr.Delta]]%*%f_421
#   }
# }

max(Effects_421)


min(Effects_421)

write.csv(Effects_421,"./data/S7/Effects_421.csv")

tab21

Effects_421_col <- Effects_421[,order(tab21[,3])]
write.csv(Effects_421_col,"./data/S7/Effects_421_col.csv")

#[52,]    3    6    2   
#[53,]    2    6    3
#[54,]    2    3    6



# tab21[102,]
# # relabelled items 4,6,7 ... so {6,8} {9} 
# 
# tab21[105,]
# # in places {5,6}{7} So last place to 9 with bottom to 6 and 8 in either order
# 
# tab21[103,] # 6 7 5
# # least 'close' in places {6,7} {5} to items {6,8} {9}
# # A strong pull away from ranking 9 above a joint bottom 6&8 in either order. 
# 
# # another small no. in row 98
# tab21[98,]# relabelled items4 7 5, so
# 
# #These rows / columns have values large in absolute value (more than 3 sd)
# red_rows <- c(15,41,42,44,50,63,64,66,70,72,78,
#               81,90,92,97,98,99,101,102,104,105)
# 
# 
# red_cols <- c(1,9,10,13,15,38,41,42,43,44,45,68,71,72,75,103,105)
# 
# 
# Effects_421 <- read.csv("./data/S7/Effects_421.csv")
# Effects_421<- Effects_421[,-1]
# 
# Red_Effects <- Effects_421[red_rows,]
# Red_Effects <- Red_Effects[,red_cols]
# 
# 
# Red_Effects[,1:17] <- sapply(Red_Effects[,1:17], round)


s <- readRDS("./data/S7/s.RData")
f <- readRDS("./data/S7/f_7.RData")

f_52 <- readRDS("./data/S7/f_52.RData")
f_421 <- readRDS("./data/S7/f_421.RData")
f_331 <- readRDS("./data/S7/f_331.RData")
f_3211 <- readRDS("./data/S7/f_3211.RData")

tab21[red_rows,]
tab21[red_cols,]

tab21[15,]


Reordering <- Effects_421[,order(tab21[,3])]
Reordering <- Effects_421[order(tab21[,3]),]
write.csv(Reordering, "./data/S7/Reordering.csv")


rr <- tab21[red_rows,]
rr[order(rr[,3]),] 

rc <- tab21[red_cols,]
rc[order(rc[,3]),]

effects_subset <- Effects_421[red_rows,]
effects_subset <- effects_subset[,red_cols]

effects_subset <- effects_subset[order(rr[,3]),]
effects_subset <- effects_subset[,order(rc[,3])]

write.csv(effects_subset, "./data/S7/effects_subset.csv")
##### 

###### Copy this analysis for 331 ######

library(combinat)

#making the equiv of the pairs from before but its now like a pair and a single
#a bit like a tableau so call it tab21
fours <- combn(c(1,2,3,4,5,6,7),4) %>% t() 
fours[,1:3] <- sapply(fours[,1:3], as.integer)

tab31 <- matrix(data= rep(0,nrow(fours)*16), nrow = nrow(fours)*4, ncol=4, byrow=TRUE)

ctr <- 0
for(i in 1:nrow(fours)){
  for(j in 1:choose(4,1)){
    ctr <- ctr + 1
    tab31[ctr,1] <- setdiff(fours[i,],fours[i,j])[1]
    tab31[ctr,2] <- setdiff(fours[i,],fours[i,j])[2]
    tab31[ctr,3] <- setdiff(fours[i,],fours[i,j])[3]
    tab31[ctr,4] <- fours[i,j]
  }
}
tab31

tab31 <- tab31[order(tab31[,4]),] #putting it in order by the singleton


Delta_331 <- list(length=nrow(tab31)^2)

matchlist <- list(length=36)
for(i in 1:36){
  matchlist[[i]] <- rep(0,7)
}

ctr <- 0
ctr.Delta <- 0
i<- 1
for(i in 1:nrow(tab31)){
  items <- tab31[i,]
  triplepos <- items[1:3]
  sgtnpos <- items[4]
  vacant <- setdiff(c(1,2,3,4,5,6,7),items)
  for(j in 1:nrow(tab31)){
    ranks <- tab31[j,] %>% setdiff(tab31[j,4]) %>% permn()
    for(m in 1:length(ranks)){
      ranks[[m]] <- union(ranks[[m]],tab31[j,4])
    }
    filler <- setdiff(c(1,2,3,4,5,6,7),tab31[j,]) %>% permn()
    ctr <- 0
    delta <- rep(0, factorial(7))
    for(k in 1:length(ranks)){
      ctr <- ctr + 1
      for(l in 1:length(filler)){
        matchlist[[ctr]][triplepos[1:3]] <- ranks[[k]][1:3]
        matchlist[[ctr]][sgtnpos[1]] <- ranks[[k]][4]
        matchlist[[ctr]][vacant[1:3]] <- filler[[l]][1:3]
        matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
        ctr <- ctr + 1
      }
      ctr <- ctr - 1
    }
    delta[match(matchlist,s)] <- 1
    ctr.Delta <- ctr.Delta + 1
    Delta_331[[ctr.Delta]] <- delta
  }
}

saveRDS(Delta_331,"./data/S7/Delta_331.RDS")


Effects_331 <- matrix(nrow=140,ncol=140)

ctr.Delta <- 0
for(i in 1:140){
  for(j in 1:140){
    ctr.Delta <- ctr.Delta + 1
    Effects_331[i,j] <- Delta_331[[ctr.Delta]]%*%f_331
  }
}

max(Effects_331)


min(Effects_331)

write.csv(Effects_331,"./data/S7/Effects_331.csv")

saveRDS(tab31,"./data/S7/tab31.RData")

###
s <- readRDS("./data/S7/s.RData")
f <- readRDS("./data/S7/f_7.RData")
f_52 <- readRDS("./data/S7/f_52.RData")
f_421 <- readRDS("./data/S7/f_421.RData")
f_331 <- readRDS("./data/S7/f_331.RData")
f_3211 <- readRDS("./data/S7/f_3211.RData")


r_31<- c(1,2,5,6,9,10,11,12,15,16,19,20,22,23,28,33,38,39,42,59,66,75,
81,83,84,85,86,88,93,95,96,97,98,100,101,103,105,106,107,109,
112,114,115,116,118,120,121,123,124,125,126,127,129,132,134,135,136,138,140)

tab31[r_31,]
c_31 <- c(1,2,3,4,5,10,11,16,17,18,19,20,
30,31,32,41,44,57,60,61,62,68,69,72,73,79,
80,81,82,85,90,91,96,99,100,121,122,123,124,125,
130,131,136,137,138,139,140)
tab31[c_31,]




#### space 3211 ####

fours <- combn(c(1,2,3,4,5,6,7),4) %>% t() 
fours[,1:3] <- sapply(fours[,1:3], as.integer)

pair <- combn(fours[i,],2)
#
tab211 <- matrix(data= rep(0,nrow(fours)*choose(4,2)*2*4), nrow = nrow(fours)*choose(4,2)*2, ncol=4, byrow=TRUE)


ctr <- 0
i<-1
j<-1
k<-1
for(i in 1:nrow(fours)){
  picked <- fours[i,]
  posspairs <- combn(picked,2) %>% t()
  for(j in 1:choose(4,2)){
    for(k in 1:2){
      ctr <- ctr + 1
      tab211[ctr,1] <- setdiff(fours[i,],c(posspairs[j,1],posspairs[j,2]))[1]
      tab211[ctr,2] <- setdiff(fours[i,],c(posspairs[j,1],posspairs[j,2]))[2]
      tab211[ctr,3] <- posspairs[j,k]
      tab211[ctr,4] <- setdiff(posspairs[j,],posspairs[j,k])
    }
  }
}
tab211

nrow(tab211)

tab211_rows <- tab211[order(tab211[,3],tab211[,4]),] #putting it in order by the singletons


# Adapt...


#Adapt this :
Delta_211 <- list(length=nrow(tab211)^2)

matchlist <- list(length=420)
for(i in 1:420){
  matchlist[[i]] <- rep(0,7)
}
ctr <- 0
ctr.Delta <- 0
m<-1
i<- 1
for(i in 1:nrow(tab211)){
  items <- tab211[i,]
  pairpos <- items[1:2]
  sgtnpos <- items[3:4]
  vacant <- setdiff(c(1,2,3,4,5,6,7),items)
  for(j in 1:nrow(tab211)){
    ranks <- tab211[j,1:2] %>% permn()
    for(m in 1:length(ranks)){
      ranks[[m]] <- union(ranks[[m]],tab211[j,3:4])
    }
    filler <- setdiff(c(1,2,3,4,5,6,7),tab211[j,]) %>% permn()
    ctr <- 0
    delta <- rep(0, factorial(7))
    for(k in 1:length(ranks)){
      ctr <- ctr + 1
      for(l in 1:length(filler)){
        matchlist[[ctr]][pairpos[1:2]] <- ranks[[k]][1:2]
        matchlist[[ctr]][sgtnpos[1:2]] <- ranks[[k]][3:4]
        matchlist[[ctr]][vacant[1:3]] <- filler[[l]][1:3]
        matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
        ctr <- ctr + 1
      }
      ctr <- ctr - 1
    }
    delta[match(matchlist,s)] <- 1
    ctr.Delta <- ctr.Delta + 1
    Delta_211[[ctr.Delta]] <- delta
  }

}

#Error: cannot allocate vector of size 64 Kb
#Error: cannot allocate vector of size 2.0 Mb

# Trying a sparse matrix
s <- readRDS("./data/S7/s.RData")
library(Matrix)
Delta_211 <- Matrix(0, nrow = 420^2,ncol = factorial(7),sparse=TRUE)

matchlist <- list(length=420) 
for(i in 1:420){
  matchlist[[i]] <- rep(0,7)
}
ctr <- 0
ctr.Delta <- 0
for(i in 1:nrow(tab211)){
  items <- tab211[i,]
  pairpos <- items[1:2]
  sgtnpos <- items[3:4]
  vacant <- setdiff(c(1,2,3,4,5,6,7),items)
  for(j in 1:nrow(tab211)){
    ranks <- tab211[j,1:2] %>% permn()
    for(m in 1:length(ranks)){
      ranks[[m]] <- union(ranks[[m]],tab211[j,3:4])
    }
    filler <- setdiff(c(1,2,3,4,5,6,7),tab211[j,]) %>% permn()
    ctr <- 0
    delta <- rep(0, factorial(7))
    for(k in 1:length(ranks)){
      ctr <- ctr + 1
      for(l in 1:length(filler)){
        matchlist[[ctr]][pairpos[1:2]] <- ranks[[k]][1:2]
        matchlist[[ctr]][sgtnpos[1:2]] <- ranks[[k]][3:4]
        matchlist[[ctr]][vacant[1:3]] <- filler[[l]][1:3]
        matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
        ctr <- ctr + 1
      }
      ctr <- ctr - 1
    }
    delta[match(matchlist,s)] <- 1
    ctr.Delta <- ctr.Delta + 1
    Delta_211[ctr.Delta,] <- delta
  }
  print(i)
  flush.console()
}

#another option if this does not work is to directly compute the effects within the loop
#so that Delta is not stored, rather delta is overwritten each iteration
f_3211 <- readRDS("./data/S7/f_3211.RData")
ctr.Delta <- 0
ctr <-0
E_3211 <- matrix(nrow=420,ncol=420)
for(i in 1:420){
  for(j in 1:420){
    ctr <- ctr + 1
    E_3211[i,j] <- Delta_211[ctr,]%*%f_3211
  }
  print(i)
  flush.console()
}



E_reordered <- E_3211[order(tab211[,3],tab211[,4]),]

write.csv(E_reordered, "./data/S7/E3211.csv") 
write.csv(E_3211,"./data/S7/E3211_noreordering.csv")
max(E_3211)
min(E_3211) #didn't work all the same....

tab211_rows <- tab211[order(tab211[,3],tab211[,4]),]

#### ####
library(combinat)
pairs <- c(1,2,3,4,5,6,7) %>% combn(2) %>% t()

tab11 <- matrix(data= rep(0,84), nrow =nrow(pairs)*2 , ncol=2, byrow=TRUE)

i<-1
j<-1
ctr <- 1
for(i in 1:nrow(pairs)){
  picked <- pairs[i,]
  possorders <- picked %>% permn()
  for(j in 1:2){
    tab11[ctr,] <- possorders[[j]]
    ctr <- ctr + 1
  }
}

tab11 <- tab11[order(tab11[,1],tab11[,2]),]

matchlist <- list(length=120)
for(i in 1:120){
  matchlist[[i]] <- rep(0,7)
}
ctr <- 0

Effects_11 <- matrix(nrow=42,ncol=42)

i<- 1
j<-1
for(i in 1:nrow(tab11)){
  pos <- tab11[i,] 
  vacant <- setdiff(c(1,2,3,4,5,6,7),pos)
  for(j in 1:nrow(pairs)){
    ranks <- pairs[j,]
    filler <- setdiff(c(1,2,3,4,5,6,7),ranks) %>% permn()
    delta <- rep(0, factorial(7))
    ctr <- 1
    for(l in 1:length(filler)){
      matchlist[[ctr]][pos[1:2]] <- ranks
      matchlist[[ctr]][vacant[1:5]] <- filler[[l]][1:5]
      matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
      ctr <- ctr + 1
    }
    delta[match(matchlist,s)] <- 1
    Effects_11[i,j] <- delta %*% f_511
  }
  print(i)
  flush.console()
}


Effects_11
max(Effects_11)


min(Effects_11)

write.csv(Effects_11,"./data/S7/Effects_11.csv")

tab11

#this duplicated information... need to consider only items {a}{b} in {c}{d} not {b}{a} in {d}{c} too.

##### just first order #####

library(combinat)
singles <- c(1,2,3,4,5,6,7) 

matchlist <- list(length=factorial(6))
for(i in 1:factorial(6)){
  matchlist[[i]] <- rep(0,7)
}
ctr <- 0

Effects_1 <- matrix(nrow=7,ncol=7)

i<- 1
j<-1
for(i in 1:length(singles)){
  pos <- singles[i] 
  vacant <- setdiff(c(1,2,3,4,5,6,7),pos)
  for(j in 1:length(singles)){
    ranks <- singles[j]
    filler <- setdiff(c(1,2,3,4,5,6,7),ranks) %>% permn()
    delta <- rep(0, factorial(7))
    ctr <- 1
    for(l in 1:length(filler)){
      matchlist[[ctr]][pos[1]] <- ranks
      matchlist[[ctr]][vacant[1:6]] <- filler[[l]][1:6]
      matchlist[[ctr]] <- matchlist[[ctr]] %>% sapply(as.integer)
      ctr <- ctr + 1
    }
    delta[match(matchlist,s)] <- 1
    Effects_1[i,j] <- delta %*% f_61
  }
  print(i)
  flush.console()
}


Effects_1
max(Effects_1)


min(Effects_1)

write.csv(Effects_1,"./data/S7/Effects_1.csv")



