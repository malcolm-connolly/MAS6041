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
    str_remove_all(pattern = "[.]")
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
    str_replace(pattern = "9", replacement = "7") %>%
    str_replace(pattern = "10", replacement = "8") 
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

S8 <- as.word(allperms(8)) 
#sometimes throws "Error: $ operator is invalid for atomic vectors" on first attempt
#solution: install package again/reload library /restart R session
#clashes with tidyverse(I think stringr) 

s<-  lapply(seq_len(factorial(8)), function(i) unclass((S8))[i,])

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
for(i in 1:factorial(8)){
  f[i] <- freqs[ind == i] %>% unlist() %>% sum()
}
f[which(is.na(f))] <- 0 

#saveRDS(f, file="./data/S8/f_8.RData")
#saveRDS(s, file="./data/S8/s.RData")



######## Now have the function of counts f, can begin projections ####
  
types <- shape(S8)

for(i in 1:factorial(8)){
  types[i] <- lapply(types[i], sort, decreasing = TRUE)
}

#saveRDS(types, file="./data/S8/types.RData")

invS8 <- inverse(S8)
#saveRDS(invS8, file ="./data/S8/invS8.RData" )

chi.8 <- rep(1,factorial(8))

multi <-function(x = permutation(),y = permutation(),i=integer()){
  x*y[i]
}  

X<- matrix(nrow=factorial(7),ncol=factorial(7))
for(i in 1:factorial(8)){
  X[,i] <- multi(invS8,S8,i)
}


X <- list(length=factorial(8))
#X <- matrix(nrow = 120, ncol = 120) #This is not the Cayley table, a large list.
for(i in 1:factorial(8)){
  X[[i]] <- multi(invS8,S8,i)
}


#X[[i]] <- lapply(seq_len(factorial(8)), function(j) unclass(X[[i]])[j,])

saveRDS(X, file ="./data/S8/X.RData" )

f_tilde <- vector(length = factorial(8))
for(i in 1:factorial(8)){
  f_tilde[i] <- (chi.8 %*% f[match(X[[i]],s)])*chi.8[1]/factorial(8)
}

