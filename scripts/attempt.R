library(tidyverse)


####read in ranked list, saving frequency ####

test <- read_lines("./data/test4.txt")



test.split <- strsplit(test, ": ")

for(i in 1:length(test.split)){
  test.split[[i]][1:2]<-test.split[[i]][2:1]
}



#### exclude certain items ####

ex.list <- test.split

#suppose we wish to exclude item 3
for(i in 1:length(ex.list)){
ex.list[[i]][1] <- ex.list[[i]][1] %>% str_replace(pattern = ",3,", replacement =",") %>%
  str_remove_all(pattern=",3") %>%
  str_remove_all(pattern="3,")
}


####renumber the items ####

for(i in 1:length(ex.list)){
ex.list[[i]][1] <- ex.list[[i]][1] %>% str_replace(pattern = "4", replacement = "3")
}





###### change first list item to ranks #####


for(i in 1:length(ex.list)){
  temp1 <- ex.list[[i]][1] %>% str_split(pattern=",")
  temp2 <- temp1
  for(j in 1:3){
    k <- temp1[[1]][j] %>% as.numeric()
    j.str <- as.character(j)
    temp2[[1]][k] <- j.str
  }
  ex.list[[i]][1] <- toString(temp2[[1]]) %>% str_remove_all(" ")
}

#This is fine but after the exclusion step some of the ranked lists are the same


#### match to revlex in first list / deal with duplicates ####

library(permutations)
#install.packages("permutations")

S3 <- as.word(allperms(3)) 
#sometimes throws "Error: $ operator is invalid for atomic vectors" on first attempt
#solution: install package again/reload library /restart R session
#clashes with tidyverse(I think stringr) 

s<-  lapply(seq_len(6), function(i) unclass((S3))[i,])

ex.list[[1]][1]

#### 

#turning comma separated string "1,2,3" into integer list 1 2 3
ranks <- list()
for(i in 1:length(ex.list)){
  ranks[i] <- ex.list[[i]][1] %>% str_split(pattern = ",") %>% lapply(function(x) as.integer(x))
}

ranks

s[[6]][1]

match(ranks,s)
# BEWARE some NAs could be introduced if using as.integer not as.numeric 


#add frequencies where match, and identifies list in lex order
freqs <- list()
for(i in 1:length(ex.list)){
  freqs[i] <- ex.list[[i]][2] %>% lapply(function(x) as.integer(x))
}


f <- vector()
for(i in 1:factorial(3)){
  f[i] <- freqs[match(ranks,s) == i] %>% unlist() %>% sum()
}
f[which(is.na(f))] <- 0  

ex.list[c(which(match(ranks,s)==1))]  
f[1]

ex.list[c(which(match(ranks,s)==2))]
f[2]


#This code gets f ready as in permutations

#### reduced list x ####

# index of sigma
# answers - which rankings actually were counted and where are they in lex order?
ind <- match(unique(ranks),s) %>% sort()

multi <-function(x = permutation(),y = permutation(),i=integer()){
  x*y[i]
} 


invS3 <- inverse(S3)
sigma <- list()
#X <- matrix(nrow = 120, ncol = 120) #This is not the Cayley table, a large list.
for(i in 1:length(ind)){
  sigma[[i]] <- multi(invS3,S3[ind,],i)
}

S3[ind,]
