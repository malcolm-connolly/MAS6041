library(permutations)

S10 <- as.word(allperms(10))

S10 <- S10[362880:1]
#head(S10)
#saveRDS(S10, file="./data/S10.RData")
#S10 <- readRDS("./data/S10.RData")

#types10 <- shape(S10)
#for(i in 1:length(types10)){
#  types10[i] <- lapply(types10[i], sort, decreasing = TRUE)
#}

#head(types10)

x<- unclass((invS10))

listx <- lapply(seq_len(nrow(x)), function(i) x[i,])

s<-  lapply(seq_len(nrow(S10)), function(i) unclass((S10))[i,])
match(listx,s)

#s <- lapply(seq_len(362880), function(j) unclass(S10)[j,])
#print_word(S5)


#write.table(my_data, file = “my_data.txt”, sep = “”)

#saveRDS(s, file="./data/s.RData")

invS10 <- inverse(S10)

#saveRDS(invS10, file="./data/invS10.RData")
invS10 <- readRDS("./data/invS10.RData")

# Need a vector 10! long in revlex order with the frequencies of Sushi rankings
# 
f <- PrefRad$Frequency #not in revlex order, not correct length

#remove duplicates from prefdata? unique(), use sushipref list

SushiRanks <- unique(SushiPref)
saveRDS(SushiRanks, file="./data/SushiRanks.RData")
SushiRanks <- readRDS("./data/SushiRanks.RData")
s <- readRDS("./data/s.RData")

toString(s)


order(PrefRad,PrefRad[,1],PrefRad[,2])

PrefRad[order(PrefRad$Item1,PrefRad$Item2,PrefRad$Item3,PrefRad$Item4,PrefRad$Item5,
              PrefRad$Item6,PrefRad$Item7,PrefRad$Item8,PrefRad$Item9,PrefRad$Item10,
              decreasing = TRUE),-11]

PrefRad %>%
  group_by(PrefRad$Item1,PrefRad$Item2,PrefRad$Item3,PrefRad$Item4,PrefRad$Item5,
           PrefRad$Item6,PrefRad$Item7,PrefRad$Item8,PrefRad$Item9,PrefRad$Item10)
  
install.packages("collapse")
library(collapse)

d <- collap(PrefRad[,-11], ~ Item1 + Item2 + Item3 + Item4 + Item5 +
       Item6 + Item7 + Item8 + Item9 + Item10, toString)

match(d,toString(s))
m <- paste0(s)
indices <- vector(length=362880)
for(i in 1:362880){
  indices[i] <- match(paste(unlist(SushiRanks[[i]])),unlist(s[i]))
}

library(tidyverse)
strsplit(SushiRanks[[i]]," ")

# SushiRanks[[4]][1]
# for(i in 1:length(SushiRanks)){
#   for(j in 1:10){
#     SushiRanks[[i]][j] <- str_remove(SushiRanks[[i]][j]," ")
#   }
# }
# 
# for(i in 1:length(s)){
#   for(j in 1:10){
#     s[[i]][j] <- str_remove(s[[i]][j]," ")
#   }
# }

match(SushiRanks,s)

s[1]
SushiRanks[[1]]
match(SushiRanks,s)

SushiRanks[1]
s[]
#it just does not get them all, some are NA which is not possible as s exhaustive. 


# Making X
# This will not work, need to be more clever here...
# X <- list(length = 362880)
# for(i in 1:362880){
#   X[[i]] <- multi(invS10,S10,i)
#   X[[i]] <- lapply(seq_len(362880), function(j) unclass(X[[i]])[j,])
# }
# 
# saveRDS(X, file="./data/X.RData")

#f is zero fairly often... can I use this?


