##################################################################
##            This script contains code experiments             ##
##       which reproduce results in the paper of Diaconis       ##
##################################################################
library(permutations)

S5 <- as.word(allperms(5))
print_word(S5)

S5 <- S5[120:1]

#print_word(S5)


invS5 <- inverse(S5)

#APA data
f <- c(29 ,67 ,37 ,24 ,43 ,28 ,57 ,49 ,22 ,22 ,34 ,26 ,54 ,44 ,26 ,
       24 ,35 ,50 ,50 ,46 ,25 ,19 ,11 ,29 ,31 ,54 ,34 ,24 ,38 ,30 ,
       91 ,84 ,30 ,35 ,38 ,35 ,58 ,66 ,24 ,51 ,52 ,40 ,50 ,45 ,31 ,
       23 ,22 ,16 ,71 ,61 ,41 ,27 ,45 ,36 ,107,133,62 ,28 ,87 ,35 ,
       41 ,64 ,34 ,75 ,82 ,74 ,30 ,34 ,40 ,42 ,30 ,34 ,35 ,34 ,40 ,
       21 ,106,79 ,63 ,53 ,44 ,28 ,162,96 ,45 ,52 ,53 ,52 ,186,172,
       36 ,42 ,24 ,26 ,30 ,40 ,40 ,35 ,36 ,17 ,70 ,50 ,52 ,48 ,51 ,
       24 ,70 ,45 ,35 ,28 ,37 ,35 ,95 ,102,34 ,35 ,29 ,27 ,28 ,30 )

types <- shape(S5)

#S5[32]
#S5[33]
#should both be 3,2 right? Biggest first...

#lapply(types[32], sort, decreasing = TRUE)

for(i in 1:120){
  types[i] <- lapply(types[i], sort, decreasing = TRUE)
}

#types[32]
#types[33]

#So now I can tell if I have a match

(types[[32]][1]=="3")&((types[[32]][2]=="2"))

delta.12 <- c(rep(0,90),rep(1,6),rep(0,18),rep(1,6))



length(types[[32]])

length(types[[120]])


#types[[35]][2] == "2" # careful!
#types[[2]]
#types[which(sapply(types, '[',1) == 3 & sapply(types, '[',2) == 2)]


chi <- vector(length = 120)
chi[which(sapply(types, '[',1) == 1 & is.na(sapply(types, '[',2)) )] <- 5
chi[which(sapply(types, '[',1) == 2 & is.na(sapply(types, '[',2)) )] <- 1
chi[which(sapply(types, '[',1) == 2 & sapply(types, '[',2) == 2)] <- 1
chi[which(sapply(types, '[',1) == 3 & is.na(sapply(types, '[',2)) )] <- -1
chi[which(sapply(types, '[',1) == 3 & sapply(types, '[',2) == 2)] <- 1
chi[which(sapply(types, '[',1) == 4)] <- -1
chi[which(sapply(types, '[',1) == 5)] <- 0
S5

types[117]


#g<- f[match(inverses,inrevlex)]

#match[unclass(print.default(invS5)),unclass(print.default(S5))]

#(chi %*% g)*5/120
#(chi %*% f) * 5/120

#need to come up with a better way

#invS5 * S5[33]
x<- unclass((invS5))

listx <- lapply(seq_len(nrow(x)), function(i) x[i,])
 
s<-  lapply(seq_len(120), function(i) unclass((S5))[i,])
match(listx, s) 

proj <- vector(length = 120)

for(i in 1:120){
  x <- lapply(seq_len(120), function(j) unclass(print.default(invS5*S5[i]))[j,])
  s <- lapply(seq_len(120), function(j) unclass(print.default(S5))[j,])
  proj[i] <- (chi %*% f[match(x,s)])*5/120
}

proj %*% delta.12
#IT WORKS!!!!! I get -137... amazing


chi.41 <- vector(length = 120)
chi.41[which(sapply(types, '[',1) == 1 & is.na(sapply(types, '[',2)) )] <- 4
chi.41[which(sapply(types, '[',1) == 2 & is.na(sapply(types, '[',2)) )] <- 2
chi.41[which(sapply(types, '[',1) == 2 & sapply(types, '[',2) == 2)] <- 0
chi.41[which(sapply(types, '[',1) == 3 & is.na(sapply(types, '[',2)) )] <- 1
chi.41[which(sapply(types, '[',1) == 3 & sapply(types, '[',2) == 2)] <- -1
chi.41[which(sapply(types, '[',1) == 4)] <- 0
chi.41[which(sapply(types, '[',1) == 5)] <- -1

f_proj <- vector(length = 120)

for(i in 1:120){
  x <- lapply(seq_len(120), function(j) unclass(print.default(invS5*S5[i]))[j,])
  s <- lapply(seq_len(120), function(j) unclass(print.default(S5))[j,])
  f_proj[i] <- (chi.41 %*% f[match(x,s)])*chi.41[120]/120
}


(f_proj %*% f_proj /120)*(16/25) #rescaling as got the chisq(id) factorincorrect.
#298.255 works

chi.32 <- vector(length = 120)
chi.32[which(sapply(types, '[',1) == 1 & is.na(sapply(types, '[',2)) )] <- 5
chi.32[which(sapply(types, '[',1) == 2 & is.na(sapply(types, '[',2)) )] <- 1
chi.32[which(sapply(types, '[',1) == 2 & sapply(types, '[',2) == 2)] <- 1
chi.32[which(sapply(types, '[',1) == 3 & is.na(sapply(types, '[',2)) )] <- -1
chi.32[which(sapply(types, '[',1) == 3 & sapply(types, '[',2) == 2)] <- 1
chi.32[which(sapply(types, '[',1) == 4)] <- -1
chi.32[which(sapply(types, '[',1) == 5)] <- 0

f_proj.32 <- vector(length = 120)

s <- lapply(seq_len(120), function(j) unclass(print.default(S5))[j,])

for(i in 1:120){
  x <- lapply(seq_len(120), function(j) unclass(print.default(invS5*S5[i]))[j,])
  f_proj.32[i] <- (chi.32 %*% f[match(x,s)])*chi.32[120]/120
}

(f_proj.32 %*% f_proj.32 /120)
#459.1535

X <- list()
#X <- matrix(nrow = 120, ncol = 120) #This is not the Cayley table, no it isn't
for(i in 1:120){
  X[[i]] <- multi(invS5,S5,i)
}

X[[1]][1]
X[13][1]
X[[13]][1]

Y <- list()
for(i in 1:120){
  Y[[i]] <- lapply(seq_len(120), function(j) unclass(X[[i]])[j,])
}

#Seems like do not need the print.default command - might save time.
match(Y[[1]],s)

(X[[1]])

############ REWRITING ##############

multi <-function(x = permutation(),y = permutation(),i=integer()){
   x*y[i]
}  

X <- list()
#X <- matrix(nrow = 120, ncol = 120) #This is not the Cayley table, a large list.
for(i in 1:120){
  X[[i]] <- multi(invS5,S5,i)
}

#Perhaps could be combined into first loop
Y <- list()
for(i in 1:120){
  Y[[i]] <- lapply(seq_len(120), function(j) unclass(X[[i]])[j,])
}

temp_s <- lapply(seq_len(120), function(j) unclass((S5))[j,])

f_tilde <- vector(length = 120)
for(i in 1:120){
  f_tilde[i] <- (chi.32 %*% f[match(Y[[i]],temp_s)])*chi.32[120]/120
}

(f_tilde %*% f_tilde /120)

#THIS IS SO MUCH FASTER AND BETTER.

# TRY IN ONE LOOP

temp_s <- lapply(seq_len(120), function(j) unclass((S5))[j,])
f_tilde <- vector(length = 120)
X <- list(length = 120)
for(i in 1:120){
  X[[i]] <- multi(invS5,S5,i)
  X[[i]] <- lapply(seq_len(120), function(j) unclass(X[[i]])[j,])
  f_tilde[i] <- (chi.32 %*% f[match(X[[i]],temp_s)])*chi.32[120]/120
}
(f_tilde %*% f_tilde /120)

# IT IS AMAZING.

## OK let's check the rest


chi.311 <- vector(length = 120)
chi.311[which(sapply(types, '[',1) == 1 & is.na(sapply(types, '[',2)) )] <- 6
chi.311[which(sapply(types, '[',1) == 2 & is.na(sapply(types, '[',2)) )] <- 0
chi.311[which(sapply(types, '[',1) == 2 & sapply(types, '[',2) == 2)] <- -2
chi.311[which(sapply(types, '[',1) == 3 & is.na(sapply(types, '[',2)) )] <- 0
chi.311[which(sapply(types, '[',1) == 3 & sapply(types, '[',2) == 2)] <- 0
chi.311[which(sapply(types, '[',1) == 4)] <- 0
chi.311[which(sapply(types, '[',1) == 5)] <- 1

f_tilde <- vector(length = 120)
#X <- list(length = 120)
for(i in 1:120){
  #X[[i]] <- multi(invS5,S5,i) #Only need to run this once...
  #X[[i]] <- lapply(seq_len(120), function(j) unclass(X[[i]])[j,]) # as above
  f_tilde[i] <- (chi.311 %*% f[match(X[[i]],temp_s)])*chi.311[120]/120
}
(f_tilde %*% f_tilde /120)
#78.2075

chi.221 <- vector(length = 120)
chi.221[which(sapply(types, '[',1) == 1 & is.na(sapply(types, '[',2)) )] <- 5
chi.221[which(sapply(types, '[',1) == 2 & is.na(sapply(types, '[',2)) )] <- -1
chi.221[which(sapply(types, '[',1) == 2 & sapply(types, '[',2) == 2)] <- 1
chi.221[which(sapply(types, '[',1) == 3 & is.na(sapply(types, '[',2)) )] <- -1
chi.221[which(sapply(types, '[',1) == 3 & sapply(types, '[',2) == 2)] <- -1
chi.221[which(sapply(types, '[',1) == 4)] <- 1
chi.221[which(sapply(types, '[',1) == 5)] <- 0

for(i in 1:120){
  #X[[i]] <- multi(invS5,S5,i) #Only need to run this once...
  #X[[i]] <- lapply(seq_len(120), function(j) unclass(X[[i]])[j,]) # as above
  f_tilde[i] <- (chi.221 %*% f[match(X[[i]],temp_s)])*chi.221[120]/120
}
(f_tilde %*% f_tilde /120)
#27.20208

chi.211 <- vector(length = 120)
chi.211[which(sapply(types, '[',1) == 1 & is.na(sapply(types, '[',2)) )] <- 4
chi.211[which(sapply(types, '[',1) == 2 & is.na(sapply(types, '[',2)) )] <- -2
chi.211[which(sapply(types, '[',1) == 2 & sapply(types, '[',2) == 2)] <- 0
chi.211[which(sapply(types, '[',1) == 3 & is.na(sapply(types, '[',2)) )] <- 1
chi.211[which(sapply(types, '[',1) == 3 & sapply(types, '[',2) == 2)] <- 1
chi.211[which(sapply(types, '[',1) == 4)] <- 0
chi.211[which(sapply(types, '[',1) == 5)] <- -1

for(i in 1:120){
  #X[[i]] <- multi(invS5,S5,i) #Only need to run this once...
  #X[[i]] <- lapply(seq_len(120), function(j) unclass(X[[i]])[j,]) # as above
  f_tilde[i] <- (chi.211 %*% f[match(X[[i]],temp_s)])*chi.211[120]/120
}
(f_tilde %*% f_tilde /120)
#6.830556

chi.11111 <- vector(length = 120)
chi.11111[which(sapply(types, '[',1) == 1 & is.na(sapply(types, '[',2)) )] <- 1
chi.11111[which(sapply(types, '[',1) == 2 & is.na(sapply(types, '[',2)) )] <- -1
chi.11111[which(sapply(types, '[',1) == 2 & sapply(types, '[',2) == 2)] <- 1
chi.11111[which(sapply(types, '[',1) == 3 & is.na(sapply(types, '[',2)) )] <- 1
chi.11111[which(sapply(types, '[',1) == 3 & sapply(types, '[',2) == 2)] <- -1
chi.11111[which(sapply(types, '[',1) == 4)] <- -1
chi.11111[which(sapply(types, '[',1) == 5)] <- 1

for(i in 1:120){
  #X[[i]] <- multi(invS5,S5,i) #Only need to run this once...
  #X[[i]] <- lapply(seq_len(120), function(j) unclass(X[[i]])[j,]) # as above
  f_tilde[i] <- (chi.11111 %*% f[match(X[[i]],temp_s)])*chi.11111[120]/120
}
(f_tilde %*% f_tilde /120)

#0.2177778

#Fantastic
