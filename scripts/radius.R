library(tidyverse)
SplitFreq <- sapply(strsplit(TextRaw, ": "), function(x) paste(x[2], x[1], sep = ","))


for(i in 1:length(SplitFreq)){
  SplitFreq[i] <- strsplit(SplitFreq[[i]],",")
}
SplitFreq


SushiPrefRad <- vector(mode = "list", length = length(SplitFreq))


length(SplitFreq) #4926
length(SplitFreq[1]) #1
length(SplitFreq[[1]]) #11


for(i in 1:length(SplitFreq)){
  for(j in 1:length(SplitFreq[[i]])-1 ){
    k <- as.numeric(unlist(SplitFreq[[i]])[j])
    SushiPrefRad[[i]][k] <- j
  }
  SushiPrefRad[[i]][11] <- SplitFreq[[i]][11]
}


SplitFreq[[1]]
SushiPrefRad[[1]]

PrefRad <- (do.call(rbind.data.frame,SushiPrefRad))

names(PrefRad) <- c(paste0("Item",1:10),"Frequency")


##### Test 2

test2 <- read_lines("./data/test2.txt")

SplitFreq.tst <- sapply(strsplit(test2, ": "), function(x) paste(x[2], x[1], sep = ","))

for(i in 1:length(SplitFreq.tst)){
  SplitFreq.tst[i] <- strsplit(SplitFreq.tst[[i]],",")
}
SplitFreq.tst

PrefRad.tst <- vector(mode = "list", length = length(SplitFreq.tst))

for(i in 1:length(SplitFreq.tst)){
  for(j in 1:length(SplitFreq.tst[[i]])-1 ){
    k <- as.numeric(unlist(SplitFreq.tst[[i]])[j])
    PrefRad.tst[[i]][k] <- j
  }
  PrefRad.tst[[i]][4] <- SplitFreq.tst[[i]][4]
}

PrefRad.tst <- data.frame(do.call(rbind,PrefRad.tst))

names(PrefRad.tst) <- c(paste0("Item",1:3),"Frequency")

# Calculations for test 2, aim for ggplot2 #####

test2df <- PrefRad.tst[,1:3]

X.t2 <- t(test2df)

X.t2 <- matrix(as.numeric(X.t2), ncol=nrow(test2df))


ncol(test2df)
nrow(X.t2) 
#value of m from df, same but may want in terms of X

c <- ((nrow(X.t2)+1)/2)*c(rep(1,nrow(X.t2)))
c

#X_bar <- matrix(rep(x_bar,n), ncol=n)
C_rep <- matrix(rep(c,ncol(X.t2)), ncol = ncol(X.t2))

W <- (X.t2-C_rep)%*%t(X.t2-C_rep)/ncol(X.t2)

g_1 <- eigen(W)$vectors[,1]
g_2 <- eigen(W)$vectors[,2]

#XA <- cbind(t(X_scaled)%*%pc1, t(X_scaled)%*%pc2)
#plot(XA[,1],XA[,2])

#XG <- cbind(t(X.t2)%*%g_1,t(X.t2)%*%g_2)

#plot(XG[,1],XG[,2])

dfx <- data.frame(x1 = t(X.t2)%*%g_1, y1 = t(X.t2)%*%g_2, f1 = as.numeric(PrefRad.tst[,4])^(5/7))
par(pty="s")
with(dfx, symbols(x= dfx$x1, y= dfx$y1, circles= dfx$f1, inches=1/8,
                  ann=F, bg="steelblue2"))

# can I include the poles?
#line segment from centre c to pt with 1 in ith place and 1+m/2 elsewhere

cG <- cbind(t(c)%*%g_1,t(c)%*%g_2) #which is new origin essentially anyway...(0,0)


poles <- matrix(rep(1+nrow(X.t2)/2,nrow(X.t2)^2), ncol = nrow(X.t2))

#replace with 1 in ith place
for(i in 1:nrow(X.t2)){
  poles[i,i] <- 1
}
poles

polesG <- cbind(t(poles)%*%g_1,t(poles)%*%g_2)


library(shape)
#install.packages("shape")

Arrows(x0=cG[1],y0=cG[2],x1=polesG[,1],y1=polesG[,2],arr.type="triangle")

#remotes::install_github("JosephCrispell/basicPlotteR")
library(basicPlotteR)

addTextLabels(x=polesG[,1],y=polesG[,2],1:nrow(X.t2),col.label = "black",lty = 0)

addTextLabels(x=XG[,1],y=XG[,2],c("123","132","213","231","312","321"),col.label = "black",lty = 0)

PrefRad.tst

#joining with a line? (not very data-scientific)
#colour code by size? could compare to size of ball for display DONE
# S4 test? displaying 3 eigenvalues in 2D later
#ggplot2

#Actual data







