test <- read_lines("./data/test3.txt")

testdf <- test %>%
  str_replace_all(","," ") %>%
  read_table(col_names = c("First","Second","Third"))

do.call(rbind.data.frame, testdf)

testdf

X <- t(testdf[,1:3])

ncol(testdf)
nrow(X) 
#value of m from df, same but may want in terms of X

c <- ((nrow(X)+1)/2)*c(rep(1,nrow(X)))
c

#X_bar <- matrix(rep(x_bar,n), ncol=n)
C_rep <- matrix(rep(c,ncol(X)), ncol = ncol(X))

W <- (X-C_rep)%*%t(X-C_rep)/ncol(X)

g_1 <- eigen(W)$vectors[,1]
g_2 <- eigen(W)$vectors[,2]

#XA <- cbind(t(X_scaled)%*%pc1, t(X_scaled)%*%pc2)
#plot(XA[,1],XA[,2])

dfx <- data.frame(x1 = t(X)%*%g_1, x2 = t(X)%*%g_2, x3 = testdf$freq)
par(pty="s")
with(dfx, symbols(x= dfx$x1, y= dfx$x2, circles= dfx$x3, inches=1/8,
                  ann=F,
                  #bg="steelblue2"))
                  bg = rgb(0.27, 0.51, 0.71, 0.4)))

#looked up steelblue rgb values on internet...
# Nice :)

#plot(XG[,1],XG[,2], pch = 19, col = rgb(0, 0, 0, 0.3))
#Not terribly clear which ones are 1,2 and 3 etc...
#perhaps in combination with size it would be clearer... ?


#Can manually append a column of frequencies, 
#could be similar to loop replicating rankings

testdf<- cbind(testdf,c(2,2,2,2,1,1,3,3,3,1))
names(testdf)[4] <- "freq"
