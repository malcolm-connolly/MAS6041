library(ggplot2)
library(ggforce)
library(stringr)
library(utils)

#### table of marginals ####

#initially zero
df <- data.frame(
  first = rep(0,10),
  second = rep(0,10),
  third = rep(0,10),
  fourth = rep(0,10),
  fifth = rep(0,10),
  sixth = rep(0,10),
  seventh = rep(0,10),
  eighth = rep(0,10),
  ninth = rep(0,10),
  tenth = rep(0,10)
)

for(j in 1:10){
  for(i in 1:10){
    #counts the number of times the jth column (for example fifth)
    #equals a particular value i (for example 7)
    #df[7,5] is the number of times item 7 is ranked fifth
    df[i,j] <- sum(SushiData[,j] == i)
  }
}

df

# first second third fourth fifth sixth seventh eighth ninth tenth
# 1    550    529   569    545   502   521     485    491   418   390
# 2    404    773   797    737   688   580     416    318   176   111
# 3    228    322   442    524   645   674     662    607   524   372
# 4    747    651   480    377   305   316     284    319   542   979
# 5    545    652   633    591   462   423     395    364   505   430
# 6    206    179   274    340   444   477     653    773   846   808
# 7   1713   1028   638    431   333   240     211    188   118   100
# 8    113    255   437    584   697   764     841    698   479   132
# 9     36     66   123    170   235   402     547    830  1111  1480
# 10   458    545   607    701   689   603     506    412   281   198



df_pc <- data.frame(
  first = rep(0,10),
  second = rep(0,10),
  third = rep(0,10),
  fourth = rep(0,10),
  fifth = rep(0,10),
  sixth = rep(0,10),
  seventh = rep(0,10),
  eighth = rep(0,10),
  ninth = rep(0,10),
  tenth = rep(0,10)
)

for(j in 1:10){
  for(i in 1:10){
    df_pc[i,j] <- sum(SushiData[,j] == i)*0.02 #(table of percentages 100/5000)
  }
}

df_pc

# first second third fourth fifth sixth seventh eighth ninth tenth
# 1  11.00  10.58 11.38  10.90 10.04 10.42    9.70   9.82  8.36  7.80
# 2   8.08  15.46 15.94  14.74 13.76 11.60    8.32   6.36  3.52  2.22
# 3   4.56   6.44  8.84  10.48 12.90 13.48   13.24  12.14 10.48  7.44
# 4  14.94  13.02  9.60   7.54  6.10  6.32    5.68   6.38 10.84 19.58
# 5  10.90  13.04 12.66  11.82  9.24  8.46    7.90   7.28 10.10  8.60
# 6   4.12   3.58  5.48   6.80  8.88  9.54   13.06  15.46 16.92 16.16
# 7  34.26  20.56 12.76   8.62  6.66  4.80    4.22   3.76  2.36  2.00
# 8   2.26   5.10  8.74  11.68 13.94 15.28   16.82  13.96  9.58  2.64
# 9   0.72   1.32  2.46   3.40  4.70  8.04   10.94  16.60 22.22 29.60
# 10  9.16  10.90 12.14  14.02 13.78 12.06   10.12   8.24  5.62  3.96

#I would like to make a plot of this as in Marsden pg 19.

M <- data.frame(
  first = rep(0,10),
  second = rep(0,10),
  third = rep(0,10),
  fourth = rep(0,10),
  fifth = rep(0,10),
  sixth = rep(0,10),
  seventh = rep(0,10),
  eighth = rep(0,10),
  ninth = rep(0,10),
  tenth = rep(0,10)
)

for(j in 1:10){
  for(i in 1:10){
    M[i,j] <- sum(SushiData[,j] == i)/5000 
  }
}

M

names(M)[1]

circ <- data.frame(x0 = rep(1:10, 10),
                   y0 = rep(1:10, each = 10),
                   r = unlist(as.list(t(M))))

ggplot() +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r),
                       data = circ) +
  scale_x_continuous(name="Preference order",position="top",breaks=1:10,
                     labels=stringr::str_to_title(names(M))) +
  scale_y_reverse(name="Sushi item",breaks=1:10) +
  theme(panel.grid.minor = element_blank()) +
  coord_fixed() +
  scale_fill_gradient(labels = scales::percent)+
  labs(fill='Marginal',title="Sushi items are not uniformly ranked",
       caption="Respondents ranked item 7 highly, item 9 poorly, and were divided by item 4.")



ggplot() +
  #draws the circles
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r),
                       data = circ) +
  #Moves x axis to top and controls tick labels.
  scale_x_continuous(name="Preference order",
                     position="top",
                     breaks=1:10,
                     labels=stringr::str_to_title(names(M))) +
  #reverse scale, and ensure a tick for each item.
  scale_y_reverse(name="Item",breaks=1:10) +
  #major grid lines only
  theme(panel.grid.minor = element_blank()) +
  #ensure square
  coord_fixed() +
  #colour in the circles
  scale_fill_gradient(labels = scales::percent) +
  #label the legend
  labs(fill="Marginal")


#### pairs ####
#Work out how to create a pairs table?

#proportion of people who prefer item i to item j

df_pair <- data.frame(
  One = rep(0,10),
  Two = rep(0,10),
  Three = rep(0,10),
  Four = rep(0,10),
  Five = rep(0,10),
  Six = rep(0,10),
  Seven = rep(0,10),
  Eight = rep(0,10),
  Nine = rep(0,10),
  Ten = rep(0,10)
)

#Makes more sense to me to count this row by row using the lines, and perhaps a regex
#SushiList

#str_detect(SushiList[i],glob2rx("*4*7*9*")) == TRUE
#if(str_detect(SushiList[i],glob2rx("*4*7*9*")) == TRUE){
#  ctr_479 <- ctr_479 + 1
#}




ctr <- 0
#There is probably a more clever / quick way than nested loops
#And the double string check to avoid the 1/10 issue
for(i in 1:10){
  for(j in 1:10){
    for(k in 1:length(SushiList)){
      if(str_detect(SushiList[k],glob2rx(paste0("*",i,",*",j,"$")))==TRUE||
         str_detect(SushiList[k],glob2rx(paste0("*",i,",*",j,",*")))==TRUE){
        ctr <- ctr + 1
      }
    }
    df_pair[i,j] <- ctr
    ctr <- 0
  }
}

names(df_pair) <- c("1","2","3","4","5","6","7","8","9","10")
df_pair

#       1    2    3    4    5    6    7    8    9   10
# 1     0 2152 2848 2570 2428 3449 1420 2859 3785 2373
# 2  2848    0 3395 2809 2701 3808 1285 3625 4347 2823
# 3  2152 1605    0 2316 2025 3117 1131 2503 3847 1815
# 4  2430 2191 2684    0 2413 2941 1421 2645 3311 2338
# 5  2572 2299 2975 2587    0 3349 1477 2964 3754 2541
# 6  1551 1192 1883 2059 1651    0  893 1779 3288 1427
# 7  3580 3715 3869 3579 3523 4107    0 4101 4414 3557
# 8  2141 1375 2497 2355 2036 3221  899    0 4103 1932
# 9  1215  653 1153 1689 1246 1712  586  897    0  777
# 10 2627 2177 3185 2662 2459 3573 1443 3068 4223    0


#Peter's CHECK
df_pair + t(df_pair)





# testing
# a<- 2
# b<- 1
# paste0("*",a,",*",b,"*")
# SushiList[768]
# 
# 
# str_detect(SushiList[768],glob2rx(paste0("*",a,",*",b,"$")))
# str_detect(SushiList[768],glob2rx(paste0("*",a,",*",b,",*")))
# 
# #Pattern locator
# SushiList[str_detect(SushiList[],glob2rx("*1*2*10*"))]

# Could exclude this by counting separately
# Ends 1, or does not end 1
#   "*2,*1$" or "*2,*1,*"







#### Plot function####


#2D and 3D separate, higher dimensions should project to 2d

#do it in base R first then use ggplot?

# do the projection first and work out the mathematical details

test <- read_lines("./data/test.txt")

testdf <- test %>%
  str_replace_all(","," ") %>%
  read_table(col_names = c("First","Second","Third"))

do.call(rbind.data.frame, testdf)

testdf

X <- t(testdf)

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

XG <- cbind(t(X)%*%g_1,t(X)%*%g_2)
par(pty="s")
plot(XG[,1],XG[,2])


g_1%*%g_1
g_1%*%g_2 #basically zero


# can I include the poles?
#line segment from centre c to pt with 1 in ith place and 1+m/2 elsewhere

cG <- cbind(t(c)%*%g_1,t(c)%*%g_2) #which is new origin essentially anyway...(0,0)


poles <- matrix(rep(1+nrow(X)/2,nrow(X)^2), ncol = nrow(X))

#replace with 1 in ith place
for(i in 1:nrow(X)){
  poles[i,i] <- 1
}
poles

long_p <-((poles - c)/(sqrt(1.5)))*sqrt(2)+c
#can be derived in general
long_p_G <- cbind(t(long_p)%*%g_1,t(long_p)%*%g_2)


polesG <- cbind(t(poles)%*%g_1,t(poles)%*%g_2)

plot(XG[,1],XG[,2],xlim=c(-sqrt(2),sqrt(2)))
arrows(x0=cG[1],y0=cG[2],x1=long_p_G[,1],y1=long_p_G[,2])
text(x=long_p_G[,1],y=long_p_G[,2],1:nrow(X))

text(x=XG[,1],y=XG[,2],c("123","132","213","231","312","321"))
#This is correct
#However for poles to mean the correct thing, need to reshape dataset
#see wrangling.R
#####
#Should be near pole 1 => 1 is ranked higher. 
#Need to change from 2,3,1 where item 2 ranked 1st, etc
#to 3,1,2 where item is field and preference is data

#Readme from Toshihiro says top item is most preferred one.

SushiList[[1]]

s <- strsplit(SushiList[[1]],",")
#"7"  "4"  "5"  "1"  "10" "2"  "8"  "3"  "9"  "6" 
unlist(s)[3]


p<- vector(mode="list",length=1)


for(i in 1:10){
  m <- as.numeric(unlist(s)[i])
  p[[1]][m] <- i
}

SushiSplit <- vector(mode = "list", length = 5000)

for(i in 1:length(SushiList)){
  SushiSplit[i] <- strsplit(SushiList[[i]],",")
}

SushiPref <- vector(mode = "list", length = 5000)

length(SushiSplit[[1]])
SushiSplit[1]


for(i in 1:length(SushiSplit)){
  for(j in 1:length(SushiSplit[[i]]) ){
    k <- as.numeric(unlist(SushiSplit[[i]])[j])
    SushiPref[[i]][k] <- j
  }
}

# Has this worked?
SushiList[[1]]
SushiPref[1]


library(data.table)
PrefData <- data.table(do.call(rbind,SushiPref))

#YES!!!
#check
SushiData[251,]
PrefData[251,]

names(PrefData) <- c(paste0("Item",1:10))

####To do####
#do in ggplot2



#in ggplot2
# with a nice density colour and poles


# Numerical av. ranking
round(colMeans(PrefData))
summary(PrefData)
median(PrefData)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(PrefData[,1:10])
# Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9 Item10
# 4     6     8     2     3    10     1     7     9      5

which(PrefData$Item9==1)

filter(PrefData,(Item7==1)&
         (Item4==2)&
         (Item5==3)&
         (Item1==4)&
         (Item10==5)&
         (Item2==6)&
         (Item8==7)&
         (Item3==8)&
         (Item9==9)&
         (Item6==10))
#There are three such ranks

PrefData[1:9,]
#7 is in 1 or 3
#4 is in 1 or 2
#5 is in 3 or 2

# ALTERNATIVE NAME 1: ebi (shrimp)
# ALTERNATIVE NAME 2: anago (sea eel)
# ALTERNATIVE NAME 3: maguro (tuna)
# ALTERNATIVE NAME 4: ika (squid)
# ALTERNATIVE NAME 5: uni (sea urchin)
# ALTERNATIVE NAME 6: sake (salmon roe)
# ALTERNATIVE NAME 7: tamago (egg)
# ALTERNATIVE NAME 8: toro (fatty tuna)
# ALTERNATIVE NAME 9: tekka-maki (tuna roll)
# ALTERNATIVE NAME 10: kappa-maki (cucumber roll)
sum(M[5,c(1,2,3,9,10)])
