#################################################################
##             Code to produce a 2D polytope-ogram             ##
#################################################################
library(tidyverse)
library(data.table)
library(latex2exp)
#2D plots polytope-ogram
#Use test2.txt to plot 

#### Formatting the data ####
# Here we need to retain the frequency, which will affect the circle radius.
# Therefore script is different from import.R

test2 <- read_lines("./data/test2.txt")

#swap so last number is frequency
TestList <- sapply(strsplit(test2, ": "), function(x) paste(x[2], x[1], sep = ","))

for(i in 1:length(TestList)){
  TestList[i] <- strsplit(TestList[[i]],",")
}
TestList

##### NEED TO CHANGE TO RANK ORDER BEFORE PLOTTING
TestList[[1]]


unlist(TestList)[3]


TestPref <- vector(mode = "list",length = 6)

length(TestList[[1]])

TestList[[1]]
length(TestList)

for(i in 1:length(TestList)){
  for(j in 1:3){
    k <- as.numeric(unlist(TestList[[i]])[j])
    TestPref[[i]][k] <- j
  }
  TestPref[[i]][4] <- TestList[[i]][4] #keep the 4th one the same as it's the freq
}

# Has this worked?
TestList[[5]]
TestPref[5]
#Yes, it is different for the two 3-cycles [4] and [5] :)

TestData <- data.table(do.call(rbind,TestPref))

names(TestData) <- c(paste0("Item",1:3),"Frequency")

#### Code for plot ####
TestData <- as.matrix(sapply(TestData, as.numeric))

TestData.2d <- matrix(nrow = 6, ncol = 3)

c_3 <- c(2,2,2) #the centre
a_1 <- c(1,3,2) #first new axis
(c(2,1,3)+c(1,2,3))*0.5
a_2 <- c(1.5,1.5,3) #second new axis

i<-1
for(i in 1:6){
  TestData.2d[i,1] <- (TestData[i,-4]-c_3)%*%(a_1-c_3)/sqrt((a_1-c_3)%*%(a_1-c_3))
  TestData.2d[i,2] <- (TestData[i,-4]-c_3)%*%(a_2-c_3)/sqrt((a_2-c_3)%*%(a_2-c_3))
  TestData.2d[i,3] <- TestData[i,4]
}

#vertices of hexagon
pts <- rbind(c(sqrt(2)/2,sqrt(3/2)),c(sqrt(2),0),c(sqrt(2)/2,-sqrt(3/2)),
             c(-sqrt(2)/2,-sqrt(3/2)),c(-sqrt(2),0), c(-sqrt(2)/2,sqrt(3/2)))

#Line segments
edges <- cbind(pts[,1],pts[,2],pts[c(2,3,4,5,6,1),1],pts[c(2,3,4,5,6,1),2])

#creating the circles
circ <- data.frame(x0 = TestData.2d[,1],
                   y0 = TestData.2d[,2],
                   r = TestData.2d[,3]^(5/7)*signif(max(TestData.2d[,3])^(-5/7)*sqrt(2)/6,1))


ggplot() +
  #These are the edges
  geom_segment(aes(x = edges[,1], y = edges[,2], xend = edges[,3], yend = edges[,4]))+
  #Poles
  geom_segment(aes(x=0,y=0,xend=0,yend=-sqrt(2)),color="blue",
               arrow = arrow(angle = 10, length = unit(0.1, "inches"),
                             ends = "last", type = "closed"))+
  annotate("text", x=0,y=-sqrt(2)-0.2,label= 3,color="blue") +
  geom_segment(aes(x=0,y=0,xend=-sqrt(6)/2,yend=sqrt(2)/2),color="blue",
               arrow = arrow(angle = 10, length = unit(0.1, "inches"),
                             ends = "last", type = "closed"))+
  annotate("text", x=-(sqrt(3)/2)*(sqrt(2)+0.2),y=(sqrt(2)+0.2)/2,label= 2,color="blue") +
  geom_segment(aes(x=0,y=0,xend=sqrt(6)/2,yend=sqrt(2)/2),color="blue",
               arrow = arrow(angle = 10, length = unit(0.1, "inches"),
                             ends = "last", type = "closed"))+
  annotate("text", x=(sqrt(3)/2)*(sqrt(2)+0.2),y=(sqrt(2)+0.2)/2,label= 1,color="blue") +
  #Circles
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r),
                       fill= "#0000FF",alpha = 0.5,
                       data = circ) +
  scale_x_continuous() +
  #theme(panel.grid.minor = element_blank()) +
  coord_fixed() +
  labs( title = "Poltope-ogram in 2 dimensions",
    x=TeX("$\\hat{\\alpha}_{1}$"),
       y=TeX("$\\hat{\\alpha}_{2}$"))


####### Example of use ####
sum(Dia1[,4])
#This is the example from Diaconis' book.
Dia1 <- rbind(
  c(1,2,3,242),
  c(1,3,2,28),
  c(2,1,3,170),
  c(3,1,2,628),
  c(2,3,1,12),
  c(3,2,1,359)
)

Dia1.2d <- matrix(nrow = 6, ncol = 3)

for(i in 1:6){
  Dia1.2d[i,1] <- (Dia1[i,-4]-c_3)%*%(a_1-c_3)/sqrt((a_1-c_3)%*%(a_1-c_3))
  Dia1.2d[i,2] <- (Dia1[i,-4]-c_3)%*%(a_2-c_3)/sqrt((a_2-c_3)%*%(a_2-c_3))
  Dia1.2d[i,3] <- Dia1[i,4]
}

circ.df <- data.frame(x0 = Dia1.2d[,1],
                      y0 = Dia1.2d[,2],
                      r = Dia1.2d[,3]^(5/7)*signif(max(Dia1.2d[,3])^(-5/7)*sqrt(2)/3,1))

ggplot() +
  #These are the edges
  geom_segment(aes(x = edges[,1], y = edges[,2], xend = edges[,3], yend = edges[,4]))+
  #Poles
  geom_segment(aes(x=0,y=0,xend=0,yend=-sqrt(2)),color="blue",
               arrow = arrow(angle = 10, length = unit(0.1, "inches"),
                             ends = "last", type = "closed"))+
  annotate("text", x=0,y=-sqrt(2)-0.2,label= "country",color="blue") +
  geom_segment(aes(x=0,y=0,xend=-sqrt(6)/2,yend=sqrt(2)/2),color="blue",
               arrow = arrow(angle = 10, length = unit(0.1, "inches"),
                             ends = "last", type = "closed"))+
  
  annotate("text", x=-(sqrt(3)/2)*(sqrt(2)+0.2),y=(sqrt(2)+0.2)/2,label= "suburbs",color="blue") +
  geom_segment(aes(x=0,y=0,xend=sqrt(6)/2,yend=sqrt(2)/2),color="blue",
               arrow = arrow(angle = 10, length = unit(0.1, "inches"),
                             ends = "last", type = "closed"))+
  annotate("text", x=(sqrt(3)/2)*(sqrt(2)+0.2),y=(sqrt(2)+0.2)/2,label= "city",color="blue") +
  #Circles
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r),
                       fill= "#0000FF",alpha = 0.5,
                       data = circ.df) +
  scale_x_continuous() +
  #theme(panel.grid.minor = element_blank()) +
  coord_fixed() +
  labs( title = "There is a clear preference for suburban or country living",
        x=TeX("$\\hat{\\alpha}_{1}$"),
        y=TeX("$\\hat{\\alpha}_{2}$"))+
  theme_void()


