#################################################################
##          Script producing the biplots of Chapter 2          ##
#################################################################
library(ggplot2)
library(viridis)
library(latex2exp)
#Run import.R, then wrangling.R to get PrefData
#Alternatively, run:
PrefData <- readRDS("./data/PrefData.RData")

#### Setup ####
X <- t(PrefData)
X[,1:10]

nrow(X)

#centre of sphere
c <- ((nrow(X)+1)/2)*c(rep(1,nrow(X)))
c

#C is a matrix of c repeated
C <- matrix(rep(c,ncol(X)), ncol = ncol(X))

W <- (X-C)%*%t(X-C)/ncol(X)

g_1 <- eigen(W)$vectors[,1]
g_2 <- eigen(W)$vectors[,2]

XG <- cbind(t(X)%*%g_1,t(X)%*%g_2)


#make the poles
cG <- cbind(t(c)%*%g_1,t(c)%*%g_2)

poles <- matrix(rep(1+nrow(X)/2,nrow(X)^2), ncol = nrow(X))

#replace with 1 in ith place
for(i in 1:nrow(X)){
  poles[i,i] <- 1
}
poles

long_p <-(poles - c)*sqrt((nrow(X)+1)/3)+c

#annotation pole end

long_p_G <- cbind(t(long_p)%*%g_1,t(long_p)%*%g_2)

          
polesG <- cbind(t(poles)%*%g_1,t(poles)%*%g_2)



##### code for the plot ####

#Alternative to name the poles by the sushi name
#pol <- as.data.frame(cbind(origin,long_p_G,sushinames))

ggplot(XG, aes(x=V1, y=V2) )+
  geom_point(alpha=0.1)+
  geom_segment(aes(x=V1,y=V2,xend=V3,yend=V4),
               data=pol,
               arrow = arrow(angle = 10, length = unit(0.1, "inches"),
                             ends = "last", type = "closed"))+
  annotate("text", x=pol$V3*(1+0.25*1/(sqrt((pol$V3)^2+(pol$V4)^2))),
           y=pol$V4*(1+0.25*1/(sqrt((pol$V3)^2+(pol$V4)^2))),
           label = pol$V5)+
  labs(title = "The biplot of the sushi preferences distribution shows some clustering.",
       x = TeX("Projection onto $g_{(1)}$"),
       y = TeX("Projection onto $g_{(2)}$"))+
  coord_fixed()



#### heatmap plot ####
#raster
ggplot(XG, aes(x=V1, y=V2) )+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_viridis(option="turbo",name="Density")+
  scale_x_continuous(expand = expansion(mult=0,add=0),breaks=seq(-8,8,2)) +
  scale_y_continuous(expand = expansion(mult=0,add=0),breaks=seq(-8,8,2)) +
  geom_segment(aes(x=V1,y=V2,xend=V3,yend=V4),
               color = "white",
               data=pol,
               arrow = arrow(angle = 10, length = unit(0.1, "inches"),
                             ends = "last", type = "closed"))+
  annotate("text", x=pol$V3*(1+0.25*1/(sqrt((pol$V3)^2+(pol$V4)^2))),
           y=pol$V4*(1+0.25*1/(sqrt((pol$V3)^2+(pol$V4)^2))),
           label= pol$V5,color="white") +
   labs(title = "The biplot heatmap displays three clear clusters.",
        x = TeX("Projection onto $g_{(1)}$"),
        y = TeX("Projection onto $g_{(2)}$"))+
  coord_fixed()





