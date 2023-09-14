#Run import.R, then wrangling.R to get PrefData

#Just use PrefData and transparency
#Sizes can be added later if necessary / required
# Might do a heatmap or something instead so...


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

#check
#g_1%*%g_1
#g_1%*%g_2

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

#annG
          
polesG <- cbind(t(poles)%*%g_1,t(poles)%*%g_2)

# library(shape)
# #install.packages("shape")
# 
# par(pty="s")
# plot(XG[,1],XG[,2],pch = 19, col = rgb(0, 0, 0, 0.10), ann=F)
# 
# Arrows(x0=cG[1],y0=cG[2],x1=long_p_G[,1],y1=long_p_G[,2],arr.type="triangle",
#        arr.width=0.1,arr.length =0.2)
# 
# library(basicPlotteR)
# 
# addTextLabels(x=long_p_G[,1],y=long_p_G[,2],1:nrow(X),col.label = "black",lty = 0)

##### ggplot
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(latex2exp)
#install.packages("latex2exp")

# basic scatter in ggplot with/without poles
XG <- as.data.frame(XG)

origin <- matrix(rep(cG,nrow(long_p_G)),ncol=2,byrow=TRUE)
pol <- as.data.frame(cbind(origin,long_p_G,1:nrow(long_p_G)))

#Alternative
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
           #label= sushinames) +
  labs(title = "The biplot of the sushi preferences distribution shows some clustering.",
       x = TeX("Projection onto $g_{(1)}$"),
       y = TeX("Projection onto $g_{(2)}$"))+
  coord_fixed()

#annotations were on the arrowhead so added a quarter of a unit vector
#in direction of the arrow
#pol[,3:4]/sqrt((pol$V3)^2+(pol$V4)^2)


#annotate()
#aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df 
#arrow(angle = 30, length = unit(0.25, "inches"),
#      ends = "last", type = "open")



# density plots
#raster
ggplot(XG, aes(x=V1, y=V2) )+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  #scale_fill_viridis()+
  scale_fill_viridis(option="turbo",name="Density")+
  #option="magma","plasma","inferno","cividis","mako","rocket","turbo"
  #scale_fill_viridis_b()+
  #scale_fill_distiller(palette= "RdYlBu", direction=-1)+
  #Diverging: BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
  scale_x_continuous(expand = expansion(mult=0,add=0),breaks=seq(-8,8,2)) +
  scale_y_continuous(expand = expansion(mult=0,add=0),breaks=seq(-8,8,2)) +
  # theme(
  #   legend.position='none'
  # ) +
  #coord_cartesian()+
  #coord_fixed(xlim = c(-8.5,8.5),ylim=c(-8.5,8.5))
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



#contourplot
#There is clustering visible in the data
# ggplot(XG, aes(x=V1, y=V2) ) +
#   scale_y_continuous(expand = expansion(mult=0,add=0),breaks=seq(-8,8,2))+
#   scale_x_continuous(expand = expansion(mult=0,add=0),breaks=seq(-8,8,2))+
#   geom_density_2d()+
#   coord_fixed(xlim = c(-9,8.5),ylim=c(-8.5,8))




#Categorical variables...
names(PrefData)

Experiment <- PrefData %>%
  mutate(fourandfive = case_when(
    ((Item4<=3)&(Item7<=2)&(Item9>=8)) ~0,
    ((Item2<Item4)&(Item7==1)&(Item9>=8)&(Item4>Item8)) ~1,
    ((Item4>=5)&(Item5>=9)&(Item7<3)&(Item2<3)&(Item9<10)) ~2
  ))

XG2 <- data.frame(cbind(XG,Experiment[,11]))
#XG2 <- XG2 %>%filter(!is.na(fourandfive))

ggplot(XG2, aes(x=V1, y=V2, color = as.factor(fourandfive)) ) +
  geom_point(alpha=0.5 ) +
  coord_fixed()

filter(PrefData,(Item7==1)&(Item9==10))

PrefData[(Item9==1),]
which(PrefData$Item9==1)

#what is the most common top 3, in any order?
#Does that explain the cluster?

#3 from 9 is 84 choices... no
#9C2 is 36... possible

#This is a bonus plot not the issue.

#gut feeling 7,4,5 top 3
# and minor cluster is 7&2 top 2 or 7,2,10
#last minor cluster may be 7&10...?

#larger cluster is approx circle centred (-7.5,2.25) radius 0.5

cluster1<- PrefData[which( (XG$V1+7.5)^2+(XG$V2-2.25)^2<0.5^2),]
summary(cluster1)

cluster2<- PrefData[which( (XG$V1+4.25)^2+(XG$V2+4)^2<0.5^2),]

cluster3<- PrefData[which( (XG$V1+2)^2+(XG$V2+7)^2<0.5^2),]

summary(cluster1)
summary(cluster2)
summary(cluster3)

cluster1[(Item4<Item2)&(Item),]
cluster3[(Item4>7)&(Item5>7)&(Item7<4)&(Item2<4),]
PrefData[(Item4>7)&(Item5>7)&(Item7<4)&(Item2<4),]
