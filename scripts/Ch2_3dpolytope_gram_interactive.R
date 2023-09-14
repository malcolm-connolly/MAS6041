#################################################################
##             This script produces an interactive             ##
##                         3D polytope                         ##
#################################################################
library(tidyverse)
library(plotly)

#### data import ####
#https://www.preflib.org/static/data/netflix/00004-00000101.soc
# ALTERNATIVE NAME 1: The Wedding Planner
# ALTERNATIVE NAME 2: Entrapment
# ALTERNATIVE NAME 3: Lost in Translation
# ALTERNATIVE NAME 4: The Exorcist

url <- "https://www.preflib.org/static/data/netflix/00004-00000101.soc"

destination <- paste0("data/",basename(url))

download.file(url,
              destination,
              method = "libcurl",
              mode="wb")


# Reading the downloaded file into R. The ranks start on 17th line.
Netflix <- read_lines(destination,skip=16)

TestList <- sapply(strsplit(Netflix, ": "), function(x) paste(x[2], x[1], sep = ","))

for(i in 1:length(TestList)){
  TestList[i] <- strsplit(TestList[[i]],",")
}
TestList

##### reformatting to rank order from ranked lists ####
TestList[[1]]


unlist(TestList)[3]


TestPref <- vector(mode = "list",length = 24)

length(TestList[[1]])

TestList[[1]]
length(TestList)

for(i in 1:length(TestList)){
  for(j in 1:4){
    k <- as.numeric(unlist(TestList[[i]])[j])
    TestPref[[i]][k] <- j
  }
  TestPref[[i]][5] <- TestList[[i]][5] #keep the 5th one the same as it's the freq
}

# Has this worked?
TestList[[5]]
TestPref[5]
#Yes
#library(data.table)
#TestData <- data.table(do.call(rbind,TestPref))
TestData <- lapply(TestPref, as.numeric)
TestData <- as.matrix.data.frame(do.call(rbind,TestData))

TestData[TestData[,1]==2,]

names(TestData) <- c("The Wedding Planner",
                     "Entrapment",
                     "Lost in Translation",
                     "The Exorcist",
                     "Frequency")
names(TestData)[1]
# ALTERNATIVE NAME 1: The Wedding Planner
# ALTERNATIVE NAME 2: Entrapment
# ALTERNATIVE NAME 3: Lost in Translation
# ALTERNATIVE NAME 4: The Exorcist

##### CODE FOR PLOT ####

m <- 4

c_4 <- c(rep(1,m))*(m+1)*0.5

#with the square on the top
a_3 <- (c(2,4,3,1)+c(1,4,3,2)+c(1,3,4,2)+c(2,3,4,1))/4
a_1 <- (c(2,4,1,3)+c(1,3,2,4)+c(1,4,2,3)+c(2,3,1,4))/4
a_2 <- (c(1,2,4,3)+c(1,2,3,4)+c(2,1,3,4)+c(2,1,4,3))/4

#### POLES

poles <- matrix(rep(1+4/2,4^2), ncol = 4)

#replace with 1 in ith place
for(i in 1:4){
  poles[i,i] <- 1
}
poles

#re scale to length=radius of sphere
poles <-(poles - c_4)*sqrt((4+1)/3)+c_4

#annotation pole end
pole.3d <- matrix(nrow = 5, ncol = 3)

for(i in 1:4){
  pole.3d[i,1] <- (poles[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  pole.3d[i,2] <- (poles[i,]-c_4)%*%(a_2-c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  pole.3d[i,3] <- (poles[i,]-c_4)%*%(a_3-c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
}

pole.3d[5,] <- 0
pole.3d
pole.3d <- as.data.frame(pole.3d)
pole.3d



TestData.3d <- matrix(nrow = 24, ncol = 8)


for(i in 1:24){
  TestData.3d[i,1] <- (TestData[i,-5]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  TestData.3d[i,2] <- (TestData[i,-5]-c_4)%*%(a_2-c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  TestData.3d[i,3] <- (TestData[i,-5]-c_4)%*%(a_3-c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
  TestData.3d[i,4] <- TestData[i,1] #first old coord
  TestData.3d[i,5] <- TestData[i,2] #second old coord
  TestData.3d[i,6] <- TestData[i,3] #third old coord
  TestData.3d[i,7] <- TestData[i,4] #fourth old coord
  TestData.3d[i,8] <- TestData[i,5] #frequency
}


TestData.3d
TestData.3d <- as.data.frame(TestData.3d)
names(TestData.3d)[8] <- "Frequency"


#### MAKING SEGMENTS FOR FACES OF POLYTOPE 

#1+
onep <-
  rbind(
    c(1,2,3,4),
    c(1,3,2,4),
    c(1,4,2,3),
    c(1,4,3,2),
    c(1,3,4,2),
    c(1,2,4,3),
    c(1,2,3,4))

#2+
twop <-
  rbind(
    c(2,1,3,4),
    c(2,1,4,3),
    c(3,1,4,2),
    c(4,1,3,2),
    c(4,1,2,3),
    c(3,1,2,4),
    c(2,1,3,4))
#3+
threep <-
  rbind(
    c(2,3,1,4),
    c(3,2,1,4),
    c(4,2,1,3),
    c(4,3,1,2),
    c(3,4,1,2),
    c(2,4,1,3),
    c(2,3,1,4))
#4+
fourp <-
  rbind(
    c(2,4,3,1),
    c(3,4,2,1),
    c(4,3,2,1),
    c(4,2,3,1),
    c(3,2,4,1),
    c(2,3,4,1),
    c(2,4,3,1))
#1-
onem <-
  rbind(
    c(4,3,1,2),
    c(4,2,1,3),
    c(4,1,2,3),
    c(4,1,3,2),
    c(4,2,3,1),
    c(4,3,2,1),
    c(4,3,1,2))
#2-
twom <-
  rbind(
    c(2,4,1,3),
    c(3,4,1,2),
    c(3,4,2,1),
    c(2,4,3,1),
    c(1,4,3,2),
    c(1,4,2,3),
    c(2,4,1,3))
#3-
threem <-
  rbind(
    c(1,3,4,2),
    c(1,2,4,3),
    c(2,1,4,3),
    c(3,1,4,2),
    c(3,2,4,1),
    c(2,3,4,1),
    c(1,3,4,2))
#4-
fourm <-
  rbind(c(1,2,3,4),
    c(2,1,3,4),
    c(3,1,2,4),
    c(3,2,1,4),
    c(2,3,1,4),
    c(1,3,2,4),
    c(1,2,3,4))


onep.3d <- matrix(nrow = 7,ncol=3)
for(i in 1:7){
  onep.3d[i,1] <- (onep[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  onep.3d[i,2] <- (onep[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  onep.3d[i,3] <- (onep[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
}

twop.3d <- matrix(nrow = 7,ncol=3)
for(i in 1:7){
  twop.3d[i,1] <- (twop[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  twop.3d[i,2] <- (twop[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  twop.3d[i,3] <- (twop[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
}

threep.3d <- matrix(nrow = 7,ncol=3)
for(i in 1:7){
  threep.3d[i,1] <- (threep[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  threep.3d[i,2] <- (threep[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  threep.3d[i,3] <- (threep[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
}

fourp.3d <- matrix(nrow = 7,ncol=3)
for(i in 1:7){
  fourp.3d[i,1] <- (fourp[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  fourp.3d[i,2] <- (fourp[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  fourp.3d[i,3] <- (fourp[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
}

onem.3d <- matrix(nrow = 7,ncol=3)
for(i in 1:7){
  onem.3d[i,1] <- (onem[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  onem.3d[i,2] <- (onem[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  onem.3d[i,3] <- (onem[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
}

twom.3d <- matrix(nrow = 7,ncol=3)
for(i in 1:7){
  twom.3d[i,1] <- (twom[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  twom.3d[i,2] <- (twom[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  twom.3d[i,3] <- (twom[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
}

threem.3d <- matrix(nrow = 7,ncol=3)
for(i in 1:7){
  threem.3d[i,1] <- (threem[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  threem.3d[i,2] <- (threem[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  threem.3d[i,3] <- (threem[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
}

fourm.3d <- matrix(nrow = 7,ncol=3)
for(i in 1:7){
  fourm.3d[i,1] <- (fourm[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
  fourm.3d[i,2] <- (fourm[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
  fourm.3d[i,3] <- (fourm[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
}


onep.3d <- as.data.frame(onep.3d)
twop.3d <- as.data.frame(twop.3d)
threep.3d <- as.data.frame(threep.3d)
fourp.3d <- as.data.frame(fourp.3d)
onem.3d <- as.data.frame(onem.3d)
twom.3d <- as.data.frame(twom.3d)
threem.3d <- as.data.frame(threem.3d)
fourm.3d <- as.data.frame(fourm.3d)


#### MAKING THE PLOT ####
# Run one line at a time

fig <- plot_ly()

fig <- fig %>% add_markers(data = TestData.3d, type="scatter3d",
                           x=~V1,y=~V2,z=~V3,mode="markers",
                           #color =~Frequency,
                           size = ~Frequency,
                           name = "Points",
                           showlegend=FALSE,
                           marker = list(symbol = 'circle',sizemode="diameter"),
                           hoverinfo = 'text',
                           text = ~paste(V4,V5,V6,V7,'<br> Freq: ',Frequency))
#pole 1
fig <- fig %>% add_trace(data= pole.3d,
                         x = ~c(V1[5],V1[1]),
                         y =  ~c(V2[5],V2[1]),
                         z =  ~c(V3[5],V3[1]),
                         type = "scatter3d",
                         mode = "lines",
                         #showlegend=FALSE,
                         name = names(TestData)[1],
                         hoverinfo = "none",
                         line=list(color='#004f5f',width=9))
#arrow head 1
fig <- fig %>% add_trace(data = pole.3d,
                         type= "cone",
                         sizemode = 'scaled', sizeref = 0.25,
                         hoverinfo = "text",
                         name = "Arrow 1",
                         colorscale = list(list(0,"#004f5f"),list(1,"#004f5f")),
                         text = ~paste("Pole of 1"),
                         x= ~c(V1[1]), y= ~c(V2[1]), z= ~c(V3[1]),
                         u= ~c(V1[1]), v= ~c(V2[1]), w= ~c(V3[1]))
#pole 2
fig <- fig %>% add_trace(data= pole.3d,
                         x = ~c(V1[5],V1[2]),
                         y =  ~c(V2[5],V2[2]),
                         z =  ~c(V3[5],V3[2]),
                         type = "scatter3d",
                         mode = "lines",
                         name = names(TestData)[2],
                         hoverinfo = "none",
                         line=list(color='#766aaf',width=9))
#arrow head 2
fig <- fig %>% add_trace(data = pole.3d,
                         type= "cone",
                         sizemode = 'scaled', sizeref = 0.25,
                         hoverinfo = "text",
                         name = "Arrow 2",
                         colorscale = list(list(0,"#766aaf"),list(1,"#766aaf")),
                         text = ~paste("Pole of 2"),
                         x= ~c(V1[2]), y= ~c(V2[2]), z= ~c(V3[2]),
                         u= ~c(V1[2]), v= ~c(V2[2]), w= ~c(V3[2]))

#pole 3
fig <- fig %>% add_trace(data= pole.3d,
                         x = ~c(V1[5],V1[3]),
                         y =  ~c(V2[5],V2[3]),
                         z =  ~c(V3[5],V3[3]),
                         type = "scatter3d",
                         mode = "lines",
                         name = names(TestData)[3],
                         hoverinfo = "none",
                         line=list(color='#38c7a6',width=9))
#arrow head 3
fig <- fig %>% add_trace(data = pole.3d,
                         type= "cone",
                         sizemode = 'scaled', sizeref = 0.25,
                         hoverinfo = "text",
                         name = "Arrow 3",
                         colorscale = list(list(0,"#38c7a6"),list(1,"#38c7a6")),
                         text = ~paste("Pole of 3"),
                         x= ~c(V1[3]), y= ~c(V2[3]), z= ~c(V3[3]),
                         u= ~c(V1[3]), v= ~c(V2[3]), w= ~c(V3[3]))
#pole 4
fig <- fig %>% add_trace(data= pole.3d,
                         x = ~c(V1[5],V1[4]),
                         y =  ~c(V2[5],V2[4]),
                         z =  ~c(V3[5],V3[4]),
                         type = "scatter3d",
                         mode = "lines",
                         name = names(TestData)[4],
                         hoverinfo = "none",
                         line=list(color='#36e9fe',width=9))
#arrow head 4
fig <- fig %>% add_trace(data = pole.3d,
                         type= "cone",
                         sizemode = 'scaled', sizeref = 0.25,
                         hoverinfo = "text",
                         name = "Arrow 3",
                         colorscale = list(list(0,"	#36e9fe"),list(1,"#36e9fe")),
                         text = ~paste("Pole of 4"),
                         x= ~c(V1[4]), y= ~c(V2[4]), z= ~c(V3[4]),
                         u= ~c(V1[4]), v= ~c(V2[4]), w= ~c(V3[4]))

fig <- fig %>% add_trace(x=~V1,y=~V2,z=~V3,
                         data = onep.3d,
                         type="scatter3d",
                         mode ="lines",showlegend = FALSE,
                         line = list(color ="#808080",width=4),
                         dash = "dashed",
                         opacity = 0.2,
                         hoverinfo = "none")

fig <- fig %>% add_trace(x=~V1,y=~V2,z=~V3,
                         data = twop.3d,
                         type="scatter3d",
                         mode ="lines",showlegend = FALSE,
                         line = list(color ="#808080",width=4),
                         dash = "dashed",
                         opacity = 0.2,
                         hoverinfo = "none")

fig <- fig %>% add_trace(x=~V1,y=~V2,z=~V3,
                         data = threep.3d,
                         type="scatter3d",
                         mode ="lines",showlegend = FALSE,
                         line = list(color ="#808080",width=4),
                         dash = "dashed",
                         opacity = 0.2,
                         hoverinfo = "none")

fig <- fig %>% add_trace(x=~V1,y=~V2,z=~V3,
                         data = fourp.3d,
                         type="scatter3d",
                         mode ="lines",showlegend = FALSE,
                         line = list(color ="#808080",width=4),
                         dash = "dashed",
                         opacity = 0.2,
                         hoverinfo = "none")

fig <- fig %>% add_trace(x=~V1,y=~V2,z=~V3,
                         data = onem.3d,
                         type="scatter3d",
                         mode ="lines",showlegend = FALSE,
                         line = list(color ="#808080",width=4),
                         dash = "dashed",
                         opacity = 0.2,
                         hoverinfo = "none")

fig <- fig %>% add_trace(x=~V1,y=~V2,z=~V3,
                         data = twom.3d,
                         type="scatter3d",
                         mode ="lines",showlegend = FALSE,
                         line = list(color ="#808080",width=4),
                         dash = "dashed",
                         opacity = 0.2,
                         hoverinfo = "none")

fig <- fig %>% add_trace(x=~V1,y=~V2,z=~V3,
                         data = threem.3d,
                         type="scatter3d",
                         mode ="lines",showlegend = FALSE,
                         line = list(color ="#808080",width=4),
                         dash = "dashed",
                         opacity = 0.2,
                         hoverinfo = "none")

fig <- fig %>% add_trace(x=~V1,y=~V2,z=~V3,
                         data = fourm.3d,
                         type="scatter3d",
                         mode ="lines",showlegend = FALSE,
                         line = list(color ="#808080",width=4),
                         dash = "dashed",
                         opacity = 0.2,
                         hoverinfo = "none")

fig <- fig %>% hide_colorbar() 

fig <- fig %>% layout( title = "Polytope-ogram of Netflix movie preferences",
                       legend = list(orientation = "h",valign = 'bottom',xanchor='bottom',yanchor='bottom',y=-0.1),
                       scene = list(camera = list(eye = list(x = 0.56, y = -1.17, z = 0.72)),
                                    projection = 'orthographic',
                       xaxis=list(showticklabels=FALSE,showgrid=FALSE,zeroline=FALSE,title=""),
                       yaxis=list(showticklabels=FALSE,showgrid=FALSE,zeroline=FALSE,title=""),
                       zaxis=list(showticklabels=FALSE,showgrid=FALSE,zeroline=FALSE,title="")))
                    
fig

############### END #################

### Optional deep magic connecting to Python environment to enable plot to export as pdf

library(reticulate)
reticulate::conda_list()

use_python("C:\\Users\\Malcolm\\anaconda3\\envs\\Rplots/python.exe")
Sys.setenv(RETICULATE_PYTHON="C:\\Users\\Malcolm\\anaconda3\\envs\\Rplots")
reticulate::import("kaleido")
kaleido()$transform(fig, "fig.pdf")
