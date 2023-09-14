#Make test data file

#Use test2.txt to plot 

#### DATA REFORMATTING
test4 <- read_lines("./data/test4.txt")

#swap so last number is frequency
TestList <- sapply(strsplit(test4, ": "), function(x) paste(x[2], x[1], sep = ","))

for(i in 1:length(TestList)){
  TestList[i] <- strsplit(TestList[[i]],",")
}
TestList


##### NEED TO CHANGE TO RANK ORDER BEFORE PLOTTING
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
#Yes, it is different for the two 3-cycles [4] and [5] :)

library(data.table)
TestData <- data.table(do.call(rbind,TestPref))

names(TestData) <- c(paste0("Item",1:4),"Frequency")

#### CHANGING COORDINATES to the 'PLANE'


library(tidyverse)


TestData <- as.matrix(sapply(TestData, as.numeric))
TestData



m <- 4

c_4 <- c(rep(1,m))*(m+1)*0.5

#with the square on the top
a_3 <- (c(2,4,3,1)+c(1,4,3,2)+c(1,3,4,2)+c(2,3,4,1))/4
a_1 <- (c(2,4,1,3)+c(1,3,2,4)+c(1,4,2,3)+c(2,3,1,4))/4
a_2 <- (c(1,2,4,3)+c(1,2,3,4)+c(2,1,3,4)+c(2,1,4,3))/4

#### POLES ####
#This part optional but get the poles working...
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



library(plotly)
library(tidyverse)
TestData.3d

x1 <- TestData.3d[,1]
x2 <- TestData.3d[,2]
x3 <- TestData.3d[,3]
x1_old <- TestData.3d[,4]
x2_old <- TestData.3d[,5]
x3_old <- TestData.3d[,6]
x4_old <- TestData.3d[,7]
f <- TestData.3d[,8]

fig <- plot_ly(data = TestData.3d,type="scatter3d",
               x=~V1,y=~V2,z=~V3,mode="markers",
               #color =~Frequency,
               size = ~Frequency,
               marker = list(symbol = 'circle', sizemode = 'diameter'),
               hoverinfo = 'text',
               text = ~paste(V4,V5,V6,V7,'<br> Freq: ',Frequency))

##### Starting afresh...one command at a time ####

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
                         name = "Pole 1",
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
                         name = "Pole 2",
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
                         name = "Pole 3",
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
                         name = "Pole 4",
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

fig <- fig %>% layout( title = "Polytope-ogram projected in 3D",
                       #showlegend=FALSE,
                       scene = list(xaxis=list(showticklabels=FALSE,showgrid=FALSE,zeroline=FALSE,title=""),
                       yaxis=list(showticklabels=FALSE,showgrid=FALSE,zeroline=FALSE,title=""),
                       zaxis=list(showticklabels=FALSE,showgrid=FALSE,zeroline=FALSE,title="")))
fig

############### END #################
fig <- plot_ly(type="scatter3d",
               x= x1,y= x2,z= x3,mode="markers",
               color = f,
               size = f,
               marker = list(symbol = 'circle', sizemode = 'area'),
               hoverinfo = 'text',
               text = paste(x1_old,x2_old,x3_old,x4_old,'<br> Freq: ',f)) %>%

  #pole 1
 add_trace(x = c(pole.3d$V1[5],pole.3d$V1[1]),
            y =  c(pole.3d$V2[5],pole.3d$V2[1]),
            z =  c(pole.3d$V3[5],pole.3d$V3[1]),
            type = "scatter3d",
            mode = "lines",
            marker = list(opacity = 0,color = '#26908C'),
            #opacity=0,
            color='#26908C',
            name = "lines",
            hoverinfo = "none",
            line=list(color='#26908C',width=9)) %>%
  add_trace(type= "cone",
            sizemode = 'scaled', sizeref = 0.25,
            hoverinfo = "text",
            text = ~paste("Pole of 1"),
            x= pole.3d$V1[1], y= pole.3d$V2[1], z= pole.3d$V3[1],
            u= pole.3d$V1[1], v= pole.3d$V2[1], w= pole.3d$V3[1]) %>%
  hide_colorbar() %>%
  layout( title = "Polytope-ogram projected in 3D",
          showlegend=FALSE,
          scene = list(xaxis=list(showticklabels=FALSE,title=""),
                       yaxis=list(showticklabels=FALSE,title=""),
                       zaxis=list(showticklabels=FALSE,title=""),),
          )


fig


TestData.3d$V1



##### POLES END ####

plot_ly(pole.3d, showlegend = FALSE) %>%
  #pole 1
  add_trace(x = ~c(V1[5],V1[1]),
            y =  ~c(V2[5],V2[1]),
            z =  ~c(V3[5],V3[1]),
            type = "scatter3d",
            mode = "lines",          
            name = "lines",
            hoverinfo = "none",
            line=list(color='#26908C',width=9)) %>%
  add_trace(type= "cone", sizemode = 'scaled', sizeref = 0.25,
            hoverinfo = "text",
            text = ~paste("Pole of 1"),
            x= ~c(V1[1]), y= ~c(V2[1]), z= ~c(V3[1]),
            u= ~c(V1[1]), v= ~c(V2[1]), w= ~c(V3[1])) %>%
  #pole 2
  add_trace(x = ~c(V1[5],V1[2]),
            y =  ~c(V2[5],V2[2]),
            z =  ~c(V3[5],V3[2]),
            type = "scatter3d",
            mode = "lines",          
            name = "lines",
            hoverinfo = "none",
            line=list(color='#26908C',width=9)) %>%
  add_trace(type= "cone", sizemode = 'scaled', sizeref = 0.25,
            hoverinfo = "text",
            text = ~paste("Pole of 2"),
            x= ~c(V1[2]), y= ~c(V2[2]), z= ~c(V3[2]),
            u= ~c(V1[2]), v= ~c(V2[2]), w= ~c(V3[2]))  %>%
  #pole 3
  add_trace(x = ~c(V1[5],V1[3]),
            y =  ~c(V2[5],V2[3]),
            z =  ~c(V3[5],V3[3]),
            type = "scatter3d",
            mode = "lines",          
            name = "lines",
            hoverinfo = "none",
            line=list(color='#26908C',width=9)) %>%
  add_trace(type= "cone", sizemode = 'scaled', sizeref = 0.25,
            hoverinfo = "text",
            text = ~paste("Pole of 3"),
            x= ~c(V1[3]), y= ~c(V2[3]), z= ~c(V3[3]),
            u= ~c(V1[3]), v= ~c(V2[3]), w= ~c(V3[3]))  %>%
  #pole 4
  add_trace(x = ~c(V1[5],V1[4]),
            y =  ~c(V2[5],V2[4]),
            z =  ~c(V3[5],V3[4]),
            type = "scatter3d",
            mode = "lines",          
            name = "lines",
            hoverinfo = "none",
            line=list(color='#26908C',width=9)) %>%
  add_trace(type= "cone", sizemode = 'scaled', sizeref = 0.25,
            hoverinfo = "text",
            text = ~paste("Pole of 4"),
            x= ~c(V1[4]), y= ~c(V2[4]), z= ~c(V3[4]),
            u= ~c(V1[4]), v= ~c(V2[4]), w= ~c(V3[4])) %>%
  hide_colorbar()
  


# plot_ly() %>%
# add_trace(type= "cone",sizemode = 'scaled',sizeref = 0.25,
#           x= 1, y= 1, z= 1,
#           u= 1, v= 1, w= 0) 

# add_trace(x = aaa, y = bbb, z = ccc, type = "scatter3d", mode = "lines", 
#           name = "lines", showlegend = FALSE) %>%
#   add_trace(x = aaaa, y = bbbb, z = cccc, type = "scatter3d", mode = "lines", 
#             name = "lines", showlegend = FALSE) %>%
#   add_trace(x = aaaaa, y = bbbbb, z = ccccc, type = "scatter3d", mode = "lines", 
#             name = "lines", showlegend = FALSE) %>%


# layout(autosize = T, margin=list( l = 50, r = 50, b = 100, t = 100,  pad = 4)) %>%
#   layout(title = 'Age at diagnosis distribution by time', font=list(size = 20)) %>%
#   layout(xaxis = list(title = 'Year of diagnosis'), yaxis = list(title = 'Count')) %>%
#   layout( xaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)),
#           yaxis = list(titlefont = list(size = 22), tickfont = list(size = 22)) )




# 
# fig <- fig %>% layout(uniformtext=list(minsize=8, mode='hide'))
# fig

# hoverinfo = 'text',
# text = ~paste('</br> Species: ', Species,
#               '</br> Petal Length: ', Petal.Length,
#               '</br> Petal Width: ', Petal.Width)




# aaa <- c(16, 12)
# bbb <- c(12, 12)
# ccc <- c(14, 14)
# 
# aaaa <- c(18, 11)
# bbbb <- c(14, 14)
# cccc <- c(8, 8)
# 
# aaaaa <- c(42, 21.5)
# bbbbb <- c(25, 12.5)
# ccccc <- c(26.5, 26)
# 
# trace_pole1 <- list( mode = "lines", name = "1", type = "scatter3d", 
#   x = pole.3d[c(1,4),1], 
#   y = pole.3d[c(1,4),2], 
#   z = pole.3d[c(1,4),3], showlegend = FALSE
# )

# plot_ly() %>%
#   add_trace(x = aaa, y = bbb, z = ccc, type = "scatter3d", mode = "lines", 
#             name = "lines", showlegend = FALSE) %>%
#   add_trace(x = aaaa, y = bbbb, z = cccc, type = "scatter3d", mode = "lines", 
#             name = "lines", showlegend = FALSE) %>%
#   add_trace(x = aaaaa, y = bbbbb, z = ccccc, type = "scatter3d", mode = "lines", 
#             name = "lines", showlegend = FALSE) %>%
#   add_trace(x = ap, y = bp, z = cp, marker = m1, type = "scatter3d", mode = "text+markers", 
#             name = "projected", linetypes = NULL, text = rnn) %>%
#   add_trace(x = a, y = b, z = c, marker = m, type = "scatter3d", mode = "text+markers", 
#             name = "original", linetypes = NULL, text = rnn)





# trace_pole1 <- list(
#   line = list(width = 9), 
#   mode = "lines", 
#   name = "1", 
#   type = "scatter3d", 
#   x = pole.3d[c(1,4),1], 
#   y = pole.3d[c(1,4),2], 
#   z = pole.3d[c(1,4),3], 
#   marker = list(line = list(color = "darkslateblue")), 
#   showlegend = FALSE
# )
# trace_arrow1 <- list(
#   mode = "lines", 
#   name = "", 
#   type = "cone", 
#   u = pole.3d[1,1], 
#   v = pole.3d[2,1], 
#   w = pole.3d[3,1], 
#   x = pole.3d[1,1], 
#   y = pole.3d[2,1], 
#   z = pole.3d[3,1], 
#   cauto = FALSE, 
#   sizeref = 0.5, 
#   sizemode = "absolute", 
#   hoverinfo = "Pole 1", 
#   showscale = FALSE, 
#   colorscale = list(c(0, "#fff5f0"),list(0.125, "#fee0d2"),list(0.25, "#fcbba1"),list(0.375, "#fc9272"),list(0.5, "#fb6a4a"),list(0.625, "#ef3b2c"),list(0.75, "#cb181d"),list(0.875, "#a50f15"),list(1, "#67000d")), 
#   reversescale = FALSE, 
#   hovertemplate = "", 
#   autocolorscale = FALSE
# )



####CODE FOR THE LINE SEGMENTS####

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
  rbind(
    c(1,2,3,4),
    c(2,1,3,4),
    c(3,1,2,4),
    c(3,2,1,4),
    c(2,3,1,4),
    c(1,3,2,4),
    c(1,2,3,4))
# #1+2-3+4-
# #+-+-
# pmpm <-
#   rbind(
#     c(1,4,2,3),
#     c(1,3,2,4),
#     c(2,3,1,4),
#     c(2,4,1,3),
#     c(1,4,2,3))
# #+--+
# pmmp <-
#   rbind(
#     c(1,3,4,2),
#     c(1,4,3,2),
#     c(2,4,3,1),
#     c(2,3,4,1),
#     c(1,3,4,2))
# #-+-+
# mpmp <-
#   rbind(
#     c(4,2,3,1),
#     c(4,1,3,2),
#     c(3,1,4,2),
#     c(3,2,4,1),
#     c(4,2,3,1))
# #-++-
# mppm <-
#   rbind(
#     c(3,1,2,4),
#     c(4,1,2,3),
#     c(4,2,1,3),
#     c(3,2,1,4),
#     c(3,1,2,4))
# #--++
# mmpp <-
#   rbind(
#     c(4,3,2,1),
#     c(3,4,2,1),
#     c(3,4,1,2),
#     c(4,3,1,2),
#     c(4,3,2,1))
# #++--
# ppmm <-
#   rbind(
#     c(2,1,4,3),
#     c(2,1,3,4),
#     c(1,2,3,4),
#     c(1,2,4,3),
#     c(2,1,4,3))

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

# pmpm.3d <- matrix(nrow=5,ncol=3)
# for(i in 1:5){
#   pmpm.3d[i,1] <- (pmpm[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
#   pmpm.3d[i,2] <- (pmpm[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
#   pmpm.3d[i,3] <- (pmpm[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
# }
# 
# pmmp.3d <- matrix(nrow=5,ncol=3)
# for(i in 1:5){
#   pmmp.3d[i,1] <- (pmmp[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
#   pmmp.3d[i,2] <- (pmmp[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
#   pmmp.3d[i,3] <- (pmmp[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
# }
# 
# mpmp.3d <- matrix(nrow=5,ncol=3)
# for(i in 1:5){
#   mpmp.3d[i,1] <- (mpmp[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
#   mpmp.3d[i,2] <- (mpmp[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
#   mpmp.3d[i,3] <- (mpmp[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
# }
# 
# mppm.3d <- matrix(nrow=5,ncol=3)
# for(i in 1:5){
#   mppm.3d[i,1] <- (mppm[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
#   mppm.3d[i,2] <- (mppm[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
#   mppm.3d[i,3] <- (mppm[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
# }
# 
# mmpp.3d <- matrix(nrow=5,ncol=3)
# for(i in 1:5){
#   mmpp.3d[i,1] <- (mmpp[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
#   mmpp.3d[i,2] <- (mmpp[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
#   mmpp.3d[i,3] <- (mmpp[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
# }
# 
# 
# ppmm.3d <- matrix(nrow=5,ncol=3)
# for(i in 1:5){
#   ppmm.3d[i,1] <- (ppmm[i,]-c_4)%*%(a_1 -c_4)/sqrt((a_1-c_4)%*%(a_1-c_4))
#   ppmm.3d[i,2] <- (ppmm[i,]-c_4)%*%(a_2 -c_4)/sqrt((a_2-c_4)%*%(a_2-c_4))
#   ppmm.3d[i,3] <- (ppmm[i,]-c_4)%*%(a_3 -c_4)/sqrt((a_3-c_4)%*%(a_3-c_4))
# }

onep.3d <- as.data.frame(onep.3d)
twop.3d <- as.data.frame(twop.3d)
threep.3d <- as.data.frame(threep.3d)
fourp.3d <- as.data.frame(fourp.3d)
onem.3d <- as.data.frame(onem.3d)
twom.3d <- as.data.frame(twom.3d)
threem.3d <- as.data.frame(threem.3d)
fourm.3d <- as.data.frame(fourm.3d)
