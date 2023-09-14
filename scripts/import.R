library(tidyverse)

# Here we download the file which is freely available on the internet

url <- "https://www.preflib.org/static/data/sushi/00014-00000001.soc"

destination <- paste0("data/",basename(url))

download.file(url,
              destination,
              method = "libcurl",
              mode="wb")


# Reading the downloaded file into R. The ranks start on 23rd line.
TextRaw <- read_lines(destination,skip=22)



#This code, and later loop, changes the format of the text file/list
#From number: ranking
#To that ranking appearing that number of times.
#For example 3: 1,2,3,4,5,6,7,8,9,10
#To...
#1,2,3,4,5,6,7,8,9,10
#1,2,3,4,5,6,7,8,9,10
#1,2,3,4,5,6,7,8,9,10

#First separate and split off the colon and number from the ranking
Sep <- strsplit(TextRaw,": ")

#this is a list of lists.
#first string is the number of times that ranking occurs, second is the ranking
#example
Sep[[1]] 
Sep[[1]][1]
Sep[[1]][2]

#creating a vector in which to store the full list of rankings
SushiList <- vector(mode = "list", length = 5000)

# Require a counter to keep track of the index in the Sushi Data list
ctr <- 0
for(i in 1:length(Sep)){
  ctr <- ctr + 1 #start from 1
  for(j in 1:Sep[[i]][1]){#Sep[[i]][1] no. of times ranking occurs
    #the new list at index is ranking
    SushiList[[ctr]] <- Sep[[i]][2] 
    #increment counter each iteration of loop
    ctr <- ctr + 1
  }
  ctr <- ctr - 1 #adjusts for final run of inner loop
}

SushiList[1:10] #a big list created, just of rankings

#turning the list of rankings into a dataframe
#exchange the commas for spaces and turn it into a table then call rbind
SushiData <- SushiList %>%
str_replace_all(","," ") %>%
read_table(col_names = c("First","Second","Third","Fourth","Fifth","Sixth","Seventh","Eighth","Ninth","Tenth"))

do.call(rbind.data.frame, SushiData)


#Save the dataframe as an RData file for later use

#save(SushiData,file ="data/SushiData.RData")



#### matching particular items ####

#Here the list is more useful for matching patterns in the data
#We use 
SushiList[[1]]
library(utils)


SushiList[4]
#"4,5,7,2,10,3,8,1,6,9"
#Aim to detect pattern ...,1,...,2,...,3,..., so that 1,2 and 3 occur in order
#Anywhere within the permutation i.e. their ranking relative to eachother
#ignoring all other data.
#Uses regular expressions "^.*1.*2.*3"
# str_detect(SushiList[4],glob2rx("*1*2*3*"))
# #returns true/false value
# 
# 
# str_detect(SushiList[1],glob2rx("*1*2*3*"))
# 
# #Here are some counters for the number of rankings containing given permutation
# ctr_123 <-0
# ctr_132 <-0
# ctr_213 <-0
# ctr_231 <-0
# ctr_312 <-0
# ctr_321 <-0
# 
# for(i in 1:length(SushiList)){
#   if(str_detect(SushiList[i],glob2rx("*1*2*3*")) == TRUE){
#     ctr_123 <- ctr_123 + 1
#   } else if(str_detect(SushiList[i],glob2rx("*1*3*2*")) == TRUE){
#     ctr_132 <- ctr_132 + 1
#   } else if(str_detect(SushiList[i],glob2rx("*2*1*3*")) == TRUE){
#     ctr_213 <- ctr_213 + 1
#   } else if(str_detect(SushiList[i],glob2rx("*2*3*1*")) == TRUE){
#     ctr_231 <- ctr_231 + 1
#   } else if(str_detect(SushiList[i],glob2rx("*3*1*2*")) == TRUE){
#     ctr_312 <- ctr_312 + 1
#   } else if(str_detect(SushiList[i],glob2rx("*3*2*1*")) == TRUE){
#     ctr_321 <- ctr_321 + 1
#   }
# }

#There are 10C3 possible analyses ... 120
#Perhaps some are sharper than others, worth exploring

#As is a larger group 4!=24 would not be onerous
#perhaps 5!=120 would make this code laborious...etc

#Eventually n=10 most of the permutations are unique so the count is less helpful
#And 10!>5000


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

df_dec <- data.frame(
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
    df_dec[i,j] <- sum(SushiData[,j] == i)/5000 
  }
}

df_dec
#scale_x_discrete(position = "top")

#Example...
# 
# circles <- data.frame(
#   x0 = rep(1:3, 3),
#   y0 = rep(1:3, each = 3),
#   r = seq(0.1, 1, length.out = 9)
# )
# 
# ggplot() +
#   geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles) +
#   coord_fixed()


#Not sure which one is correct here...?
#df_dec.list <- split(df_dec,seq(nrow(df_dec)))

#unlist(as.list(t(df_dec)))

# x_ord <- factor(names(df_dec),levels=c(names(df_dec)[1],names(df_dec)[2],
#                                        names(df_dec)[3],names(df_dec)[4],
#                                        names(df_dec)[5],names(df_dec)[6],
#                                        names(df_dec)[7],names(df_dec)[8],
#                                        names(df_dec)[9],names(df_dec)[10]))

names(df_dec)[1]

circles2 <- data.frame(
  x0 = rep(1:10, 10),
  y0 = rep(1:10, each = 10),
  r = unlist(as.list(t(df_dec)))
)
library(ggplot2)
library(ggforce)
library(stringr)
ggplot()+
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles2) +
  scale_x_continuous(name="Rank",position="top",breaks=1:10,
                     labels=str_to_title(names(df_dec))) +
  scale_y_reverse(name="Sushi item",breaks=1:10) +
  coord_fixed()+
  scale_fill_gradient(labels = scales::percent)+
  labs(fill='Marginal',title="Sushi items are not uniformly ranked",
       caption="Respondents ranked item 7 highly, item 9 poorly, and were divided by item 4.")




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


library(utils)

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
