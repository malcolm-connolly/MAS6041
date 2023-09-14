##################################################################
##                This script will transform data               ##
##               from ranked lists to rank orders               ##
##                  file is saved for later use                 ##
##################################################################
#Need to change from 2,3,1 where item 2 ranked 1st, etc
#to 3,1,2 where item is field and preference is data
#Readme from Toshihiro says top item is most preferred one.

SushiList <- readRDS("./data/SushiList.RData")
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

#consider item names instead
sushinames <- c("ebi","anago","maguro","ika","uni","sake","tamago","toro","tekka-maki","kappa-maki")

#save for later use
saveRDS(PrefData,"./data/PrefData.RData")
