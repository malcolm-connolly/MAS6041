#### Spectral Analysis on 3 ####
#write.csv(df_pc,paste0("data/","df_pc.csv"))

#Looks like... 7, 9 and 4

ctr_479 <-0
ctr_497 <-0
ctr_749 <-0
ctr_794 <-0
ctr_947 <-0
ctr_974 <-0

for(i in 1:length(SushiList)){
  if(str_detect(SushiList[i],glob2rx("*4*7*9*")) == TRUE){
    ctr_479 <- ctr_479 + 1
  } else if(str_detect(SushiList[i],glob2rx("*4*9*7*")) == TRUE){
    ctr_497 <- ctr_497 + 1
  } else if(str_detect(SushiList[i],glob2rx("*7*4*9*")) == TRUE){
    ctr_749 <- ctr_749 + 1
  } else if(str_detect(SushiList[i],glob2rx("*7*9*4*")) == TRUE){
    ctr_794 <- ctr_794 + 1
  } else if(str_detect(SushiList[i],glob2rx("*9*4*7*")) == TRUE){
    ctr_947 <- ctr_947 + 1
  } else if(str_detect(SushiList[i],glob2rx("*9*7*4*")) == TRUE){
    ctr_974 <- ctr_974 + 1
  }
}

ctr_479 
ctr_497
ctr_749 
ctr_794 
ctr_947 
ctr_974

#This strategy will not scale very well...