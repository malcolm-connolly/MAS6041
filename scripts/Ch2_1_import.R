##################################################################
##              Data import and dataframe creation              ##
##################################################################
library(tidyverse)
# Here we download the data which is openly available on the internet

url <- "https://www.preflib.org/static/data/sushi/00014-00000001.soc"

destination <- paste0("data/",basename(url))

download.file(url,
              destination,
              method = "libcurl",
              mode="wb")

# Reading the downloaded file into R. The ranks start on 23rd line.
TextRaw <- read_lines(destination,skip=22)

#This code, and later loop, changes the format of the text file/list
#From number: ranked list
#To that ranked list appearing the given number of times.
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

saveRDS(SushiData,file ="data/SushiData.RData")

#Also save the list as it will be useful to use in this form.
#saveRDS(SushiList,file ="data/SushiList.RData")