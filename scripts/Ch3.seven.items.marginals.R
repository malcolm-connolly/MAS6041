library(tidyverse)

##### making a basic margins plot for the collapsed rankings #####

#install.packages("vctrs")
library(vctrs)
matrix()

m<- matrix(unlist(s),nrow =factorial(7),ncol=7, byrow=TRUE)
m[1,]
collated <- vec_cbind(m,f) %>% as.data.frame()
?order()
collated <- collated[order(collated[,8],decreasing = TRUE),] %>% filter(collated[,8] != 0)

write.csv(collated,"./data/S7/collated2.csv")

rankings_ex1310 <- read_lines("./data/S7/collated.csv") %>% str_split(pattern=":")

List_ex1310 <- vector(mode = "list", length = 5000)

# Require a counter to keep track of the index in the Sushi Data list
ctr <- 0
for(i in 1:length(rankings_ex1310)){
  ctr <- ctr + 1 
  for(j in 1:rankings_ex1310[[i]][2]){
    List_ex1310[[ctr]] <- rankings_ex1310[[i]][1] 
    ctr <- ctr + 1
  }
  ctr <- ctr - 1
}

Data_ex1310 <- List_ex1310 %>%
  str_replace_all(","," ") %>%
  read_table(col_names = paste0("Item",1:7))

do.call(rbind.data.frame, Data_ex1310)


df <- data.frame(
  first = rep(0,7),
  second = rep(0,7),
  third = rep(0,7),
  fourth = rep(0,7),
  fifth = rep(0,7),
  sixth = rep(0,7),
  seventh = rep(0,7)
)

for(j in 1:7){
  for(i in 1:7){
    #counts the no of times the jth col is given rank i
    df[i,j] <- sum(Data_ex1310[,i] == j)
  }
}

df

#     first second third fourth fifth sixth seventh
# 1   584   1197  1090   1058   633   286     152
# 2   918    822   594    503   423   573    1167
# 3   720    867   945    732   583   633     520
# 4   360    335   463    722   898  1213    1009
# 5  2120   1158   759    382   288   171     122
# 6   207    452   905   1155  1327   748     206
# 7    91    169   244    448   848  1376    1824


circles2 <- data.frame(
  x0 = rep(1:7, 7),
  y0 = rep(1:7, each = 7),
  r = unlist(as.list(t(df/5000)))
)
library(ggplot2)
library(ggforce)
library(stringr)
ggplot()+
  geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles2) +
  scale_x_continuous(name="Rank",position="top",breaks=1:7,
                     labels=str_to_title(names(df))) +
  scale_y_reverse(name="Sushi item",breaks=1:7, labels = c(2,4:9)) +
  coord_fixed()+
  scale_fill_gradient(labels = scales::percent)+
  labs(fill='Marginal',title="",
       caption="")

saveRDS(df,"./data/S7/marginals_table_collapsed.RData")


##### Confirming count #####
library(combinat)

matches <- list(length = factorial(6))

for(i in 1:factorial(6)){
  matches[[i]] <- rep(0,7)
}

g <- c(2,3,4,5,6,7) %>% permn

for(i in 1:length(matches)){
  matches[[i]][1] <- 1
  matches[[i]][2:7] <- g[[i]][1:6]
  matches[[i]] <- matches[[i]] %>% sapply(as.integer)
}


match(matches[1:48],s)
s[match(matches,s)]

f[match(matches[1:factorial(6)],s)] %>% sum()
df[1,1]



