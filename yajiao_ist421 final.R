##final project
##Yanrui Jiao
my.data.path <- file.choose()
my.data.path
suicide<-read.csv(my.data.path,header=TRUE,stringsAsFactors=FALSE)

suicide$population[1:10]
suicide$age
teen<-suicide[suicide$age !="12-24 years",]

suicide<-suicide[!is.na(suicide$suicides_no),]
# graph 1 suicide ratio for different age group###

suicide.by.year<-tapply(suicide$suicides_no,suicide$age,sum)

pie(suicide.by.year)

#no_suicide by years
suicide.by.year<-tapply(suicide$suicides_no, suicide$year,sum)
barplot(suicide.by.year)

#nu_suicide by gender

suicide.byage.bysex<-tapply(suicide$suicides_no,list(suicide$age,suicide$sex),sum)
boxplot(suicide.byage.bysex)
#are number of sucide people grouwing over time for each gender

M<-tapply(suicide$suicides_no, list(suicide$gender,suicide$year), FUN=sum)

suicide.byage.byyear<-tapply(suicide$suicides_no,list(suicide$age,suicide$year),sum)
suicide.byage.byyear
suicide.byage.byyear<-as.matrix(suicide.byage.byyear)
options(scipen=999)
plot(1979:2016,suicide.byage.byyear[3,],type='l', col="red",lwd=2,
     ylim=c(0,100500))
lines(1979:2016,suicide.byage.byyear[4,], col="green", lwd=2)
lines(1979:2016,suicide.byage.byyear[2,], col="blue", lwd=2)
lines(1979:2016,suicide.byage.byyear[1,], col="yellow", lwd=2)
lines(1979:2016,suicide.byage.byyear[5,], col="brown", lwd=2)
lines(1979:2016,suicide.byage.byyear[6,], col="purple", lwd=2)

legend("topleft",  border="red", legend = rownames(suicide.byage.byyear)
       , fill =c("red", "green", "blue","yellow","brown","purple"))
#install.packages('ggmap')
install.packages('ggplot2')
install.packages('tidyverse')
library(ggmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
map.world <- map_data("world")
intersect(suicide$country,map.world$region)

suicide.bycountry<-aggregate(suicide$suicides_no,list(suicide$country),sum)
colnames(suicide.bycountry)<-c('country','suicide')
map.world_joined <- right_join(map.world, suicide.bycountry, by = c('region' = 'country'))

ggplot() +
  geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group,fill=suicide)) 
  


