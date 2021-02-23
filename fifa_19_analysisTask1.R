# DATASET FIFA 2019
setwd("~/Desktop/FIFA2019_data_analysis/")
fifa_19 <- read.csv("data.csv")
library(dplyr)
library(stringr)
stocky_subset<-filter(fifa_19,fifa_19$Body.Type=="Stocky")
lean_subset<-filter(fifa_19,fifa_19$Body.Type=="Lean")
normal_subset<-filter(fifa_19,fifa_19$Body.Type=="Normal")

#Task 1 : Distribution of Heigth, Weight
heightDist<-function(height){
  if(is.na(height)){return("Not Found")}
  else{
    height <- unlist(str_split(height,""))
    height_inches <- (as.numeric(height[1]))* 12
    if(is.na(height[4])){
      height_inches <- height_inches + as.numeric(height[3])
    }
    else{
      height_inches <- height_inches + as.numeric(height[3])*10 + as.numeric(height[4])
    }
    return(height_inches)
  }
  
}
heightClass <- NULL  
for (i in 1:nrow(fifa_19)){
  heightClass<- c(heightClass,heightDist(fifa_19[i,"Height"]))
} 
fifa_19$heightClass <- as.character(heightClass)

fifa_19_height <- fifa_19[order(fifa_19$heightClass),]
library(ggplot2)
ggplot(data = fifa_19_height,aes(x=heightClass))+
  geom_bar(width = 0.85)+
  xlab("Heigth")+
  ylab("Count")


lbs_removal<-function(weight){
  str_remove(weight,"lbs")}
Weight<-NULL
for(i in 1:nrow(fifa_19)){
  Weight <-c(Weight,lbs_removal(fifa_19[i,"Weight"]))
}
fifa_19$Weight <- as.integer(Weight)

weightDist<-function(weight){
  if(is.na(weight)){return("Not Found")}
  else if(weight <= 120){return("<120")}
  else if(weight <=130 &&weight >120){return("120-130")}
  else if(weight <=140 &&weight >130){return("130-140")}
  else if(weight <=150 &&weight >140){return("140-150")}
  else if(weight <=160 &&weight >150){return("150-160 ")}
  else if(weight <=170 &&weight >160){return("160-170")}
  else if(weight <=180 &&weight >170){return("170-180")}
  else if(weight <=190 &&weight >180){return("180-190")}
  else if(weight <=200 &&weight >190){return("190-200")}
  else if(weight <=210 &&weight >200){return("200-210")}
  else if(weight >= 210){return(">210")}
}
weigthClass <- NULL  
for (i in 1:nrow(fifa_19)){
  weigthClass<- c(weigthClass,weightDist(fifa_19[i,"Weight"]))
} 
fifa_19$weigthClass <- as.character(weigthClass)

ggplot(data = fifa_19, aes(Weight))+
  geom_bar(width = 0.99)+
  xlab("weight class")+
  ylab("Count")


ggplot(data = fifa_19, aes(weigthClass))+
  geom_bar(width = 0.75)+
  xlab("weight class")+
  ylab("Count")




