# DATASET FIFA 2019
setwd("~/Desktop/FIFA2019_data_analysis/")
fifa_19 <- read.csv("data.csv")
library(dplyr)
library(stringr)
stocky_subset<-filter(fifa_19,fifa_19$Body.Type=="Stocky")
lean_subset<-filter(fifa_19,fifa_19$Body.Type=="Lean")
normal_subset<-filter(fifa_19,fifa_19$Body.Type=="Normal")

#Task 4 : Combined Atheticism Score disctributed against Position, Nation(top 12 nations)
combnd_athltcsm<-function(i){
  combnd_athltcsm<-(fifa_19[i,65]+fifa_19[i,66]+fifa_19[i,67]+fifa_19[i,68]+fifa_19[i,69]+
                      fifa_19[i,70]+fifa_19[i,71]+fifa_19[i,72]+fifa_19[i,73])     
  return(combnd_athltcsm)
}
AthleticScore <- NULL
for (i in 1:nrow(fifa_19)){
  AthleticScore<-c(AthleticScore,combnd_athltcsm(i))
}
fifa_19$AthleticScore <- as.integer(AthleticScore) 
combnd_tecnque<-function(i){
  combnd_tecnque<-(fifa_19[i,55]+fifa_19[i,56]+fifa_19[i,57]+fifa_19[i,58]+fifa_19[i,59]+
                     fifa_19[i,60]+fifa_19[i,61]+fifa_19[i,62]+fifa_19[i,63]+fifa_19[i,64]+fifa_19[i,74])     
  return(combnd_tecnque)
}
TechniqueScore <- NULL
for (i in 1:nrow(fifa_19)){
  TechniqueScore<-c(TechniqueScore,combnd_tecnque(i))
}
fifa_19$TechniqueScore <- as.integer(TechniqueScore) 

fifa_19_temp_no_gK<-subset(fifa_19,Position!="GK")

ggplot(fifa_19_temp_no_gK[1:500,], aes(x=Position, y=AthleticScore)) +
  geom_point()+
  geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
ggplot(data = fifa_19_temp_no_gK[1:50,],aes(x= AthleticScore, y = TechniqueScore))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(data = fifa_19_temp_no_gK[1:50,],aes(x= AthleticScore, y = Acceleration))+
  geom_point()+
  geom_smooth(method="lm") 
ggplot(data = fifa_19_temp_no_gK[1:50,],aes(x= AthleticScore, y = Strength))+
  geom_point()+
  geom_smooth(method="lm") 

