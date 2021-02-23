#Task 3 : Combined Tecnhique Score distributed against Position, Strength
# DATASET FIFA 2019
setwd("~/Desktop/FIFA2019_data_analysis/")
fifa_19 <- read.csv("data.csv")
library(dplyr)
library(stringr)
stocky_subset<-filter(fifa_19,fifa_19$Body.Type=="Stocky")
lean_subset<-filter(fifa_19,fifa_19$Body.Type=="Lean")
normal_subset<-filter(fifa_19,fifa_19$Body.Type=="Normal")

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

ggplot(data = fifa_19[1:300,],aes(x= Strength, y = TechniqueScore))+
  geom_point()+
  ggtitle("Technique vs Strength")+
  geom_smooth(method="lm") 

tapply(fifa_19$TechniqueScore[1:250],fifa_19$Position[1:250],mean)

#Task 3a : Combined Tecnhique Score distributed against Position, Strength but filtered w Out GKs
combnd_tecnque<-function(i){
  combnd_tecnque<-(fifa_19_no_gk[i,55]+fifa_19_no_gk[i,56]+fifa_19_no_gk[i,57]+fifa_19_no_gk[i,58]+fifa_19_no_gk[i,59]+
                     fifa_19_no_gk[i,60]+fifa_19_no_gk[i,61]+fifa_19_no_gk[i,62]+fifa_19_no_gk[i,63]+fifa_19_no_gk[i,64]+fifa_19_no_gk[i,74])     
  return(combnd_tecnque)
}
TechniqueScore <- NULL
for (i in 1:nrow(fifa_19_no_gk)){
  TechniqueScore<-c(TechniqueScore,combnd_tecnque(i))
}
fifa_19_no_gk$TechniqueScore <- as.integer(TechniqueScore) 

ggplot(data = fifa_19_no_gk[1:300,],aes(x= Strength, y = TechniqueScore))+
  geom_point()+
  ggtitle("Technique vs Strength")+
  geom_smooth(method="lm") 

tapply(fifa_19_no_gk$TechniqueScore[1:250],fifa_19_no_gk$Position[1:250],mean)

