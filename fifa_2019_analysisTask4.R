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

#Analyze the market share of the first 100 players by nation 
frequency_nat <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
unique_Nations <- unique(fifa_19[1:100,"Nationality"])
for (i in 1:nrow(fifa_19[1:100,])){
  for(k in 1:length(unique_Nations)){
    if(fifa_19[i,"Nationality"] == unique_Nations[k]){
      frequency_nat[k] <- frequency_nat[k] + 1
    }
  }
}

#market_share_nation_df <- data.frame("Nation" = unique_Nations, "Frequency" = frequency_nat)

library(ggplot2)

#turning Market Value into Numeric
value_to_numeric<-function(value){
  value<-str_remove(value,"€")
  value<-str_remove(value,"M")
  return(value)
}
ValueNum<-NULL
fifa_19_100 <- fifa_19[1:100,]
for(i in 1:nrow(fifa_19_100)){
  ValueNum <-c(ValueNum,value_to_numeric(fifa_19[i,"Value"]))
}
fifa_19_100$ValueNum <- as.numeric(ValueNum)

#Analyze the market share of the first 100 players by nation 
value_tot_nat <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
unique_Nations <- unique(fifa_19[1:100,"Nationality"])
for (i in 1:nrow(fifa_19_100)){
  for(k in 1:length(unique_Nations)){
    if(fifa_19_100[i,"Nationality"] == unique_Nations[k]){
      value_tot_nat[k] <- value_tot_nat[k] + fifa_19_100[i,"ValueNum"]
    }
  }
}
pct <- round(value_tot_nat/sum(value_tot_nat)*100)
unique_Nations <- paste(unique_Nations, pct) # add percents to labels
unique_Nations <- paste(unique_Nations,"%",sep="") # ad % to labels


# Create a basic bar
pie(frequency_nat, labels = unique_Nations,col=rainbow(length(unique_Nations)), main="Frequency of Players by Nationality (Top 100)")
# Create a basic bar
pie(value_tot_nat, labels = unique_Nations,col=rainbow(length(unique_Nations)), main="Market Share of Value by Nationality (Top 100)")

