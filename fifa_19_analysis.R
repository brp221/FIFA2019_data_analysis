# DATASET FIFA 2019
setwd("C:/R Language/FIFA19 Project/fifa19/")
fifa_19 <- read.csv("data.csv")
library(dplyr)
library(stringr)
stocky_subset<-filter(fifa_19,fifa_19$Body.Type=="Stocky")
lean_subset<-filter(fifa_19,fifa_19$Body.Type=="Lean")
normal_subset<-filter(fifa_19,fifa_19$Body.Type=="Normal")

#Task 1 : Distribution of Heigth, Weight
library(ggplot2)
ggplot(data = fifa_19,aes(x=fifa_19$Height))+
  geom_bar(width = 0.5)+
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

ggplot(data = fifa_19, aes(x=fifa_19$weigthClass))+
  geom_bar(width = 0.75)+
  xlab("weight class")+
  ylab("Count")

#Task 2 : Correlation between weight and (acceleration, sprintSpeed, Agility, Balance, Stamina) for the 1st 250 players
#need to exclude goalkeepers
fifa_19_no_gk <- filter(fifa_19,fifa_19$Position != "GK")
stamina_weight_corr<-ggplot(fifa_19_no_gk[1:250,], aes(x=Weight, y=Stamina)) + geom_point()+geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
plot(stamina_weight_corr)

acc_weight_corr<-ggplot(fifa_19_no_gk[1:250,], aes(x=Weight, y=Acceleration)) + geom_point()+geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
plot(acc_weight_corr)

sprint_weight_corr<-ggplot(fifa_19_no_gk[1:250,], aes(x=Weight, y=SprintSpeed)) + geom_point()+geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
plot(sprint_weight_corr)

strngth_weight_corr<-ggplot(fifa_19_no_gk[1:250,], aes(x=Weight, y=Strength)) + geom_point()+geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
plot(strngth_weight_corr)

balnce_weight_corr<-ggplot(fifa_19_no_gk[1:250,], aes(x=Weight, y=Balance)) + geom_point()+geom_smooth(method="lm")  # set se=FALSE to turnoff confidence bands
plot(balnce_weight_corr)


ggplot(data = fifa_19_temp1[1:1500,],aes(x=Strength))+
  geom_bar()+
  facet_wrap(~Body.Type,scales = "free")+
  ggtitle("Strength")+
  xlab("Strength")+
  ylab("Count")
  ylim(0,250)
  
  
ggplot(data = fifa_19_no_gk[1:1500,],aes(x=SprintSpeed))+
  geom_bar()+
  facet_wrap(~Body.Type)+
  ggtitle("SprintSpeed")+
  xlab("SprintSpeed")+
  ylab("Count")
ylim(0,250)


ggplot(data = fifa_19_no_gk[1:1500,],aes(x=Acceleration))+
  geom_bar()+
  facet_wrap(~Body.Type)+
  ggtitle("Acceleration")+
  xlab("Acceleration")+
  ylab("Count")
ylim(0,250)


#Task 3 : Combined Tecnhique Score distributed against Position, Strength, 
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

#Task 4 : Combined Athelticism Score disctributed against Position, Nation(top 12 nations)
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


#Task 5: Gather (General&Specific)Position versus average c(stamina, salary, technique)
#try to inlude the distribution of data 

#NEED TO CONVERT ALL VALUE + WAGE INTO INTEGER
factor_to_int<-function(i,column){
  string<-fifa_19[i,column]
  partial<-str_replace_all(string,"[[:punct:]]","")
  complete<-str_replace_all(partial,"[ â¬ MK]","")
  return(complete)
}
Salary <- NULL
for (i in 1:nrow(fifa_19)){
  Salary<-c(Salary,factor_to_int(i,column = "Wage"))
}
#at this point, fifa_19 and fifa_19_temp_no_gK diverge, fifa_19 receives Salary Column 
fifa_19$Salary <- as.integer(Salary) 

factor_to_int_2<-function(i,column){
  if(grepl("M",x = fifa_19[i,column])){
    string<-fifa_19[i,column]
    partial<-str_remove(string,pattern =  "â,¬")
    complete<-str_remove(partial,"M")}
  else if(grepl("K",x = fifa_19[i,column])){
    string<-fifa_19[i,column]
    partial<-str_remove(string,pattern =  "â,¬")
    complete<-str_remove(partial,"K")
    complete<-as.numeric(complete)/1000}
  else{complete <- 0}
  return(complete)}
ValueInt <- NULL
for (i in 1:nrow(fifa_19)){
  ValueInt<-c(ValueInt,factor_to_int_2(i,column = "Value"))
}
fifa_19$ValueInt <- as.numeric(ValueInt) 
# how does the gap between position vs salaries correlation change as we add lower rated player
#going to have to create new positions as well
#LB/LWB  = LB
#RB/RWB = RB
#LS/RS/ST/CF = CF
#LW/LM/LF = LW
#RW/RM/RF = RW
#RCM/LCM/CM = CM
#LDM/RDM = DM
#LAM/RAM/CAM = AM
#LCB/RCB = CB
#IN ADDITION, the players will be assigned generalPosition(Defense,Middle,Attack)

specificPos <- function(pos){
  if(pos == 'LB' || pos == 'LWB'){specPos<-'LB'}
  else if(pos == 'RB'||pos== 'RWB'){specPos<-'RB'}
  else if(pos == 'LS'||pos=='RS'||pos=='ST'||pos=='CF'){specPos<-'CF'}
  else if(pos == 'LW'||pos=='LM'||pos=='LF'){specPos<-'LW'}
  else if(pos == 'RW'||pos=='RM'||pos=='RF'){specPos<-'RW'}
  else if(pos == 'RCM'||pos=='LCM'||pos=='CM'){specPos<-'CM'}
  else if(pos == 'LDM'||pos=='RDM'||pos=='CDM'){specPos<-'DM'}
  else if(pos == 'RAM'||pos=='LAM'||pos=='CAM'){specPos<-'AM'}
  else if(pos == 'LCB'||pos=='RCB'||pos=='CB'){specPos<-'CB'}
  else if(pos=='GK'){specPos<-'GK'}
  else{specPos<-"W.E."}
  return(specPos)
}

SecondaryPos<-NULL
for (i in 1:nrow(fifa_19)){
  SecondaryPos <-c(SecondaryPos,specificPos(fifa_19[i,"Position"]))
}
fifa_19$SpecPos <- as.factor(SecondaryPos)
fifa_19$SpecPos2<-as.character(SecondaryPos)

fifa_19_first2000<-fifa_19[1:2000,]
fifa_19_first2000$SpecPos <- as.factor(SecondaryPos)
fifa_19 <- fifa_19[-c(remove_index),]


#fifa_19_by_averages_100
position_vect2 <- levels(fifa_19$SpecPos[1:100])
technque_by_pos2<-as.vector(tapply(fifa_19$TechniqueScore[1:100],fifa_19$SpecPos[1:100],mean))
salary_by_pos2<-as.vector(tapply(fifa_19$Salary[1:100],fifa_19$SpecPos[1:100],mean))
stamina_by_pos2<-as.vector(tapply(fifa_19$Stamina[1:100],fifa_19$SpecPos[1:100],mean))
athletics_by_pos2<-as.vector(tapply(fifa_19$AthleticScore[1:100],fifa_19$SpecPos[1:100],mean))
vakue_by_pos2<-as.vector(tapply(fifa_19$ValueInt[1:100],fifa_19$SpecPos[1:100],mean))

position_vect2<-position_vect2[-11]
technque_by_pos2<-technque_by_pos2[-11]
salary_by_pos2<-salary_by_pos2[-11]
stamina_by_pos2<-stamina_by_pos2[-11]
athletics_by_pos2<-athletics_by_pos2[-11]
vakue_by_pos2<-vakue_by_pos2[-11]

fifa19_average_specPos100 <- data.frame(SpecPos = position_vect2, Technique = technque_by_pos2, Stamina = stamina_by_pos2,
                                      Athleticism = athletics_by_pos2, Salary = salary_by_pos2,ValueInt = vakue_by_pos2 )



#fifa_19_by_averages_200
position_vect3 <- levels(fifa_19$SpecPos[1:200])
technque_by_pos3<-as.vector(tapply(fifa_19$TechniqueScore[1:200],fifa_19$SpecPos[1:200],mean))
salary_by_pos3<-as.vector(tapply(fifa_19$Salary[1:200],fifa_19$SpecPos[1:200],mean))
stamina_by_pos3<-as.vector(tapply(fifa_19$Stamina[1:200],fifa_19$SpecPos[1:200],mean))
athletics_by_pos3<-as.vector(tapply(fifa_19$AthleticScore[1:200],fifa_19$SpecPos[1:200],mean))
vakue_by_pos3<-as.vector(tapply(fifa_19$ValueInt[1:200],fifa_19$SpecPos[1:200],mean))

position_vect3<-position_vect3[-11]
technque_by_pos3<-technque_by_pos3[-11]
salary_by_pos3<-salary_by_pos3[-11]
stamina_by_pos3<-stamina_by_pos3[-11]
athletics_by_pos3<-athletics_by_pos3[-11]
vakue_by_pos3<-vakue_by_pos3[-11]

fifa19_average_specPos200 <- data.frame(SpecPos = position_vect3, Technique = technque_by_pos3, Stamina = stamina_by_pos3,
                                        Athleticism = athletics_by_pos3, Salary = salary_by_pos3,ValueInt = vakue_by_pos3 )



#fifa_19_by_averages_300
position_vect4 <- levels(fifa_19$SpecPos[1:300])
technque_by_pos4<-as.vector(tapply(fifa_19$TechniqueScore[1:300],fifa_19$SpecPos[1:300],mean))
salary_by_pos4<-as.vector(tapply(fifa_19$Salary[1:300],fifa_19$SpecPos[1:300],mean))
stamina_by_pos4<-as.vector(tapply(fifa_19$Stamina[1:300],fifa_19$SpecPos[1:300],mean))
athletics_by_pos4<-as.vector(tapply(fifa_19$AthleticScore[1:300],fifa_19$SpecPos[1:300],mean))
vakue_by_pos4<-as.vector(tapply(fifa_19$ValueInt[1:300],fifa_19$SpecPos[1:300],mean))

position_vect4<-position_vect3[-11]
technque_by_pos4<-technque_by_pos4[-11]
salary_by_pos4<-salary_by_pos4[-11]
stamina_by_pos4<-stamina_by_pos4[-11]
athletics_by_pos4<-athletics_by_pos4[-11]
vakue_by_pos4<-vakue_by_pos4[-11]

fifa19_average_specPos300 <- data.frame(SpecPos = position_vect4, Technique = technque_by_pos4, Stamina = stamina_by_pos4,
                                        Athleticism = athletics_by_pos4, Salary = salary_by_pos4,ValueInt = vakue_by_pos4 )



#fifa_19_by_averages_75000
position_vect5 <- levels(fifa_19$SpecPos[1:750])
technque_by_pos5<-as.vector(tapply(fifa_19$TechniqueScore[1:750],fifa_19$SpecPos[1:750],mean))
salary_by_pos5<-as.vector(tapply(fifa_19$Salary[1:750],fifa_19$SpecPos[1:750],mean))
stamina_by_pos5<-as.vector(tapply(fifa_19$Stamina[1:750],fifa_19$SpecPos[1:750],mean))
athletics_by_pos5<-as.vector(tapply(fifa_19$AthleticScore[1:750],fifa_19$SpecPos[1:750],mean))
vakue_by_pos5<-as.vector(tapply(fifa_19$ValueInt[1:750],fifa_19$SpecPos[1:750],mean))

position_vect5<-position_vect5[-11]
technque_by_pos5<-technque_by_pos5[-11]
salary_by_pos5<-salary_by_pos5[-11]
stamina_by_pos5<-stamina_by_pos5[-11]
athletics_by_pos5<-athletics_by_pos5[-11]
vakue_by_pos5<-vakue_by_pos5[-11]

fifa19_average_specPos750 <- data.frame(SpecPos = position_vect5, Technique = technque_by_pos5, Stamina = stamina_by_pos5,
                                        Athleticism = athletics_by_pos5, Salary = salary_by_pos5,ValueInt = vakue_by_pos5 )


# AVERAGE SALARY VS SPECIFIC POSITION
ggplot(data = fifa19_average_specPos100,aes(x=SpecPos,y=Salary))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Salary (BEST 100 PLAYERS)")

ggplot(data = fifa19_average_specPos200,aes(x=SpecPos,y=Salary))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Salary (BEST 200 PLAYERS)")

ggplot(data = fifa19_average_specPos300,aes(x=SpecPos,y=Salary))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Salary (BEST 300 PLAYERS)")

ggplot(data = fifa19_average_specPos750,aes(x=SpecPos,y=Salary))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Salary (BEST 750 PLAYERS)")

# AVERAGE VALUE VS SPECIFIC POSITION
ggplot(data = fifa19_average_specPos100,aes(x=SpecPos,y=ValueInt))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. ValueInt (BEST 100 PLAYERS)")

ggplot(data = fifa19_average_specPos200,aes(x=SpecPos,y=ValueInt))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. ValueInt (BEST 200 PLAYERS)")

ggplot(data = fifa19_average_specPos300,aes(x=SpecPos,y=ValueInt))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. ValueInt (BEST 300 PLAYERS)")

ggplot(data = fifa19_average_specPos750,aes(x=SpecPos,y=ValueInt))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. ValueInt (BEST 750 PLAYERS)")

# AVERAGE STAMINA VS SPECIFIC POSITION 
ggplot(data = fifa19_average_specPos100,aes(x=SpecPos,y=Stamina))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Stamina (BEST 100 PLAYERS)")

ggplot(data = fifa19_average_specPos200,aes(x=SpecPos,y=Stamina))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Stamina (BEST 200 PLAYERS)")

ggplot(data = fifa19_average_specPos300,aes(x=SpecPos,y=Stamina))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Stamina (BEST 300 PLAYERS)")

ggplot(data = fifa19_average_specPos750,aes(x=SpecPos,y=Stamina))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Stamina (BEST 750 PLAYERS)")

# AVERAGE ATHLETICISM VS SPECIFC POSITION
ggplot(data = fifa19_average_specPos100,aes(x=SpecPos,y=Athleticism))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Athleticism (BEST 100 PLAYERS)")

ggplot(data = fifa19_average_specPos200,aes(x=SpecPos,y=Athleticism))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Athleticism (BEST 200 PLAYERS)")

ggplot(data = fifa19_average_specPos300,aes(x=SpecPos,y=Athleticism))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Athleticism (BEST 300 PLAYERS)")

ggplot(data = fifa19_average_specPos750,aes(x=SpecPos,y=Athleticism))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Athleticism (BEST 750 PLAYERS)")

# AVERAGE TECHNIQUE VS SPECIFIC POSITION
ggplot(data = fifa19_average_specPos100,aes(x=SpecPos,y=Technique))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. TechniqueScore (BEST 100 PLAYERS)")

ggplot(data = fifa19_average_specPos200,aes(x=SpecPos,y=Technique))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. TechniqueScore (BEST 200 PLAYERS)")

ggplot(data = fifa19_average_specPos300,aes(x=SpecPos,y=Technique))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. TechniqueScore (BEST 300 PLAYERS)")

ggplot(data = fifa19_average_specPos750,aes(x=SpecPos,y=Technique))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. TechniqueScore (BEST 750 PLAYERS)")

ggplot(fifa_19[1:700,],aes(x=Salary,y=TechniqueScore))+
  geom_point()+
  geom_smooth(method="lm")+
  ggtitle("Salary Vs. TechniqueScore (BEST 700 PLAYERS)")

  
ggplot(fifa_19[1:700,],aes(x=Salary,y=AthleticScore,col = SpecPos))+
  geom_point()+
  geom_smooth(method="lm") +
  ggtitle("Salary Vs. AthleticScore (BEST 700 PLAYERS)")
ggplot(fifa_19[1:50,],aes(x=Salary,y=AthleticScore,col = Nationality))+
  geom_point()+
  #geom_smooth(method="lm") +
  ggtitle("Salary Vs. AthleticScore (BEST 700 PLAYERS)")

ggplot(fifa_19[1:700,],aes(SpecPos,TechniqueScore))+
  geom_bar(stat = "summary",fun.y="mean")

ggplot(fifa_19[1:700,],aes(SpecPos,AthleticScore))+
  geom_bar(stat = "summary",fun.y="mean")

ggplot(fifa_19[1:700,],aes(SpecPos,Stamina))+
  geom_bar(stat = "summary",fun.y="mean")

#Task #6 or whatever: Nationality(top 10 w best 300 players)
fifa_19_nationality<-filter(fifa_19,Nationality=="Argentina"|Nationality=="Brazil"|Nationality=="France"|
                              Nationality=="Germany"|Nationality=="Belgium"|Nationality=="Portugal"|
                              Nationality=="Colombia"|Nationality=="Uruguay"|Nationality=="Serbia"|Nationality=="Greece")

ggplot(fifa_19_nationality[1:500,],aes(Nationality,TechniqueScore))+
  geom_bar(stat = "summary",fun.y="mean")+
  ggtitle("AVERAGE TECHNIQUE BY NATIONALITY")

ggplot(fifa_19_nationality[1:500,],aes(Nationality,AthleticScore))+
  geom_bar(stat = "summary",fun.y="mean")+
  ggtitle("AVERAGE TECHNIQUE BY NATIONALITY")


ggplot(fifa_19_nationality[1:200,],aes(Nationality,Stamina))+
  geom_bar(stat = "summary",fun.y="mean")+
  ggtitle("AVERAGE TECHNIQUE BY NATIONALITY")

#Task #7
