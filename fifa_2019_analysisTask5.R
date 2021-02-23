# DATASET FIFA 2019
setwd("~/Desktop/FIFA2019_data_analysis/")
fifa_19 <- read.csv("data.csv")
library(dplyr)
library(stringr)
stocky_subset<-filter(fifa_19,fifa_19$Body.Type=="Stocky")
lean_subset<-filter(fifa_19,fifa_19$Body.Type=="Lean")
normal_subset<-filter(fifa_19,fifa_19$Body.Type=="Normal")

#Task 5: Gather (General&Specific)Position versus average c(stamina, salary, technique)
#try to inlude the distribution of data 
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

#NEED TO CONVERT ALLWAGEs INTO NUMERIC
wage_to_int <-function(wage_char){
  if(is.na(wage_char)){return("Not Found")}
  else{
    wage_char <- sub('\\€','',wage_char)
    wage_char <- sub('K','',wage_char)
    wage_char <- as.numeric(wage_char)
    return(wage_char)
  }
}
Salary <- NULL
for (i in 1:nrow(fifa_19)){
  Salary<-c(Salary,wage_to_int(fifa_19[i, "Wage"]))
}
#Cleaning the salary column into numeric
fifa_19$Salary <- as.integer(Salary) 

value_to_int <-function(val_char){
  if(is.na(wage_char)){return("Not Found")}
  else{
    val_char <- sub('\\€','',val_char)
    if()
    val_char <- sub('K','',val_char)
    val_char <- sub('M','',val_char)
    val_char <- as.numeric(val_char)
    return(val_char)
  }
}

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


#fifa_19_by_averages_100

position_vect2 <- levels(fifa_19$SpecPos[1:100])
technque_by_pos2<-as.vector(tapply(fifa_19$TechniqueScore[1:100],fifa_19$SpecPos[1:100],mean))
salary_by_pos2<-as.vector(tapply(fifa_19$Salary[1:100],fifa_19$SpecPos[1:100],mean))
stamina_by_pos2<-as.vector(tapply(fifa_19$Stamina[1:100],fifa_19$SpecPos[1:100],mean))
athletics_by_pos2<-as.vector(tapply(fifa_19$AthleticScore[1:100],fifa_19$SpecPos[1:100],mean))


position_vect2<-position_vect2[-11]
technque_by_pos2<-technque_by_pos2[-11]
salary_by_pos2<-salary_by_pos2[-11]
stamina_by_pos2<-stamina_by_pos2[-11]
athletics_by_pos2<-athletics_by_pos2[-11]


fifa19_average_specPos100 <- data.frame(SpecPos = position_vect2, Technique = technque_by_pos2, Stamina = stamina_by_pos2,
                                        Athleticism = athletics_by_pos2, Salary = salary_by_pos2 )



#fifa_19_by_averages_200
position_vect3 <- levels(fifa_19$SpecPos[1:200])
technque_by_pos3<-as.vector(tapply(fifa_19$TechniqueScore[1:200],fifa_19$SpecPos[1:200],mean))
salary_by_pos3<-as.vector(tapply(fifa_19$Salary[1:200],fifa_19$SpecPos[1:200],mean))
stamina_by_pos3<-as.vector(tapply(fifa_19$Stamina[1:200],fifa_19$SpecPos[1:200],mean))
athletics_by_pos3<-as.vector(tapply(fifa_19$AthleticScore[1:200],fifa_19$SpecPos[1:200],mean))

position_vect3<-position_vect3[-11]
technque_by_pos3<-technque_by_pos3[-11]
salary_by_pos3<-salary_by_pos3[-11]
stamina_by_pos3<-stamina_by_pos3[-11]
athletics_by_pos3<-athletics_by_pos3[-11]

fifa19_average_specPos200 <- data.frame(SpecPos = position_vect3, Technique = technque_by_pos3, Stamina = stamina_by_pos3,
                                        Athleticism = athletics_by_pos3, Salary = salary_by_pos3)



#fifa_19_by_averages_300
position_vect4 <- levels(fifa_19$SpecPos[1:300])
technque_by_pos4<-as.vector(tapply(fifa_19$TechniqueScore[1:300],fifa_19$SpecPos[1:300],mean))
salary_by_pos4<-as.vector(tapply(fifa_19$Salary[1:300],fifa_19$SpecPos[1:300],mean))
stamina_by_pos4<-as.vector(tapply(fifa_19$Stamina[1:300],fifa_19$SpecPos[1:300],mean))
athletics_by_pos4<-as.vector(tapply(fifa_19$AthleticScore[1:300],fifa_19$SpecPos[1:300],mean))

position_vect4<-position_vect3[-11]
technque_by_pos4<-technque_by_pos4[-11]
salary_by_pos4<-salary_by_pos4[-11]
stamina_by_pos4<-stamina_by_pos4[-11]
athletics_by_pos4<-athletics_by_pos4[-11]


fifa19_average_specPos300 <- data.frame(SpecPos = position_vect4, Technique = technque_by_pos4, Stamina = stamina_by_pos4,
                                        Athleticism = athletics_by_pos4, Salary = salary_by_pos4 )



#fifa_19_by_averages_75000
position_vect5 <- levels(fifa_19$SpecPos[1:750])
technque_by_pos5<-as.vector(tapply(fifa_19$TechniqueScore[1:750],fifa_19$SpecPos[1:750],mean))
salary_by_pos5<-as.vector(tapply(fifa_19$Salary[1:750],fifa_19$SpecPos[1:750],mean))
stamina_by_pos5<-as.vector(tapply(fifa_19$Stamina[1:750],fifa_19$SpecPos[1:750],mean))
athletics_by_pos5<-as.vector(tapply(fifa_19$AthleticScore[1:750],fifa_19$SpecPos[1:750],mean))

position_vect5<-position_vect5[-11]
technque_by_pos5<-technque_by_pos5[-11]
salary_by_pos5<-salary_by_pos5[-11]
stamina_by_pos5<-stamina_by_pos5[-11]
athletics_by_pos5<-athletics_by_pos5[-11]

fifa19_average_specPos750 <- data.frame(SpecPos = position_vect5, Technique = technque_by_pos5, Stamina = stamina_by_pos5,
                                        Athleticism = athletics_by_pos5, Salary = salary_by_pos5)


# AVERAGE SALARY VS SPECIFIC POSITION
ggplot(data = fifa19_average_specPos100,aes(x=SpecPos,y=Salary))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Salary (BEST 100 PLAYERS)")

ggplot(data = fifa19_average_specPos750,aes(x=SpecPos,y=Salary))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Salary (BEST 750 PLAYERS)")


# AVERAGE STAMINA VS SPECIFIC POSITION 
ggplot(data = fifa19_average_specPos100,aes(x=SpecPos,y=Stamina))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Stamina (BEST 100 PLAYERS)")


ggplot(data = fifa19_average_specPos750,aes(x=SpecPos,y=Stamina))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Stamina (BEST 750 PLAYERS)")

# AVERAGE ATHLETICISM VS SPECIFC POSITION
ggplot(data = fifa19_average_specPos100,aes(x=SpecPos,y=Athleticism))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Athleticism (BEST 100 PLAYERS)")

ggplot(data = fifa19_average_specPos750,aes(x=SpecPos,y=Athleticism))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. Athleticism (BEST 750 PLAYERS)")

# AVERAGE TECHNIQUE VS SPECIFIC POSITION
ggplot(data = fifa19_average_specPos100,aes(x=SpecPos,y=Technique))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. TechniqueScore (BEST 100 PLAYERS)")

ggplot(data = fifa19_average_specPos750,aes(x=SpecPos,y=Technique))+
  geom_bar(stat = "identity")+
  ggtitle("Position Vs. TechniqueScore (BEST 750 PLAYERS)")

# SALARY VS ATHLETIC SCORE (BEST 700 PLAYERS) BY Nationality
ggplot(fifa_19[1:50,],aes(x=Salary,y=AthleticScore,col = Nationality))+
  geom_point()+
  #geom_smooth(method="lm") +
  ggtitle("Salary Vs. AthleticScore (BEST 700 PLAYERS)")

# SALARY VS ATHLETIC SCORE (BEST 700 PLAYERS) BY POSITION

ggplot(fifa_19[1:700,],aes(x=Salary,y=AthleticScore,col = SpecPos))+
  geom_point()+
  geom_smooth(method="lm") +
  ggtitle("Salary Vs. AthleticScore (BEST 700 PLAYERS)")

