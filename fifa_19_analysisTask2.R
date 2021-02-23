# DATASET FIFA 2019
setwd("~/Desktop/FIFA2019_data_analysis/")
fifa_19 <- read.csv("data.csv")
library(dplyr)
library(stringr)
stocky_subset<-filter(fifa_19,fifa_19$Body.Type=="Stocky")
lean_subset<-filter(fifa_19,fifa_19$Body.Type=="Lean")
normal_subset<-filter(fifa_19,fifa_19$Body.Type=="Normal")


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


# Aggregations Against Body Type
ggplot(data = fifa_19_no_gk[1:1500,],aes(x=Strength))+
  geom_bar()+
  facet_wrap(~Body.Type)+
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
