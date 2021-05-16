library(MASS)
library(MPV)
library(car)
library(leaps)

team<- read.csv("TeamStats.csv", sep = ";")
plot(pitches$slg_percent~pitches$xslg)
teammodel<- lm(W~HR+HRAgnst, data=team)
slgmodel<- lm(W~SLG+SLGAgnst, data=team)
summary(teammodel)
summary(slgmodel)

homeruns<- read.csv("hrlaunchangle.csv", sep=";")
evla<- lm(b_home_run~exit_velocity_avg+launch_angle_avg, data=homeruns)
homeruns$HR_AB= homeruns$b_home_run/homeruns$b_ab
summary(evla)
evla<- lm(HR_AB~exit_velocity_avg+launch_angle_avg, data=homeruns)
summary(evla)


pitches<- read.csv("2019PitchArsenal.csv", sep=";")
summary(pitches)
plot(pitches$slg_percent, pitches$xslg)
#pitches<- subset(pitches, select=-c(first_name, last_name, yea p_formatted_ip, p_total_hits, p_home_run))
names(pitches)[9]<- "avg_horizontal_brk"
names(pitches)[10]<- "avg_vertical_brk"
pitches$X30_count_percent <- sapply(pitches$X30_count_percent, function(x) gsub("%", "", x))
pitches$X20_count_percent <- sapply(pitches$X20_count_percent, function(x) gsub("%", "", x))
pitches$X20_count_percent<- as.numeric(pitches$X20_count_percent)
pitches$X30_count_percent<- as.numeric(pitches$X30_count_percent)

#Add a 0 for rows that are empty
#These are all % variables for certain pitches, so these pitchers throw these pitches 0% of the time
#Again, another approach is probably better
pitches$n_sift_formatted[is.na(pitches$n_sift_formatted)] <- 0
pitches$n_sl_formatted[is.na(pitches$n_sl_formatted)] <- 0
pitches$n_ch_formatted[is.na(pitches$n_ch_formatted)] <- 0
pitches$n_cukc_formatted[is.na(pitches$n_cukc_formatted)] <- 0
pitches$n_ff_formatted[is.na(pitches$n_ff_formatted)] <- 0
pitches$n_breaking_formatted[is.na(pitches$n_breaking_formatted)] <- 0
pitches$n_fastball_formatted[is.na(pitches$n_fastball_formatted)] <- 0

#Fill remaining missing with column mean.
for(i in 1:ncol(pitches)){
  pitches[is.na(pitches[,i]), i] <- mean(pitches[,i], na.rm = TRUE)
}

#for later
dt = sort(sample(nrow(pitches), nrow(pitches)*.5))
train<-pitches[dt,]
test<-pitches[-dt,]
#

#Predicting xslg
pitcherslg<- lm(xslg~exit_velocity_avg+launch_angle_avg+avg_horizontal_brk+avg_vertical_brk+sl_avg_break+ch_avg_break+cu_avg_break+si_avg_break+n_fastball_formatted+n_breaking_formatted+fastball_avg_speed+breaking_avg_speed+n_offspeed_formatted+offspeed_avg_speed+meatball_percent+in_zone_percent+edge_percent+f_strike_percent+z_swing_miss_percent+oz_swing_miss_percent+X30_count_percent+X20_count_percent, pitches)
summary(pitcherslg)
vif(pitcherslg)
pitcherslg<- lm(xslg~exit_velocity_avg+launch_angle_avg+avg_horizontal_brk+avg_vertical_brk+sl_avg_break+ch_avg_break+cu_avg_break+si_avg_break+n_fastball_formatted+fastball_avg_speed+breaking_avg_speed+n_offspeed_formatted+offspeed_avg_speed+meatball_percent+in_zone_percent+edge_percent+f_strike_percent+z_swing_miss_percent+oz_swing_miss_percent+X30_count_percent+X20_count_percent, pitches)

max(cooks.distance(pitcherslg))
boxcox(pitcherslg)
boxcox(pitcherslg,lambda=seq(0,1,.1))
pitches$Expectedslg<-(pitches$xslg)
pitches$xslg<-(pitches$xslg)^(.42)
pitcherslg<- lm(xslg~exit_velocity_avg+launch_angle_avg+avg_horizontal_brk+avg_vertical_brk+sl_avg_break+ch_avg_break+cu_avg_break+si_avg_break+n_fastball_formatted+fastball_avg_speed+breaking_avg_speed+n_offspeed_formatted+offspeed_avg_speed+meatball_percent+in_zone_percent+edge_percent+f_strike_percent+z_swing_miss_percent+oz_swing_miss_percent+X30_count_percent+X20_count_percent, pitches)
summary(pitcherslg)

empty<-lm(xslg~1,pitches)
full<-lm(xslg~exit_velocity_avg+launch_angle_avg+avg_horizontal_brk+avg_vertical_brk+sl_avg_break+ch_avg_break+cu_avg_break+si_avg_break+n_fastball_formatted+fastball_avg_speed+breaking_avg_speed+n_offspeed_formatted+offspeed_avg_speed+meatball_percent+in_zone_percent+edge_percent+f_strike_percent+z_swing_miss_percent+oz_swing_miss_percent+X30_count_percent+X20_count_percent, pitches)



reg1<-stepAIC(empty,direction='forward',scope=list(lower=empty,upper=full))
summary(reg1)
reg2<-stepAIC(empty,direction='both',scope=list(lower=empty,upper=full))
summary(reg2)
reg3<-stepAIC(full,direction='backward',scope=list(lower=empty,upper=full))
summary(reg3)

pitchesfull<- pitches
myvars<- c("xslg", "avg_horizontal_brk", "sl_avg_break", "ch_avg_break", "cu_avg_break", "si_avg_break", "n_fastball_formatted", "fastball_avg_speed", "z_swing_miss_percent", "oz_swing_miss_percent", "X20_count_percent", "exit_velocity_avg", "launch_angle_avg")
pitches<- pitches[myvars]

r1<-regsubsets(pitches[,c(2:13)],pitches[,1],nbest=4,nvmax=8)
rsum<- summary(r1)
bic<- rsum$bic
rsq<- (rsum$rsq)
rss<- (rsum$rss)
cp<- (rsum$cp)
adjr2<- (rsum$adjr2)
rsum
modelstats<- data.frame(bic,rsq, rss, cp, adjr2)
write.csv(modelstats, "RegProjectModelStats.csv")

model1<- lm(xslg~sl_avg_break+X20_count_percent+n_fastball_formatted+z_swing_miss_percent+oz_swing_miss_percent+exit_velocity_avg+launch_angle_avg, data=pitches)
model2<- lm(xslg~sl_avg_break+X20_count_percent+n_fastball_formatted+z_swing_miss_percent+oz_swing_miss_percent+exit_velocity_avg+launch_angle_avg, data=pitches)                            
model3<- lm(xslg~sl_avg_break+ch_avg_break+X20_count_percent+z_swing_miss_percent+oz_swing_miss_percent+exit_velocity_avg+launch_angle_avg, data=pitches)                            
summary(model1)
summary(model2)
summary(model3)
anova(model1)
anova(model2)
anova(model3)
PRESS(model1)
PRESS(model2)
PRESS(model3)


reg2<- lm(xslg~sl_avg_break+X20_count_percent+z_swing_miss_percent+oz_swing_miss_percent+exit_velocity_avg+launch_angle_avg, train)                            
summary(reg2)
reg3<- lm(xslg~sl_avg_break+ch_avg_break+X20_count_percent+z_swing_miss_percent+oz_swing_miss_percent+exit_velocity_avg+launch_angle_avg, train)                            
summary(reg3)

PRESS(reg2)
PRESS(reg3)


p2<-predict(reg2,test)
sum((p2-test["xslg"])^2)
p3<-predict(reg3,test)
sum((p3-test["xslg"])^2)




reg2a<- lm(xslg~sl_avg_break+X20_count_percent+z_swing_miss_percent+oz_swing_miss_percent+exit_velocity_avg+launch_angle_avg, test)                            
summary(reg2a)
reg3a<- lm(xslg~sl_avg_break+ch_avg_break+X20_count_percent+z_swing_miss_percent+oz_swing_miss_percent+exit_velocity_avg+launch_angle_avg, test)                            
summary(reg3a)

PRESS(reg2a)
PRESS(reg3a)


p2a<-predict(reg2a,train)
sum((p2a-train["xslg"])^2)
p3a<-predict(reg3a,train)
sum((p3a-train["xslg"])^2)


library(caret)
c<-trainControl(method="cv",number=10)
m2<-train(xslg~sl_avg_break+X20_count_percent+z_swing_miss_percent+oz_swing_miss_percent+exit_velocity_avg+launch_angle_avg, pitches, trControl=c,method="lm")
m3<-train(xslg~sl_avg_break+ch_avg_break+X20_count_percent+z_swing_miss_percent+oz_swing_miss_percent+exit_velocity_avg+launch_angle_avg, pitches, trControl=c, method="lm")




summary(m2)
m2$results
m2$finalModel
m2$resample

summary(m3)
m3$results
m3$finalModel
m3$resample



pitchesfull$PredictedXSLG<- predict(m2, pitchesfull)
write.csv(pitchesfull, "FinalExpectedSlugging.csv")
