library(MASS)
library(MPV)
library(car)

pitches<- read.csv("2019PitchArsenal.csv", sep=";")
#pitches<- subset(pitches, select=-c(first_name, last_name, yea p_formatted_ip, p_total_hits, p_home_run))
names(pitches)[8]<- "avg_horizontal_brk"
names(pitches)[9]<- "avg_vertical_brk"


#Added constants because some variables are negative
#Another approach may be better
pitches$avg_horizontal_brk<- (pitches$avg_horizontal_brk)+16.8
pitches$avg_vertical_brk<- (pitches$avg_vertical_brk)+37.9
pitches$launch_angle_avg<- (pitches$launch_angle_avg)+8.5

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

#Predicting Launch Angle
pitcherla<- lm(launch_angle_avg~n_ff_formatted+avg_horizontal_brk+avg_vertical_brk+n_sl_formatted+sl_avg_break+n_ch_formatted+ch_avg_break+n_cukc_formatted+cu_avg_break+n_sift_formatted+si_avg_break+n_fastball_formatted+fastball_avg_speed+n_breaking_formatted+breaking_avg_speed+n_offspeed_formatted+offspeed_avg_speed+meatball_percent+in_zone_percent+edge_percent+f_strike_percent, pitches)
summary(pitcherla)

#Predicting Exit Velocity
pitcherev<- lm(exit_velocity_avg~n_ff_formatted+avg_horizontal_brk+avg_vertical_brk+n_sl_formatted+sl_avg_break+n_ch_formatted+ch_avg_break+n_cukc_formatted+cu_avg_break+n_sift_formatted+si_avg_break+n_fastball_formatted+fastball_avg_speed+n_breaking_formatted+breaking_avg_speed+n_offspeed_formatted+offspeed_avg_speed+meatball_percent+in_zone_percent+edge_percent+f_strike_percent, pitches)
summary(pitcherev)

