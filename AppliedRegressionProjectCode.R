team<- read.csv("2019TeamPitchingStats.csv", sep=";")
player<- read.csv("2019PlayerPitchingStats.csv", sep=";")
player<- subset(player, select=-c(innings_pitched,home_runs_allowed))
names(team)[1]<- "runs_allowed_per_game"


#Basic Team pitching are used to predict team Wins
#H=#Hits
#HR=# Homeruns
#BB=Walks (4 balls in an at bat, runner takes first base)
#SO=Strikeout(3 Strikes in an at bat, batter is out)
#IBB=Intentional walk(team gives batter first base on purpose)
#HBP=Hit by Pitch(team accidentally hits batter and gives him first base)
#WHIP= Walks+Hits per inning pitched
#Anything with "9" after means "Per 9 innings" this is a way of standardizing the variables
teamreg<- lm(W~., data=team)
summary(teamreg)


#We will be using advanced statistics to predict individual player ERA.
#Pitchers included have pitched at least 150 innings
#4 Categories: 
#Contact Quality by the batter (Exit Velocity, Launch Angle, Solid Contact %, Hard Contact %)
#Pitch selection (Fastball %, FastBall average speed, Non-fastball%, Non-Fastball average speed)
#Pitch location in the strike zone(out zone %, out zone contact%, in zone %, in-zone contact %) "in zone" is defined by "meatball" meaning the pitch is right in the middle of the strike zone
#Basic stats (ERA=(EarnedRuns/InningsPitched)*9)- A way of calculating how many runs a pitcher allows per 9 inning  (HomeRuns per inning= how many homeruns a pitcher allows per inning pitched)
playerreg<- lm(ERA~., data=player)
summary(playerreg)


#-------------For the purpose of the project---------------
#Part 1 (Team Regression)
#Predicting Team wins is a good way to introduce regression and analyze simply how to predict wins
#Intuitively, the less runs a team gives up, the more they are likely to win, so there shouldnt be too many surprises that these variables are going to translate to runs and wins (R^2 is .82)
#This is a good way to introduce the idea of ERA and how individual velocities/angles relate in part 2

#Part 2 (Player Regression)
#For the player regression, this is a way to analyze "Stat Cast" stats that are recorded during games using advanced tools
#ERA is the dependent variable to see how velocity and launch angles can be used in different situations/pitches

#Conclusion
#I would like to illustrate:
#Stat Cast Stats------->ERA----------->Wins

#Note: ERA is a stat that is controlled by the pitcher, "Earned" means fielding errors are not calculated in this average
#Wins are the most important goal for the team
#Ideally Id like to show how these velocities can translate to wins
#We could also perform a third regression at the end if we have established variables that are most important

