library(Lahman)
##contains all batters
data(Batting)
unique(Batting$yearID)
Batting <- Batting[Batting$yearID==2019,]
##contains bathand info and names
data(Master)
merged <- merge(Batting, Master)
merged <- merged[,c("yearID", "bats", "nameFirst", "nameLast")]

