####################################
## Confirming the Platoon Effect ###
####################################

######################
# Preparing the data #
######################

library(dplyr)
#importing the data I webscraped from mlb.com/stats/
versusLeft <- read.csv("data/versusLeft.csv")
versusRight <- read.csv("data/versusRight.csv")
versusLeft <- versusLeft[,c("FIRSTNAME", "LASTNAME", "OBP")]
versusRight <- versusRight[,c("FIRSTNAME", "LASTNAME", "OBP")]

##This data doesn't have the batting hand yet, for that we merge with Lahman database
library(Lahman)
##contains all batters
data(Batting)
Batting <- Batting[Batting$yearID==2019,]
##contains bathand info and names
data(Master)
merged <- merge(Batting, Master)
merged <- merged[,c("bats","nameFirst","nameLast")]
colnames(merged)[2] <- "FIRSTNAME"
colnames(merged)[3] <- "LASTNAME"
df = merged[!duplicated(merged$LASTNAME, merged$LASTNAME),]

withBattingHand <- merge(versusLeft, df, by = c("LASTNAME", "FIRSTNAME"))
battingVersusLeft <- withBattingHand[!duplicated(withBattingHand$LASTNAME, withBattingHand$LASTNAME),]

withBattingHand <- merge(versusRight, df, by = c("LASTNAME", "FIRSTNAME"))
battingVersusRight <- withBattingHand[!duplicated(withBattingHand$LASTNAME, withBattingHand$LASTNAME),]
summary(battingVersusLeft)
summary(battingVersusRight)
##39 Lefties, 65 Righties
##mean OBP is incredibly similar

#creating a list of dataframes
dfList <- NULL
dfList[[1]] = battingVersusLeft
dfList[[2]] = battingVersusRight
library(data.table)
boundBatting <- rbindlist(dfList, idcol = "origin")
boundBatting$origin <- factor(boundBatting$origin, levels=c(1,2),labels = c("vsLeft", "vsRight"))

##################################
## Exploring the platoon effect ##
##################################
library(ggplot2)
ggplot(boundBatting, aes(x=origin, y=OBP, fill=bats)) + 
  geom_boxplot()
par(mfrow=c(1,2))
boxplot(OBP ~ bats, battingVersusLeft)
boxplot(OBP ~ bats, battingVersusRight)

###########
## Trees ##
###########

##here I build a tree to see if based on OBP and batting hand, the tree can guess if this stat combination is from the vsLeft or vsRight table
library(rpart)
library(tree)
hand.ind<-rpart(origin~OBP + bats,boundBatting,method="class")
plot(hand.ind,uniform=F)         
text(hand.ind)
printcp(hand.ind)
plotcp(hand.ind)

rpart.pred <- predict(hand.ind,type="class")
table(boundBatting$origin, rpart.pred, dnn=c("From","Classified into"))
#it has about a 70% chance of getting it correct, which is pretty cool.
summary(hand.ind)


