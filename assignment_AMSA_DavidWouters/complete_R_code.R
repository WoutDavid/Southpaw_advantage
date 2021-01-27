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
battingVersusLeft <- battingVersusLeft[!battingVersusLeft$bats=="B",]

withBattingHand <- merge(versusRight, df, by = c("LASTNAME", "FIRSTNAME"))
battingVersusRight <- withBattingHand[!duplicated(withBattingHand$LASTNAME, withBattingHand$LASTNAME),]
battingVersusRight <- battingVersusRight[!battingVersusRight$bats=="B",]

summary(battingVersusLeft)
##39 Lefties, 65 Righties
hist(battingVersusLeft$OBP)
summary(battingVersusRight)
hist(battingVersusRight$OBP)
##Both OBP distributions seem normally distributed enough

#creating a list of dataframes
dfList <- NULL
dfList[[1]] = battingVersusLeft
dfList[[2]] = battingVersusRight
library(data.table)
combinedBatting <- rbindlist(dfList, idcol = "origin")
combinedBatting$origin <- factor(combinedBatting$origin, levels=c(1,2),labels = c("vsLeft", "vsRight"))

table(combinedBatting$origin, combinedBatting$bats)

########################
## Exploring the data ##
########################
library(ggplot2)
ggplot(combinedBatting, aes(x=origin, y=OBP, fill=bats)) + 
  geom_boxplot() +
  ggtitle("Batting performance versus left/right-handed pitchers") + xlab("Pitcher's handedness")

##################################
## Exploring the platoon effect ##
##################################

tempLeft <- battingVersusLeft[battingVersusLeft$bats=="R" | battingVersusLeft$bats=="L",c("OBP", "bats")]
tempRight <- battingVersusRight[battingVersusRight$bats=="R" | battingVersusRight$bats=="L",c("OBP", "bats")]
#Logistic regression for batting vs left
left.log <- glm(bats ~ OBP, data=tempLeft, family=binomial(link="logit"))
summary(left.log)
exp(cbind(OR =left.log$coefficients, confint(left.log)))

#Logistic regression for batting vs right
right.log <- glm(bats ~ OBP, data=tempRight, family=binomial(link="logit"))
summary(right.log)
exp(cbind(OR =right.log$coefficients, confint(right.log)))

anova(left.log, test="Chisq")
anova(right.log,test="Chisq")

###########
## Trees ##
###########
##here I build a tree to see if based on OBP and batting hand, the tree can guess if this stat combination is from the vsLeft or vsRight table
library(rpart)
library(rpart.plot)
combinedBatting <- combinedBatting[combinedBatting$bats=="R" | combinedBatting$bats=="L"]
obp.tree<-rpart(origin~OBP + bats,combinedBatting,method="class")
rpart.plot(obp.tree,yesno=T)         

rpart.pred <- predict(pruned.tree,type="class")
table(combinedBatting$origin, rpart.pred, dnn=c("From","Classified into"))
#it has about a 70% chance of getting it correct, which is pretty cool.
summary(obp.tree)


#################################
## Ordination of pitching data ##
#################################

##loading the data
library(dplyr)
pitchers <- read.csv("data/baseballsavant_2019.csv")
pitchers <- as.data.frame(pitchers)
names(pitchers)
##checking missing values
for(i in 1:ncol(pitchers)) {   
  print(sum(is.na(pitchers[,i])))
}
pitchers <- subset(pitchers, select=-X)
pitchers <- na.omit(pitchers)

##Exploratory analysis
attach(pitchers)

##PCA
##selecting the numerical columns
mat <- pitchers[,7:15]
dim(mat)
pairs(mat)
##linear correlation between spin rate and break, and also between the different speeds
##little correlation between speed and spin, which is good
#install.packages("plot.matrix")
library(plot.matrix)
plot(cor(mat))
##from this I can see that there is a  negative correlation between breaking avg speed and offspeed avg speed
pca <- princomp(mat)
plot(pca)
##it's clear that the first 3-4 pca's are probably just picking up on the three different types of pitches
##It's probably just catching up on the 3 types of pitches
pca$loadings
screeplot(pca, type="lines")
summary(pca)
library(ggplot2)
qplot(pca$scores[,1], pca$scores[,2], colour=pitchers$pitch_hand)

############
## BIPLOT ##
############

library(plotrix)
PCA.biplot<- function(x) {
  #x is the matrix to biplot, x is numeric, thus variables on ratio or interval scales
  #x has dimnames(x)[[2]] defined (and eventually dimnames(x)[[1]] defined)
  xm<-apply(x,2,mean)
  y<-sweep(x,2,xm)
  ss<-(t(y)%*%y)
  s<-ss/(nrow(x)-1)
  d<-(diag(ss))^(-1/2)
  e<-diag(d,nrow=ncol(x),ncol=ncol(x))
  z<-y%*%e   #variables of z are unit length scaled
  r<-t(z)%*%z
  q<-svd(z)
  gfd<-((q$d[1])+(q$d[2]))/sum(q$d)
  gfz<-(((q$d[1])^2)+((q$d[2])^2))/sum((q$d)^2)
  gfr<-(((q$d[1])^4)+((q$d[2])^4))/sum((q$d)^4)
  l<-diag(q$d,nrow=ncol(x),ncol=ncol(x))
  R.B<-q$u        #scores matrix
  C.B<-q$v%*%l    #loadings
  #possibility to stretch scores by a scale factor
  #scalefactor<-3.5
  #R.B<-q$u *scalefactor
  
  par(mar=c(4,4,4,4),pty='s',oma=c(5,0,0,0),font=2)
  plot(R.B[ ,1],R.B[ ,2],axes=F,xlim=c(-1,1),ylim=c(-1,1),xlab=' ',ylab=' ',cex=.8)
  mtext('First component',side=1,line=3,cex=.8)
  mtext('Second component',side=2,line=3,cex=.8)
  title("Correlational Biplot on Pitch Quality Metric")
  axis(1,at=c(-1,-.8,-.6,-.4,-.2,0,.2,.4,.6,.8,1),cex=.8)
  axis(2,at=c(-1,-.8,-.6,-.4,-.2,0,.2,.4,.6,.8,1),cex=.8)
  box( )
  
  points(R.B[,1],R.B[,2],pch=".")
  
  
  points(C.B[,1],C.B[,2],pch=".")
  text(C.B[,1]-.05,C.B[,2]+.05,as.character(dimnames(x)[[2]]),cex=0.8)
  
  for (i in seq(1,nrow(C.B),by=1))
    arrows(0,0,C.B[i,1],C.B[i,2])
  
  #Draw circle unit
  draw.circle(0,0,1,border='black')
}

p.mat<- as.matrix(mat)
par(mfrow=c(1,1))
PCA.biplot(p.mat)

#####################
## Factor analysis ##
#####################

##WORKING FA from source####################
pit.fmle.r <- factanal(mat,factors=4,scores="Bartlett",rotation="varimax")
pit.fmle.r

plot(pit.fmle.r$loadings[,1], 
     pit.fmle.r$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")

text(pit.fmle.r$loadings[,1]-0.08, 
     pit.fmle.r$loadings[,2]+0.08,
     colnames(mat),
     col="blue")
###########################################
## ----->> Factor 2 might be the difference in lefties/righties we see


#####################################
## Clustering of pitch metric data ##
#####################################


## This Rscript uses several (blind) clustering methods to see if any of them pick up the difference in lefties and righties as a cluster
library(plot.matrix)
pitchers <- read.csv("data/baseballsavant_2019.csv")
for(i in 1:ncol(pitchers)) {   
  print(sum(is.na(pitchers[,i])))
}
pitchers <- subset(pitchers, select=-X)
pitchers <- na.omit(pitchers)
table(pitchers$pitch_hand)
##72 lefties, 190 righties

##exploring known grouping structure "Pitch hand"
par(mfrow=c(1,1))
group <- NA
group[pitchers$pitch_hand =="R"] <- 2
group[pitchers$pitch_hand == "L"] <- 1
pairs(pitchers[,7:15], col = c("blue","red")[group])
##It's obvious that I'm not going to find a nice linear split anywhere.
##lefties seem to be on the "lower" side of all of the variables tho

#############################
## Hierarchical Clustering ##
#############################
pit.mat <- as.matrix(pitchers[,7:15])
rownames(pit.mat) <-pitchers$last_name
colnames(pit.mat) <- colnames(pitchers)[7:15]
pit.clus <-hclust(dist(pit.mat), method="average")
plot(pit.clus)
##it looks like there is an outlier the causes an immediate split, let's take that one out
pit.mat_no_outliers <- pit.mat[!rownames(pit.mat)%in%"Baez",]
pit.clus2 <-hclust(dist(pit.mat_no_outliers), method="average")
plot(pit.clus2)
##looks a bit better, i'll continue without the outlier
pit.cutTree <- cutree(pit.clus2,k=2) ## --> contains index vector you can use to color your plots
pairs(pitchers[,7:15], col = c("red","purple")[pit.cutTree])
#It's obvious that hierarchical clustering at k=2 level finds a completely different seperation

#################################
## Non-Hierarchical Clustering ##
#################################
pit.kmeans <- kmeans(pit.mat, 2)
pairs(pitchers[,7:15], col= c("blue","red")[pit.kmeans$cluster] )
#def not a perfecct group structure
library(cluster)
clusplot(pit.mat,pit.kmeans$cluster,stand=TRUE,main="k-means clustering on pitchers data")
table(pitchers$pitch_hand,pit.kmeans$cluster)
##left vs right is also here not the type of split that is being picked up by kmeans

#################################
## Bayesian clustering with EM ##
#################################
library(mclust)
fit <- Mclust(pit.mat)
plot(fit)
###choose option 2 here to look at a similar classification scheme as i've been doing with the pairs plot
summary(fit)
table(pitchers$pitch_hand, fit$classification)



############################################
## Classification of pitching metric data ##
############################################

#The idea of this script is to see if i can make a supervised classifier that can tell the difference between a left handed
#or right handed pitcher based on their pitch metrics.
pitchers <- read.csv("data/baseballsavant_2019.csv")
for(i in 1:ncol(pitchers)) {   
  print(sum(is.na(pitchers[,i])))
}
pitchers <- subset(pitchers, select=-X)
pitchers <- na.omit(pitchers)
table(pitchers$pitch_hand)

###########
## TREES ##
###########
attach(pitchers)
library(rpart)
library(rpart.plot)
rpart.tree<-rpart(pitch_hand~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers,method="class")
rpart.plot(rpart.tree, yesno=TRUE)
printcp(rpart.tree)
plotcp(rpart.tree)

rpart.pred <- predict(rpart.tree,type="class")
table(pitchers$pitch_hand, rpart.pred, dnn=c("From","Classified into"))
##alright alright this tree is starting to do a lil better: 38-34

##checking overfitting by applying this tree on the pitching dataset of 2020
pitchers_2020 <- read.csv("data/baseballsavant_2020.csv")
##how many Left and Right handed pitchers are there actually
pitchers_2020 <- subset(pitchers_2020, select=-X)
pitchers_2020 <- na.omit(pitchers_2020)
rpart.pred <- predict(rpart.tree, pitchers_2020,type="class")
table(pitchers_2020$pitch_hand, rpart.pred, dnn=c("From","Classified into"))

##################################
## Attempt at boosting the tree ##
##################################
library(rsample)      
library(gbm)          
library(caret)        
pitchers$pitch_hand <- as.numeric(as.factor(pitchers$pitch_hand))-1
##0 = left, 1 = right
##split data in training and test
pit.split <- initial_split(pitchers, prop = .7)
pit.train <- training(pit.split)
pit.test  <- testing(pit.split)
attach(pit.train)

##try a random hyper parameter config
gbm.fit <- gbm(
  formula = pitch_hand ~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,
  data = pit.train,
  distribution = "bernoulli",
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
print(gbm.fit)
##RMSE
sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")

##grid searching for the best parameters
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,              
  min_RMSE = 0                    
)

# randomize data before using train.fraction
random_index <- sample(1:nrow(pit.train), nrow(pit.train))
random_pit_train <- pit.train[random_index, ]

##grid search to find the best parameters
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  gbm.tune <- gbm(
    formula = pitch_hand ~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,
    distribution = "bernoulli",
    data = random_pit_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

##this shows the best 10 model configs, I choose the parameters of the top model
##This is a stochastic algorithm so if you rerun this, the parameters might differ
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

gbm.fit.final <- gbm(
  formula = pitch_hand ~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,
  distribution = "bernoulli",
  data = pit.train,
  n.trees = 6,
  interaction.depth = 5,
  shrinkage = 0.3,
  n.minobsinnode = 15,
  bag.fraction = .65, 
  train.fraction = 1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
##predict using the final model on the training data
pred <- predict.gbm(gbm.fit.final, n.trees = gbm.fit.final$n.trees, pit.test, type="response")
calibrate.plot(
  pit.test$pitch_hand,
  pred,
  distribution = "bernoulli",
  replace = TRUE,
  line.par = list(col = "black"),
  shade.col = "lightyellow",
  shade.density = NULL,
  rug.par = list(side = 1),
  xlab = "Predicted value",
  ylab = "Observed average",
  xlim = NULL,
  ylim = NULL,
  knots = NULL,
  df = 6,
)

## exploring results
##calculating RMSE
caret::RMSE(pred, pit.test$pitch_hand)
##plotting decision making of variables
plot.gbm(gbm.fit.final, i.var=c(1,4))
plot.gbm(gbm.fit.final, i.var=c(1,8))

##exploring relative influence of all variables
library(dplyr)
effects <- tibble::as_tibble(gbm::summary.gbm(gbm.fit.final, 
                                                  plotit = FALSE))
effects %>% utils::head()
library(ggthemes)
effects %>% 
  # arrange descending to get the top influencers
  dplyr::arrange(desc(rel.inf)) %>%
  # sort to top 10
  dplyr::top_n(9) %>%
  # plot these data using columns
  ggplot(aes(x = forcats::fct_reorder(.f = var, 
                                      .x = rel.inf), 
             y = rel.inf, 
             fill = rel.inf)) +
  geom_col() +
  # flip
  coord_flip() +
  # format
  scale_color_brewer(palette = "Dark2") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  xlab('Features') +
  ylab('Relative Influence') +
  ggtitle("variables plotted by influence in classification score")

#I choose to put an arbitrary threshold on 0.7, this is a part that i do not 100% understand, so there is bias in this decisian
table(pit.test$pitch_hand==0,pred<0.7)

-##################################################################################################################
## End of boosting attempt: This turned out to be quite a topic, so i'm not 100% sure I did everything correctly ##
###################################################################################################################

##trying another tree building method with pruning
library(tree)
attach(pitchers)
pitchers$pitch_hand <- as.factor(pitchers$pitch_hand)  
tree.tree <-tree(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers,method="recursive.partition",split="deviance")
plot(tree.tree)         
text(tree.tree) 
##here i have unnecessary splits in the end, so i'm gonna have to do some pruning
tree.tree.cv <- cv.tree(tree.tree,FUN=prune.tree)
plot(tree.tree.cv$size,tree.tree.cv$k,type="l") 
plot(tree.tree.cv$size,tree.tree.cv$dev,type="l",xlab="Number of end nodes",ylab="Deviance") 
#use info from these plots to then prune the actual tree
pruned.tree<-prune.tree(tree.tree,best=3) 
plot(pruned.tree)
text(pruned.tree)
summary(pruned.tree)
tree.pred <- predict(pruned.tree,type="class")
table(pitchers$pitch_hand, tree.pred, dnn=c("From","Classified into"))
##here it seems that the process really seems to prefer classifying things into R instead of R, since it's dominating. 
##this might be a clue that pointing the tree towards classifying L correctly (like boosting) might improve performance.
##40-32



######################################
## Multiple vanilla ML Classifiers: ##
######################################

#Mclust
library(mclust)
clas <- MclustDA(pitchers[,7:15], pitchers$pitch_hand)
summary(clas)
##33-39
plot(clas)
##choose 2 for a pairs-classification plot like previously

##flexible discriminant analysis
library(mda)
# fit model
fit <- fda(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers)
summary(fit)
predictions <- predict(fit, pitchers[,7:15])
table(pitchers$pitch_hand, predictions)
##24-48

##KNN
# fit model
attach(pitchers)
library(caret)
fit <- knn3(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers, k=5)
summary(fit)
predictions <- predict(fit, pitchers[,7:15], type="class")
table(pitchers$pitch_hand, predictions)
##28-44

##Naive Bayes
library(e1071)
# fit model
fit <- naiveBayes(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers)
summary(fit)
predictions <- predict(fit, pitchers[,7:15])
table(pitchers$pitch_hand, predictions)
##31-41