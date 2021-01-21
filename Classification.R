#The idea of this script is to see if i can make a supervised classifier that can tell the difference between a left handed
#or right handed pitcher based on their pitch metrics.
pitchers <- read.csv("data/baseballsavant_2019.csv")
##how many Left and Right handed pitchers are there actually
table(pitchers$pitch_hand)
names(pitchers)
for(i in 1:ncol(pitchers)) {   
  print(sum(is.na(pitchers[,i])))
}
pitchers <- subset(pitchers, select=-X)
pitchers <- na.omit(pitchers)
nrow(pitchers)
table(pitchers$pitch_hand)

##subsets########
pitchers_speed <- subset(pitchers, select=c("pitch_hand", "fastball_avg_speed", "breaking_avg_speed", "offspeed_avg_speed"))
pitchers_break <- subset(pitchers, select=c("pitch_hand", "fastball_avg_break", "breaking_avg_break", "offspeed_avg_break"))
pitchers_spin <- subset(pitchers, select=c("pitch_hand", "fastball_avg_spin", "breaking_avg_spin", "offspeed_avg_spin"))
###################

###########################
## Discriminant Analysis ##
###########################
library(MASS)
attach(pitchers)
attach(pitchers_spin)
kernel.lda<-lda(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers)
kernel.lda
plot(kernel.lda)
kernel.pred<-predict(kernel.lda)
plot(kernel.pred$posterior,main="Posterior probabilities for belonging to Left or Right")
table(pitchers$pitch_hand,kernel.pred$class,dnn=c("From","Classified into"))
##lda still does pretty bad, it gets only 1/3d correct for the L's, which is bad ofcourse.
kernel.ldacv<-lda(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers, CV=TRUE)
table(pitchers$pitch_hand,kernel.ldacv$class,dnn=c("From","Classified into"))
##cross validation actually does worse!!!

##quadratic
kernel.qda<-qda(pitch_hand~fastball_avg_speed+fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers)
kernel.predq<-predict(kernel.qda)
table(pitchers$pitch_hand,kernel.predq$class,dnn=c("From","Classified into"))
##interesting, this one does a bit better, half of the L's is correct
kernel.qdacv<-qda(pitch_hand~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers, CV=TRUE)
table(pitchers$pitch_hand,kernel.qdacv$class,dnn=c("From","Classified into"))
##worse again, that's super weird.

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
summary(hand.ind)

##RandomForest
attach(pitchers)
?randomForest()
pitchers$pitch_hand <- as.factor(pitchers$pitch_hand)  
fit <- randomForest(pitchers$pitch_hand~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break)
print(fit) # view results
plot(fit)

library(tree)
attach(pitchers)
##had some issues here, it's important that the response variable is a vector for some reason
pitchers$pitch_hand <- as.factor(pitchers$pitch_hand)  
tree.tree <-tree(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,method="recursive.partition",split="deviance")
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

#Mclust
library(mclust)
clas <- MclustDA(pitchers[,7:15], pitchers$pitch_hand)
summary(clas)
##33-39
plot(clas)

##logistic regression
attach(pitchers)
pit.log <- glm(pitch_hand ~ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers, family=binomial(link="logit"))
summary(pit.log)
exp(cbind(OR =pit.log$coefficients, confint(pit.log)))
##this is NOT doing well, this is obviously not a logistic regression type of problem.

##TODO import a new pitching dataset to serve as a validation dataset.

######
##small extra classifiers

##Neural Net
library(nnet)
fit <- nnet(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers, size=10, decay=0.0001, maxit=500)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, pitchers[,7:15], type="class")
predictions
# summarize accuracy
table(pitchers$pitch_hand,predictions)

##flexible discriminant analysis
library(mda)
# fit model
fit <- fda(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, pitchers[,7:15])
# summarize accuracy
table(predictions, pitchers$pitch_hand)


##SVM
library(kernlab)
fit <- ksvm(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, pitchers[,7:15], type="response")
# summarize accuracy
table(predictions, pitchers$pitch_hand)

##KNN
# fit model
library(caret)
fit <- knn3(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers, k=5)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, pitchers[,7:15], type="class")
# summarize accuracy
table(predictions, pitchers$pitch_hand)

# load the package
library(e1071)
data(pitchers)
# fit model
fit <- naiveBayes(pitch_hand~fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break, data=pitchers)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, pitchers[,7:15])
# summarize accuracy
table(predictions, pitchers$pitch_hand)

##TODO use either boosting, bagging or stacking to improve the performance of the decision tree