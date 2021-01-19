##The idea of this Rscript is to import the statcast data of the MLB pitchers of 2020 and see if i can use their pitch stats to find a classifier for them being a lefty or righty
pitchers <- read.csv("data/baseballsavant_2019.csv")
##how many Left and Right handed pitchers are there actually
table(pitchers$pitch_hand)
pitchers <- as.data.frame(pitchers)
names(pitchers)
for(i in 1:ncol(pitchers)) {   
  print(sum(is.na(pitchers[,i])))
}
pitchers <- subset(pitchers, select=-X)
pitchers <- na.omit(pitchers)

##profile plot
pit.mat <- as.matrix(pitchers[,5:17]) #13 remaining variables
 #max=3100

##we need to standarize for these numbers to make any sense
rmean<-apply(pit.mat,2,mean)
rvar<-apply(pit.mat,2,var)
rstd <- sqrt(rvar)
for(k in (1:13)){
  pit.mat[,k] <- (pit.mat[,k]-rmean[k])/rstd[k]
}
var(pit.mat)
apply(pit.mat,2,min) #min=0
apply(pit.mat,2,max)
#create plot without dots
plot(c(0,14),c(-5,7),type="n",xlab="var",ylab="Value",main="Profile Plot")
for (k in (1:385)){
  points(1:13,pit.mat[k,],type="l")
}
##super cluttered, i'll probably have cut down on my observations

##andrews plot
t <-seq(-pi,pi,length=500)
plot(c(-pi,pi),c(-6,7),type="n",xlab="var",ylab="Value",main="Andrews Plot - Pitcher Data")
for (k in (1:385)){
  crseq <- (pit.mat[k,1]/sqrt(2))+pit.mat[k,2]*sin(t) + pit.mat[k,3]*cos(t)+pit.mat[k,4]*cos(2*t)
  points(t,crseq,type="l")
}
#another option
library(andrews)                   #See ?andrews
andrews(pit.mat,type=4,ymax=2,clr=2)  #With color suggestions for 2 groups
#same problem, it's pretty crowded
##stars lol
stars(pit.mat)

###########################
##Hierarchical clustering #
###########################
##recreating the matrix of the data
pit.mat <- as.matrix(pitchers[,5:17])
#performing hclustering
pit.clus <-hclust(dist(pit.mat), method="average")
plot(pit.clus)

pit.gp <- cutree(pit.clus,k=2)
class(pit.gp)
table(pit.gp)
plot(pitchers$fastball_avg_speed,pitchers$breaking_avg_speed, col = pit.gp, cex=1.5,main="")
plot(pitchers$fastball_avg_spin,pitchers$breaking_avg_spin, col = pit.gp, cex=1.5,main="")

#converting the hierarchical k=2 clustring calls into a character vector
pit.clus.results <- character()
for (i in 1:length(pit.gp)){
  if (pit.gp[i]==1){
    pit.clus.results[i] = "R"
  }
  if (pit.gp[i]==2){
    pit.clus.results[i] = "L"
  }
}

##compare that character vector to the actual data
correct_L_calls = 0
for (i in 1:nrow(pitchers)){
  if ((pitchers$pitch_hand[i]=="L") & pit.clus.results[i]=="L"){
    correct_L_calls = correct_L_calls + 1
  }
}
correct_L_calls
##hierarchical clustering looks to be doing quite well numberwise, but it only gets 24 correct L calls, which is not great

###############################
##non-Hierarchical clustering #
###############################
pit.kcl <- kmeans(pit.mat, 2, 100)
pit.kcl
library(cluster)
clusplot(pit.mat,pit.kcl$cluster,stand=TRUE,labels=3,main="k-means clustering on pitcher data")

##try with PCA
pit.pca <- princomp(pit.mat)
pit.pkcl <- kmeans(pit.pca$scores,2,20)
par(mfrow=c(1,1))
plot(pit.pca$scores[,1:2], col = pit.pkcl$cluster)
points(pit.pkcl$centers[,c(1,2)], col = 1:2, pch ="+", cex=2)
title ("k-means clustering on the first two Principal Components - Rock Data")

table(pitchers$pitch_hand,pit.kcl$cluster)
###according to hierarchical get 35 right, but a whole lot more wrong, so this is not succesful.
##obviously not good, it's finding some other thing to base the k's on, which makes sense, the lefty-vs-righty is not the most prominent distinction you can make here

###########################
## Discriminant Analysis ##
###########################
library(MASS)
names(pitchers)
attach(pitchers)
kernel.lda<-lda(pitch_hand~n_fastball_formatted+ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ n_breaking_formatted+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ n_offspeed_formatted+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers)
kernel.lda
plot(kernel.lda)
kernel.pred<-predict(kernel.lda)
plot(kernel.pred$posterior,main="Posterior probabilities for belonging to Left or Right")
table(pitchers$pitch_hand,kernel.pred$class,dnn=c("From","Classified into"))
##lda still does pretty bad, it gets only 1/3d correct for the L's, which is bad ofcourse.
kernel.ldacv<-lda(pitch_hand~n_fastball_formatted+ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ n_breaking_formatted+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ n_offspeed_formatted+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers, CV=TRUE)
table(pitchers$pitch_hand,kernel.ldacv$class,dnn=c("From","Classified into"))
##cross validation actually does worse!!!

##quadratic
kernel.qda<-qda(pitch_hand~n_fastball_formatted+ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ n_breaking_formatted+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ n_offspeed_formatted+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers)
kernel.predq<-predict(kernel.qda)
table(pitchers$pitch_hand,kernel.predq$class,dnn=c("From","Classified into"))
##interesting, this one does a bit better, half of the L's is correct
kernel.qdacv<-qda(pitch_hand~n_fastball_formatted+ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ n_breaking_formatted+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ n_offspeed_formatted+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers, CV=TRUE)
table(pitchers$pitch_hand,kernel.qdacv$class,dnn=c("From","Classified into"))
##worse again, that's super weird.


#####################
## TREES ##
###########

library(rpart)
library(tree)
hand.ind<-rpart(pitch_hand~+n_fastball_formatted+ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ n_breaking_formatted+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ n_offspeed_formatted+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,pitchers,method="class")
plot(hand.ind,uniform=F)         
text(hand.ind)
printcp(hand.ind)
plotcp(hand.ind)

rpart.pred <- predict(hand.ind,type="class")
table(pitchers$pitch_hand, rpart.pred, dnn=c("From","Classified into"))
##alright alright this tree is starting to do a lil better: 53-43
summary(hand.ind)

attach(pitchers)
##had some issues here, it's important that the response variable is a vector for some reason
pitch_hand <- as.factor(pitch_hand)  
handtree.ind <-tree(pitch_hand~n_fastball_formatted+ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ n_breaking_formatted+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ n_offspeed_formatted+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,method="recursive.partition",split="deviance")
plot(handtree.ind,uniform=F)         
text(handtree.ind) 
##here i have unnecessary splits in the end, so i'm gonna have to do some pruning
handtree.ind.cv <- cv.tree(handtree.ind,FUN=prune.tree)
plot(handtree.ind.cv$size,handtree.ind.cv$k,type="l") 
plot(handtree.ind.cv$size,handtree.ind.cv$dev,type="l",xlab="Number of end nodes",ylab="Deviance") 
#use info from these plots to then prune the actual tree
pruned.handtree.ind<-prune.tree(handtree.ind,best=6) 
summary(pruned.handtree.ind)
plot(pruned.handtree.ind)
text(pruned.handtree.ind)
tree.pred <- predict(pruned.handtree.ind,type="class")
table(pitchers$pitch_hand, tree.pred, dnn=c("From","Classified into"))
##here it seems that the process really seems to prefer classifying things into R instead of R, since it's dominating. 
##this might be a clue that pointing the tree towards classifying L correctly (like boosting) might improve performance.


########################
## Logistic regression #
########################    NOT WORKING YET
temp=numeric()
for (i in 1:length(pitch_hand)){
  if (pitch_hand[i]==1){
    temp[i] = 0
  }
  if (pitch_hand[i]==2){
    temp[i] = 1
  }
}
temp
pitch_hand <- temp
temp <- 2-pitch_hand
logitmodel<-glm(cbind(pitch_hand,2-pitch_hand) ~ n_fastball_formatted+ fastball_avg_speed+ fastball_avg_spin+ fastball_avg_break+ n_breaking_formatted+ breaking_avg_speed+ breaking_avg_spin+ breaking_avg_break+ n_offspeed_formatted+ offspeed_avg_speed+ offspeed_avg_spin+ offspeed_avg_break,family=binomial,pitchers)
2-pitch_hand

