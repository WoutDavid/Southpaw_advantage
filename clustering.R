## This Rscript uses several (blind) clustering methods to see if any of them pick up the difference in lefties and righties as a cluster
library(plot.matrix)
pitchers <- read.csv("data/baseballsavant_2019.csv")
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

##exploring known grouping structure "Pitch hand"
par(mfrow=c(1,1))
group <- NA
group[pitchers$pitch_hand =="R"] <- 2
group[pitchers$pitch_hand == "L"] <- 1
table(group)
table(pitchers$pitch_hand)
pairs(pitchers[,7:15], col = c("red","purple")[group])
##It's obvious that I'm not going to find a nice linear split anywhere.

##########################
## Graphical clustering ##
##########################
pit.mat <- as.matrix(pitchers[,7:15]) #9 remaining variables̉
#pit.mat<- as.matrix(pitchers_spin[,2:4])
##we need to standarize for these numbers to make any sense
rmean<-apply(pit.mat,2,mean)
rvar<-apply(pit.mat,2,var)
rstd <- sqrt(rvar)
for(k in (1:9)){
  pit.mat[,k] <- (pit.mat[,k]-rmean[k])/rstd[k]
}
##check if it is scaled and normalized
plot(var(pit.mat))
min(apply(pit.mat,2,min)) #min=-4.5
max(apply(pit.mat,2,max)) #max= 3
#create plot without dots
plot(c(0,10),c(-4.5,3),type="n",xlab="var",ylab="Value",main="Profile Plot of spin rate variables")
for (k in (1:262)){
  points(1:9,pit.mat[k,],type="l")
}
##No clear structuring going on, this might change if we only look at spin rate, where offspeed spin rate should be higher for Lefties
##did that, profile plot is indeed a bit more clear
##stars
stars(pit.mat)



#############################
## Hierarchical Clustering ##
#############################
#recreating so it isn't normalized anymore
## I might actually want to center and scale, because 
pit.mat <- as.matrix(pitchers[,7:15])
rownames(pit.mat) <-pitchers$last_name
colnames(pit.mat) <- colnames(pitchers)[7:15]
pit.clus <-hclust(dist(pit.mat), method="average")
plot(pit.clus)
pit.mat_no_outliers <- pit.mat[!rownames(pit.mat)%in%"Baez",]
pit.clus2 <-hclust(dist(pit.mat_no_outliers), method="average")
plot(pit.clus2)
##looks a bit better, i'll continue without the outlier
pit.cutTree <- cutree(pit.clus2,k=2) ## --> contains index vector you can use to color your plots
plot(pit.cutTree) ##it's obvious that this is not going to be the L vs R grouping structure
pairs(pitchers[,7:15], col = c("red","purple")[pit.cutTree])
#hey look at that, it's not the grouping structure I wanted

##TODO Retry with scaled/centered data and then also try different values of K, or complete linakge


