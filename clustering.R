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
