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


