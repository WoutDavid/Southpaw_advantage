##loading the data
library(plotrix)
library(dplyr)
##Baseball
pitchers <- read.csv("data/baseballsavant_2019.csv")
pitchers <- as.data.frame(pitchers)
names(pitchers)
for(i in 1:ncol(pitchers)) {   
  print(sum(is.na(pitchers[,i])))
}
pitchers <- subset(pitchers, select=-X)
pitchers <- na.omit(pitchers)
##for now i'm working with 78 pitchers that don't have an offspeed pitch, I might come back from that
##I already came back from that

##Exploratory analysis
attach(pitchers)
head(pitchers)
##PCA
##selecting the numerical coumns
##pairs doesn't do much cause shit is correlated and i have way too many variables?
mat <- pitchers[,7:15]
mat
pairs(mat)
cor(mat)

pca <- princomp(mat)
pca$loadings
screeplot(pca, type="lines")
summary(pca)

##compute cov matrix
cov <- cov(mat)
xxx <- cbind(mat,pca$scores)
##check correlation between variables and principal components
cor(xxx)
##possibly todo: PCA on centered/scaled data, in which you use the cov matrix to perform PCA on.
#5 PCA's seem enough to describe most of the variance, almost too well acutally, seems a bit strange.
#it's intersting thet the first 4 princomps actually hold the variance well distributed
##some exploring plots:
##BIPLOT
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
  axis(1,at=c(-1,-.8,-.6,-.4,-.2,0,.2,.4,.6,.8,1),cex=.8)
  axis(2,at=c(-1,-.8,-.6,-.4,-.2,0,.2,.4,.6,.8,1),cex=.8)
  box( )
  
  #ability to plot rownames on the plot, dimnames(x)[[1]] necessary
  #text(R.B[,1]-.05,R.B[,2]+.05,as.character(dimnames(x)[[1]]),cex=0.5)
  points(R.B[,1],R.B[,2],pch=".")
  
  
  points(C.B[,1],C.B[,2],pch=".")
  text(C.B[,1]-.05,C.B[,2]+.05,as.character(dimnames(x)[[2]]),cex=0.8)
  
  for (i in seq(1,nrow(C.B),by=1))
    arrows(0,0,C.B[i,1],C.B[i,2])
  
  #Draw circle unit
  draw.circle(0,0,1,border='black')
  
  mtext('Correlational Biplot on IRIS data (Factor extraction with Principal Components)',side=1,outer=T,cex=1,line=3,adj=0)
  results<-list('correlation matrix'=r,'column effects'=C.B,'row effects'=R.B)
  cat('The goodness of fit for the correlation matrix is',gfr,'for the centered, standardized design matrix',gfz,'and for the Mahalanobis distances is',gfd,' ')
  results
}
p.mat<- as.matrix(mat)
class(p.mat)
p.mat
par(mfrow=c(1,1))
PCA.biplot(p.mat)

#PCA's looked good, so we'll use those to calculate the factor analysis
p.cor <- cor(p.mat)
p.pca <-eigen(p.cor)
#i'm using 5 largest eigenvalues, cause that's what the screeplot implied.
p.p <- p.pca$vectors[,1:4]            #Select first 3 eigenvectors, corresponding to the three largest eigenvalues
p.d <- diag(sqrt(p.pca$values[1:4]))  #Make a diagonal matrix of the standard deviations of the Principal Components
p.B <- p.p%*%p.d  #  Scale Principal Component loadings so they become correlations, thus Factor Loadings. Post-multiply with the standard deviations of the Principal Components
rownames(p.B) <- names(pitchers)[7:15]      #As row names for the factor loadings matrix, take the variable names of cereal
colnames(p.B) <- c("B1","B2","B3","B4")       #Give the columns of the factor loadings matrix names
p.B                                      #Show loadings pattern                                
p.B%*%t(p.B)                           #Calculate and show the estimated correlation matrix, based on the loadings pattern. The communalities are on the head diagonal
p.psi <- p.cor - p.B%*%t(p.B)      #Calculate the difference between the observed correlation matrix and the estimated one, specific variances are on the head diagonal
p.psi                                    #Show the residual correlation matrix, with the specific variances (uniqueness) on the main diagonal
diag(p.B%*%t(p.B))                     #The communalities are the sum of the squared loadings, thus the head diagonal of the estimated correlation matrix 
1-diag(p.psi)                            #The communalities are  1-specific variances
##this all works, I just havent taken the time to actually interpret it

