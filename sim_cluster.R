### normal center
# center
c1 <- sample(c(-1,1),100,replace=T)
c2 <- sample(c(-1,1),100,replace=T)
# check center
cor(c1,c2)
dist(rbind(c1,c2), method = "euclidean")
# generate data
cls1 <- matrix(c1+rnorm(100*200),ncol=200)
cls2 <- matrix(c2+rnorm(100*200),ncol=200)
# headmap by euclidean, pearson, abspearson
library(amap)
dat <- cbind(cls1,cls2)
X11()
heatmap(dat,ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="euclidean")},scale="none")
X11()
heatmap(dat,ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="pearson")},scale="none")
X11()
heatmap(dat,ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="abspearson")},scale="none")
### cor center by correlation matrix
library(mvtnorm)
s <- rep(c(0.9,-0.9),each=50)%*%t((1:100)/(1:100))
s[,51:100] <- -1*s[,51:100]
diag(s) <- 1 
# check center
image(s)
cor(t(rmvnorm(2,sigma=s)))
# generate data;
dat <- rmvnorm(200,sigma=s)
# headmap by euclidean, pearson, abspearson
library(amap)
X11()
heatmap(dat,distfun = function(x){Dist(x,method="euclidean")},scale="none")
X11()
heatmap(dat,distfun = function(x){Dist(x,method="pearson")},scale="none")
X11()
heatmap(dat,distfun = function(x){Dist(x,method="abspearson")},scale="none")
### cor center by linear model
# y=ax+b; where non-limited b
x0 <- rnorm(100)
a1 <-  1
x1 <- x0*a1
a2 <- -1
x2 <- x0*a2
# check center
cor(x1,x2)
dist(rbind(x1,x2), method = "euclidean")
# generate data;
b1 <- runif(200,20,50)
cls1 <- matrix(rep(x1,200),ncol=200)+matrix(rep(b1,100),ncol=200,byrow=T)+matrix(rnorm(100*200,sd=.2),ncol=200)
b2 <- runif(200,20,50)
cls2 <- matrix(rep(x2,200),ncol=200)+matrix(rep(b1,100),ncol=200,byrow=T)+matrix(rnorm(100*200,sd=.2),ncol=200)
cor(cbind(cls1[,1:3],cls2[,1:3]))
dat <- cbind(cls1,cls2)
# headmap by euclidean, pearson, abspearson
library(amap)
X11()
heatmap(dat,ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="euclidean")},scale="none")
X11()
heatmap(dat,ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="pearson")},scale="none")
X11()
heatmap(dat,ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="abspearson")},scale="none")
X11()
heatmap(dat,ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="euclidean")},scale="row")
X11()
heatmap(dat,ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="pearson")},scale="row")
X11()
heatmap(dat,ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="abspearson")},scale="row")
### scale on column to remove intercept
X11()
heatmap(apply(dat,2,scale),ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="euclidean")},scale="none")
X11()
heatmap(apply(dat,2,scale),ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="pearson")},scale="none")
X11()
heatmap(apply(dat,2,scale),ColSideColors=rep(c("red","blue"),each=200),distfun = function(x){Dist(x,method="abspearson")},scale="none")


