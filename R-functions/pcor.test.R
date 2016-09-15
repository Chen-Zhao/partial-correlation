
pcor.test <- function(dat1,dat1condition,dat2,dat2condition,conf.int,plot,...){
  #https://onlinecourses.science.psu.edu/stat505/node/47
  require(ppcor)
  pcor1 <- pcor(cbind(dat1,dat1condition))
  pcor1 <- pcor1$estimate[1,2]
  pcor2 <- pcor(cbind(dat2,dat2condition))
  pcor2 <- pcor2$estimate[1,2]
  z1 <- 0.5*log((1+pcor1)/(1-pcor1))
  v1 <- 1/(nrow(dat1)-3-ncol(dat1condition))
  z2 <- 0.5*log((1+pcor2)/(1-pcor2))
  v2 <- 1/(nrow(dat2)-3-ncol(dat2condition))
  pvalue <- (pnorm(0,abs(z1-z2),sqrt(v1+v2)))*2
  
  conf1.1  <- qnorm((1-conf.int)/2,z1,sqrt(v1))
  conf1.1 <- (exp(2*conf1.1)-1)/(exp(2*conf1.1)+1)
  conf1.2  <- qnorm(1-(1-conf.int)/2,z1,sqrt(v1))
  conf1.2 <- (exp(2*conf1.2)-1)/(exp(2*conf1.2)+1)
  conf2.1  <- qnorm((1-conf.int)/2,z2,sqrt(v2))
  conf2.1 <- (exp(2*conf2.1)-1)/(exp(2*conf2.1)+1)
  conf2.2  <- qnorm(1-(1-conf.int)/2,z2,sqrt(v2))
  conf2.2 <- (exp(2*conf2.2)-1)/(exp(2*conf2.2)+1)
  if(plot==T){
    pcorcinf <- rbind(conf1=c(pcor1,conf1.1,conf1.2)，conf2=c(pcor2,conf2.1,conf2.2))
    require(plotrix)
    plotCI(1:2,pcorcinf[,1], li=pcorcinf[,2], ui=pcorcinf[,3],xlim=c(0.5,2.5),xlab="",ylab="Pcor",axes=F,
           main=paste("Pcor Z test (p-value: ",round(pvalue,3),")",sep=""),...)
    axis(1,at=c(1,2),labels = c("pcor1","pcor2"))
    axis(2)
    box()
  }
  return(list(pvalue,conf1=c(pcor1,conf1.1,conf1.2)，conf2=c(pcor2,conf2.1,conf2.2)))
}


dat1 <- matrix(rnorm(3000),ncol=6)[,1:2]
dat1condition <-  matrix(rnorm(3000),ncol=6)[,3:6]

dat2 <- matrix(rnorm(3000),ncol=6)[,1:2]
dat2condition <-  matrix(rnorm(3000),ncol=6)[,3:5]

conf.int <- 0.95

(pcorres <- pcor.test(dat1,dat1condition,dat2,dat2condition,conf.int=0.95,plot=T))
  



require(plotrix)
pcorcinf <- matrix(unlist(pcorres[2:3]),byrow=T,nrow=2)
plotCI(1:2,pcorcinf[,1], li=pcorcinf[,2], ui=pcorcinf[,3],xlim=c(0.5,2.5))

