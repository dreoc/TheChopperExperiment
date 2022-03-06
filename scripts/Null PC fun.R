#automatically setwd() to directory of source code
library(rstudioapi)    

null.pc.sim<-function(gpa, nsims=100, plot=FALSE){
  gpaEX <- gpa
  
  co<-two.d.array(gpaEX$coords)
  dim(co)
  
  #create distribution of normal values
  gpa.two.d <- co
  
  #sample random variables into a matrix
  meanvec<-apply(gpa.two.d, 2, FUN = mean)
  sdvec<-apply(gpa.two.d, 2, FUN = sd)
  
  # create PC simulation function
  pc.sim<-function(gpa.two.d){
    sim.mat<-t(sapply(1:nrow(gpa.two.d), 
                      function(x)rnorm(ncol(gpa.two.d), 
                                       mean=meanvec, 
                                       sd=sdvec)))
    dim(sim.mat) 
    pc<-prcomp(sim.mat, rank. = ncol(gpa.two.d) ,tol = 1.490116e-200)
    su<-summary(pc)
    return(su$importance[1,])
  }
  
  sds<-NULL
  for (i in 1:nsims){
    cat("iter. #", i, "\n")
    sim<-pc.sim(gpa.two.d)
    sds<-rbind(sds,sim)
  }
  dim(sds)

# Compute 95% CIs  
ci<-matrix(NA, nrow = ncol(sds), ncol=2 )
  for (k in 1:ncol(sds)){
    ci[k,]<-quantile(sds[,k], c(.025,.975))
  }
  ci
  pcgpa <-prcomp(co)
  gpa.su<-summary(pcgpa)
  STD <- gpa.su$importance[1,] 
  
  npcs<-25
  
if(plot==TRUE){
  plot(1:npcs, sds[1, 1:npcs], ylim=range(c(sds,STD)), type="n", axes = F,
       ylab="Variance Explained",
       xlab="PCs")
  axis(2)
  axis(1, seq(npcs))
  for (i in 1:nrow(sds)){
    lines(1:npcs, sds[i,1:npcs], col="#F6F5F5")
  }
  lines(1:npcs, STD[1:npcs], type = "l")
  abline(v=10, col="blue", lty=2)
  
  lines(1:npcs, ci[1:npcs,1], col="red", lwd=.5)
  lines(1:npcs, ci[1:npcs,2], col="red", lwd=.5)
}

  
  # find which PC explained variation is < the null explained variation
  # then find the first PC in this vector, then find the last PC that explains more
  # variation than the null range
  last.pc<-which((STD[1:npcs]-ci[1:npcs,2])<0)[1]-1
  return(last.pc)
}
