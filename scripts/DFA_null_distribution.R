nperms<-10000
post<-numeric(nperms)
acc<-numeric(nperms)
probdist<-matrix(NA, ncol = nrow(scores)*2, nrow=nperms)

meanvec<-apply(scores[,1:p], 2, FUN = mean)
sdvec<-apply(scores[,1:45], 2, FUN = sd)

dim(sim.mat) 

for(i in 1:nperms){
  cat("iter. #", i, "\n")
  scores.perm<-scores
  scores.perm[,1:45]<-t(sapply(1:nrow(scores[,1:45]), 
                    function(x)rnorm(ncol(scores[,1:45]), 
                                     mean=meanvec, 
                                     sd=sdvec)))
  r.perm<-NULL
  r.perm <- lda(formula = scores.perm$gp ~ as.matrix(scores.perm[,1:p]) , 
                prior = c(1 , 1)/2,
                CV=TRUE)
  options(scipen = 35)
  # overall score
  acc[i]<-sum(scores.perm$gp ==r.perm$class, na.rm = TRUE) / length(scores.perm$gp) 
  # confusion table
  table(r.perm$class, scores.perm$gp)
  probdist[i,]<-unlist(r.perm$posterior)
  post[i]<-sum(r.perm$posterior>.90)
}

jpeg(filename = "../output/FigureS2.1.jpg",
     width = 480, height = 480, units = "px", pointsize = 12,
     quality = 100,
     bg = "white", res = NA, family = "", restoreConsole = TRUE,
     type = c("windows", "cairo"), 
     symbolfamily="default")

par(mfrow=c(2,1))
hist(acc, freq=F, main="Distribution of model accuracy from random data", xlab = "Model accuracy")
mean(acc)
abline(v=mean(acc), col="red", lwd=3)
hdi(acc, .90)
abline(v=hdi(acc, .90), col="blue", lwd=2,lty=2)

hist(post, freq=F, main="Distribution of simulated posteriors >0.90 from random data", xlab = "Number of Posteriors >0.90")
median(post)
abline(v=median(post), col="red", lwd=3)
hdi(post, .68)
abline(v=hdi(post, .68), col="blue", lwd=2,lty=2)
dev.off()



# hist(as.vector(probdist), freq=F, main="Distribution of all Posterior values from random data")
# mean(probdist)
# quantile(probdist, c(.1,.90))
# hdi(as.vector(probdist),credMass = .9)
# abline(v=hdi(as.vector(probdist),credMass = .9), col="blue", lwd=2,lty=2)
# sum(as.vector(probdist)>.9)/length(as.vector(probdist))
