library(vegan)
library(MASS)

# #setwd("~/Box/Desiree_cut marks/Harris/Rcode/TLK Code/Experimental_Kmeans/BR/BR_kmeansdownsampled")
# #setwd("C:/Users/eotarola/Box/Box_LCA/Cutmarks_project/Desiree_cut marks/Harris/Rcode/TLK Code/Experimental_Kmeans/BR/BR_kmeansdownsampled")
# #dat <- readRDS(file.choose())
# 
# rownames(dat)
# col <- rep("red",length(rownames(dat)))
# col
# col[grep(x = rownames(dat), pattern="Mastodon")] <- "black"
# col[grep(x = rownames(dat), pattern="EJS")] <- "blue"
# col
# 
# gp <- rep("PALEO",length(col))
# gp
# gp[grep(x = rownames(dat), pattern="Mastodon")] <- "Bowser Rd"
# gp[grep(x = rownames(dat), pattern="EJS")] <- "Chopper"
# gp
# 


mod1<-adonis(dat ~ gp, permutations = 999,method = "euclidean")
mod1

chop <- co[1:20,]
br <- co[21:45,]

chop.vcv<-var(chop)
br.vcv<-var(br)

S<-seq(5,100, by=5)
pmat<-matrix(NA, nrow = 1000, ncol=length(S))
for (i in 1:length(S)){
  for(j in 1:dim(pmat)[1]){
    cat("iter no", j, "of",i, "\n")
    chop.rand<-mvrnorm(n=S[i],mu=colMeans(chop), Sigma = chop.vcv)
    br.rand<-mvrnorm(n=S[i],mu=colMeans(br), Sigma = br.vcv)
    gp.rand<-as.factor(rep(c("CH", "BR"), each=S[i]))
    m<-rbind(chop.rand,br.rand)
    mod1.rand<-adonis(m ~ gp.rand, permutations = 999,method = "euclidean")
    pmat[j,i]<-mod1.rand$aov.tab$`Pr(>F)`[1]
  }
}

me<-apply(pmat, MARGIN = 2, FUN=mean)
sds<-apply(pmat, MARGIN = 2, FUN=sd)
plot(S, pmat[1,], ylim=c(0,1), type="n")
abline(h=0.05, col="red", lty=2)
for (i in 1:dim(pmat)[1]){
  points(S, pmat[i,], cex=.5)
}

me<-apply(pmat, MARGIN = 2, FUN=mean)
sd<-apply(pmat, MARGIN = 2, FUN=sd)
ran<-apply(pmat, MARGIN = 2, FUN=range)
ran<-apply(pmat, MARGIN = 2, FUN=quantile, probs=c(.025,.975))

#sds <- (sd/(sqrt(37)))*1.96
plot(S, me, ylim=c(0,.45), cex=1, pch=16, bg="white", xlab="Per group sample size", 
     ylab= "p-value",lab=c(20,10,7))
segments <- segments (x0=S,y0=ran[1,], y1=ran[2,], lwd=1)
segments <- segments (x0=(S-1), x1=(S+1),y0=ran[1,], y1=ran[1,], lwd=1)
segments <- segments (x0=(S-1), x1=(S+1),y0=ran[2,], y1=ran[2,], lwd=1)

abline(h=0.05, col="red", lty=2, lwd=2)

# boxplot(pmat, outline=FALSE, names=S, xlab="Per group sample size", 
#         ylab= "p-value")
# abline(h=0.01, col="red", lty=2)
# 
# ### This is the concept behind this code
# library(vegan)
# chop<-matrix(rnorm(1000,mean=.5,sd=1), nrow = 100)
# br<-matrix(rnorm(1000,mean=.75,sd=1), nrow = 100)
# gp<-as.factor(rep(c("CH", "BR"), each=100))
# gp
# mat<-rbind(chop,br)
# mod1<-adonis(mat ~ gp, permutations = 999,method = "euclidean")
# mod1
# chop.vcv<-var(chop)
# br.vcv<-var(br)
# library(MASS)
# S<-seq(5,100, by=5)
# pmat<-matrix(NA, nrow = 10, ncol=length(S))
# for (i in 1:length(S)){
#   for(j in 1:dim(pmat)[1]){
#     cat("iter no", j, "of",i, "\n")
#     chop.rand<-mvrnorm(n=S[i],mu=colMeans(chop), Sigma = chop.vcv)
#     br.rand<-mvrnorm(n=S[i],mu=colMeans(br), Sigma = br.vcv)
#     gp.rand<-as.factor(rep(c("CH", "BR"), each=S[i]))
#     m<-rbind(chop.rand,br.rand)
#     mod1.rand<-adonis(m ~ gp.rand, permutations = 99,method = "euclidean")
#     pmat[j,i]<-mod1.rand$aov.tab$`Pr(>F)`[1]
#   }
# }
# me<-apply(pmat, MARGIN = 2, FUN=mean)
# sds<-apply(pmat, MARGIN = 2, FUN=sd)
# plot(S, pmat[1,], ylim=c(0,1))
# abline(h=0.05, col="red", lty=2)
# for (i in 1:dim(pmat)[1]){
#   points(S, pmat[i,])
# }
