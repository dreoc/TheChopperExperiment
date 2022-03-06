rm(list=ls())

# Load dependent packages and set up working directory. (Install geomorph version 3.3.2 if necessary).
#install_version("geomorph", version = "3.3.2", repos = "http://cran.us.r-project.org")
library(geomorph) 
library(rgl) # 3D Visualization
library(vegan) # for NP MANOVA
library(rstudioapi)  
library(MASS) # for LDA
options(scipen = 10)
setwd(dirname(rstudioapi:::getActiveDocumentContext()$path))
getwd()

# Read in GPA from Github library.
gpa <- readRDS("../data/thechopper_GPA.rds")

# Plot the 1152 GPA landmark data points for the 45 chopper and mastodon marks in 3D.
clear3d()
i=1
plot3d(gpa$coords[,,1],aspect=FALSE, col="black", size=3)
for (i in 1:dim(gpa$coords)[3]){
  points3d(gpa$coords[,,i],aspect=FALSE)
}

# Plot GPA consensus (mean shape) for the 1152 GPA landmark data points.
clear3d()
plot3d(gpa$consensus, aspect=FALSE, col="red", size=5)

# Perform a PCA on the landmark coordinate data. 
# Save the PCA results as "pc" and view summary of the 45 components created by the analysis.
co <- two.d.array(gpa$coords)
pc <- prcomp(co)
summary(pc)

# Extract  PCA component values for the 45 marks and save as "dat".
dat <- pc$x

# Create a color vector separating chopper and BR mastodon marks.
col <- rep("black",length(rownames(dat)))
col[grep(x = rownames(dat), pattern="EJS")] <- "blue"
col

# Create a group label vector separating  BR mastodon and chopper marks.
gp <- rep(NA,length(col))
gp[grep(x = rownames(dat), pattern="Mastodon")] <- "BR"
gp[grep(x = rownames(dat), pattern="EJS")] <- "Chopper"
gp

# 3D plot of the 20 chopper and 25 Bowser Road data along the first three PC axes.
plot3d(dat, aspect = FALSE, type="n")
text(x= dat, labels = gp, col=col)

## Permutational MANOVA Analysis 
# NP MANOVA of PC scores between Chopper and Bowser Road groups.
mod1 <- adonis(dat ~ gp, permutations = 9999, method = "euclidean")
mod1

# NP MANOVA of PC scores, log centroid size, and the interaction between group classification and loge centroid size between Chopper and Bowser Road groups.
mod2 <- adonis(dat ~ gp + log(gpa$Csize) + gp:log(gpa$Csize), 
             permutations = 9999, method = "euclidean")
mod2

# NP MANOVA of PC scores and log centroid size between Chopper and Bowser Road groups.
mod3 <- adonis(dat ~ gp + log(gpa$Csize), 
               permutations = 9999, method = "euclidean")
mod3

# NP MANOVA of PC scores and log centroid size.
mod4 <- adonis(dat ~ log(gpa$Csize), 
               permutations = 9999, method = "euclidean")
mod4

## LDA Aanalysis
# Identify which  PC axes are informative above random noise using Null PCA analysis. 
# 100 random noise PC axes are simulated using the nsims function. Output "p" indicates the last informative axis.

#
# centroid size
mean(log(gpa$Csize)[1:20])
#[1] 4.448279
mean(log(gpa$Csize)[21:45])
#[1] 4.534236
sd(log(gpa$Csize)[1:20])/sqrt(20)
#[1] 0.08477267
sd(log(gpa$Csize)[21:45])/sqrt(25)
#[1] 0.1288015
####################
### LDA ANALYSIS ###
####################
library(MASS); options(scipen = 10)
#### SELECT P FROM NULL PC ANALYSIS

source("Null PC fun.R")
p <- null.pc.sim(gpa = gpa, nsims=100, plot=FALSE)
p 

# Create data frame of the PCA data, GPA centroid size, and group classification.
scores <- data.frame(dat, size=gpa$Csize, gp=as.character(gp))
names(scores)
dim(scores)

# Perform an LDA differentiating the Bowser Road and Chopper mark groups using data from the first to the "p"th axis of the PCA model. 
# Prior probabilities are assumed to be even. 
# Data are modeled using a leave-one-out cross-validation approach.
r<-NULL
r <- lda(formula = scores$gp ~ as.matrix(scores[,1:p]) , 
         prior = c(1 , 1)/2,
         CV=TRUE)

# Classification accuracy of the LDA model when differentiating the 45 chopper and Bowser Road marks.
sum(scores$gp ==r$class, na.rm = TRUE) / length(scores$gp)

# Confusion table of the observed and expected values from the LDA analysis. 
# This table indicates how many chopper and Bowser Road marks are correctly and incorrectly differentiated. 
options(scipen = 35)
# overall score
sum(scores$gp ==r$class, na.rm = TRUE) / length(scores$gp) 
# confusion table
table(r$class, scores$gp)

# plot LDA histogram

r2 <- lda(formula = scores$gp ~ as.matrix(scores[,1:p]) , 
         prior = c(1 , 1)/2,
         CV=FALSE)
col2 <- rep("white",21)
col2[9:21] <- "dark grey"
col2

r2.values <- predict(r2)

jpeg(filename = "../output/Figure14.jpg",
     width = 480, height = 480, units = "px", pointsize = 12,
     quality = 100,
     bg = "white", res = NA, family = "", restoreConsole = TRUE,
     type = c("windows", "cairo"), 
     symbolfamily="default")

ldahist(data = r2.values$x[,1], g = gp, col=col2 )
dev.off()
