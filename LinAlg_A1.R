#ASSIGNMENT 1 - LINEAR ALGEBRA
#AUTHOR: JACKSON PERRY

#first, we load in the data file
load('/Users/jacksonperry/Desktop/Fall1/LeukError.RData')

#explore data
leuk[1,1:10]
leuk[1,5001]
leuk[,5001]
table(leuk$V5001)

#plot random data
pal=palette(brewer.pal(n = 3, name = "Set1"))
plot(leuk$V45, leuk$V675, col = leuk$V5001, pch=19, cex=2,)
text(leuk$V675 ~ leuk$V45, labels=rownames(leuk),data=leuk, cex=0.5, font=2)

#run a principal component analysis for unscaled
pcaOut = prcomp(leuk[,1:5000], scale = F)

#plot again
plot(pcaOut$x[,1], pcaOut$x[,2], col = leuk$V5001, pch=19, cex=2,)
text(pcaOut$x[,2] ~ pcaOut$x[,1], labels=rownames(leuk), data=leuk, cex=0.5, font=2)
legend(40000,-25000, legend=c("AML","ALL-B","ALL-T"), col = c("#4DAF4A","#EF1A1C","#377EB8"), pch=19, cex=1)#blue is allt, aml is green)


#check for 0 variance columns
count = 0
leuk2 = leuk[,5001]
for (i in 1:5000){
  if (var(leuk[,i]) != 0) {leuk2 <- cbind(leuk2, leuk[,i])}
}

#run a principal component analysis for scaled
pcaOut2 = prcomp(leuk2[,2:4813], 2, scale = T)

#plot again
plot(pcaOut2$x[,1], pcaOut2$x[,2], col = leuk$V5001, pch=19, cex=2)
text(pcaOut2$x[,2] ~ pcaOut2$x[,1], labels=rownames(leuk), data=leuk, cex=0.5, font=2)
legend(40000,-25000, legend=c("AML","ALL-B","ALL-T"), col = c("#4DAF4A","#EF1A1C","#377EB8"), pch=19, cex=1)#blue is allt, aml is green)

###########################
#scaled was way worse
###########################

#here is my idea: we can compute the variance for each group as is
#find the change in color that lowers the variance the most
#1-red, 2-blue, 3-green

#establish the baseline
new <- cbind(pcaOut$x[,1],pcaOut$x[,2],leuk$V5001)

reds <- new[(new[,3]==1),]
blues <- new[(new[,3]==2),]
greens <- new[(new[,3]==3),]

meanR <- c(mean(reds[,1]), mean(reds[,2]))
meanB <- c(mean(blues[,1]), mean(blues[,2]))
meanG <- c(mean(greens[,1]), mean(greens[,2]))
varR <- c(var(reds[,1]), var(reds[,2]))
varB <- c(var(blues[,1]), var(blues[,2]))
varG <- c(var(greens[,1]), var(greens[,2]))

tracker <- cbind(c(varR,varB,varG))

#make 22 blue
new <- cbind(pcaOut$x[,1],pcaOut$x[,2],leuk$V5001)
new[19,3] <- 2

reds <- new[(new[,3]==1),]
blues <- new[(new[,3]==2),]
greens <- new[(new[,3]==3),]

meanR <- c(mean(reds[,1]), mean(reds[,2]))
meanB <- c(mean(blues[,1]), mean(blues[,2]))
meanG <- c(mean(greens[,1]), mean(greens[,2]))
varR <- c(var(reds[,1]), var(reds[,2]))
varB <- c(var(blues[,1]), var(blues[,2]))
varG <- c(var(greens[,1]), var(greens[,2]))

tracker <- cbind(tracker, c(varR,varB,varG))

#make 29 red
new <- cbind(pcaOut$x[,1],pcaOut$x[,2],leuk$V5001)
new[2,3] <- 1

reds <- new[(new[,3]==1),]
blues <- new[(new[,3]==2),]
greens <- new[(new[,3]==3),]

meanR <- c(mean(reds[,1]), mean(reds[,2]))
meanB <- c(mean(blues[,1]), mean(blues[,2]))
meanG <- c(mean(greens[,1]), mean(greens[,2]))
varR <- c(var(reds[,1]), var(reds[,2]))
varB <- c(var(blues[,1]), var(blues[,2]))
varG <- c(var(greens[,1]), var(greens[,2]))

tracker <- cbind(tracker, c(varR,varB,varG))

##############################
#it looks like changing obs 22 to blue reduced group 
#variance the most, so I think this is the mislabeled point
##############################

