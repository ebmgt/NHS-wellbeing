#This file is best used within R Studio
#---------------------------------
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
getwd()
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
#---------------------------------
library(stringr)
library(plyr)
#---------------------------------

# Analysis
mydata <- NULL
#mydata<- read.table("all.years.NHS.rda", header=TRUE)
mydata<- read.csv("csv/all.years.NHS.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
#dataNHS <- mydadta

library(Rcmdr)

# Descriptive statistics
# Table 1
table1 <- numSummary(dataNHS[,"Survey.index.observations", drop=FALSE], groups=dataNHS$Year.index, statistics=c("mean", "sd"))
table1
# Mortality
table1 <- numSummary(dataNHS[,"SHMI.index", drop=FALSE], groups=dataNHS$Year.index, statistics=c("mean", "sd"))
table1$table[,1]
(cor1 <- cor.test(1:6,table1$table[,1]))
plot(row.names(table1$table),table1$table[,1],pch=16,xlab="Year", ylab="SHMI", main="SHMI over time", col.main = KUBlue)
(pearson <- sprintf(cor1$estimate, fmt='%#.2f'))
(pvalue <- sprintf(cor1$p.value, fmt='%#.3f'))
text(par("usr")[2] - strwidth("A"),par("usr")[3]+2.5*strheight("A"),cex=1,adj=c(1,0),paste("Pearson ?? = ",pearson, sep=""), font=1)
text(par("usr")[2] - strwidth("A"),par("usr")[3]+1.5*strheight("A"),cex=1,adj=c(1,0),paste("p = ",pvalue, sep=""), font=1)
# Engagement
table1 <- numSummary(dataNHS[,"Engagement.index", drop=FALSE], groups=dataNHS$Year.index, statistics=c("mean", "sd"))
table1$table[,1]
(cor1 <- cor.test(1:6,table1$table[,1]))
plot(row.names(table1$table),table1$table[,1],pch=16,xlab="Year", ylab="Engagement", main="Engagement over time", col.main = KUBlue)
(pearson <- sprintf(cor1$estimate, fmt='%#.2f'))
(pvalue <- sprintf(cor1$p.value, fmt='%#.3f'))
text(par("usr")[2] - strwidth("A"),par("usr")[3]+2.5*strheight("A"),cex=1,adj=c(1,0),paste("Pearson ?? = ",pearson, sep=""), font=1)
text(par("usr")[2] - strwidth("A"),par("usr")[3]+1.5*strheight("A"),cex=1,adj=c(1,0),paste("p = ",pvalue, sep=""), font=1)

# Analyses
# Univariate regression
lm.out1 <- lm(SHMI.index  ~ Engagement.index,  weights = Survey.index.observations, data = dataNHS)
lm.out2 <- lm(SHMI.index  ~ Engagement.after,  weights = Survey.index.observations, data = dataNHS)
lm.out3 <- lm(SHMI.index  ~ Engagement.before, weights = Survey.index.observations, data = dataNHS)

# Adjusted for SHMI.before
# Riketta analysis. Riketta reported beta = 0.12 for lags of 1 to 6 months and insig for longer lags
lm.out1 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before, weights = Survey.index.observations, data = dataNHS)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.after  + SHMI.before, weights = Survey.index.observations, data = dataNHS)
(lm.summary <- summary(lm.out2))
lm.out3 <- lm(SHMI.index  ~ Engagement.before + SHMI.before, weights = Survey.index.observations, data = dataNHS)
(lm.summary <- summary(lm.out3))
anova.out <- anova(lm.out1,lm.out2)
anova.out$`Pr(>F)`

# Convert beta-coefficients to odds ratios
coefficient <- lm.summary$coefficients[2, 1] 
std.err <- lm.summary$coefficients[2, 2] 
(odds.ratio <- exp(coefficient))
ci.L <- coefficient - 1.96*std.err 
(ci.L <- exp(ci.L))
ci.U <- coefficient + 1.96*std.err 
(ci.U <- exp(ci.U))

lm.out1 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before, weights = Survey.index.observations, data = data2017)
lm.summary <- summary(lm.out1)

TRY:
  https://stackoverflow.com/questions/33113544/how-to-subset-a-range-of-values-in-lms
