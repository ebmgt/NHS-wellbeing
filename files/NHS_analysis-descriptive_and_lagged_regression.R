#This file is best used within R Studio
#---------------------------------
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
#windows(600, 600, pointsize = 12) # Size 600x600
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
getwd()
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
#---------------------------------
library(stringr)
library(plyr)
#library(MASS)
library(Rcmdr)
#---------------------------------

# Analysis
mydata <- NULL
#mydata<- read.table("all.years.NHS.rda", header=TRUE)
mydata<- read.csv("csv/all.years.NHS.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#dataNHS <- mydata
head(mydata)

# Descriptive statistics
par(oma=c(5,1,0,0))
# Table 1. Number of observations
attach(dataNHS.2013.2017)
summary(dataNHS.2013.2017$Code)
aggregate(data.frame(count = Code), list(value = Code), length)
length(summary(as.factor(Code),maxsum=1000)) # but 5 of these have 0 instances
table1 <- numSummary(dataNHS[,"Survey.observations.index", drop=FALSE], groups=dataNHS$Year.index, statistics=c("mean", "sd"))
table1

attach(dataNHS.2013.2017)
summary(dataNHS.2013.2017$Code)
aggregate(data.frame(count = Code), list(value = Code), length)
summary(as.factor(Code),maxsum=1000)
length(summary(as.factor(Code),maxsum=1000))

# Mortality
table1 <- numSummary(dataNHS[,"SHMI.index", drop=FALSE], groups=dataNHS$Year.index, statistics=c("mean", "sd"))
table1$table[,1]
shapiro.test(dataNHS$SHMI.index) #OOPS, Not normally distributed
# Switching to median and Spearman
table1 <- numSummary(dataNHS[,"SHMI.index", drop=FALSE], groups=dataNHS$Year.index, statistics=c("quantiles", "IQR"))
table1$table[,4]
(cor1 <- cor.test(1:7,table1$table[,1],method="spearman"))
plot(row.names(table1$table),table1$table[,4],pch=16,xlab="Year", ylab="SHMI (median)", main="SHMI over time", col.main = KUBlue)
(pearson <- sprintf(cor1$estimate, fmt='%#.2f'))
(pvalue <- sprintf(cor1$p.value, fmt='%#.3f'))
text(par("usr")[2] - strwidth("A"),par("usr")[4]-1.0*strheight("A"),cex=1,adj=c(1,0),paste("Spearman's rho = ",pearson, sep=""), font=1)
text(par("usr")[2] - strwidth("A"),par("usr")[4]-2.5*strheight("A"),cex=1,adj=c(1,0),paste("p = ",pvalue, sep=""), font=1)
mtext("Notes:",side=1,line=-1, cex=1, font=2,adj=0, outer = TRUE)
mtext("SMHI. Summary Hospital-level Mortality Indicator.",side=1,line=0, cex=1, font=1,adj=0, outer = TRUE)
# Size 600x600
# Engagement
par(oma=c(5,1,0,0))
table1 <- numSummary(dataNHS[,"Engagement.index", drop=FALSE], groups=dataNHS$Year.index, statistics=c("mean", "sd"))
table1$table[,1]
shapiro.test(dataNHS$Engagement.index) #OOPS, Not normally distributed
# Switching to median and Spearman
table1 <- numSummary(dataNHS[,"Engagement.index", drop=FALSE], groups=dataNHS$Year.index, statistics=c("quantiles", "IQR"))
table1$table[,4]
 (cor1 <- cor.test(1:7,table1$table[,4],method="spearman"))
windows(300, 300, pointsize = 12) # Size 600x600
plot(row.names(table1$table),table1$table[,4],pch=16,ylim=c(0,4), xlab="Year", ylab="Engagement (median)", main="Engagement over time", col.main = KUBlue)
(pearson <- sprintf(cor1$estimate, fmt='%#.2f'))
(pvalue <- sprintf(cor1$p.value, fmt='%#.3f'))
text(par("usr")[2] - strwidth("A"),par("usr")[3]+4.0*strheight("A"),cex=1,adj=c(1,0),paste("Spearman's rho = ",pearson, sep=""), font=1)
text(par("usr")[2] - strwidth("A"),par("usr")[3]+1.5*strheight("A"),cex=1,adj=c(1,0),paste("p = ",pvalue, sep=""), font=1)
mtext("Notes:",side=1,line=-1, cex=1, font=2,adj=0, outer = TRUE)
mtext("Engagement based on three validated factors by the UWES-9 survey: vigor,",side=1,line=0, cex=1, font=1,adj=0, outer = TRUE)
mtext("absorption, and dedication.",side=1,line=1, cex=1, font=1,adj=0, outer = TRUE)

# Analyses
# Testing weights
lm.out1 <- lm(SHMI.index  ~ Engagement.index + SHMI.before,  weights = Survey.observations.index, data = dataNHS)
summary(lm.out1)
lm.out2 <- lm(SHMI.index  ~ Engagement.index + SHMI.before,  weights = Survey.clinical.proportion.index, data = dataNHS)
summary(lm.out2)
lm.out3 <- lm(SHMI.index  ~ Engagement.index + SHMI.before,  weights = Survey.ClinicalStaffCount, data = dataNHS)
summary(lm.out3)
lm.out4 <- lm(SHMI.index  ~ Engagement.index + SHMI.before,  data = dataNHS)
summary(lm.out4)
(anova.out <- anova(lm.out1,lm.out2))
anova.out$`Pr(>F)`

# Univariate regression
lm.out1 <- lm(SHMI.index  ~ Engagement.index,  weights = NULL, data = dataNHS)
summary(lm.out1)
lm.out2 <- lm(SHMI.index  ~ Engagement.after,  weights = NULL, data = dataNHS)
summary(lm.out2)
lm.out3 <- lm(SHMI.index  ~ Engagement.before, weights = NULL, data = dataNHS)
summary(lm.out3)

# Sandbox
dataNHS$Survey.ClinicalStaffCount <- dataNHS$Survey.clinical.proportion.index*dataNHS$Survey.engagement.observations.index
lm.out1 <- lm(Engagement.index ~ Survey.clinical.proportion.index,  weights = Survey.observations.index, data = dataNHS)
summary(lm.out1)
lm.out1 <- lm(SHMI.index  ~ Survey.ClinicalStaffCount,  weights = NULL, data = dataNHS)
summary(lm.out1)

summary(dataNHS$Year.index)

# Our analysis. Riketta reported beta = 0.12 for lags of 1 to 6 months and insig for longer lags
# Use 2013-2017 and all adjusted for SHMI.before
# Predict the index year's mortality
dataNHS.2013.2017 <- subset(dataNHS, dataNHS$Year.index >= 2013 & dataNHS$Year.index <= 2017)
summary(dataNHS.2013.2017$Year.index)
# Weights: none
# Covariates: none
lm.out1 <- lm(SHMI.index  ~ Engagement.before + SHMI.before, weights = NULL, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before, weights = NULL, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out2))
lm.out3 <- lm(SHMI.index  ~ Engagement.after  + SHMI.before, weights = NULL, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out3))
# Weights: Survey.observations.index
# Covariates: none
lm.out1 <- lm(SHMI.index  ~ Engagement.before + SHMI.before, weights = Survey.observations.index, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before, weights = Survey.observations.index, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out2))
lm.out3 <- lm(SHMI.index  ~ Engagement.after  + SHMI.before, weights = Survey.observations.index, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out3))
# Weights: Survey.clinical.proportion.index  
# Covariates: none
lm.out1 <- lm(SHMI.index  ~ Engagement.before + SHMI.before, weights = Survey.clinical.proportion.index, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before, weights = Survey.clinical.proportion.index, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out2))
lm.out3 <- lm(SHMI.index  ~ Engagement.after  + SHMI.before, weights = Survey.clinical.proportion.index, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out3))
# Weights: Survey.observations.index
# Covariate:Survey.clinical.proportion.index
lm.out1 <- lm(SHMI.index  ~ Engagement.before + SHMI.before + Survey.clinical.proportion.index, weights = Survey.observations.index, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before + Survey.clinical.proportion.index, weights = Survey.observations.index, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out2))
lm.out3 <- lm(SHMI.index  ~ Engagement.after  + SHMI.before + Survey.clinical.proportion.index, weights = Survey.observations.index, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out3))
# Weights: none
# Covariate:Survey.observations.index
lm.out1 <- lm(SHMI.index  ~ Engagement.before + SHMI.before + Survey.clinical.proportion.index, weights = NULL, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before + Survey.clinical.proportion.index, weights = NULL, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out2))
lm.out3 <- lm(SHMI.index  ~ Engagement.after  + SHMI.before + Survey.clinical.proportion.index, weights = NULL, data = dataNHS.2013.2017)
(lm.summary <- summary(lm.out3))

# Compare
data.compare <- dataNHS.2013.2017[!(is.na(dataNHS.2013.2017$SHMI.index)) & !(is.na(dataNHS.2013.2017$SHMI.before)) & !(is.na(dataNHS.2013.2017$Engagement.index)) & !(is.na(dataNHS.2013.2017$Engagement.after)),]
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before + Survey.clinical.proportion.index, weights = NULL, data = data.compare)
lm.out3 <- lm(SHMI.index  ~ Engagement.after  + SHMI.before + Survey.clinical.proportion.index, weights = NULL, data = data.compare)
anova.out <- anova(lm.out2,lm.out3)
anova.out$`Pr(>F)`

# Riketta analysis: long lag. Riketta reported beta = 0.12 for lags of 1 to 6 months and insig for longer lags
# Use 2012-2017
# Predict the ensuing year
# Adjusted for baseline value of the dependent variable
dataNHS.2012.2017 <- subset(dataNHS, dataNHS$Year.index >= 2012 & dataNHS$Year.index <= 2017)
summary(dataNHS.2012.2017$Year.index)
# Weights: dataNHS$Survey.observations.index
# Covariate: none
lm.out1 <- lm(Engagement.after  ~ Engagement.index  + SHMI.index, weights = Survey.observations.index, data = dataNHS.2012.2017)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.after  ~ Engagement.index  + SHMI.index, weights = Survey.observations.index, data = dataNHS.2012.2017)
(lm.summary <- summary(lm.out2))
# Weights: dataNHS$Survey.observations.index
# Covariate: Survey.clinical.proportion.index
lm.out1 <- lm(Engagement.after  ~ SHMI.index + Engagement.index  + Survey.clinical.proportion.index, weights = Survey.observations.index, data = dataNHS.2012.2017)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.after  ~ Engagement.index  + SHMI.index + Survey.clinical.proportion.index, weights = Survey.observations.index, data = dataNHS.2012.2017)
(lm.summary <- summary(lm.out2))
# Weight: none
# Covariate: none
lm.out1 <- lm(Engagement.after  ~ SHMI.index + Engagement.index, weights = NULL, data = dataNHS.2012.2017)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.after  ~ Engagement.index  + SHMI.index, weights = NULL, data = dataNHS.2012.2017)
(lm.summary <- summary(lm.out2))
# Weight: none
# Covariate: Survey.clinical.proportion.index
lm.out1 <- lm(SHMI.after  ~ Engagement.index  + SHMI.index + Survey.clinical.proportion.index, weights = NULL, data = dataNHS.2012.2017)
(lm.summary <- summary(lm.out1))
lm.out1 <- lm(Engagement.after  ~ Engagement.index  + SHMI.index + Survey.clinical.proportion.index, weights = NULL, data = dataNHS.2012.2017)
(lm.summary <- summary(lm.out1))

# Riketta short lag analysis. Riketta reported beta = 0.12 for lags of 1 to 6 months and insig for longer lags
# Use 2013-2018
# Predict the index year
dataNHS.2013.2018 <- subset(dataNHS, dataNHS$Year.index >= 2013 & dataNHS$Year.index <= 2018)
summary(dataNHS.2013.2018$Year.index)
# Weights: none
# Covariate: none
lm.out1 <- lm(Engagement.index  ~ SHMI.index + Engagement.before, weights = NULL, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before, weights = NULL, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out2))
# Weights: dataNHS$Survey.observations.index
# Covariate: none
lm.out1 <- lm(Engagement.index  ~ SHMI.index + Engagement.before, weights = Survey.observations.index, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before, weights = Survey.observations.index, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out2))
# Weights: dataNHS$Survey.observations.index
# Covariate: Survey.clinical.proportion.index
lm.out1 <- lm(Engagement.index  ~ SHMI.index + Engagement.before  + Survey.clinical.proportion.index, weights = Survey.observations.index, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index + SHMI.before + Survey.clinical.proportion.index, weights = Survey.observations.index, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out2))
# Weights: none
# Covariate: none
lm.out1 <- lm(Engagement.index  ~ SHMI.index + Engagement.before, weights = NULL, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before, weights = NULL, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out2))
# Weights: none
# Covariate: Survey.clinical.proportion.index
lm.out1 <- lm(Engagement.index  ~ SHMI.index + Engagement.before  + Survey.clinical.proportion.index, weights = NULL, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out1))
lm.out2 <- lm(SHMI.index  ~ Engagement.index  + SHMI.before + Survey.clinical.proportion.index, weights = NULL, data = dataNHS.2013.2018)
(lm.summary <- summary(lm.out2))
# Comparison
anova.out <- anova(lm.out1,lm.out2)
anova.out$`Pr(>F)`

# MASS
Likes 1/variance
	https://www.itl.nist.gov/div898/handbook/pmd/section1/pmd143.htm
	https://www.datasciencecentral.com/profiles/blogs/weighted-linear-regression-in-r
https://stats.stackexchange.com/questions/34325/regression-modelling-with-unequal-variance 
# https://stats.idre.ucla.edu/r/dae/robust-regression/
library(MASS)
library(sfsmisc) # For a Wald test
rlm.out1 <- rlm(Engagement.index  ~ SHMI.index + Survey.clinical.proportion.index + Engagement.before, dataNHS.2013.2018, Engagement.index, wt.method = "inv.var")
(rlm.summary <- summary(rlm.out1))
f.robftest(rlm.out1, var = "SHMI.index")
f.robftest(rlm.out1, var = "Survey.clinical.proportion.index")
rlm.out2 <- rlm(SHMI.index  ~ Engagement.index + Survey.clinical.proportion.index + SHMI.before, dataNHS.2013.2018, SHMI.index, wt.method = "inv.var")
(rlm.summary <- summary(rlm.out2))
f.robftest(rlm.out2, var = "Engagement.index")
f.robftest(rlm.out2, var = "Survey.clinical.proportion.index")

OR TRY
Robust regression does not address issues of heterogeneity of variance. This problem can be addressed by using functions in the sandwich package after the lm function

TRY:
  https://stackoverflow.com/questions/33113544/how-to-subset-a-range-of-values-in-lms
