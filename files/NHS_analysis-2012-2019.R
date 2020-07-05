#This file is best used within R Studio
#---------------------------------
version
citation(package = "base", lib.loc = NULL, auto = NULL)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
#windows(600, 600, pointsize = 12) # Size 600x600
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
getwd()
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + strheight("A")
ymax <- par("usr")[4] - strheight("A")
old.par <- par(no.readonly=T)
#---------------------------------
library(stringr)
library(plyr)
library(metafor)
library (meta) # metamean
library(Rcmdr)  # For NumSummary
library(grid)
library(boot) #inv.logit
library(dominanceanalysis) # Helps interpret LM by estimating contriutions to R2 from each predictor
library("rqdatatable") # natutal_join (replaces NA values in a dataframe merge)
#---------------------------------
# For troubleshooting
options(error=NULL)
library(tcltk) 
# msgbox for troubleshooting: 
#tk_messageBox(type = "ok", paste(current.state,': ', nrow(data.state.current),sepo=''), caption = "Troubleshooting")
#browser()
# Finish, quit is c or Q
# enter f press enter and then enter Q
#---------------------------------

# Grab data
# Use data.NHS.2015_2019.csv
file.filter <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename = choose.files(filters = file.filter,caption = "Select data file",multi=FALSE)
data.NHS.2012_2019 <- read.table(filename, header=TRUE, sep=",", na.strings="NA", stringsAsFactors = FALSE, dec=".", strip.white=TRUE)
head(data.NHS.2012_2019)

#Step 1: do states independently add for SHMI prediction
lm.out1 <- lm(SHMI.Index ~ Engagement.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ RecommendWork.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ WorkStress.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ FreedomSpeak.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ Openness.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
# Forwards
lm.out1 <- lm(SHMI.Index ~ Engagement.Index + RecommendWork.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
linearHypothesis(lm.out1, "Engagement.Index = RecommendWork.Index")
# Backwards
lm.out1 <- lm(SHMI.Index ~ Engagement.Index + RecommendWork.Index + WorkStress.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ RecommendWork.Index + Engagement.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ RecommendWork.Index + WorkStress.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)

lm.out1 <- lm(SHMI.Index ~ RecommendCare.Index + SHMI.before,  weights =  Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)

da<-dominanceAnalysis(lm.out1)
print(da)

#Region
lm.out1 <- lm(SHMI.Index ~ Engagement.Index + North,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ Engagement.Index + SHMI.before + North,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)

nrow(data.NHS.2012_2019)/5

## Over time
https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group
#par(oma=c(5,1,0,0))
table.responses <- aggregate(Responses  ~ Year.Index, data.NHS.2012_2019, sum)

# Engagement over time
table.temp1 <- aggregate(Engagement.Index.dichot  ~ Year.Index, data.NHS.2012_2019, mean )
table.temp2 <- aggregate(Responses  ~ Year.Index, data.NHS.2012_2019, sum)
table.temp3 <- merge(x = table.temp1, y = table.temp2, by = c("Year.Index"), all.x=TRUE)

Title <-'Engagement since 2012'
plot(table.temp3$Year.Index, table.temp3$Engagement.Index.dichot,xlim=c(2011.75, 2019.25),ylim=c(0,1),xlab='Year',ylab='Engagement rate', main=Title)
text(table.temp3$Year.Index, table.temp3$Engagement.Index.dichot,paste(sprintf(table.temp3$Engagement.Index.dichot*100, fmt='%#.1f'),'%',sep=''),pos=3)

# CHI-SQUARE TEST FOR TRENDS
prop.trend.summary <- prop.trend.test(round(table.temp3[,2]* table.temp3$Responses,0), table.temp3$Responses)
P.value <- paste('P-value (chi-squared test for trend): ',sprintf(prop.trend.summary$p.value, fmt='%#.3f'),sep='')
#P.value <- cor.test(table.temp$Year.Index, table.temp$WorkStress.Index)
#P.value <- paste('P-value: ',sprintf(P.value$p.value, fmt='%#.3f'),sep='')
text( par("usr")[2] - strwidth("A"),par("usr")[3] + strheight("A"), P.value,adj=c(1,0))

lm.out1 <- lm(Engagement.Index.dichot ~ Year.Index,  weights =  NULL, data = table.temp3 )
summary(lm.out1)
abline(lm.out1)

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  'Change in engagement 2012 - 2019.png',
  format = "png", width = 600, height = 400)

# RecommendWork
table.temp1 <- aggregate(RecommendWork.Index.dichot  ~ Year.Index, data.NHS.2012_2019, mean )
table.temp2 <- aggregate(Responses  ~ Year.Index, data.NHS.2012_2019, sum)
table.temp3 <- merge(x = table.temp1, y = table.temp2, by = c("Year.Index"), all.x=TRUE)

Title <-'Recommendation of NHS as a place to work'
#plot(table.temp, ylim=c(0,5),xlab='Year',ylab='Mean of recommendation', main=Title)
#text(table.temp$Year.Index,table.temp$RecommendWork.Index,paste(round(table.temp$RecommendWork.Index,2),'',sep=''),pos=3)
plot(table.temp3$Year.Index, table.temp3$RecommendWork.Index.dichot, xlim=c(2011.75, 2019.25),ylim=c(0,1),xlab='Year',ylab='Recommendation rate', main=Title)
text(table.temp3$Year.Index, table.temp3$RecommendWork.Index.dichot,paste(sprintf(table.temp3$RecommendWork.Index.dichot*100, fmt='%#.1f'),'%',sep=''),pos=3)

# CHI-SQUARE TEST FOR TRENDS
prop.trend.summary <- prop.trend.test(round(table.temp3[,2]* table.temp3$Responses,0), table.temp3$Responses)
P.value <- paste('P-value (chi-squared test for trend): ',sprintf(prop.trend.summary$p.value, fmt='%#.3f'),sep='')
#P.value <- cor.test(table.temp$Year.Index, table.temp$WorkStress.Index)
#P.value <- paste('P-value: ',sprintf(P.value$p.value, fmt='%#.3f'),sep='')
text( par("usr")[2] - strwidth("A"),par("usr")[3] + strheight("A"), P.value,adj=c(1,0))

lm.out1 <- lm(RecommendWork.Index.dichot ~ Year.Index,  weights =  NULL, data = table.temp3)
summary(lm.out1)#CHI-SQUARE TEST FOR TRENDS
#P.value <- paste('P-value (chi-squared test for trend): ',sprintf(prop.trend.summary$p.value, fmt='%#.3f'),sep='')
abline(lm.out1)

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  'Change in recommended work 2012 - 2019.png',
  format = "png", width = 600, height = 400)

# WorkStress
table.temp1 <- aggregate( WorkStress.Index ~ Year.Index, data.NHS.2012_2019, mean )
table.temp2 <- aggregate(Responses  ~ Year.Index, data.NHS.2012_2019, sum)
table.temp3 <- merge(x = table.temp1, y = table.temp2, by = c("Year.Index"), all.x=TRUE)

Title <-'Feeling unwell from work stress since 2012'
plot(table.temp, ylim=c(0,1),xlab='Year',ylab='Rate of work stress', xlim=c(2011.75, 2019.25), main=Title)
text(table.temp$Year.Index,table.temp$WorkStress.Index,paste(round(table.temp$WorkStress.Index*100,1),'%',sep=''),pos=3)

#CHI-SQUARE TEST FOR TRENDS
prop.trend.summary <- prop.trend.test(round(table.temp3[,2]* table.temp3$Responses,0), table.temp3$Responses)
P.value <- paste('P-value (chi-squared test for trend): ',sprintf(prop.trend.summary$p.value, fmt='%#.3f'),sep='')
#P.value <- cor.test(table.temp$Year.Index, table.temp$WorkStress.Index)
#P.value <- paste('P-value: ',sprintf(P.value$p.value, fmt='%#.3f'),sep='')
text( par("usr")[2] - strwidth("A"),par("usr")[3] + strheight("A"), P.value,adj=c(1,0))

lm.out1 <- lm(WorkStress.Index ~ Year.Index, weights =  NULL, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.summary <- summary(lm.out1)
#P.value <- lm.summary[["coefficients"]][2, 4]
#P.value <- paste('P-value: ',sprintf(P.value, fmt='%#.3f'),sep='')
abline(lm.out1)

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  'Change in work stress 2012 - 2019.png',
  format = "png", width = 600, height = 400)

#Associations
table.temp1 <- aggregate( WorkStress.Index ~ Year.Index, data.NHS.2012_2019, mean )
table.temp2 <- aggregate(Engagement.Index ~ Year.Index, data.NHS.2012_2019, mean )
table.temp3 <- merge(x = table.temp1, y = table.temp2, by = c("Year.Index"), all.x=TRUE)

Title <-'Association between engagement and work stress'
plot(table.temp3$Engagement.Index,table.temp3$WorkStress.Index,xlab='Engagement',ylab='Work stress (rate)',ylim=c(0.35,0.40),main=Title)
text(table.temp3$Engagement.Index,table.temp3$WorkStress.Index,paste(table.temp3$Year.Index,'',sep=''),pos=3)

#table.temp <- data.NHS.2012_2019[data.NHS.2012_2019$Year.Index < 2025, ]

lm.out1 <- lm(WorkStress.Index ~ Engagement.Index,  weights = NULL, data = table.temp3 )
lm.summary <- summary(lm.out1)
P.value <- lm.summary[["coefficients"]][2, 4]
P.value <- paste('P-value: ',sprintf(P.value, fmt='%#.3f'),sep='')

text(xmin,ymax, P.value, adj=c(0,1))
abline(lm.out1)

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  'Association between engagement and stress 2012 - 2019.png',
  format = "png", width = 600, height = 400)
