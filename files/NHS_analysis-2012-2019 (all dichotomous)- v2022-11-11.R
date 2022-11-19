#This file is best used within R Studio
# rbadgett@kumc.edu
### Start----------------------------------------
version
citation(package = "base", lib.loc = NULL, auto = NULL)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd("../plots")
##* Graphics --------------------------
#windows(600, 600, pointsize = 12) # Size 600x600
getwd()
par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
old.par <- par(no.readonly=T)
plot.new()
xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + strheight("A")
ymax <- par("usr")[4] - strheight("A")

##* Libraries------------------------------------
library(openxlsx) # read.xlsx
#library(xlsx)    # read.xlsx2
library(plyr)
library(metafor)
library (meta)    # metamean
library(grid)
library(boot)     #inv.logit
library(dominanceanalysis) # Helps interpret LM by estimating contributions to R2 from each predictor
library(splines)

##* Constants declaration -------------------
`%notin%` <- Negate(`%in%`)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
current.date <- as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE))
(current.date.pretty <- as.character(strftime (Sys.time(), format="%m/%d/%Y", tz="", usetz=FALSE)))
p.value <- sprintf(p.value, fmt='%#.3f')
I2.label <- expression(paste( plain("I") ^ plain("2"), plain("(%)")))
summary.I2.label <- bquote(paste("RE Model (I^2 = ",(formatC(res$I2, digits=1, format="f")), "%)"))
R2.label <- expression(paste( plain("R") ^ plain("2"), plain("(adjusted)")))

##* Encoding characters---------------
# https://www.unicodepedia.com/groups/
# http://www.unicode.org/charts/
##* Footnotes
# https://www.unicodepedia.com/groups/general-punctuation/
# Dagger  \u2020
# Double dagger  \u2021
# Section symbol \A7
# Double Vertical Line \u2016
# Para    \B6 or \u0086
##*Greek
# https://unicode.org/charts/nameslist/n_0370.html

##* Troubleshooting grab -----------------------
options(error=NULL)
library(tcltk) # For troubleshooting
# msgbox for troubleshooting: 
# tk_messageBox(type = "ok", paste(current.state,': ', nrow(data.state.current),sepo=''), caption = "Troubleshooting")
# browser()
# Finish, quit is c or Q
# enter f press enter and then enter Q

### Data grab -----------------------------------
# co <- read.table("https://data.princeton.edu/eco572/datasets/cohhpop.dat", col.names=c("age","pop"), header=FALSE)
file.filter   <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename      <- choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
data.NHS.2012_2019 <- read.table(filename, header=TRUE, sep=",", na.strings="NA", stringsAsFactors = FALSE, dec=".", strip.white=TRUE)
head(data.NHS.2012_2019)
nrow(data.NHS.2012_2019)
summary(data.NHS.2012_2019)

attach(data.NHS.2012_2019)

### Data cleaning--------------------------------
##* Rename columns?--------------------

##* Remove anyone? --------------------
#data.harm   <- data.harm[complete.cases(data.harm), ]
# All added 2021-02-01
nrow(data.NHS.2012_2019)
data.NHS.2012_2019 <- data.NHS.2012_2019[!is.na(data.NHS.2012_2019$SHMI.Index),]
nrow(data.NHS.2012_2019)
data.NHS.2012_2019 <- data.NHS.2012_2019[!is.na(data.NHS.2012_2019$Survey.clinical.proportion),]
nrow(data.NHS.2012_2019)
data.NHS.2012_2019 <- data.NHS.2012_2019[!is.na(data.NHS.2012_2019$Responses),]
bob <- data.NHS.2012_2019[!is.na(data.NHS.2012_2019$Responses),]
nrow(data.NHS.2012_2019)
sum(data.NHS.2012_2019$Responses, na.rm = TRUE)
nrow(bob)
sum(bob$Responses)

## Descriptive stats-----------------------------
unique(data.NHS.2012_2019$Code)
length(unique(data.NHS.2012_2019$Code))
nrow(data.NHS.2012_2019)

attach (data.NHS.2012_2019)
nrow(data.NHS.2012_2019)
summary (Responses)
summary (Survey.clinical.proportion)
summary(Engagement.Index.dichot)
length(Engagement.Index.dichot)
unique(data.NHS.2012_2019$Engagement.Index.dichot)
summary(RecommendWork.Index.dichot)
summary(WorkStress.Index)
dettach (data.NHS.2012_2019)

dt <- ddply(data.NHS.2012_2019, .(Year.Index), summarize, Trust.count = length(Code), Responses.median= median (Responses, na.rm = TRUE))

# 2019
data.NHS.2019 <- data.NHS.2012_2019[data.NHS.2012_2019$Year.Index == 2019, ]
nrow(data.NHS.2019)

##* Pearson----------------------------
attach(data.NHS.2012_2019)
cor.test(SHMI.Index, Engagement.Index)
cor.test(SHMI.Index, RecommendWork.Index)
cor.test(SHMI.Index, WorkStress.Index)

## Univariable regression------------------------
# 2021-02-01 had to add na.action = na.omit to each regression
## Step 1: do states independently add for SHMI prediction
lm.out1 <- lm(SHMI.Index ~ Engagement.Index,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ RecommendWork.Index,  weights = Survey.clinical.proportion*Responses, na.action = na.omit, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ WorkStress.Index,  weights = Survey.clinical.proportion*Responses, na.action = na.omit, data = data.NHS.2012_2019 )
summary(lm.out1)

# Dichotomous indices
lm.out1 <- lm(SHMI.Index ~ Engagement.Index.dichot,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ RecommendWork.Index.dichot,  weights = Survey.clinical.proportion*Responses, na.action = na.omit, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ WorkStress.Index,  weights = Survey.clinical.proportion*Responses, na.action = na.omit, data = data.NHS.2012_2019 )
summary(lm.out1)

## Univariable cross-------------
lm.out1 <- lm(SHMI.Index ~ Engagement.Index.dichot + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
da<-dominanceAnalysis(lm.out1)
print(da)
lm.out1 <- lm(SHMI.Index ~ RecommendWork.Index.dichot + SHMI.before,  weights = Survey.clinical.proportion*Responses, na.action = na.omit, data = data.NHS.2012_2019 )
summary(lm.out1)
da<-dominanceAnalysis(lm.out1)
print(da)
lm.out1 <- lm(SHMI.Index ~ WorkStress.Index + SHMI.before,  weights = Survey.clinical.proportion*Responses, na.action = na.omit, data = data.NHS.2012_2019 )
summary(lm.out1)
da<-dominanceAnalysis(lm.out1)
print(da)

## Univariable cross with spline-------------
# Best for engagement is df = 5
df.e  <- 3 # 6 if you recalc with cross-lag
# Best for recommend is df = 3
df.r  <- 5 # 4 if you recalc with cross-lag
# Best for work stress is df = 3
df.ws <- 1 #3 # Failed test of non-linearity
# simple benchmark
lm.uni <- lm(SHMI.Index ~ SHMI.before, na.action = na.omit, data=data.temp)
# Engagement
lm.uni <- lm(SHMI.Index ~ data.temp$Engagement.Index.dichot + SHMI.before, na.action = na.omit , data=data.temp)
lm.uni.test <- lm(SHMI.Index ~ ns(data.temp$Engagement.Index.dichot, df=df.e) + SHMI.before, na.action = na.omit , data=data.temp)
# Recommend work
lm.uni <- lm(SHMI.Index ~ data.temp$RecommendWork.Index.dichot + SHMI.before, na.action = na.omit , data=data.temp)
lm.uni.test <- lm(SHMI.Index ~ ns(data.temp$RecommendWork.Index.dichot, df=df.r) + SHMI.before, na.action = na.omit , data=data.temp)
# Work stress
lm.uni <- lm(SHMI.Index ~ data.temp$WorkStress.Index + SHMI.before, na.action = na.omit , data=data.temp)
lm.uni.test <- lm(SHMI.Index ~ ns(data.temp$WorkStress.Index, df=1) + SHMI.before, na.action = na.omit , data=data.temp)
# Summary and calculate the difference
summary(lm.uni.test)
anova.out <- anova(lm.uni, lm.uni.test)
anova.out

## Multivariable linear--------------------------
# BACKWARD
## NOT LAGGED
lm.out1 <- lm(SHMI.Index ~ Engagement.Index.dichot + RecommendWork.Index.dichot + WorkStress.Index,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
# Remove engagement
lm.out1 <- lm(SHMI.Index ~ RecommendWork.Index.dichot + WorkStress.Index,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
da<-dominanceAnalysis(lm.out1)
print(da)
## CROSS-LAGGED
lm.out1 <- lm(SHMI.Index ~ Engagement.Index + RecommendWork.Index + WorkStress.Index  + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
### Drop engagement
lm.out1 <- lm(SHMI.Index ~ RecommendWork.Index + WorkStress.Index  + SHMI.before,  weights = Survey.clinical.proportion*Responses, data = data.NHS.2012_2019 )
summary(lm.out1)
da<-dominanceAnalysis(lm.out1)
print(da)

## Spline analysis-------------------------------
# In version 5, switched spline to means rather than dichot// LOOKS LIKE DICHOT (05/01/2021)
# https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-019-0666-3
# natural cubic spline basis function with K knots has K+1 degrees of freedom
##* Grab data--------------------------
data.temp <- data.NHS.2012_2019
##* Cleaning data, added 02/21/2021----
data.temp  = data.temp[!is.na(data.temp$Engagement.Index.dichot),]
data.temp  = data.temp[!is.na(data.temp$RecommendWork.Index.dichot),]
data.temp  = data.temp[!is.na(data.temp$WorkStress.Index),]

#--
xlab <- "Engagement"
data.temp$factor <- data.temp$Engagement.Index.dichot
xlab <- "Recommend Work"
data.temp$factor <- data.temp$RecommendWork.Index.dichot
xlab <- "Work Stress"
data.temp$factor <- data.temp$WorkStress.Index
#--
nrow(data.temp)
data.temp  = data.temp[!is.na(data.temp$factor),]
nrow(data.temp)
data.temp <- data.temp[order(data.temp$factor),]
summary(data.temp$factor)

##* Find optimal number of knots NOT CROSS-LAGGED----
r.squared.current <- NULL
df <- 1
for(df in 1:7) {
  # Use natural rather than B-splines
  # 2021,weighting not done
  # sf <- lm(SHMI.Index ~ ns(factor, df=df), data=data.temp) # weighting not done in 2021
  # 11/2022, try weighting
  sf <- lm(SHMI.Index ~ ns(factor, df=df), weights = Survey.clinical.proportion*Responses, data=data.temp)
  sf.out <- summary(sf)
  sf.out$fstatistic
  (pvalue <- sprintf(pf(sf.out$fstatistic[1], sf.out$fstatistic[2], sf.out$fstatistic[3], lower.tail=F), fmt='%#.4f'))
  r.squared.current <- sf.out$adj.r.squared
  print (paste('Knots (interior): ',df-1, '; df: ',df, '; R2 (adjusted) = ', sprintf(sf.out$adj.r.squared, fmt='%#.4f'), '; p-value: ',pvalue,sep = ''))
  df = df+1
}
# Best for engagement in non-cross lagged from above
df <- 3
# Best for recommend in non-cross lagged from above
df <- 5
# Best for work stress in non-cross lagged from above
df <- 4 # 2021
df <- 6 # 11/1022: optimal df = 6 when weighted by clinical proportion, BUT STILL FAILS TEST OF LINEARITY
# Test of non-linearity in non-cross lagged
sf      <- lm(SHMI.Index ~ ns(factor, df = df), data=data.temp)
lm.out  <- lm(SHMI.Index ~ ns(factor, df =  1), data=data.temp)
summary(sf)
summary(lm.out)
anova(lm.out, sf)
# 11/2022 summary of above: weighting splines only change optimal knots for workstress, but workstress STILL FAILS test of nonlinearity

##* Find optimal number of knots CROSS-LAGGED (01/07/2021 - who cares) -----
#--
xlab <- "Engagement"
data.temp$factor <- data.temp$Engagement.Index.dichot
xlab <- "Recommend Work"
data.temp$factor <- data.temp$RecommendWork.Index.dichot
xlab <- "Work Stress"
data.temp$factor <- data.temp$WorkStress.Index
nrow(data.temp )
#--
r.squared.current <- NULL
for(df in 1:7) {
  # Use natural rather than B-splines
  sf <- lm(SHMI.Index ~ ns(factor, df=df) + SHMI.before, data=data.temp)
  sf.out <- summary(sf)
  sf.out$fstatistic
  (pvalue <- sprintf(pf(sf.out$fstatistic[1], sf.out$fstatistic[2], sf.out$fstatistic[3], lower.tail=F), fmt='%#.4f'))
  r.squared.current <- sf.out$adj.r.squared
  print (paste('Knots (interior): ',df-1, '; df: ',df, '; R2 (adjusted) = ', sprintf(sf.out$adj.r.squared, fmt='%#.4f'), '; p-value: ',pvalue,sep = ''))
  df = df+1
}
# Best for engagement is
df <- 3 #if use same as in non-cross-lagged
df <- 6
# Best for recommend
df <- 5 #if use same as in non-cross-lagged
df <- 5 
# Best for work stress
df <- 1 #if use same as in non-cross-lagged
df <- 1
##* Test of non-linearity--------------
# Below, correct with SHMI.before added on 01/07/2021
sf      <- lm(SHMI.Index ~ ns(factor, df = df) + SHMI.before, data=data.temp)
lm.out  <- lm(SHMI.Index ~ ns(factor, df =  1) + SHMI.before, data=data.temp)
anova(lm.out, sf)
# 11/2021: none are significant

##* Multipanel plots-------------------
windows(800, 800, pointsize = 12) # Size 600x600
plot.new()
par(mfrow= c(2, 2))
mtext("Spline regression (univariable):", side = 3, line = -2, outer = TRUE, cex=1.5, font=2)
mtext("Testing for non-linearity", side = 3, line = -3.2, outer = TRUE, cex=1, font=2)

#-- USE NON-CROSS-LAGGED knots!
xlab <- "Engagement (rate of 'Often/Always')"
data.temp$factor <- data.temp$Engagement.Index.dichot
df <- 3 
xlab <- "Recommend Work (rate of 'Agree/Strongly agree')"
data.temp$factor <- data.temp$RecommendWork.Index.dichot
df <- 5 
xlab <- "Burnout (rate of 'Yes')"
data.temp$factor <- data.temp$WorkStress.Index
df <- 4 
#--
# Use optimal number of knots from above
# Use natural rather than B-splines
data.temp <- data.temp[order(data.temp$factor),] #switched from SHMI.Index 02/17/2021
sf <- lm(SHMI.Index ~ ns(factor, df=df), data=data.temp)
# Location of knots on x-axis
attr(ns(data.temp$factor, df=df), "knots")
sf.out <- summary(sf)
attr(terms(sf), "predvars")
print (paste('Knots (interior): ', df-1, '; R2 (adjusted) = ', sf.out$adj.r.squared,sep = ''))
lm.out <- lm(SHMI.Index ~ factor, data=data.temp)
anova.out <- anova(lm.out, sf)
lm.out <- summary(lm.out)
data.temp <- mutate(data.temp, smooth=fitted(sf))
Main <- '' # paste("Spline versus linear regression: ",xlab,sep='')
base::plot(data.temp$factor, data.temp$SHMI.Index, pch = 19, cex=0.5, col="gray", xlab = xlab, ylab = "SHMI", main=Main)
#data.temp <- data.temp[order(data.temp$factor),]
lines(data.temp$factor, data.temp$smooth, col = 'red')
# Knots plotted
for(i in 1:df-1) {
  knot.index <- round(length(data.temp$factor)/df,0)*i
  points(data.temp$factor[knot.index],data.temp$smooth[knot.index], pch=16,col="red")
  }
# Linear regression line
abline(lm.out$coefficients[1,1],lm.out$coefficients[2,1], col='blue')
R2 <- sprintf("%1.1f%%", 100*sf.out$adj.r.squared)
pvalue <- round(anova.out$`Pr(>F)`[2],3)
text(par("usr")[1]+strwidth("A"),par("usr")[3]+3.4* strheight("A"),adj=c(0,0),paste("Knots (interior) = ",df-1,sep=))
text(par("usr")[1]+strwidth("A"),par("usr")[3]+2.2* strheight("A"),adj=c(0,0),paste("R2 (adjusted) = ",R2,sep=))
text(par("usr")[1]+strwidth("A"),par("usr")[3]+strheight("A"),adj=c(0,0),paste("P (non-linear) = ",pvalue,sep=))

# Legend AFTER ALL THREE DONE
plot.new()
par(mar=c(5,0,4,2) + 0.1)
text(par("usr")[1]+strwidth("A"),par("usr")[4]-4.6 * strheight("A"),adj=c(0,0),paste("Notes: ",sep=), font = 2)
text(par("usr")[1]+strwidth("A"),par("usr")[4]-5.8 * strheight("A"),adj=c(0,0),paste("Each point represents one Trust",sep=))
text(par("usr")[1]+strwidth("A"),par("usr")[4]-7.0 * strheight("A"),adj=c(0,0),paste("Red is spline with optimal number of knots",sep=))
text(par("usr")[1]+strwidth("A"),par("usr")[4]-7.0 * strheight("A"),adj=c(0,0),paste("Red",sep=), font = 1, col = 'red')
text(par("usr")[1]+strwidth("A"),par("usr")[4]-8.2 * strheight("A"),adj=c(0,0),paste("Blue is linear regression",sep=))
text(par("usr")[1]+strwidth("A"),par("usr")[4]-8.2 * strheight("A"),adj=c(0,0),paste("Blue",sep=), font = 1, col = 'blue')
text(par("usr")[1]+strwidth("A"),par("usr")[4]-9.4 * strheight("A"),adj=c(0,0),paste("Splines are natural cubic splines",sep=))
text(par("usr")[1]+strwidth("A"),par("usr")[4]-10.6 * strheight("A"),adj=c(0,0),paste("SHMI. Summary Hospital-level Mortality Indicator",sep=))


rstudioapi::savePlotAsImage( # Print 
  paste('Spline - ', 'univariable',' - summary - ',  current.date,'.png',sep=""),
  format = "png", width = 800, height = 800)

##Spline: Multivariable-------------------------
# with cross-lag 2021-01-03
# 2021-02-01 had to add na.action = na.omit to all regressions
data.temp <- data.NHS.2012_2019
#--
nrow(data.temp)
data.temp  = data.temp[!is.na(data.temp$Engagement.Index.dichot),]
data.temp  = data.temp[!is.na(data.temp$RecommendWork.Index.dichot),]
data.temp  = data.temp[!is.na(data.temp$WorkStress.Index),]
nrow(data.temp)
#--
## Are factors significant in the model??
# Best for engagement is df = 5
df.e  <- 3 # 6 if you recalc with cross-lag
# Best for recommend is df = 3
df.r  <- 5 # 4 if you recalc with cross-lag
# Best for work stress is df = 3
df.ws <- 1 #3 # Failed test of non-linearity
#--

# All splined
# - Without weighting
sf.multi <- lm(SHMI.Index ~ ns(data.temp$Engagement.Index.dichot, df=df.e) + ns(data.temp$RecommendWork.Index.dichot, df=df.r) + ns(data.temp$WorkStress.Index, df=df.ws) + SHMI.before, na.action = na.omit, data=data.temp)
# - With weighting: weights = Survey.clinical.proportion*Responses
sf.multi <- lm(SHMI.Index ~ ns(data.temp$Engagement.Index.dichot, df=df.e) + ns(data.temp$RecommendWork.Index.dichot, df=df.r) + ns(data.temp$WorkStress.Index, df=df.ws) + SHMI.before, weights = Survey.clinical.proportion*Responses, na.action = na.omit, data=data.temp)
# All but engagement splined 
## Use this  to isolate roles of recommend and workstress
# - Without weighting
sf.multi <- lm(SHMI.Index ~ ns(data.temp$RecommendWork.Index.dichot, df=df.r) + ns(data.temp$WorkStress.Index, df=df.ws) + SHMI.before, na.action = na.omit, data=data.temp)
# - With weighting (2022-11-04): weights = Survey.clinical.proportion*Responses
sf.multi <- lm(SHMI.Index ~ ns(data.temp$RecommendWork.Index.dichot, df=df.r) + ns(data.temp$WorkStress.Index, df=df.ws) + SHMI.before, weights = Survey.clinical.proportion*Responses, na.action = na.omit, data=data.temp)
# All but burnout splined
sf.multi <- lm(SHMI.Index ~ ns(data.temp$Engagement.Index.dichot, df=df.e) + ns(data.temp$RecommendWork.Index.dichot, df=df.r) + SHMI.before, na.action = na.omit, data=data.temp)
# Only recommend (EG DROPPING engagement and workstress)
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$RecommendWork.Index.dichot, df=df.r) + SHMI.before, na.action = na.omit , data=data.temp)
summary(sf.multi)

# Adding engagement (weighted) to only RecommendWork and Workstress (weighted)
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$RecommendWork.Index.dichot, df=df.r) + ns(data.temp$WorkStress.Index, df=df.ws) + SHMI.before, weights = Survey.clinical.proportion*Responses, na.action = na.omit, data=data.temp)
# Dropping engagement
# - Without weighting
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$RecommendWork.Index.dichot, df=df.r) + ns(data.temp$WorkStress.Index, df=df.ws) + SHMI.before, na.action = na.omit, data=data.temp)
# - With weighting (2022-11-04): weights = Survey.clinical.proportion*Responses
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$RecommendWork.Index.dichot, df=df.r) + ns(data.temp$WorkStress.Index, df=df.ws) + SHMI.before, weights = Survey.clinical.proportion*Responses, na.action = na.omit, data=data.temp)
# Dropping recommend.work
# - Without weighting
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$Engagement.Index.dichot, df=df.e) + ns(data.temp$WorkStress.Index, df=df.ws) + SHMI.before, na.action = na.omit, data=data.temp)
# - With weighting: weights = Survey.clinical.proportion*Responses
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$Engagement.Index.dichot, df=df.e) + ns(data.temp$WorkStress.Index, df=df.ws) + SHMI.before, weights = Survey.clinical.proportion*Responses, na.action = na.omit, data=data.temp)
# Dropping work.stress
# - Without weighting
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$Engagement.Index.dichot, df=df.e) + ns(data.temp$RecommendWork.Index.dichot, df=df.r) + SHMI.before, na.action = na.omit, data=data.temp)
# - With weighting: weights = Survey.clinical.proportion*Responses
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$Engagement.Index.dichot, df=df.e) + ns(data.temp$RecommendWork.Index.dichot, df=df.r) + SHMI.before, weights = Survey.clinical.proportion*Responses, na.action = na.omit, data=data.temp)
# Dropping engagement and recommend (EG ONLY WORKSTRESS)
# - Without weighting
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$WorkStress.Index, df=df.r) + SHMI.before, na.action = na.omit , data=data.temp)
# - With weighting: weights = Survey.clinical.proportion*Responses
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$WorkStress.Index, df=df.r) + SHMI.before, weights = Survey.clinical.proportion*Responses, na.action = na.omit , data=data.temp)
# Dropping engagement and workstress (EG ONLY RECOMMEND)
# - Without weighting
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$RecommendWork.Index.dichot, df=df.r) + SHMI.before, na.action = na.omit , data=data.temp)
# - With weighting: weights = Survey.clinical.proportion*Responses
sf.multi.test <- lm(SHMI.Index ~ ns(data.temp$RecommendWork.Index.dichot, df=df.r) + SHMI.before, weights = Survey.clinical.proportion*Responses, na.action = na.omit , data=data.temp)
# ONLY RECOMMEND - BUT NO SPLINE EITHER
sf.multi.test <- lm(SHMI.Index ~ data.temp$RecommendWork.Index.dichot + SHMI.before, weights = Survey.clinical.proportion*Responses, na.action = na.omit , data=data.temp)
# Testing for model difference:
summary(sf.multi.test)
anova.out <- anova(sf.multi, sf.multi.test)
anova.out

## Region----------------------------------------
# 2021-02-01 had to add na.action = na.omit to all regressions
lm.out1 <- lm(SHMI.Index ~ Engagement.Index + North,  weights = Survey.clinical.proportion*Responses, na.action = na.omit, data = data.NHS.2012_2019 )
summary(lm.out1)
lm.out1 <- lm(SHMI.Index ~ Engagement.Index + SHMI.before + North,  weights = Survey.clinical.proportion*Responses, na.action = na.omit, data = data.NHS.2012_2019 )
summary(lm.out1)

nrow(data.NHS.2012_2019)/5

## Over time-------------------------------------
# https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group
# par(oma=c(5,1,0,0))
table.responses <- aggregate(Responses  ~ Year.Index, data.NHS.2012_2019, sum)

# Engagement over time
table.temp1 <- aggregate(Engagement.Index.dichot  ~ Year.Index, data.NHS.2012_2019, mean )
table.temp2 <- aggregate(Responses  ~ Year.Index, data.NHS.2012_2019, sum)
table.temp3 <- merge(x = table.temp1, y = table.temp2, by = c("Year.Index"), all.x=TRUE)

# Engagement and workstress over time
table.temp1 <- aggregate( WorkStress.Index ~ Year.Index, data.NHS.2012_2019, mean )
table.temp2 <- aggregate(Engagement.Index ~ Year.Index, data.NHS.2012_2019, mean )
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

Title <- 'Change in engagement 2012 - 2019.png'
Title <- gsub("\\?|\\!|\\'", "", Title) 
plotname <- Title
rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste(plotname," - ",current.date,".png",sep=""),
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
base::plot(table.temp3, ylim=c(0,1),xlabel='Year',ylabel='Rate of work stress', xlim=c(2011.75, 2019.25), main=Title)
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

## Associations----------------------------------

#table.temp <- data.NHS.2012_2019[data.NHS.2012_2019$Year.Index < 2025, ]
##* Grab data--------------------------
data.temp <- data.NHS.2012_2019
##* Cleaning data, added 02/21/2021----
data.temp  = data.temp[!is.na(data.temp$Engagement.Index.dichot),]
data.temp  = data.temp[!is.na(data.temp$RecommendWork.Index.dichot),]
data.temp  = data.temp[!is.na(data.temp$WorkStress.Index),]

##* Engage & work stress---------------
Title <-'Association between engagement and work stress'
plot(data.temp$Engagement.Index.dichot,data.temp$WorkStress.Index,xlab='Work stress (rate)',ylab='Work stress (rate)',main=Title)
#text(table.temp$Engagement.Index,table.temp$WorkStress.Index,paste(table.temp$Year.Index,'',sep=''),pos=3)
xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + 1.2* strheight("A")
ymax <- par("usr")[4] - strheight("A")

lm.out1 <- lm(WorkStress.Index ~ Engagement.Index.dichot,  weights = NULL, data = data.temp )
lm.summary <- summary(lm.out1)
P.value <- lm.summary[["coefficients"]][2, 4]
P.value <- paste('P-value: ',sprintf(P.value, fmt='%#.3f'),sep='')
lm.summary$r.squared

text(xmin,ymin, P.value, adj=c(0,1))
abline(lm.out1, col='red')

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste('Association between engagement and stress 2012 - 2019',current.date,'.png'),
  format = "png", width = 600, height = 400)

##* Engage & work recommendation-------
Title <-'Association between engagement and work recommendation'
plot(data.temp$Engagement.Index.dichot,data.temp$RecommendWork.Index.dichot,xlab='Engagement (rate)',ylab='Work recommendation (rate)',main=Title)
#text(table.temp$Engagement.Index,table.temp$WorkStress.Index,paste(table.temp$Year.Index,'',sep=''),pos=3)
xmin <- par("usr")[1] + strwidth("A")
xmax <- par("usr")[2] - strwidth("A")
ymin <- par("usr")[3] + 1.2* strheight("A")
ymax <- par("usr")[4] - strheight("A")

lm.out1 <- lm(RecommendWork.Index.dichot ~ Engagement.Index.dichot,  weights = NULL, data = data.temp )
abline(lm.out1, col='blue')
lm.summary <- summary(lm.out1)
P.value <- lm.summary[["coefficients"]][2, 4]
P.value <- paste('P-value: ',sprintf(P.value, fmt='%#.3f'),sep='')
text(xmin,ymax, P.value, adj=c(0,1))
R2 <- sprintf("%1.1f%%", 100*lm.summary$adj.r.squared)
text(xmin,ymax-1.1* strheight("A"),adj=c(0,1),bquote(R^2 == .(R2)))

rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste('Association between engagement and recommendation 2012 - 2019',current.date,'.png'),
  format = "png", width = 600, height = 600)

##* Determine optimal number of knots NOT CROSS-LAGGED----
r.squared.current <- NULL
for(df in 1:10) {
  # Use natural rather than B-splines
  sf <- lm(RecommendWork.Index.dichot ~ ns(Engagement.Index.dichot, df=df), data=data.temp)
  sf.out <- summary(sf)
  sf.out$fstatistic
  (pvalue <- sprintf(pf(sf.out$fstatistic[1], sf.out$fstatistic[2], sf.out$fstatistic[3], lower.tail=F), fmt='%#.4f'))
  r.squared.current <- sf.out$adj.r.squared
  print (paste('Knots (interior): ',df-1, '; df: ',df, '; R2 (adjusted) = ', sprintf(sf.out$adj.r.squared, fmt='%#.4f'), '; p-value: ',pvalue,sep = ''))
  df = df+1
}
# Best for engagement in non-cross lagged from above
df <- 9

# Test of non-linearity in non-cross lagged
sf      <- lm(RecommendWork.Index.dichot ~ ns(Engagement.Index.dichot, df = df), data=data.temp)
lm.out  <- lm(RecommendWork.Index.dichot ~ ns(Engagement.Index.dichot, df =  1), data=data.temp)
summary(sf)
summary(lm.out)
anova(lm.out, sf)

# Plotting spline and knots
data.temp <- mutate(data.temp, smooth=fitted(sf))
data.temp <- data.temp[order(data.temp$Engagement.Index.dichot),]
lines(data.temp$Engagement.Index.dichot, data.temp$smooth, col = 'red')
# Knots plotted
for(i in 1:df-1) {
  knot.index <- round(length(data.temp$Engagement.Index.dichot)/df,0)*i
  points(data.temp$Engagement.Index.dichot[knot.index],data.temp$smooth[knot.index], pch=16,col="red")
}

R2 <- sprintf("%1.1f%%", 100*sf.out$adj.r.squared)
pvalue <- round(anova.out$`Pr(>F)`[2],3)
text(par("usr")[4]-strwidth("A"),par("usr")[3]+3.4* strheight("A"),adj=c(1.5,0),paste("Knots (interior) = ",df-1,sep=))
text(par("usr")[4]-strwidth("A"),par("usr")[3]+2.2* strheight("A"),adj=c(1,0),paste("R2 (adjusted) = ",R2,sep=))
text(par("usr")[4]-strwidth("A"),par("usr")[3]+strheight("A"),adj=c(1,0),paste("P (non-linear) = ",pvalue,sep=))


##* Plot ------------------------------
PlotName = 'Association between engagement and work recommendation 2012 - 2019'
PlotName = 'Association between engagement and work recommendation 2012 - 2019 (with spline)'
rstudioapi::savePlotAsImage( # Print at 800*plotheight
  paste(PlotName,current.date,'.png'),
  format = "png", width = 600, height = 600)


## 2019 Meta-analysis----------------------------
summary(data.NHS.2012_2019$Year.Index)
data.NHS.2019 <- data.NHS.2012_2019[data.NHS.2012_2019$Year.Index == 2019, ]
nrow(data.NHS.2019)
# For metaprop
# Engagement (mean of Vigor, dedication, adsorption
summary(data.NHS.2019$Engagement.Index.dichot)

#data.NHS.2019 <- data.NHS.2019[order(data.NHS.2019$q2a.vigor.OftenAlways/data.NHS.2019$q2a.Respondents),]
data.NHS.2019 <- data.NHS.2019[order(data.NHS.2019$Engagement.Index),]
Title = "Meta-analysis of mean Engagement proportion of often/always"
meta1 <- metaprop(Engagement.Index.dichot*Responses,Responses,studlab = Code, data=data.NHS.2019,comb.fixed=FALSE,hakn=TRUE,
                  sm = "PLOGIT", method ='GLMM', 
                  title = "Meta-analysis of mean Engagement proportion of often/always")
data.NHS.2019 <- data.NHS.2019[order(data.NHS.2019$RecommendWork.Index.dichot),]
Title = "Meta-analysis of RecommendWork proportion of often/always"
meta1 <- metaprop(RecommendWork.Index.dichot*Responses,Responses,studlab = Code, data=data.NHS.2019,comb.fixed=FALSE,hakn=TRUE,
                  sm = "PLOGIT", method ='GLMM', 
                  title = "Meta-analysis of RecommendWork proportion of often/always")
data.NHS.2019 <- data.NHS.2019[order(data.NHS.2019$WorkStress.Index),]
Title = "Meta-analysis of WorkStress  proportion of 'yes'"
meta1 <- metaprop(WorkStress.Index*Responses,Responses,studlab = Code, data=data.NHS.2019,comb.fixed=FALSE,hakn=TRUE,
                  sm = "PLOGIT", method ='GLMM', 
                  title = "Meta-analysis of WorkStress  proportion of 'yes'")
#Results
(summary(meta1))
(summary(inv.logit(meta1$TE)))
(meta1$I2*100)
positive.deviants <- 0
negative.deviants <- 0
for(i in 1:length(meta1$TE)){
  if (meta1$lower[i]>inv.logit(meta1$TE.random)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");positive.deviants <- positive.deviants + 1}
  if (meta1$upper[i]<inv.logit(meta1$TE.random)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");negative.deviants <- negative.deviants + 1}
}
(positive.deviants)
(negative.deviants)

Title <- "The 5 highest and lowest Trusts in dedication"
forest(meta1, xlim = c(0,1)) # Size: 1000 x 4000

benchmark_value <- 0.75


meta1 <- meta1[order(meta1$TE),]


meta::forest(meta1, leftlabs=c("Trust","SHMI"),
             leftcols = c("studlab"), # "SHMI.index"), 
             xlab="Rate of 'always' dedicated", 
             #ref = benchmark_value,
             digits.addcols.left = 2, just.addcols.left = "center", colgap.left = "4mm",
             overall = TRUE, 
             #sortvar = TE.random,
             subgroup = TRUE, print.tau2 = FALSE, print.Q = FALSE, print.pval.Q = FALSE, print.subgroup.labels = TRUE, print.byvar = FALSE, col.diamond = "blue", xlim = c(0,1)) # Size: 800 x 1300
grid.text(Title, 0.5, 0.95, gp=gpar(cex=1.4))

FileName <- 'Forest plot - recommendwork and SHMI 2019'# was 2017 before 11/11/2022
FileName <- 'Forest plot - workstress and SHMI 2019'# was 2017 before 11/11/2022
FileName <- 'Forest plot - engagement and SHMI 2019'# was 2017 before 11/11/2022
rstudioapi::savePlotAsImage(
  paste(FileName, ' - ', current.date, '.png', sep=''),
  format = "png", width = 850, height = 3000)


