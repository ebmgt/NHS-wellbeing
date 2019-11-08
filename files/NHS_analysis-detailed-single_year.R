#This file is best used within R Studior
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
library(metafor)
library (meta) # metamean
library(boot) #inv.logit
#---------------------------------


##IF FILE IS ALREADY MADE< SKIP THIS SECTION

# -------------------------------------

source('NHS_analysis-file_merging-functions.R')

data<- NULL

# Get SHMI data
data.temp <- NULL
year <- "2017"
filename = paste(year, "/SHMI data at trust level - mortality - ", year ,".csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
(nrow(data.temp))
names(data.temp)[1] <- "Code"
# Summary hospital mortality index (SHMI)
#  Standardized Mortality Ratio (SMR)
data.temp$SHMI <-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$SHMI)))))
data.temp$year.index <- as.numeric(year)
data.temp <- rename(data.temp, c("SHMI" = paste("SHMI.", "index" , sep="")))
myvars <- c("Code","Site","year.index","SHMI.index")
data.temp <- data.temp[myvars]
(nrow(data.temp))
#CHoose whether merging or starting
#data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code")
data <- data.temp
(nrow(data))
head(data)

# Engagement
# Note: EWES used a 7 point (0 to 6) scale. Also responses were specific frequencies
# q2a. I look forward to going to work (vigor)
# q2b. I am enthusiastic about my job (dedication)
# q2c. Time passes quickly when I am working (absorption)
year <- "2017"
data.temp <- NULL
filename = paste(year, "/ST", substr(year, 3, 4), " - job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
data.temp = data.temp[data.temp$Code != "",]
(nrow(data.temp))
head(data.temp)
#Get site size
data.temp$Observations <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$q1.Respondents)))))
data.temp$q2a.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2a.Respondents)))))
data.temp$q2a.vigor <-      (data.temp$q2a.Never + data.temp$q2a.Rarely*2 + data.temp$q2a.Sometimes*3 + data.temp$q2a.Often*4 + data.temp$q2a.Always*5)/100
data.temp$q2a.vigor.OftenAlways <- 0
data.temp$q2b.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2b.Respondents)))))
data.temp$q2b.dedication <-      (data.temp$q2b.Never + data.temp$q2b.Rarely*2 + data.temp$q2b.Sometimes*3 + data.temp$q2b.Often*4 + data.temp$q2b.Always*5)/100
data.temp$q2b.dedication.OftenAlways <- 0
data.temp$q2c.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2c.Respondents)))))
data.temp$q2c.absorption <-      (data.temp$q2c.Never + data.temp$q2c.Rarely*2 + data.temp$q2c.Sometimes*3 + data.temp$q2c.Often*4 + data.temp$q2c.Always*5)/100
data.temp$q2c.absorption.OftenAlways <- 0

for(i in 1:nrow(data.temp))
{
  # 2a Vigor
  q2a.Never <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2a.Never[i]))))
  q2a.Rarely <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2a.Rarely[i]))))
  q2a.Sometimes <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2a.Sometimes[i]))))
  q2a.Often <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2a.Often[i]))))
  q2a.Always <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2a.Always[i]))))
  sumcheck <- sum(q2a.Never,q2a.Rarely,q2a.Sometimes,q2a.Often,q2a.Always)
  temp.vector1 <- rep(1,round(q2a.Never*data.temp$q2a.Respondents[i]/100,digits=0))
  temp.vector2 <- rep(2,round(q2a.Rarely*data.temp$q2a.Respondents[i]/100,digits=0))
  temp.vector3 <- rep(3,round(q2a.Sometimes*data.temp$q2a.Respondents[i]/100,digits=0))
  temp.vector4 <- rep(4,round(q2a.Often*data.temp$q2a.Respondents[i]/100,digits=0))
  temp.vector5 <- rep(5,round(q2a.Always*data.temp$q2a.Respondents[i]/100,digits=0))
  temp.vector <- c(temp.vector1,temp.vector2,temp.vector3,temp.vector4,temp.vector5)
  (data.temp$q2a.Respondents[i])
  length(temp.vector)
  (data.temp$q2a.vigor.mean[i] <- mean(temp.vector))
  (data.temp$q2a.vigor.sd[i] <- sd(temp.vector))
  (data.temp$q2a.vigor.Always[i] <- round((q2a.Always)*data.temp$q2a.Respondents[i]/100,0))
  (data.temp$q2a.vigor.OftenAlways[i] <- round((q2a.Often + q2a.Always)*data.temp$q2a.Respondents[i]/100,0))
  # 2b dedication
  q2b.Never <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2b.Never[i]))))
  q2b.Rarely <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2b.Rarely[i]))))
  q2b.Sometimes <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2b.Sometimes[i]))))
  q2b.Often <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2b.Often[i]))))
  q2b.Always <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2b.Always[i]))))
  sumcheck <- sum(q2b.Never,q2b.Rarely,q2b.Sometimes,q2b.Often,q2b.Always)
  temp.vector1 <- rep(1,round(q2b.Never*data.temp$q2b.Respondents[i]/100,digits=0))
  temp.vector2 <- rep(2,round(q2b.Rarely*data.temp$q2b.Respondents[i]/100,digits=0))
  temp.vector3 <- rep(3,round(q2b.Sometimes*data.temp$q2b.Respondents[i]/100,digits=0))
  temp.vector4 <- rep(4,round(q2b.Often*data.temp$q2b.Respondents[i]/100,digits=0))
  temp.vector5 <- rep(5,round(q2b.Always*data.temp$q2b.Respondents[i]/100,digits=0))
  temp.vector <- c(temp.vector1,temp.vector2,temp.vector3,temp.vector4,temp.vector5)
  (data.temp$q2b.Respondents[i])
  length(temp.vector)
  (data.temp$q2b.dedication.mean[i] <- mean(temp.vector))
  (data.temp$q2b.dedication.sd[i] <- sd(temp.vector))
  (data.temp$q2b.dedication.Always[i] <- round((q2b.Always)*data.temp$q2b.Respondents[i]/100,0))
  (data.temp$q2b.dedication.OftenAlways[i] <- round((q2b.Often + q2b.Always)*data.temp$q2b.Respondents[i]/100,0))
  # 2c absorption
  q2c.Never <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2c.Never[i]))))
  q2c.Rarely <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2c.Rarely[i]))))
  q2c.Sometimes <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2c.Sometimes[i]))))
  q2c.Often <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2c.Often[i]))))
  q2c.Always <- as.numeric(gsub(",", "", as.character(str_trim(data.temp$q2c.Always[i]))))
  sumcheck <- sum(q2c.Never,q2c.Rarely,q2c.Sometimes,q2c.Often,q2c.Always)
  temp.vector1 <- rep(1,round(q2c.Never*data.temp$q2c.Respondents[i]/100,digits=0))
  temp.vector2 <- rep(2,round(q2c.Rarely*data.temp$q2c.Respondents[i]/100,digits=0))
  temp.vector3 <- rep(3,round(q2c.Sometimes*data.temp$q2c.Respondents[i]/100,digits=0))
  temp.vector4 <- rep(4,round(q2c.Often*data.temp$q2c.Respondents[i]/100,digits=0))
  temp.vector5 <- rep(5,round(q2c.Always*data.temp$q2c.Respondents[i]/100,digits=0))
  temp.vector <- c(temp.vector1,temp.vector2,temp.vector3,temp.vector4,temp.vector5)
  (data.temp$q2c.Respondents[i])
  length(temp.vector)
  (data.temp$q2c.absorption.mean[i] <- mean(temp.vector))
  (data.temp$q2c.absorption.sd[i] <- sd(temp.vector))
  (data.temp$q2c.absorption.Always[i] <- round((q2c.Always)*data.temp$q2c.Respondents[i]/100,0))
  (data.temp$q2c.absorption.OftenAlways[i] <- round((q2c.Often + q2c.Always)*data.temp$q2c.Respondents[i]/100,0))
}
# Get Engagement observations count
data.temp$Survey.engagement.observations.index <- Survey.engagement.observations(data.temp)
# Get engagement
data.temp$Engagement.index <- Survey.engagement(data.temp)
(nrow(data.temp))
head(data.temp)
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code", all = TRUE)
(nrow(data))
head(data)

# Burnout
######## WARNING - BURNOUT SHIFTED QUESTION NUMBERS. 2017 WAS q9 AND 2018 WAS q11#######################
###ST18 - health_sheet4-non-specialtyAcuteTrusts.csv############################
# Note: Maslach is propietary, so:
# q11c.During the last 12 months have you felt unwell as a result of work related stress?
# q11c.Yes
# Index year
year <- "2017"
#year <- year.index
year.label <- "index" # "index" "before" "after"
data.temp <- NULL
filename = paste(year, "/ST", substr(year, 3, 4), " - health_sheet4-non-specialtyAcuteTrusts.csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
data.temp = data.temp[data.temp$Code != "",]
(nrow(data.temp))
head(data.temp)
# Get burnout
data.temp$Burnout.count <- Survey.burnout.count(data.temp)
data.temp$Burnout.proportion <- Survey.burnout.proportion(data.temp)
data.temp <- rename(data.temp, c("Burnout.count" = paste("Burnout.count", year.label , sep=".")))
data.temp <- rename(data.temp, c("Burnout.proportion" = paste("Burnout.proportion", year.label , sep=".")))
# Limit columns
myvars <- c("Code", paste("Burnout.count", year.label, sep="."), paste("Burnout.proportion", year.label, sep="."))
data.temp <- data.temp[myvars]
head(data.temp)
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code", all = TRUE)
(nrow(data))
head(data)
data.temp <- NULL
NHS.detailed <- data

(filename = paste("csv/" , year, ".NHS-detailed.csv",sep=""))
write.csv(data, file = filename, quote = TRUE, eol = "\n", na = "NA", row.names = FALSE)
(filename = paste("rda/" , year, ".NHS-detailed.rda",sep=""))
save(data.temp, file = filename)

# -------------------------------------

#IF FILE IS ALREADY MADE< START HERE AND LOAD THE FILE
# Grab data
# Use NHS 2017. 2017.NHS-detailed.csv
file.filter <- matrix(c("Spreadsheets","*.csv;*.xls;*.xlsx","All","*.*"),byrow=TRUE,ncol=2)
filename = choose.files(filters = file.filter,caption = "Select data file",index = 1,multi=FALSE)
NHS.detailed<- read.table(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
head(NHS.detailed)

# Analyses: engagement + burnout
head(NHS.detailed)
lm.out1 <- lm(SHMI.index ~ Burnout.proportion.index,  weights = NULL, data = NHS.detailed )
summary(lm.out1)
lm.out1 <- lm(SHMI.index ~ Engagement.index,  weights = NULL, data = NHS.detailed )
summary(lm.out1)
lm.out1 <- lm(SHMI.index ~ Engagement.index + Burnout.proportion.index,  weights = NULL, data = NHS.detailed )
summary(lm.out1)

nrow(NHS.detailed)
NHS.detailed <- na.omit(NHS.detailed, cols="q2b.dedication.mean")
nrow(NHS.detailed)

# For metamean
# Vigor
NHS.detailed <- NHS.detailed[order(NHS.detailed$q2a.vigor.mean),]
NHS.detailed <- NHS.detailed[order(NHS.detailed$q2b.dedication.mean),]
NHS.detailed <- NHS.detailed[order(NHS.detailed$q2c.absorption.mean),]
meta1 <- metamean(q2a.Respondents,q2a.vigor.mean,q2a.vigor.sd,studlab = Code, data=NHS.detailed,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of mean vigor")
meta1 <- metamean(q2b.Respondents,q2b.dedication.mean,q2b.dedication.sd,studlab = Code, data=NHS.detailed,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of mean dedication")
meta1 <- metamean(q2c.Respondents,q2c.absorption.mean,q2c.absorption.sd,studlab = Code, data=NHS.detailed,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of mean absorption")
summary(meta1)
summary(meta1$TE)
(meta1$I2*100)
positive.deviants <- 0
negative.deviants <- 0
for(i in 1:length(meta1$TE)){
  if (meta1$lower[i]>meta1$TE.random){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");positive.deviants <- positive.deviants + 1}
  if (meta1$upper[i]<meta1$TE.random){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");negative.deviants <- negative.deviants + 1}
}
# Alternative method #1
positive.deviants <- 0
negative.deviants <- 0
for(i in 1:length(meta1$TE)){
  SD <- meta1$seTE[i] * sqrt(meta1$n[i])
  Z <- (meta1$mean[i] - meta1$TE.random)/(SD/sqrt(meta1$n[i]))
  #print (Z)
  if (Z > qnorm(0.975)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");positive.deviants <- positive.deviants + 1}
  if (Z < -qnorm(0.975)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");negative.deviants <- negative.deviants + 1}
}
(positive.deviants)
(negative.deviants)
# Alternative method #2 - THESE RESULTS SEEM MORE SENSIBLE
NOT WORKING YET
(CI.lower <- qnorm(0.025, mean = meta1$TE.random, sd = meta1$seTE.random * sqrt(meta1$k), log = FALSE))
(CI.upper <- qnorm(0.975, mean = meta1$TE.random, sd = meta1$seTE.random * sqrt(meta1$k), log = FALSE))
positive.deviants <- 0
negative.deviants <- 0
for(i in 1:length(meta1$TE)){
  SD <- meta1$seTE[i] * sqrt(meta1$n[i])
  Z <- (meta1$mean[i] - meta1$TE.random)/(SD/sqrt(meta1$n[i]))
  #print (Z)
  if (Z > qnorm(0.975)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");positive.deviants <- positive.deviants + 1}
  if (Z < -qnorm(0.975)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");negative.deviants <- negative.deviants + 1}
}
(positive.deviants)
(negative.deviants)

forest(meta1, xlim = c(0,5))
(minvalue <- paste(round(min(meta1$TE)*1,1),"",sep=""))
(maxvalue <- paste(round(max(meta1$TE)*1,1),"",sep=""))
plot(NULL)
mtext(paste("Range:", minvalue,"to",maxvalue,sep=" "),side=1, line=1)

# For metaprop
# Vigor
#data.temp <- data.temp[order(data.temp$q2a.vigor.Always/data.temp$q2a.Respondents),]
#meta1 <- metaprop(q2a.vigor.Always,q2a.Respondents,studlab = Code, data=data.temp,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion always vigor")
#(meta1$I2*100)
#Vigor
temp <- NHS.detailed[order(NHS.detailed$q2a.vigor.OftenAlways/NHS.detailed$q2a.Respondents),]
temp <- subset(NHS.detailed,!(is.na(NHS.detailed["q2a.Respondents"])))
meta1 <- metaprop(q2a.vigor.OftenAlways,q2a.Respondents,studlab = Code, data=temp,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion often/always vigor")
#Dedication
temp <- NHS.detailed[order(NHS.detailed$q2b.dedication.OftenAlways/NHS.detailed$q2b.Respondents),]
temp <- subset(NHS.detailed,!(is.na(NHS.detailed["q2b.Respondents"])))
meta1 <- metaprop(q2b.dedication.OftenAlways,q2b.Respondents,studlab = Code, data=temp,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion often/always dedication")
#Absorption
temp <- NHS.detailed[order(NHS.detailed$q2c.absorption.OftenAlways/NHS.detailed$q2c.Respondents),]
temp <- subset(NHS.detailed,!(is.na(NHS.detailed["q2b.Respondents"])))
meta1 <- metaprop(q2c.absorption.OftenAlways,q2c.Respondents,studlab = Code, data=temp,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion often/always absorption")
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
forest(meta1, xlim = c(0,1)) # Size: 1000 x 4000
(minvalue <- paste(round(min(inv.logit(meta1$TE))*100),"%",sep=""))
(maxvalue <- paste(round(max(inv.logit(meta1$TE))*100),"%",sep=""))
mtext(paste("Range:", minvalue,"to",maxvalue,sep=" "),side=1, line=2)

# Dedication
NHS.detailed <- NHS.detailed[order(NHS.detailed$q2b.dedication.Always/NHS.detailed$q2b.Respondents),]
meta1 <- metaprop(q2b.dedication.Always,q2b.Respondents,studlab = Code, data=NHS.detailed,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion always dedication")
(meta1$I2*100)
data.temp <- data.temp[order(data.temp$q2b.dedication.OftenAlways/data.temp$q2b.Respondents),]
meta1 <- metaprop(q2b.dedication.OftenAlways,q2b.Respondents,studlab = Code, data=NHS.detailed,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion often/always dedication")
(meta1$I2*100)
meta1
positive.deviants <- 0
negative.deviants <- 0
for(i in 1:length(meta1$TE)){
  if (meta1$lower[i]>inv.logit(meta1$TE.random)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");positive.deviants <- positive.deviants + 1}
  if (meta1$upper[i]<inv.logit(meta1$TE.random)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");negative.deviants <- negative.deviants + 1}
}
(positive.deviants)
(negative.deviants)
forest(meta1, xlim = c(0,1)) # Size: 1000 x 4000
(minvalue <- paste(round(min(inv.logit(meta1$TE))*100),"%",sep=""))
(maxvalue <- paste(round(max(inv.logit(meta1$TE))*100),"%",sep=""))
mtext(paste("Range:", minvalue,"to",maxvalue,sep=" "),side=1, line=2)

# Absorption
NHS.detailed <- NHS.detailed[order(NHS.detailed$q2c.absorption.OftenAlways/NHS.detailed$q2c.Respondents),]
meta1 <- metaprop(q2c.absorption.Always,q2c.Respondents,studlab = Code, data=NHS.detailed,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion always absorption")
(meta1$I2*100)
meta1 <- metaprop(q2c.absorption.OftenAlways,q2c.Respondents,studlab = Code, NHS.detailed=data.temp,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion often/always absorption")
(meta1$I2*100)
meta1
positive.deviants <- 0
negative.deviants <- 0
for(i in 1:length(meta1$TE)){
  if (meta1$lower[i]>inv.logit(meta1$TE.random)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");positive.deviants <- positive.deviants + 1}
  if (meta1$upper[i]<inv.logit(meta1$TE.random)){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");negative.deviants <- negative.deviants + 1}
}
(positive.deviants)
(negative.deviants)
forest(meta1, xlim = c(0,1)) # Size: 1000 x 4000
(minvalue <- paste(round(min(inv.logit(meta1$TE))*100),"%",sep=""))
(maxvalue <- paste(round(max(inv.logit(meta1$TE))*100),"%",sep=""))
mtext(paste("Range:", minvalue,"to",maxvalue,sep=" "),side=1, line=2)

