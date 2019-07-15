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
library(metafor)
library (meta) # metamean
library(boot) #inv.logit
#---------------------------------

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
myvars <- c("Site","year.index","SHMI.index")
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

for(i in 1:nrow(temp.data))
{
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
}
head(data.temp)
# For metamean
data.temp <- data.temp[order(data.temp$q2a.vigor.mean),]
meta1 <- metamean(q2a.Respondents,q2a.vigor.mean,q2a.vigor.sd,studlab = Code, data=data.temp,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of mean vigor")
positive.deviants <- 0
negative.deviants <- 0
for(i in 1:length(meta1$TE)){
  if (meta1$lower[i]>meta1$TE.random){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");positive.deviants <- positive.deviants + 1}
  if (meta1$upper[i]<meta1$TE.random){meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="");negative.deviants <- negative.deviants + 1}
}
(positive.deviants)
(negative.deviants)
forest(meta1, xlim = c(0,5))
(minvalue <- paste(round(min(meta1$TE)*100),"",sep=""))
(maxvalue <- paste(round(max(meta1$TE)*100),"",sep=""))
plot(NULL)
mtext(paste("Range:", minvalue,"to",maxvalue,sep=" "),side=1, line=1)

# For metaprop
data.temp <- data.temp[order(data.temp$q2a.vigor.OftenAlways/data.temp$q2a.Respondents),]
meta1 <- metaprop(q2a.vigor.Always,q2a.Respondents,studlab = Code, data=data.temp,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion always vigor")
meta1 <- metaprop(q2a.vigor.OftenAlways,q2a.Respondents,studlab = Code, data=data.temp,comb.fixed=FALSE,hakn=TRUE, title = "Meta-analysis of proportion often/always vigor")
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

data.temp$q2a.vigor <-  mean(data.temp$q2a.Never + data.temp$q2a.Rarely*2 + data.temp$q2a.Sometimes*3 + data.temp$q2a.Often*4 + data.temp$q2a.Always*5)/100
data.temp$q2b.dedication <- (data.temp$q2b.Never + data.temp$q2b.Rarely*2 + data.temp$q2b.Sometimes*3 + data.temp$q2b.Often*4 + data.temp$q2b.Always*5)/100
data.temp$q2c.absorption <- (data.temp$q2c.Never + data.temp$q2c.Rarely*2 + data.temp$q2c.Sometimes*3 + data.temp$q2c.Often*4 + data.temp$q2c.Always*5)/100
data.temp$Engagement <- (data.temp$q2a.vigor + data.temp$q2b.dedication  + data.temp$q2c.absorption)/3
data.temp$Engagement <- (data.temp$q2a.vigor + data.temp$q2b.dedication  + data.temp$q2c.absorption)/3
myvars <- c("Code", "Observations","Engagement")
data.temp <- data.temp[myvars]
data.temp <- rename(data.temp, c("Observations"= paste("Observations.",year,sep="")))
data.temp <- rename(data.temp, c("Engagement"= paste("Engagement.",year,sep="")))
head(data.temp)
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code")
(nrow(data))
head(data)

