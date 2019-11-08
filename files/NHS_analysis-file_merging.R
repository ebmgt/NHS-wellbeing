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
library(compareDF)
library(Rcmdr)
#---------------------------------

source('NHS_analysis-file_merging-functions.R')

# Comments
# 08/17/2019
# Merge statements need all = TRUE so that rows are not lost

# Set your index year
year.index <- as.numeric("2018") # DO NOT NEED 2011 OR 2018 HERE

year.label <- "index" # "index" "before" "after"
year.before  <- year.index - 1
year.after <- year.index + 1

# Get SHMI data
# Index year
data      <- NULL
data.temp <- NULL
year <- year.index
year.label <- "index" # "index" "before" "after"
filename = paste(year, "/SHMI data at trust level - mortality - ", year ,".csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
(nrow(data.temp))
names(data.temp)[1] <- "Code"
data.temp$Year.index <- year
# Summary hospital mortality index (SHMI)
#  Standardized Mortality Ratio (SMR)
data.temp$SHMI.index <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$SHMI)))))
myvars <- c("Code","Year.index", "SHMI.index")
data.temp <- data.temp[myvars]
(nrow(data.temp))
#Choose whether merging or starting
#data <- merge(data, data.temp, all = TRUE, by.x = "Code",  by.y = "Code")
data <- data.temp
(nrow(data))
head(data)
# Year before
year <- year.before
year.label <- "before" # "index" "before" "after"
(nrow(data))
head(data)
data.temp <- NULL
filename = paste(year, "/SHMI data at trust level - mortality - ", year ,".csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
(nrow(data.temp))
names(data.temp)[1] <- "Code"
# Summary hospital mortality index (SHMI)
#  Standardized Mortality Ratio (SMR)
data.temp$SHMI.before <-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$SHMI)))))
# Limit columns
myvars <- c("Code","SHMI.before")
data.temp <- data.temp[myvars]
(nrow(data.temp))
(head(data.temp))
#CHOOSE whether merging or starting
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code", all = TRUE)
#data <- data.temp
(nrow(data))
head(data)
# Year after
if (year != "2019"){
  year <- year.after
  year.label <- "after" # "index" "before" "after"
  (nrow(data))
  head(data)
  data.temp <- NULL
  filename = paste(year, "/SHMI data at trust level - mortality - ", year ,".csv",sep="")
  data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  (nrow(data.temp))
  names(data.temp)[1] <- "Code"
  # Summary hospital mortality index (SHMI)
  #  Standardized Mortality Ratio (SMR)
  data.temp$SHMI.after <-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$SHMI)))))
  # Limit columns
  myvars <- c("Code","SHMI.after")
  data.temp <- data.temp[myvars]
  (nrow(data.temp))
  (head(data.temp))
  #CHOOSE whether merging or starting
  data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code", all = TRUE)
  #data <- data.temp
}
if (year == "2019"){
  # NO SHMI data for 2019
  data$SHMI.after <- as.integer(NA)
}
(nrow(data))
head(data)

###ST1x - job1_sheet1-non-specialtyAcuteTrusts############################
# Observations count and clinical proportion
#In 2012 thru 2014, this was q30 from background_sheet9_mean.xls
#In 2015 on, this was q1 from ST15 - job1_sheet1-non-specialtyAcuteTrusts.csv
# Index year
year <- year.index
year.label <- "index" # "index" "before" "after"
data.temp <- NULL
if (year < 2015){
  filename = paste(year, "/ST", substr(year, 3, 4), "_background_sheet9_mean_9-1-non-specialtyAcuteTrusts.csv",sep="")
}else{
  filename = paste(year, "/ST", substr(year, 3, 4), " - job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
  }
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
data.temp = data.temp[data.temp$Code != "",]
(nrow(data.temp))
head(data.temp)
# Get Observations count
data.temp$Survey.observations.index <- Survey.observations(data.temp)
# Get Clinical.proportions - New 2019-08-02
data.temp$Survey.clinical.proportion.index <- Survey.clinical.proportion(data.temp)
# Limit columns
myvars <- c("Code", "Survey.observations.index","Survey.clinical.proportion.index")
data.temp <- data.temp[myvars]
head(data.temp)
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code", all = TRUE)
(nrow(data))
head(data)
data$Survey.ClinicalStaffCount.index <- data$Survey.clinical.proportion.index * data$Survey.observations.index


###ST1x - job1_sheet1-non-specialtyAcuteTrusts############################
# Engagement
# Note: UWES used a 7 point (0 to 6) scale. Also, responses were specific frequencies
# q2a. I look forward to going to work (vigor)
# q2b. I am enthusiastic about my job (dedication)
# q2c. Time passes quickly when I am working (absorption)
# Index year
year <- year.index
year.label <- "index" # "index" "before" "after"
data.temp <- NULL
if (year < 2015){
  filename = paste(year, "/ST", substr(year, 3, 4), "_job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
}else{
  filename = paste(year, "/ST", substr(year, 3, 4), " - job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
}
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
data.temp = data.temp[data.temp$Code != "",]
(nrow(data.temp))
head(data.temp)
# Get Engagementobservations count
data.temp$Survey.engagement.observations.index <- Survey.engagement.observations(data.temp)
# Get engagement
data.temp$Engagement.index <- Survey.engagement(data.temp)
# Limit columns
myvars <- c("Code", "Survey.engagement.observations.index", "Engagement.index")
data.temp <- data.temp[myvars]
head(data.temp)
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code", all = TRUE)
(nrow(data))
head(data)
# Year before
(year <- year.before)
year.label <- "before" # "index" "before" "after"
data.temp <- NULL
if (year.before != "2011"){
  data.temp <- NULL
  if (year < 2015){
    filename = paste(year, "/ST", substr(year, 3, 4), "_job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
  }else{
    filename = paste(year, "/ST", substr(year, 3, 4), " - job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
  }
  data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  data.temp = data.temp[data.temp$Code != "",]
  (nrow(data.temp))
  head(data.temp)
  # Get Engagement observations count
  data.temp$Survey.engagement.observations.before <- Survey.engagement.observations(data.temp)
  # Get engagement
  data.temp$Engagement.before <- Survey.engagement(data.temp)
  # Limit columns
  myvars <- c("Code", "Survey.engagement.observations.before","Engagement.before")
  data.temp <- data.temp[myvars]
  head(data.temp)
  data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code", all = TRUE)
}
if (year.before == "2011"){
  # NO engagement data for 2011
  data$Survey.engagement.observations.before <- as.integer(NA)
  data$Engagement.before <- as.numeric(NA)
}
(nrow(data))
head(data)
# Year after
(year <- year.after)
year.label <- "after" # "index" "before" "after"
if (year.after != "2019"){
  data.temp <- NULL
  if (year < 2015){
    filename = paste(year, "/ST", substr(year, 3, 4), "_job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
  }else{
    filename = paste(year, "/ST", substr(year, 3, 4), " - job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
  }  
  data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  data.temp = data.temp[data.temp$Code != "",]
  (nrow(data.temp))
  head(data.temp)
  # Get Engagement observations count
  data.temp$Survey.engagement.observations.after <- Survey.engagement.observations(data.temp)
  # Get engagement
  data.temp$Engagement.after <- Survey.engagement(data.temp)
  # Limit columns
  myvars <- c("Code", "Survey.engagement.observations.after", "Engagement.after")
  data.temp <- data.temp[myvars]
  head(data.temp)
  data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code", all = TRUE)
}
if (year.after == "2019"){
  data$Survey.engagement.observations.after <- as.numeric(NA)
  data$Engagement.after <- as.numeric(NA)
  }
(nrow(data))
head(data)

##SKIP NEXT SESSION (BURNOUT) FOR FIRST STUDY
###ST18 - health_sheet4-non-specialtyAcuteTrusts.csv############################
# Burnout
# Note: Maslach is propietary, so:
# q11c.During the last 12 months have you felt unwell as a result of work related stress?
# q11c.Yes
# Index year
year <- year.index
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
# Year before
(year <- year.before)
year.label <- "before" # "index" "before" "after"
data.temp <- NULL
#...

##SKIP NEXT SESSION (Satisfaction) FOR FIRST STUDY
###ST18 - ST18 - organisation_sheet8-non-specialtyAcuteTrusts-q21.csv############################
# Satisfaction
# q21c.I would recommend my organisation as a place to work?
year <- year.index
year.label <- "index" # "index" "before" "after"
data.temp <- NULL
filename = paste(year, "/ST", substr(year, 3, 4), " - organisation_sheet8-non-specialtyAcuteTrusts-q21.csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
data.temp = data.temp[data.temp$Code != "",]
(nrow(data.temp))
head(data.temp)
data.temp$Satisfaction <- Survey.satisfaction(data.temp)
head(data.temp)
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code", all = TRUE)
(nrow(data))
head(data)

#data$Survey.ClinicalStaffCount.index <- NULL

# Write this year's files
head(assign(paste("data",year.index,sep=""),data))
(filename = paste("csv/", year.index, ".NHS.csv",sep=""))
write.csv(data, file = filename, quote = TRUE, eol = "\n", na = "NA", row.names = FALSE)
filename = paste("rda/", year.index, ".NHS.rda",sep="")
save(data, file = filename)

#After each file is made, merge all files:
dataNHS <- NULL
dataNHS <- rbind(data2018,data2017,data2016,data2015,data2014,data2013,data2012)
(filename = paste("csv/all.years.NHS.csv",sep=""))
write.csv(dataNHS, file = filename, quote = TRUE, eol = "\n", na = "NA", row.names = FALSE)
(filename = paste("rda/all.years.NHS.rda",sep=""))
save(dataNHS, file = filename)
(nrow(dataNHS))
head(dataNHS)

ctable = compare_df(dataNHS.v1, dataNHS, c("Code"), exclude = c("SHMI.after","Survey"))
print(ctable$comparison_df)
ctable$html_output
