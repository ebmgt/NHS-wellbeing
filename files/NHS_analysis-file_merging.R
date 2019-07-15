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

source('NHS_analysis-file_merging-functions.R')

# Set your index year
year.index <- as.numeric("2012")

year.label <- "index" # "index" "before" "after"
year.before  <- year.index - 1
year.after <- year.index + 1

# Get SHMI data
# Index year
data      <- NULL
data.temp <- NULL
year <- year.index
filename = paste(year, "/SHMI data at trust level - mortality - ", year ,".csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
(nrow(data.temp))
names(data.temp)[1] <- "Code"
data.temp$Year.index <- year
# Summary hospital mortality index (SHMI)
#  Standardized Mortality Ratio (SMR)
data.temp$SHMI <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$SHMI)))))
data.temp <- rename(data.temp, c("SHMI" = paste("SHMI.", year.label  , sep="")))
myvars <- c("Code","Year.index", paste("SHMI.", year.label  , sep=""))
data.temp <- data.temp[myvars]
(nrow(data.temp))
#CHoose whether merging or starting
#data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code")
data <- data.temp
(nrow(data))
head(data)
# Previous year
year <- year.before
year.label <- "before" # "index" "before" "after"
data.temp <- NULL
filename = paste(year, "/SHMI data at trust level - mortality - ", year ,".csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
(nrow(data.temp))
names(data.temp)[1] <- "Code"
# Summary hospital mortality index (SHMI)
#  Standardized Mortality Ratio (SMR)
data.temp$SHMI <-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data.temp$SHMI)))))
data.temp <- rename(data.temp, c("SHMI" = paste("SHMI.", year.label , sep="")))
# Limit columns
myvars <- c("Code",paste("SHMI.", year.label , sep=""))
data.temp <- data.temp[myvars]
(nrow(data.temp))
(head(data.temp))
#CHOOSE whether merging or starting
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code")
#data <- data.temp
(nrow(data))
head(data)

# Engagement
# Note: UWES used a 7 point (0 to 6) scale. Also, responses were specific frequencies
# q2a. I look forward to going to work (vigor)
# q2b. I am enthusiastic about my job (dedication)
# q2c. Time passes quickly when I am working (absorption)
# Index year
year <- year.index
year.label <- "index" # "index" "before" "after"
data.temp <- NULL
filename = paste(year, "/ST", substr(year, 3, 4), " - job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
data.temp = data.temp[data.temp$Code != "",]
(nrow(data.temp))
head(data.temp)
# Get Observations count
data.temp$Survey.observations <- Survey.observations(data.temp)
data.temp <- rename(data.temp, c("Survey.observations" = paste("Survey", year.label ,"observations", sep=".")))
# Get engagement
data.temp$Engagement <- Survey.engagement(data.temp)
data.temp <- rename(data.temp, c("Engagement" = paste("Engagement", year.label , sep=".")))
# Limit columns
myvars <- c("Code", paste("Survey", year.label , "observations", sep="."),paste("Engagement", year.label , sep="."))
data.temp <- data.temp[myvars]
head(data.temp)
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code")
(nrow(data))
head(data)
# Year before
(year <- year.before)
year.label <- "before" # "index" "before" "after"
data.temp <- NULL
if (year.before != "2011"){
  data.temp <- NULL
  filename = paste(year, "/ST", substr(year, 3, 4), " - job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
  data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
  data.temp = data.temp[data.temp$Code != "",]
  (nrow(data.temp))
  head(data.temp)
  # Get Observations count
  data.temp$Survey.observations <- Survey.observations(data.temp)
  data.temp <- rename(data.temp, c("Survey.observations" = paste("Survey", year.label ,"observations", sep=".")))
  # Get engagement
  data.temp$Engagement <- Survey.engagement(data.temp)
  data.temp <- rename(data.temp, c("Engagement" = paste("Engagement", year.label , sep=".")))
  # Limit columns
  myvars <- c("Code", paste("Survey", year.label , "observations", sep="."),paste("Engagement", year.label , sep="."))
  data.temp <- data.temp[myvars]
  head(data.temp)
  data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code")
}
if (year.before == "2011"){
  # NO engagement data for 2011
  data$Survey.before.observations <- NA
  data$Survey.before.observations <- as.numeric(data$Survey.before.observations)
  data$Engagement.before <- NA
  data$Engagement.before <- as.numeric(data$Engagement.before)
}
(nrow(data))
head(data)
# Year after
(year <- year.after)
year.label <- "after" # "index" "before" "after"
data.temp <- NULL
filename = paste(year, "/ST", substr(year, 3, 4), " - job1_sheet1-non-specialtyAcuteTrusts.csv",sep="")
data.temp<- read.csv(filename, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
data.temp = data.temp[data.temp$Code != "",]
(nrow(data.temp))
head(data.temp)
# Get Observations count
data.temp$Survey.observations <- Survey.observations(data.temp)
data.temp <- rename(data.temp, c("Survey.observations" = paste("Survey", year.label ,"observations", sep=".")))
# Get engagement
data.temp$Engagement <- Survey.engagement(data.temp)
data.temp <- rename(data.temp, c("Engagement" = paste("Engagement", year.label , sep=".")))
# Limit columns
myvars <- c("Code", paste("Survey", year.label , "observations", sep="."),paste("Engagement", year.label , sep="."))
data.temp <- data.temp[myvars]
head(data.temp)
data <- merge(data, data.temp,  by.x = "Code",  by.y = "Code")
(nrow(data))
head(data)

# Write this year's files
head(assign(paste("data",year.index,sep=""),data))
(filename = paste("csv/", year.index, ".NHS.csv",sep=""))
write.csv(data, file = filename, quote = TRUE, eol = "\n", na = "NA", row.names = FALSE)
filename = paste("rda/", year.index, ".NHS.rda",sep="")
save(data, file = filename)

#After each file is made, merge all files:
dataNHS <- rbind(data2017,data2016,data2015,data2014,data2013,data2012)
(filename = paste("csv/all.years.NHS.csv",sep=""))
write.csv(dataNHS, file = filename, quote = TRUE, eol = "\n", na = "NA", row.names = FALSE)
(filename = paste("rda/all.years.NHS.rda",sep=""))
save(dataNHS, file = filename)
(nrow(dataNHS))
head(dataNHS)
