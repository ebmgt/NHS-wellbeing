Survey.observations <- function(x){
  #In 2012 thru 2014, this was q30 from background_sheet9_mean.xls
  #In 2015 on,        this was q1  from ST15 - job1_sheet1-non-specialtyAcuteTrusts.csv
  if (year < 2015){
    x$Survey.observations <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q30.Respondents)))))
    }
  else{
    x$Survey.observations <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q1.Respondents)))))
    }
  return(x$Survey.observations)
  }
Survey.clinical.proportion <- function(x){
  #In 2012 thru 2014, this was q30 from background_sheet9_mean.xls
  #In 2015 on,        this was q1  from ST15 - job1_sheet1-non-specialtyAcuteTrusts.csv
  if (year < 2015){
    x$q30.Frequently  <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q30.Frequently)))))
    x$Survey.clinical.proportion <- x$q30.Frequently/100 # OR x$q1.frequently * x$q1a.Respondents
    }
  else{
    x$q1.Frequently <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q1.Frequently)))))
    x$Survey.clinical.proportion <- x$q1.Frequently/100 # OR x$q1.frequently * x$q1.Respondents
    }
	return(x$Survey.clinical.proportion)
}
Survey.engagement.observations <- function(x){
  x$q2a.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q2a.Respondents)))))
  x$q2b.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q2b.Respondents)))))
  x$q2c.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q2c.Respondents)))))
  x$Survey.engagement.observations <- round((x$q2a.Respondents+x$q2b.Respondents+x$q2c.Respondents)/3,0)
  return(x$Survey.engagement.observations)
}
Survey.engagement <- function(x){
  # Engagement
  # Note: UsWES used a 7 point (0 to 6) scale. Also, responses were specific frequencies
  # q2a. I look forward to going to work (vigor)
  # q2b. I am enthusiastic about my job (dedication)
  # q2c. Time passes quickly when I am working (absorption)
  x$q2a.vigor      <- (x$q2a.Never + x$q2a.Rarely*2 + x$q2a.Sometimes*3 + x$q2a.Often*4 + x$q2a.Always*5)/100
  x$q2b.dedication <- (x$q2b.Never + x$q2b.Rarely*2 + x$q2b.Sometimes*3 + x$q2b.Often*4 + x$q2b.Always*5)/100
  x$q2c.absorption <- (x$q2c.Never + x$q2c.Rarely*2 + x$q2c.Sometimes*3 + x$q2c.Often*4 + x$q2c.Always*5)/100
  x$Engagement <- (x$q2a.vigor + x$q2b.dedication  + x$q2c.absorption)/3
  return(x$Engagement)
}
Survey.burnout.proportion <- function(x){
  x$Burnout.Yes <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$Burnout.Yes)))))
  x$Burnout.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$Burnout.Respondents)))))
  x$Survey.Burnout.proportion <- x$Burnout.Yes/100
  return(x$Survey.Burnout.proportion) 
}
Survey.burnout.proportionOLD <- function(x){
  # If (year = 2018)
  x$q11c.Yes <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q11c.Yes)))))
  x$q11c.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q11c.Respondents)))))
  x$Survey.Burnout.proportion <- x$q11c.Yes/100
  return(x$Survey.Burnout.proportion) 
}
Survey.burnout.count <- function(x){
  x$Burnout.Yes <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$Burnout.Yes)))))
  x$Burnout.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$Burnout.Respondents)))))
  x$Survey.Burnout.proportion <- x$Burnout.Yes/100
  x$Survey.Burnout.count <- round(x$Survey.Burnout.proportion * x$Burnout.Respondents,0)
  return(x$Survey.Burnout.count) 
}
Survey.burnout.countOLD <- function(x){
  x$q11c.Yes <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q11c.Yes)))))
  x$q11c.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q11c.Respondents)))))
  x$Survey.Burnout.proportion <- x$q11c.Yes/100
  x$Survey.Burnout.count <- round(x$Survey.Burnout.proportion * x$q11c.Respondents,0)
  return(x$Survey.Burnout.count) 
}
Survey.satisfaction <- function(x){
  x$q21c      <- (x$q21c.Strongly.disagree + x$q21c.Disagree*2 + x$q21c.Neither.agree.nor.disagree*3 + x$q21c.Agree*4 + x$q21c.Strongly.agree*5)/100
  return(x$q21c) 
}
# x <- 	data.temp
# Survey.observations (x)
