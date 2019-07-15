Survey.observations <- function(x){
	x$q2a.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q2a.Respondents)))))
	x$q2b.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q2b.Respondents)))))
	x$q2c.Respondents <- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(x$q2c.Respondents)))))
	x$Survey.observations <- round((x$q2a.Respondents+x$q2b.Respondents+x$q2c.Respondents)/3,0)
	return(x$Survey.observations)
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

# x <- 	data.temp
# Survey.observations (x)
