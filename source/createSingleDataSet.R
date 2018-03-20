library(dplyr)

 
  setwd("C:\\Users\\admin\\Desktop\\R\\My NMAP Project\\Nmap 9-11pm")
  
  dfTest <- readLines("0520.txt")
  dfTest <- as.data.frame(dfTest)
  
  # first time used this pipe infix operator to clear all the empty rows
  dfTest <- dfTest %>% filter(!(dfTest == ""))
  
  # convert all the rows into character format
  dfTest <- sapply(dfTest, function(x) { as.character(x)})
  
  str(dfTest)
  summary(dfTest)
  
  # splitting string using white space (regular expression is given inside)
  strsplit(dfTest[1], split = "\\s+")[[1]]
  
  
  
  # creating an empty data frame, type of column should be specified
  x <- data.frame(xolo = character(), platinum = integer(), stringsAsFactors = FALSE)
  
  # way to add new rows
  x[1, ] <- c("LOL", 4)
  
  
  # extract date and day
  date <- strsplit(dfTest[1], split = "\\s+")[[1]][8]
  date <- as.Date(date)
  day <- weekdays(date) # inbuilt function
  day <- factor(day)
  
  # this time variable will be used to write csv files for 9pm and 11pm
  time <- strsplit(dfTest[1], split = "\\s+")[[1]][9] %>% substring(1, 2) %>% as.integer()
  # for time values like 00.15 hrs (they belong in the 11pm category)
  if(time < 5) {
    time <- time + 24
  }
  
  
  # extract date from the entire date format; DO IT LATER
  # date <- factor(substring(date, 9, 10))
  
  # create the output data frame
  op <- data.frame(IpAddress = c(1:254), Date = rep(date, 254), Day = rep(day, 254), Status = rep("Free", 254), Latency = rep("null", 254), stringsAsFactors = FALSE)
  
  # going to run a for loop for the repetitive sets of 3 lines; total hosts active are one more than this number (i.e. Me!)
  loopTimes <- (nrow(dfTest) - 4)/3
  
  # loop
  for(i in c(1:loopTimes)) {
    # try to write this using pipe infix operator; actually pipe is almost same as without pipe in this case (see, so many parenthesis used here also)
    # ip <- ((dfTest[(3*i - 1)] %>% strsplit(split = "\\s+"))[[1]][5] %>% strsplit(split = "[.]"))[[1]][4]
    ip <- strsplit((strsplit(dfTest[(3*i - 1)], split = "\\s+")[[1]][5]), split = "[.]")[[1]][4]
    ip <- as.integer(ip)
    
    laten <- strsplit(dfTest[3*i], split = "\\s+")[[1]][4]
    laten <- substring(laten, 2, nchar(laten))      # nchar() used for length of string
    op$Latency[op$IpAddress == ip] <- laten
    
    mac <- strsplit(dfTest[(3*i + 1)], split = "\\s+")[[1]][3]
    op$Status[op$IpAddress == ip] <- mac
  }
  
  # my IP
  mip <- strsplit((strsplit(dfTest[(nrow(dfTest) - 2)], split = "\\s+")[[1]][5]), split = "[.]")[[1]][4]
  mip <- as.integer(mip)
  op$Status[op$IpAddress == mip] <- "VIP"
  
  setwd("C:\\Users\\admin\\Desktop\\R\\My NMAP Project\\Datasets")
  
  write.csv(op, "b28.csv", row.names = FALSE)
