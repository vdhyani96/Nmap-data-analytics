## APPROACH OF IMPUTATION IS CHANGED. WILL NOW USE MODE INSTEAD OF ANY PREDICTION (LIKE MICE).
## THIS IS SENSIBLE IMPUTATION.

# install.packages("mice")
# install.packages("ggplot2")
library(ggplot2)
# library(mice)
library(dplyr)

setwd("C:\\Users\\admin\\Desktop\\R\\My NMAP Project\\Datasets") # could use single forward slashes instead 

nineDf <- read.csv("a1.csv")

# will think if to let Latency be numeric or just a factor after appending more dataframes into one
# and checking how many levels I get. If they're too many, then it better be numeric.

# this code below combines all the 9pm datasets together
for(i in c(2:23)) {
  
  file <- paste0("a", i, ".csv")
  readFile <- read.csv(file)
  nineDf <- rbind(nineDf, readFile)
  
}

# many other devices other than "VIP" also have 0.00s Latency, just checked
sort(table(nineDf$Latency), decreasing = TRUE)

sort(nineDf$Latency, decreasing = TRUE)

# Ok, now I think I will keep Latency as numeric and not as factor. Even if it gives erratic decimal numbers
# in predictions, we don't have much to worry because it isn't a major component of prediction.
nineDf_Lat <- nineDf
nineDf_Lat$Latency <- as.character(nineDf_Lat$Latency)
nineDf_Lat$Latency <- nineDf_Lat$Latency %>% sapply(FUN = function(x) { substring(x, 1, (nchar(x) - 1))}) %>% as.numeric()
# now replace the NAs with Latency = -1

nineDf_Lat$Latency[which(is.na(nineDf_Lat$Latency))] <- -1

# Don't worry too much about Latency and predictions. It will play no role in building the model and 
# it also won't be a predictor variable; there are only two: Date and Day (and now I realized, even IP is a predictor)
# Moreover, only one thing is to be predicted, i.e. Status, everything else is secondary (i.e. Latency)

# now acting on date to keep just the date and reject everything else

nineDf3 <- nineDf_Lat

# we use "sapply()" because it gives a vector back, not a list, and a vector is what the LHS needs!
nineDf3$DateOnly <- nineDf3$Date %>% sapply(FUN = function(x) { substring(x, 9, 10)})

str(nineDf3)
summary(nineDf3)



########################################
# now can start mice operations
# after completing, write into the same folder with others

# first appending the missing day; 26 May is missing day
mis <- data.frame(IpAddress = c(1:254), Date = rep("2017-05-26", 254), Day = rep("Friday", 254), Status = NA, Latency = rep(-1, 254), DateOnly = rep("26", 254))

misNineDf <- rbind(nineDf3, mis)
# the total dataframe containing NA values obtained!!

str(misNineDf)

# columns to be converted to factors for mice to be successfully applied
# Done this too in the new approach
convert2Fact <- c('IpAddress', 'DateOnly')

# now we use "lapply()" because we want a list to be returned to the LHS that contains a list of columns
misNineDf[convert2Fact] <- misNineDf[convert2Fact] %>% lapply(function(x) as.factor(x))

################################ LETS BYPASS THIS ########################################

##################### not good ################################
mice_fun <- mice(misNineDf, method = 'rf')

mice_res <- complete(mice_fun)
######################################################

############test(after not good)##################
nineDf3$IpAddress[nineDf3$Status == '18:D6:C7:12:31:16']
table(mice_res$Status[5843:6096])

# good way to count a specific "character" in a column
sum(mice_res$Status[5843:6096] == 'Free')

##############################################################################################

# Now tryna find mode for observations grouped by IP and Day
# define a mode function

modeFinder <- function(par) {
  #print("gg", dim(par))
  modeTable <- par %>% table() %>% sort(decreasing = TRUE) %>% data.frame()
  modeVal <- modeTable[1, 1] %>% as.character()      # originally: modeTable$Var1[1]
  #print(modeVal)
  return(modeVal)
  #return(par) # this is working perfectly, so it means the parameter was reading perfectly.
}

# Now use this mode function inside summarize
# this will find out the Status that are more frequently found on a given IP and Day 
# in other words, calculating mode on data grouped by Ipaddress and Day
# this mode will be used to fill the NAs for the missing date; I chose the mode while grouping by
# both IP and Day just so that the imputed values don't get too inconsistent with the same row groups (or 
# those which share the same day with the missing date.

modeStatus <- misNineDf[1:5842, ] %>%
              group_by(IpAddress, Day) %>% 
              summarize(mode_Status = modeFinder(Status))


###########
# the above didn't work; so check what's wrong >>> SEEMS TO WORK NOW (UPDATE - 18/03/18)
# the first step is the first step inside the Mode function

#################### TESTED AND RESOLVED MODE FUNCTION ###################
# This below is the contents of the function in individual statements
temp <- data.frame(sort(table(nineDf3$Status), decreasing = TRUE))
# and below is the second step
temp2 <- as.character(temp$Var1[1])
# works fine here!
#########################################################################

# finally, once more... ; WORKING NOW!!
temp2 <- modeFinder(nineDf3$Status)
# didn't work; something is wrong with my function definition (gave empty character);; NOW working.... 
temp3 <- c(rep("m", 3), rep("g", 2))
gg <- modeFinder(temp3)
########## STOP TESTING NOW ############

# Now impute NAs in Status

# a vector is being transferred to the LHS
misNineDf$Status[5843:6096] <- modeStatus$mode_Status[modeStatus$Day == "Friday"]

# done imputing, but there are some errors. See by running below command
misNineDf$Status[5843:6096] %>% table() %>% sort(decreasing = TRUE)

# some Status like "18:3D:A2:EA:14:74" have entered into several IPs (3), which is exactly what I feared
# but rejected it as unlikely; turns out, there can be same Mac addresses in different IPs, Just saw
# and still astounded. Commented on a Quora answer and got insights. So, will choose to ignore the inconsistency.

# Now I can think of two ways out of this and both of them involves taking a U-turn from what I was achieving
# to do; 
# 1. Simply copy-paste the 26 April data to this 26 May
# 2. Find mode again but this time while grouping by IP only and not Day; since days are only maximum
#    of 3 in 22 days total, finding out Mode is not very helpful (mode of only 3 entities), especially when 
#    3 distinct Status are all present once in the same group of IP and Day. However, this different 
#    mode finding strategy is still not fool-proof, but the error as above is definitely less likely here. 
#    Let's try now.  OR LET's GO WITH THE PREVIOUS RESULT!!! SKIP BELOW MODE TAKING AGAIN.

# TAKING NEW MODE!!! (UPDATE - NOT ANYMORE)

modeStatus2 <- misNineDf[1:5842, ] %>%
              group_by(IpAddress) %>% 
              summarize(mode_Status = modeFinder(Status))

# Not good this way too, it gives very few Status about 16 only, including VIP, Router and Network Address.
# Ok no problem. Let's proceed this way. At least there are no duplicates here. Just keep in mind, our choices
# at this stage is going to impact our predictions and observations in the later stages. This change here
# shouldn't be very disruptive to the common pattern that should be coming out. 

# imputing correctly this time >> NOT REQUIRED NOW
# misNineDf$Status[5843:6096] <- modeStatus2$mode_Status

# Let's do the same for the mis data.frame which we will write to CSV later...
# mis$Status <- modeStatus2$mode_Status
mis$Status <- modeStatus$mode_Status[modeStatus$Day == "Friday"]

# Let's now work some more for Latency. 
# Good! I was idiotically finding mode here too, mean or median would be definitely better here!

medianLatency <- misNineDf[1:5842, ] %>%
                group_by(Status) %>% 
                summarize(median_Latency = median(Latency))

# Now imputing for Latency NAs
# first try for mis data frame

mis <- inner_join(x = mis, y = medianLatency) # a new column for latency is created, replace the old one with this

mis$Latency <- mis$median_Latency # replaced!

# now delete the last column
mis <- mis[, c(1:6)]
# may now rbind this to the actual previous dataset... first make the columns compatible
str(mis)
str(nineDf3)
mis$Status <- as.factor(mis$Status)
nineDf4 <- nineDf3
nineDf4$DateOnly <- as.factor(nineDf4$DateOnly)
# now rowbind both

nineDf4 <- rbind(nineDf4, mis)

# DONE!!!!
# Now, first write mis to a CSV after proper formatting like the other CSVs

mis2 <- mis
# below code will append "s" with all the Latency values
mis2$Latency <- mis2$Latency %>% as.character %>% sapply(FUN = function(x) paste0(x, "s"))
# now make all the -1s Latency = null
mis2$Latency[mis2$Status == "Free"] <- "null"
# finally, remove the "DateOnly" column
mis2 <- mis2[, c(1:5)]
str(mis2)

# now write to file
getwd()
write.csv(mis2, "missed0526_a24.csv", row.names = FALSE)
write.csv(mis2, "a24.csv", row.names = FALSE)

##### OK NOW THINKING OF NOT INCLUDING THAT SPECIFIC DATE INTO MY DATASET
##### ALTHOUGH IMPUTING A FEW NA'S SHOULDN'T CAUSE MUCH DISRUPTION, HERE IT COULD BE A PROBLEM
##### BECAUSE THE CATEGORIES ARE TOO MANY (1 - 254 IP'S). SO FOR EACH CATEGORY, THERE ARE NOT MANY DATA POINTS

########## WELL, LET'S MOVE ALONG WITH THIS ADDITION AND HOPE WE DON'T REGRET
# FIRST PRINT THIS ENTIRE DATASET (WHILE MAKING MODELS AND ELSE, SHUFFLING THE ROWS MAY BE HELPFUL)
# AND THEN SAME FOR THE 11PM DATA

write.csv(nineDf4, "ninePmDataset.csv", row.names = FALSE)

# Now, create elevenPmDataset.csv using another R script


