# This dataset should be more reliable since maximum number of active Status would be found here

library(dplyr)

setwd("C:/Users/admin/Desktop/R/My NMAP Project/Datasets")

elevenDf <- read.csv("b1.csv")

# reading all the eleven PM files
for(i in c(2:42)) {
  
  file <- paste0("b", i, ".csv")
  readFile <- read.csv(file)
  elevenDf <- rbind(elevenDf, readFile)
  
  # if(i == 26) { # didn't work for some reason; that's why creating new loop below
  #  i <- 28
  # }
  
}

######>>>>> IGNORE THIS PIECE <<<<<<###### (Since now the 11pm files are complete from 1 to 42; check notebook)
for(i in c(28:42)) {
  
  file <- paste0("b", i, ".csv")
  readFile <- read.csv(file)
  elevenDf <- rbind(elevenDf, readFile)

}
#########################################


sort(table(elevenDf$Latency), decreasing = TRUE)

# change Latency to numeric
elevenDf_Lat <- elevenDf
elevenDf_Lat$Latency <- as.character(elevenDf_Lat$Latency)
elevenDf_Lat$Latency <- elevenDf_Lat$Latency %>% sapply(FUN = function(x) { substring(x, 1, (nchar(x) - 1))}) %>% as.numeric()

# replacing the NAs in latency to -1
elevenDf_Lat$Latency[which(is.na(elevenDf_Lat$Latency))] <- -1

## now separate date from the whole date format

elevenDf3 <- elevenDf_Lat

# we use "sapply()" because it gives a vector back, not a list, and a vector is what the LHS needs!
elevenDf3$DateOnly <- elevenDf3$Date %>% sapply(FUN = function(x) { substring(x, 9, 10)})

str(elevenDf3)
summary(elevenDf3)

# houston, we have a problem. It seems that data belonging to some specific dates have appeared twice.
# HOUSTON, CALM DOWN, THE PROBLEM IS NOT MORE :D >> CHECK NOTEBOOK, THE ERRONEOUS DATES ARE CORRECTED IN THE 
# SOURCE FILES.
table(elevenDf3$Date)

# I say we pick the delinquents, delete them and then re-extract them from the txt files, but order of b
# files would need to be changed manually.

# After solving the above, I came across another new revelation. One Mac address on the same day is on multiple
# IPs!!!!! Can't believe this is real!!! (B0:C0:90:52:1E:74) >>> Must be a superhuman.
# On 3rd June, this Mac had occupied 8 IP Addresses!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# https://www.quora.com/Ethernet-What-happens-if-two-devices-connected-to-internet-have-same-MAC-address-Do-they-work-well
# I commented on the first answere there
# And this one:
# https://superuser.com/questions/519409/can-two-devices-with-the-same-mac-address-be-on-the-same-network?newreg=9afcdff5927b496e9082fbe21d9fe710
# I couldn't comment there because I didn't have enough reputation

# Well this is what it is; Can't change the fact that NMAP has given me. Gonna go ahead with this


# write elevenDf3 into CSV dataset
# but first convert DateOnly into a factor

elevenDf3$DateOnly <- as.factor(elevenDf3$DateOnly)

# now write to file
write.csv(elevenDf3, "elevenPmDataset.csv", row.names = FALSE)

# Yay! Finally, I have both the datasets ready!! Data wrangling is over! Now, I can work on them!




