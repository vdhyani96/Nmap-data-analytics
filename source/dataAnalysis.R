library(dplyr)
library(ggplot2)

setwd("C:/Users/admin/Desktop/R/My NMAP Project/Datasets")

# read both datasets - 9pm and 11pm
elevenDf <- read.csv("elevenPmDataset.csv")
nineDf <- read.csv("ninePmDataset.csv")
combi <- rbind(nineDf, elevenDf)


# Can directly go to FreeCounter function below and skip the following... (if in a hurry)

# Now do analysis for all three of these cases
# 1. How many Free IPs are available on an average on each weekday?

countFree_nine <- nineDf[nineDf$Status == "Free", ] %>% group_by(Day) %>% summarize(count = n())


# Now, calculate the average count of Free IPs.
# First I find the counts of each weekday and then divide them with 254 to get the number of each weekday
countDay_nine <- nineDf %>% group_by(Day) %>% summarize(count = n())
countDay_nine$count <- countDay_nine$count/254
countFree_nine$count <- countFree_nine$count/countDay_nine$count # averaging
countFree_nine$count <- countFree_nine$count %>% sapply(FUN = function(x) floor(x))
# manually changing the order of weekdays (Monday to Sunday)
countFree_nine <- countFree_nine[c(2, 6, 7, 5, 1, 3, 4), ]
# this is not enough to change the order for ggplot, so try to change the levels of factors, as done in function.

# Print the findings
countFree_nine

# Great! Now, visualizing this by the help of a bar plot
ggplot(countFree_nine, aes(x = Day, y = count, fill = Day)) +
        geom_bar(stat = "identity", alpha = 0.5) +
        ylim(0, 254) +
        geom_text(aes(label = count), vjust = 0, size = 5)
# would be nice if bars were from the top too representing the count of active IPs. (complement of "Free")


# INSIGHTS - Tuesday is the day which has, on average, least no. of Free IPs (154). The closest one has 7 Free IPs
# more, which is the day Sunday. On the other hand, Saturday is the day with the maximum no. of Free IPs (182).
# This is for 9pm data only


## FREECOUNTER function ####
# CREATE A FUNCTION TO DO THE ABOVE STEPS FOR THE REST OF THE DATASETS TOO -- (OPPORTUNITY FOR REUSABILITY)
freeCounter <- function(df) {
  countFree <- df[df$Status == "Free", ] %>% group_by(Day) %>% summarize(count = n())
  countDay <- df %>% group_by(Day) %>% summarize(count = n())
  countDay$count <- countDay$count/254
  countFree$count <- countFree$count/countDay$count
  countFree$count <- countFree$count %>% sapply(FUN = function(x) floor(x))
  countFree <- countFree[c(2, 6, 7, 5, 1, 3, 4), ]
  countFree$Day <- factor(countFree$Day, levels = countFree$Day)
  return(countFree)
}
#####

# Now use the above function
countFree_nine <- freeCounter(nineDf)
countFree_eleven <- freeCounter(elevenDf)
countFree_combi <- freeCounter(combi)

# INSIGHTS - Saturday seems to be consistently scoring higher in all the three findings. Moreover, all the 
# days in the second half of the week (Thu, Fri, Sat) have more number of Free IPs than the first half (Mon, 
# Tue, Wed). The first half has less Free IPs. This pattern is captured in both 9pm and 11pm data too! 
# 2. Also, we will obviously see overall less number of free IPs in case of 11pm data because this is
#  kind of peak time!

# Print visualizations below.

# Also removing ylim() below so as to present the differences between individual bars properly. 
# 9pm
ggplot(countFree_nine, aes(x = Day, y = count, fill = Day)) + 
        geom_bar(stat = "identity", alpha = 0.5) +
        geom_text(aes(label = count), vjust = 0, size = 5)

# 11pm
ggplot(countFree_eleven, aes(x = Day, y = count, fill = Day)) + 
        geom_bar(stat = "identity", alpha = 0.5) + 
        geom_text(aes(label = count), vjust = 0, size = 5)

# combi
ggplot(countFree_combi, aes(x = Day, y = count, fill = Day)) + 
        geom_bar(stat = "identity", alpha = 0.5) + 
        geom_text(aes(label = count), vjust = 0, size = 5)


# INSIGHTS - The pattern is "fairly" consistent from the above visualizations. First half has less Free 
# IPs while the second half has more Free IPs, no matter what the time. 













