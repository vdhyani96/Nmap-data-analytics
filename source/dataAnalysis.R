library(dplyr)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/admin/Desktop/R/My NMAP Project/Datasets")

# read both datasets - 9pm and 11pm
elevenDf <- read.csv("elevenPmDataset.csv")
nineDf <- read.csv("ninePmDataset.csv")
combi <- rbind(nineDf, elevenDf)

# Remember, the number of days in each dataset:
# nineDf or 9 pm data = 24 days
# elevenDf or 11 pm data = 42 days
# combi = 66 days!


# Can directly go to FreeCounter function below and skip the following... (if in a hurry)

# Now do analysis for all three of these cases



# 1. How many Free IPs are available on average on each weekday?

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





# 2. Which IPs are found to be mostly "Free" and which ones are found to be mostly "active"? Can give some
# idea about the prediction as well which I'm going to do in the end.

# Collecting a count of days for which the IPs were Free and active. 
# First do for Free and later, can do in parallel for "active" >> Well, "active" is just the 
# complement of Free. Why do it separately? 


# below just to check if group_by works with integer columns as well (IpAddress) -- It does!
# temp <- nineDf
# temp$IpAddress <- as.factor(temp$IpAddress)
# temp2 <- temp[temp$Status == "Free", ] %>% group_by(IpAddress) %>% summarize(count = n())

# So, below code does the task!
IpFreeCount_nine <- nineDf[nineDf$Status == "Free", ] %>% group_by(IpAddress) %>% summarize(count = n())
# INSIGHT - Interestingly, IpAddress = 1 has Status = "Free" in one case. I tried to narrow it down to 
# find that it was on 7th April!

sort(IpFreeCount_nine$count, decreasing = TRUE)
# from this result, it seems like showing which IPs are free in most cases isn't SUPER important, because many
# IPs have common values for count. The table can be referred to in order to get more info into that.
# But, I can take a look at more insightful patterns.



# writing a function to get the IpFreeCount for all the datasets while also including the IPs having count = 0

## IPFREECOUNTER function ####
IpFreeCounter <- function(df) {
  tempCount <- df[df$Status == "Free", ] %>% group_by(IpAddress) %>% summarize(daysFree = n())
  temp <- data.frame(IpAddress = c(1:254))
  temp <- left_join(temp, tempCount)
  temp$daysFree[is.na(temp$daysFree)] <- 0
  return(temp)
}
#####


# Use function here
IpFreeCount_nine <- IpFreeCounter(nineDf)
IpFreeCount_eleven <- IpFreeCounter(elevenDf)
IpFreeCount_combi <- IpFreeCounter(combi)
# INSIGHT - Another interesting one. IpAddress = 251 is always active, apart from 254 and 1 (at least for 11pm
# data). Let's check if there is always a single device or many devices.
unique(combi$Status[combi$IpAddress == 251])
# Bingo - 3C:1E:04:24:82:0B always stays online on this IP Address, ALL THE DAYS, ALL THE TIME (9 AND 11PM)


ggplot(IpFreeCount_nine, aes(x = daysFree)) +
        geom_density(stat = "density")




ggplot(IpFreeCount_nine, aes(x = IpAddress, y = daysFree)) +
        geom_histogram(stat = "identity") +
        geom_smooth()

# This shows some noticeable dip at the beginning, middle and the end, shows that IPs lying at those portions
# of this spectrum are less found to be "Free" and so, mostly "active".
# But not very remarkable, and hence can ignore this effect as it's not much consistent.


# Can isolate the numbers which are mostly found active and mostly found Free. Can use a threshold to filter
# out such IpAddresses on both sides. And create heatmaps for those filtered.


# Find a way to visualize the findings! and erase this afterwards! 
# A heat Map can be great! LIke a chart of all IPs -- not sure now



ggplot(IpFreeCount_nine, aes(x = daysFree)) +
        geom_histogram() 


########################################################################
### SKIP THE BELOW STEPS, not very important and relevant analysis.
# Let's make the below plots
# for 9pm
plotn1 <- ggplot(IpFreeCount_nine, aes(x = daysFree)) +
            geom_density(stat = "density")

plotn2 <- ggplot(IpFreeCount_nine, aes(x = "Days as Free", y = daysFree)) +
            geom_boxplot()

grid.arrange(plotn1, plotn2, nrow = 1, ncol = 2)

# now for 11pm
plote1 <- ggplot(IpFreeCount_eleven, aes(x = daysFree)) +
  geom_density(stat = "density")

plote2 <- ggplot(IpFreeCount_eleven, aes(x = "Days as Free", y = daysFree)) +
  geom_boxplot()

grid.arrange(plote1, plote2, nrow = 1, ncol = 2)
# INSIGHT - Very Gaussian-like. 
##############################################################################################

# Remark - It would be great if I were able to create a histogram with x = IpAddress and y = daysFree
# with a larger binwidth so that I could study the pattern on groups of IPs. Is there any special meaning
# behind why some IPs are more popular than the rest?

# ordering the IPs according to their free days count
arngd_nine <- arrange(IpFreeCount_nine, desc(daysFree))
arngd_eleven <- arrange(IpFreeCount_eleven, desc(daysFree))
arngd_combi <- arrange(IpFreeCount_combi, desc(daysFree))
# IP = 226 was always FREE!!

# Display top 10 and bottom 10 for all 3
# more likely to be free
head(arngd_nine, 10)
head(arngd_eleven, 10)
head(arngd_combi, 10)

# more likely to be occupied
tail(arngd_nine, 10)
tail(arngd_eleven, 10)
tail(arngd_combi, 10)

# IP = 251 also seems to be occupied by some "permanent" device, like in the closed computer room on 
# the 1st floor. 







# 3. Most active MAC Addresses or Status, apart from Free?

table(nineDf$Status)
# instead of simply printing table(), I need something more sophisticated because I know that there are 
# cases where one MAC Address or Status appears in mutliple IPs! Need to take count of them for 1 day = 1 only

# below command serves my purpose, will check it too after that
table((nineDf %>% group_by(Date) %>% distinct(Status))$Status)
# distinct of dplyr performs my operation only within the scope defined by group_by

# check using below way
table((nineDf %>% group_by(Date) %>% distinct(Status))$Status) == table(nineDf$Status)


# Now, find the most active MACs and the least active MACs
sort(table((nineDf %>% group_by(Date) %>% distinct(Status))$Status), decreasing = TRUE)
# Obviously, I'm one of the most active guys, all 24 days for 9pm! This is also because I was active
# on some IP at that time that's why I was even able to collect this data through NMAP! So, this pattern
# is expected for 11pm data as well.
# Moreover, a lot of MACs are seen only once for all the 24 days. 
# Also, I'm guessing the other guys who are with me online all the time must be the router, don't know
# about the others, gotta have to check the IPs of all most active guys.

sort(table((nineDf %>% group_by(Date) %>% distinct(Status))$Status), decreasing = TRUE)
sort(table((elevenDf %>% group_by(Date) %>% distinct(Status))$Status), decreasing = TRUE)
sort(table((combi %>% group_by(Date) %>% distinct(Status))$Status), decreasing = TRUE)

# (nineDf %>% group_by(Date) %>% distinct(Status))$Status %>% table() %>% sort(decreasing = TRUE)
# This one is better too, but let's forsake it and adopt the older ones for historical reasons

# Check the IPs of most active guys:
# a. 9 pm
# 00:1A:E3:28:80:00, 3C:1E:04:24:82:0B, 30:B5:C2:3D:C7:F2, VIP
table(nineDf$IpAddress[nineDf$Status == "00:1A:E3:28:80:00"])
table(nineDf$IpAddress[nineDf$Status == "3C:1E:04:24:82:0B"])
table(nineDf$IpAddress[nineDf$Status == "30:B5:C2:3D:C7:F2"])
table(nineDf$IpAddress[nineDf$Status == "VIP"])
# I changed my IPs, but mostly stayed on 161 :) Range = 134 - 182

# Hmm, the other 3 guys seem to be "permanent" devices. Let's research about other "human" guys.
# 00:71:CC:00:AA:B3, 18:D6:C7:12:31:16, 90:FD:61:EA:0B:76 (active for 21/24 days)
table(nineDf$IpAddress[nineDf$Status == "00:71:CC:00:AA:B3"])
# this guy is a man of culture, changes a lot of IPs, mostly stick with 184(7), a true human! Stays in the
# 150 - 194 range
table(nineDf$IpAddress[nineDf$Status == "18:D6:C7:12:31:16"])
# this guys also flits like a butterfly, but more so than the last one. Range is 110 - 211. Mostly = 176 (5)

table(nineDf$IpAddress[nineDf$Status == "90:FD:61:EA:0B:76"])
# he seems to be more consistent. Range = 126 - 171. Mostly = 171 (16 times)


# b. 11pm
# Run this first
sort(table((elevenDf %>% group_by(Date) %>% distinct(Status))$Status), decreasing = TRUE)
# Now, choose 3 "human" guys ;)
# VIP, A0:48:1C:12:E3:D6(36), 14:2D:27:D4:73:95(34), 70:18:8B:B9:0E:79(33)
table(elevenDf$IpAddress[elevenDf$Status == "VIP"])
# I began to stick mostly with 114. Range = 114 - 182
table(elevenDf$IpAddress[elevenDf$Status == "A0:48:1C:12:E3:D6"])
# Almost never stays at one place. Very huge range = 24 - 196. Mostly = 139 (7)
table(elevenDf$IpAddress[elevenDf$Status == "14:2D:27:D4:73:95"])
# Small range = 209-231. Mostly = 209(26). Looks like never haves a conflict with anyone. Has a good, secure base.
table(elevenDf$IpAddress[elevenDf$Status == "70:18:8B:B9:0E:79"])
# big range = 24 - 252. Mostly = 159(9)


# c. combi (for overall score)
sort(table((combi %>% group_by(Date) %>% distinct(Status))$Status))
# removed "decreasing = TRUE" from above
# most active MACs (guys, not "permanent" devices) here = VIP (66 obviously), A0:48:1C:12:E3:D6 (53),
# 14:2D:27:D4:73:95 (53), 90:FD:61:EA:0B:76 (51) (our 70:18... lost the race, probably wasn't active at 9pm
# in the earlier days)
table(combi$IpAddress[combi$Status == "VIP"])
table(combi$IpAddress[combi$Status == "A0:48:1C:12:E3:D6"])
table(combi$IpAddress[combi$Status == "14:2D:27:D4:73:95"])
table(combi$IpAddress[combi$Status == "90:FD:61:EA:0B:76"])

# INSIGHT - From the above exercise (for all the 9pm, 11pm and combi), we learn that different MACs (guys)
# have different strategy for staying active the maximum number of times and winning their place. 
# Some continuously use hit and trial over a big range of IPs to get success, while others (including me), 
# remember a small memorable list of IPs that they visit most frequently and have, kind of, ownership to. 
# My memorable list = (114, 125, 134, 161, 182)







# I think I can wrap up my analysis part here and start the predictive modeling part next.
# I'm already in a crunch of time. There is more scope in this but whatever I've spent on this analysis 
# so far has been truly worth it and is definitely good enough for now. Really good analysis and 
# insights have been produced. 

# 23rd March, 2018 12.14am.







