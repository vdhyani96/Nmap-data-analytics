library(dplyr)


modeFinder <- function(par) {
  #print("gg", dim(par))
  modeTable <- par %>% table() %>% sort(decreasing = TRUE) %>% data.frame()
  modeVal <- modeTable[1, 1] %>% as.character()
  #print(modeVal)
  return(modeVal)
  #return(par) # this is working perfectly, so it means the parameter was reading perfectly.
}

temp3 <- c(rep("m", 3), rep("g", 2))
gg <- modeFinder(temp3)
