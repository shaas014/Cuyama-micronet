## load libraries
library(tidyr)
library(stringr)

## load functions
Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}


## aquire all CSV files
occurdat<-list.files("data\\loggers.csv\\2017.2\\",pattern=".csv$",full=T)

## drop files with errors
occurdat <- occurdat[c(1:2,4:13,15:34)]


all.logger.data <- data.frame()
for(i in 1:length(occurdat)){
  ## load data, skip first row
  temp <- read.csv(occurdat[i], sep=",", skip=1, header=T, comment.char="", check.names = FALSE)
  temp <- temp[,2:ncol(temp)] ## columns for date, temperature, and RH only
  
  ## convert data from wide to long. Create a column with all the logger information
  data.long <- gather(temp, logger, measurement, 2:ncol(temp))
  names(data.long)[1] <- "Date"
  
  t.str <- strptime(data.long[,"Date"], "%m/%d/%Y %H:%M")
  data.long [,"year"] <- as.numeric(format(t.str, "%Y")) ## separate date
  data.long [,"month"] <- as.numeric(format(t.str, "%m")) ## separate date
  data.long [,"days"] <- as.numeric(format(t.str, "%d")) ## separate date
  data.long [,"hours"] <- as.numeric(format(t.str, "%H")) ##specify hour logged
  
  ## extract the logger information
  logger.codes <- str_extract_all(data.long$logger,"[0-9]+")
  logger.codes <- data.frame(matrix(unlist(logger.codes), nrow=length(data.long$logger), byrow=T),stringsAsFactors=FALSE)
  
  for(j in 1:length(data.long$logger)){
    logger.codes[j,"type"] <- Clean_String(data.long$logger[j])[1]
  }
  colnames(logger.codes) <- c("logger.ID","sensor.ID","Type sensor")
  logger.codes[,"measurement"] <- data.long[,"measurement"]
  
  logger.codes <- cbind(data.long[,c("year","month","days","hours")],logger.codes)
  
  all.logger.data <- rbind(all.logger.data,logger.codes)
  print(i)
}
write.csv(all.logger.data,"data//Oct2017_microdata.csv")


## combine with other master file

april2017 <- read.csv("data//April2017_microdata.csv") %>% select(-X)
oct2017 <- read.csv("data//Oct2017_microdata.csv") %>% select(-X)

all.logger.data <- rbind(april2017,oct2017)


write.csv(all.logger.data, "data//All2017_microdata.csv.csv")




