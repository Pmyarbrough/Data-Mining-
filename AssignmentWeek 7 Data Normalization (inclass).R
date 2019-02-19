#11.3
library(tidyverse)
normalize <- function(v, range=1){
  v.norm = (v - min(v,na.rm = TRUE)) / (max(na.omit(v))-min(v,na.rm = TRUE)) * range
  return(v.norm)
}


normalizeDataset <- function (data){
  data.norm <- data
  types <- sapply(data, is.numeric)
  for (i in 1:length(types)) {
    if(types[i]==TRUE){
      data.norm[ ,i] <- normalize(data[ ,i])
    }
  }
  return(data.norm)
}


train.normalize <- normalizeDataset(train)
test.normalize <- normalizeDataset(test)
