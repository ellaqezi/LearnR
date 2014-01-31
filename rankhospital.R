rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  stateData <- data[data$State == state,]
## Check that state and outcome are valid
if (length(stateData[,1]) > 0) {
  if (outcome == "heart attack") {
    index = 11      
  } else if (outcome == "heart failure") {
    index = 17  
  } else if (outcome == "pneumonia") {
    index = 23     
  } else {
    stop("invalid outcome")
  }
  death <- suppressWarnings(as.numeric(stateData[,index]))
} else {
  stop("invalid state")
}
## Return hospital name in that state with the given rank
## 30-day death rate
hospitals <- stateData[order(rank(death,na.last=T, ties.method="min"), stateData$Hospital.Name),2]
hospitals <- hospitals[!is.na(hospitals)]
if (num=="best") {
  num = 1
} else if (num=="worst") {
  num =length(hospitals)
}
return(hospitals[num])
}