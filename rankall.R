rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  states <- names(table(data$State))
  hospitals <- matrix(ncol=2)
  for (i in 1:length(states)) {
    state = states[i]
    stateData <- data[data$State == state,]
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
    hospital <- stateData[order(death,stateData$Hospital.Name,na.last=NA),2]
    if(num=="best") {
      num=1
    } else if (num=="worst") {
      num=length(hospital)
    }
    hospitals<- rbind(hospitals,c(hospital[num],state))
  }
  hospitals <- hospitals[-1,]
  colnames(x=hospitals) <- c("hospital", "state")
  rownames(hospitals) <- states
  return(as.data.frame(hospitals))
}