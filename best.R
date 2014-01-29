best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  stateData <- data[data$State == state,]
  ## Check that state and outcome are valid
  if (length(stateData[,1]) > 0) {
    if (outcome == "heart attack") {
      death <- stateDate[,11]      
    } else if (outcome == "heart failure") {
      death <- stateDate[,17]      
    } else if (outcome == "pneumonia") {
      death <- stateDate[,23]      
    } else {
      return("Error: invalid outcome")
    }
    
  } else {
    return("Error: invalid state")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  y<- stateData
}