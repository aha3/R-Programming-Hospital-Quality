best <- function(state, outcome) { 
  ## Read outcome data 
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid 
  if(!any(state == outcomeData$State)){
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure",
                          "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  outcomeSubset <- subset(outcomeData, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }  
  
  ## rate 
  minimum_row <- which(as.numeric(outcomeSubset[ ,colnum]) == 
                         min(as.numeric(outcomeSubset[ ,colnum]), na.rm = TRUE))
  hospitals <- outcomeSubset[minimum_row,2]
  hospitals <- sort(hospitals)
  return(hospitals[1])
}
