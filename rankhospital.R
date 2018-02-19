### 3 Ranking Hospital by Outcome in a State ###

rankhospital <- function(state, outcome, num = "best") { ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!any(state == outcomeData$State)){
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure",
                          "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  
  ## Return hospital name in that state with the given rank ## 30-day death rate
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
  outcomeSubset[ ,colnum] <- as.numeric(outcomeSubset[ ,colnum])
  outcomeRank <- outcomeSubset[order(outcomeSubset[ ,colnum],outcomeSubset[,2]), ]
  outcomeRank <- outcomeRank[(!is.na(outcomeRank[ ,colnum])),]
  if(num == "best"){
    num <- 1
  }
  else if (num == "worst"){
    num <- nrow(outcomeRank)
  }  
  return(outcomeRank[num,2]) # 2 indicates 2nd column from outcome file, which is hospital name
}