### 4 Ranking hospitals in all states ###

library(dplyr)
options(warn = -1)

rankall <- function(outcome, num = "best"){
  outcomeDataFrame <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  hospitalRankDataFrame <- data.frame(hospital=character(54), state=character(54)) #indicates the 54 possibilities
  hosp <- character() #creating empty hospital vector 
  state <- character() #creating empty state vector, which will be populated at end
  
  # use split function 
  outcomeByState <- split(outcomeDataFrame, outcomeDataFrame$State)
  
  stateNames = names(outcomeByState)
  # Create outcomes subset of only the data we are interested in - 
  # should be names here, not numbers
  for(i in 1:length(stateNames)){
    
    outcomeSubset <- select(outcomeByState[[stateNames[i]]],2,7,11,17,23)
    
    # rename to make it readable
    outcomeSubset <- rename(outcomeSubset, Hospital = Hospital.Name)
    outcomeSubset <- rename(outcomeSubset, HeartAttack = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    outcomeSubset <- rename(outcomeSubset, HeartFailure = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    outcomeSubset <- rename(outcomeSubset, Pneumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    if(outcome == "heart attack"){      #if heart attack outcome, do following
      outcomeSubset$HeartAttack <- as.numeric(outcomeSubset$HeartAttack) #coerces renamed HeartAttack column of outcomeSubset to numeric values
      NAs <- is.na(outcomeSubset$HeartAttack) #identifies NAs
      outcomeSubset <- outcomeSubset[!NAs,] #selects non-NAs
      outcomeSubsetRanked <- arrange(outcomeSubset,HeartAttack)
      outcomeSubsetRanked <- outcomeSubsetRanked[order(outcomeSubsetRanked$HeartAttack, outcomeSubsetRanked$Hospital), ]
      if(num == 'best'){                                  #if 'best' selected
        hospital <- head(outcomeSubsetRanked$Hospital,1)  #return 1st entry of outcomeSubsetRanked using head function
      }else if(num == 'worst'){                           #if 'worst' selected
        hospital <- tail(outcomeSubsetRanked$Hospital,1)  #returns last entry using tail function (nrow function should also work)                        
      }else{                                              #if a number is selected
        hospital <- outcomeSubsetRanked[num,1]            #returns 1st column (hospital name) of specified number
      }
      
    }else if(outcome == "heart failure"){
      outcomeSubset$HeartFailure <- as.numeric(outcomeSubset$HeartFailure)
      NAs <- is.na(outcomeSubset$HeartFailure)
      outcomeSubset <- outcomeSubset[!NAs,]
      outcomeSubsetRanked <- arrange(outcomeSubset,HeartFailure)
      outcomeSubsetRanked <- outcomeSubsetRanked[order(outcomeSubsetRanked$HeartFailure, outcomeSubsetRanked$Hospital), ]
      if(num == 'best'){
        hospital <- head(outcomeSubsetRanked$Hospital,1)
      }else if(num == 'worst'){
        hospital <- tail(outcomeSubsetRanked$Hospital,1)                        
      }else{
        hospital <- outcomeSubsetRanked[num,1]
      }
    }else if(outcome == "pneumonia"){
      outcomeSubset$Pneumonia <- as.numeric(outcomeSubset$Pneumonia)
      NAs <- is.na(outcomeSubset$Pneumonia)
      outcomeSubset <- outcomeSubset[!NAs,]
      outcomeSubsetRanked <- arrange(outcomeSubset,Pneumonia)
      outcomeSubsetRanked <- outcomeSubsetRanked[order(outcomeSubsetRanked$Pneumonia, outcomeSubsetRanked$Hospital), ]
      if(num == 'best'){
        hospital <- head(outcomeSubsetRanked$Hospital,1)
      }else if(num == 'worst'){
        hospital <- tail(outcomeSubsetRanked$Hospital,1)                        
      }else{
        hospital <- outcomeSubsetRanked[num,1]
      }
    }else{stop("invalid outcome")}    #throw error if outcome is not valid
    
    hosp[i] <- hospital
    state[i] <- outcomeSubsetRanked$State[1]
    
    #Populate the dataframe
    #hospitalRankDataFrame$hospital[i] <- hospital
    #hospitalrankDataFrame$state[i] <- state
  }
  hospitalrankDataFrame <- data.frame(hospital = hosp,state = state)
}
