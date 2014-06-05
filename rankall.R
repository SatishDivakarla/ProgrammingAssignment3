rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_list <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")
  ## Check that state and outcome are valid
  states <- outcome_list$State
  #hospital_names <- outcome_list$Hospital.Name
  outcome_names <- c("heart attack","heart failure","pneumonia")   

  states <- unique(states)
  states <- sort(states)
  ## For each state, find the hospital of the given rank
  
  hospital <- c()
  state <- c()
  for(newstate in states){
    hospital_name <- rankhospital(newstate, outcome_list,outcome, num)
    hospital <- append(hospital,hospital_name)
    state <- append(state,newstate)
  }
    
  #results <- data.frame(hospital_list,unique_states)
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  results <- data.frame(hospital,state,row.names=state)
  results
}


rankhospital <- function(state, outcome_list, outcome, num = "best") {
  
  df <- subset(outcome_list, State == state, select = c("hospital" = 2, "heart attack"= 11, 
                                                        "heart failure" = 17,  "pneumonia" = 23) )
  df[,2] <- as.numeric(df[,2])
  df[,3] <- as.numeric(df[,3])
  df[4] <- as.numeric(df[,4])
  
  if(outcome == "heart attack"){
    sorteddf <- df[order(df[,2],df[,1]),]
    completesorteddf <- sorteddf[complete.cases(sorteddf[,2]),]
  } 
  else if(outcome == "heart failure"){
    sorteddf <- df[order(df[,3],df[,1]),]
    completesorteddf <- sorteddf[complete.cases(sorteddf[,3]),] 
  }
  else if(outcome=="pneumonia"){
    sorteddf <- df[order(df[,4],df[,1]),]
    completesorteddf <- sorteddf[complete.cases(sorteddf[,4]),]    
  }
  

    
  numrows <- nrow(completesorteddf)
  if(num == "best"){
    num <- 1
  }
  else if(num == "worst"){
    num <- numrows
  }
  else if(num > numrows){
    return(NA)
  }
  return(completesorteddf[num,1])
}
