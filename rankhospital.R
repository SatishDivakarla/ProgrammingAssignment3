rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_list <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")
  ## Check that state and outcome are valid
  states <- outcome_list$State
  hospital_names <- outcome_list$Hospital.Name
  outcome_names <- c("heart attack","heart failure","pneumonia") 
  if(!any(states == state)){
    stop("invalid state") 
  }
  else if(!any(outcome_names == outcome)){
    stop("invalid outcome")
  }
  else{    
    df <- subset(outcome_list, State == state, select = c("hospital" = 2, "heart attack"= 11, 
                                                          "heart failure" = 17,  "pneumonia" = 23) )
    #print(head(df[,1]))
    df[,2] <- as.numeric(df[,2])
    df[,3] <- as.numeric(df[,3])
    df[4] <- as.numeric(df[,4])
    if(outcome == "heart attack"){
      sorteddf <- df[order(df[,2],df[,1]),]
    } 
    else if(outcome == "heart failure"){
      sorteddf <- df[order(df[,3],df[,1]),]
    }
    else if(outcome=="pneumonia"){
      sorteddf <- df[order(df[,4],df[,1]),]
    }
    
    sorteddf[complete.cases(sorteddf),]
    sorteddf_nona <- sorteddf[complete.cases(sorteddf),]
    numrows <- nrow(sorteddf_nona)
    if(num == "Best"){
      num <- 1
    }
    else if(num == "worst"){
      num <- numrows 
    }
    else if(num > numrows){
      return(NA)
    }
      return(sorteddf_nona[num,1])
    
    
  }
  
  ## Return hospital name in that state with lowest 30-day death
  
  ## rate
}
