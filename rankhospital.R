## Add 'DC' to the list of possible state abbreviations
stateabb <- append(state.abb, "DC")

rankhospital <- function(state, outcome, num = "best") { 
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome_data[,11] <- as.numeric(outcome_data[,11])
  outcome_data[,17] <- as.numeric(outcome_data[,17])
  outcome_data[,23] <- as.numeric(outcome_data[,23])
  outcome_data_state <- outcome_data[outcome_data$State == state,]
  
  ## Check that state and outcome are valid
  if (!(state %in% stateabb)){
    stop("Invalid state")
  }
  
  if(!(outcome %in% c("Heart Attack", "Heart Failure", "Pneumonia"))){
    stop("Invalid outcome")
  }
  
  #Assign num an integer value if 'best' is inputted
  if (num == "best"){
    num <- as.integer(1)
  }
  
  ## Return hospital name in that state with the given rank 
  ## 30-day death rate
  if(outcome == "Heart Failure") {
    Hospitals.Ranked <- outcome_data_state[order(outcome_data_state[,17],outcome_data_state[,2]),]
    Hospitals.Ranked.NA <- Hospitals.Ranked[
      !is.na(Hospitals.Ranked$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    if(num == "worst"){
      num <- length(Hospitals.Ranked.NA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    }
    Hospital.Name <- Hospitals.Ranked[num,2]
    Hospital.Name
  } 
  else if(outcome == "Heart Attack") {
    Hospitals.Ranked <- outcome_data_state[order(outcome_data_state[,11],outcome_data_state[,2]),]
    Hospitals.Ranked.NA <- Hospitals.Ranked[
      !is.na(Hospitals.Ranked$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    if(num == "worst"){
      num <- length(Hospitals.Ranked.NA$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    }
    Hospital.Name <- Hospitals.Ranked[num,2]
    Hospital.Name
  } 
  else if(outcome == "Pneumonia") {
    Hospitals.Ranked <- outcome_data_state[order(outcome_data_state[,23],outcome_data_state[,2]),]
    Hospitals.Ranked.NA <- Hospitals.Ranked[
      !is.na(Hospitals.Ranked$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    if(num == "worst"){
      num <- length(Hospitals.Ranked.NA$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    }
    Hospital.Name <- Hospitals.Ranked[num,2]
    Hospital.Name
  } 
}