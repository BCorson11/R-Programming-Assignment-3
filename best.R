## Add 'DC' to thel ist of possible state abbreviations
stateabb <- append(state.abb, "DC")

best <- function(state, outcome) { 
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
  
  ## Return hospital name in that state with lowest 30-day death ## rate
  if(outcome == "Heart Failure") {
    Hospitals.Ranked <- outcome_data_state[order(outcome_data_state[,17],outcome_data_state[,2]),]
    Hospital.Name <- Hospitals.Ranked[1,2]
    Hospital.Name
   
    } else if(outcome == "Heart Attack") {
      Hospitals.Ranked <- outcome_data_state[order(outcome_data_state[,11],outcome_data_state[,2]),]
      Hospital.Name <- Hospitals.Ranked[1,2]
      Hospital.Name
    
    } else if(outcome == "Pneumonia") {
      Hospitals.Ranked <- outcome_data_state[order(outcome_data_state[,23],outcome_data_state[,2]),]
      Hospital.Name <- Hospitals.Ranked[1,2]
      Hospital.Name
  }

}