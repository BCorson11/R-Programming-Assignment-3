## Add 'DC' to thel ist of possible state abbreviations
stateabb <- append(state.abb, "DC")

best <- function(state, outcome) { 
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", 
                           colClasses = "character")
  ## Check that state and outcome are valid
  if (!(state %in% stateabb)){
    stop("Invalid state")
  }
  
  if(!(outcome %in% c("Heart Attack", "Heart Failure", "Pneumonia" ))){
       stop("Invalid outcome")
     }
  
  hospitals <- vector()
  hospitals <- vector()
  ## Return hospital name in that state with lowest 30-day death ## rate
  if(outcome == "Heart Attack") {
    min_value <- min(outcome_data[which(outcome_data$State == state),11], 
                     na.rm = TRUE)
    hospitals <- outcome_data[which(outcome_data$State == state,
                                    outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min_value),2]
    hospitals_alp <- sort(hospitals)
    return(hospitals_alp[1])
    } else if(outcome == "Heart Failure") {
    min_value <- min(outcome_data[which(outcome_data$State == state),17], 
                     na.rm = TRUE)
    hospitals <- outcome_data[which(outcome_data$State == state,
                                    outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min_value),2]
    hospitals_alp <- sort(hospitals)
    return(hospitals_alp[1])
    } else if(outcome == "Pneumonia") {
    min_value <- min(outcome_data[which(outcome_data$State == state),23], 
                     na.rm = TRUE)
    hospitals <- outcome_data[which(outcome_data$State == state,
                                    outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min_value),2]
    hospitals_alp <- sort(hospitals)
    return(hospitals_alp[1])
  }

}


##    return(outcome_data[which(outcome_data$State == state,
##outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min_value),2])

##hospitals <- vector()

#for(state in outcome_data$State){
#  for(min_value in outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack){
#    print(outcome_data$Hospital.Name)
#  }