##best function take two arguments: the 2-character abbreviated name of a state and anoutcome name. 
#The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the best 
#(i.e. lowest) 30-day mortality for the specified outcomein that state.  
#The hospital name is the name provided in the Hospital.Namevariable.  The outcomes canbe one of “heart attack”, “heart failure”, or “pneumonia”.  
# Hospitals that do not have data on a particularoutcome should be excluded from the set of hospitals when deciding the rankings.
#If there is a tie for the best hospital for a given outcome, then the hospital names shouldbe sorted in alphabetical order and the first 
#hospital in that set should be chosen (i.e.  if hospitals “b”, “c”,and “f” are tied for best, then hospital “b” should be returned).
source("utilities.R")

best <- function(state, outcome) {
        out <- read_outcome()
        check_state(out, state)
        check_outcome(out, outcome)
        str_col <- select_col(outcome)
        data <- out[which(out[,str_col] != "Not Available" & out$State == state),c("Hospital.Name",str_col)]
        if(nrow(data) == 0) {
                stop("Not Available")
        }
        data <- data[which(as.numeric(data[,str_col]) == min(as.numeric(data[,str_col]))),]
        data <- sort(data$Hospital.Name)
        data[1]
}