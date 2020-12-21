##Write a function called rankhospital that takes three arguments:  the 2-character abbreviated name of astate (state), an outcome (outcome), 
##and the ranking of a hospital in that state for that outcome (num).The function reads theoutcome-of-care-measures.csvfile and returns a 
##character vector with the nameof the hospital that has the ranking specified by thenumargument.  
##For example, the callrankhospital("MD", "heart failure", 5) would return a character vector containing the name 
##of the hospital with the 5th lowest 30-day death ratefor heart failure.  
##The num argument can take values “best”, “worst”,  or an integer indicating the ranking(smaller numbers are better).  
##If the number given bynumis larger than the number of hospitals in thatstate, then the function should return NA. 
##Hospitals that do not have data on a particular outcome shouldbe excluded from the set of hospitals when deciding the rankings.
##It may occur that multiple hospitals have the same 30-day mortality rate for a given causeof death.  
##In those cases ties should be broken by using the hospital name.  For example, in Texas (“TX”),the hospitals with lowest 30-day mortality 
##rate for heart failure are shown here
source("best.R")
source("utilities.R")

rankhospital <- function(state, outcome, num = "best") {
        out <- read_outcome()
        check_state(out, state)
        check_outcome(out, outcome)
        if(num == "best" | num == 1) {
                best(state, outcome)
        }
        else {
                str_col <- select_col(outcome)
                data <- out[which(out[,str_col] != "Not Available" & out$State == state),c("Hospital.Name",str_col)]
                if(nrow(data) == 0) {
                        stop("Not Available")
                }
                new_col <- lapply(data[str_col], as.numeric)
                data[,"new_col"] <- new_col
                data <- data[order(data[,"new_col"], data["Hospital.Name"]),]
                data <- data$Hospital.Name
                if(num == "worst") {
                        return(data[length(data)])
                }
                return(data[num])
        }
        
}