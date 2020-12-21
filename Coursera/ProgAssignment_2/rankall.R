##Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital rank-
##ing (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
##containing the hospital in each state that has the ranking specified in num. For example the function call
##rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
##are the best in their respective states for 30-day heart attack death rates. The function should return a value
##for every state (some may be NA). The first column in the data frame is named hospital, which contains
##the hospital name, and the second column is named state, which contains the 2-character abbreviation for
##the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
##hospitals when deciding the rankings.
##The rankall function should handle ties in the 30-day mortality rates in the same way
##that the rankhospital function handles ties.
source("utilities.R")

rankall <- function(outcome, num = "best") {
        out <- read_outcome()
        check_outcome(out, outcome)
        if(num == "best") num <- 1
        st <- sort(unique(out$State))
        str_col <- select_col(outcome)
        data <- out[which(out[,str_col] != "Not Available"),c("Hospital.Name","State",str_col)]
        if(nrow(data) == 0) {
                stop("Not Available")
        }
        new_col <- lapply(data[str_col], as.numeric)
        data[,"new_col"] <- new_col
        hospital <- character(0)
        for(state in st) {
                d <- data[which(data$State == state),]
                head(d)
                d <- d[order(d[,"new_col"], d["Hospital.Name"]),]
                d <- d$Hospital.Name
                if(num == "worst") {
                        hospital <- append(hospital, d[length(d)])
                }
                else {
                        hospital <- append(hospital, d[num])
                }
        }
        data.frame(hospital = hospital, state = st)
}