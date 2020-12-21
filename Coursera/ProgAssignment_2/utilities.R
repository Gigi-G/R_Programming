read_outcome <- function(...) {
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
}

check_state <- function(outcome, state) {
        st <- unique(outcome$State)
        if(!any(st == state)) {
                stop("invalid state")
        }
}

check_outcome <- function(outcome, out) {
        if(!any(c("heart attack", "heart failure", "pneumonia") == out)) {
                stop("invalid outcome")
        }
}

select_col <- function(out) {
        switch (out,
                "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        )
}