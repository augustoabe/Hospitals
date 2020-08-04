rankhospital <- function( state, outcome, num ){
        data <- read.csv("outcome-of-care-measures.csv")
        data <- split(data, data$State)
        
        valid_state <- FALSE
        i <- 0
        #validate state
        while( i < 54 ){
                i <- i + 1
                if( data[[i]][,"State"] == state ){
                        valid_state = TRUE
                        break
                }
        }
        
        if( !valid_state ){
          stop( "invalid state" )
        }
        #validate outcome
        if( outcome == "heart attack" ){
                outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        }else if( outcome == "heart failure" ){
                outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        }else if( outcome == "pneumonia" ){
                outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"  
        }else{
                stop( "invalid outcome" )
        }
        
        hospitals <- data[[i]]
        hospitals[,outcome] <- as.numeric(hospitals[,outcome])
        hospitals <- hospitals[order(hospitals[,outcome], 
                                     hospitals[,"Hospital.Name"],
                                     decreasing = c(FALSE, FALSE)),]
        qtdd_hospitals <- nrow( hospitals )
        
        #eliminate NAs
        nas <- c()
        while( is.na(hospitals[qtdd_hospitals, outcome]) ){
                nas <- c( nas, qtdd_hospitals )
                qtdd_hospitals <- qtdd_hospitals - 1
        }
        hospitals <- hospitals[-nas,]
        
        #return value
        if( num != "best" & num != "worst" & num > qtdd_hospitals ){
          NA
        }else{
          if( num == "best" ){
                  hospitals[1,"Hospital.Name"]
          }else if( num == "worst" ){
                  hospitals[qtdd_hospitals,"Hospital.Name"]
          }else{
                  hospitals[num,"Hospital.Name"]
          }
        }
        
}
