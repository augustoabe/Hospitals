rankall <- function( outcome, num = "best" ){
        data <- read.csv("outcome-of-care-measures.csv")
        data <- split(data, data$State)
        
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
        
        list_hospitals <- c()
        list_state <- c()
        i <- 1
        num1 <- 0
        while( i < 55 ){
                hospitals <- data[[i]]
                
                hospitals[,outcome] <- as.numeric(hospitals[,outcome])
                hospitals <- hospitals[order(hospitals[,outcome], 
                                             hospitals[,"Hospital.Name"],
                                             decreasing = c(FALSE, FALSE)),]
                qtdd_hospitals <- nrow( hospitals )
                
                while(is.na(hospitals[qtdd_hospitals,outcome])){
                        qtdd_hospitals <- qtdd_hospitals - 1
                }
                
                if( num == "best" ){
                        num1 <- 1
                }else if( num == "worst" ){
                        num1 <- qtdd_hospitals
                }else{
                        num1 <- num
                }
                list_state <- c(list_state, hospitals[1,"State"])
                list_hospitals <- c(list_hospitals, hospitals[num1,"Hospital.Name"])
                i <- i + 1
          
          
        }
        
        data.frame(hopital = list_hospitals, state = list_state)
}
