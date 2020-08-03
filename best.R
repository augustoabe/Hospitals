best <- function(state, outcome){
        data <- read.csv("outcome-of-care-measures.csv")
        states <- data[,"State"]
        listStates <- split( data, states )
        
        i <- 0
        valid_state <- FALSE
        #discover if state exist and maintain the position in i
        while( i < 54 ){
                i <- i + 1
                ##print(stt)
                if( listStates[[i]][,"State"] == state ){
                        valid_state <- TRUE
                        break
                }
        }
        
        if( !valid_state ){
                stop( "invalid state" )
        }
        
        index <- 0
        #dicover if output is valid and assing index
        if( outcome == "heart attack" ){
                outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        }else if( outcome == "heart failure" ){
                outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        }else if( outcome == "pneumonia" ){
                outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"  
        }else{
                stop( "invelid outcome" )
        }
      
        minMortality <- 1000000
        minHospital <- character()
        index_name <- 0
        
        # calculate minMortality and minHospital
        for( value in x[[i]][,outcome] ){
                v <- as.numeric(value)
                minMortality <- as.numeric(minMortality)
                index_name <- index_name + 1
                if(is.na(v)){
                        next
                }
                if( v < minMortality ){
                        minMortality <- v
                        minHospital <- listStates[[i]][index_name,"Hospital.Name"]
                }
        }
        minHospital
}