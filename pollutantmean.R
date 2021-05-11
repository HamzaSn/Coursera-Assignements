
pollutantmean <- function(pollutant , id= 1:332,directory = getwd()){
        
       sums <- NA
       lens <- NA
        for(i in id){ 
                read <- read.csv(paste(formatC(i,width = 3,flag="0"), ".csv",sep = ""))  
                DataNa <- read[pollutant]
                Data <- DataNa[!is.na(DataNa)]
              lens[i] = length(Data)
               sums[i] =  sum(Data)
                
        }
        mean <- sum(sums)/sum(lens)
        mean
}





pollutantmean2 <- function(pollutant , id= 1:332,directory = getwd()){
        
       
        total <- NA
        for(i in id){ 
                read <- read.csv(paste(formatC(i,width = 3,flag="0"), ".csv",sep = ""))  
                DataNa <- read[pollutant]
                Data <- DataNa[!is.na(DataNa)]
               total <- c(total , Data)
                
        }
        mean(total,na.rm = TRUE)
}



#pollutantmean( "sulfate", 1:10)
#
#pollutantmean2( "sulfate", 1:10)



pollutantmean2( "nitrate")












