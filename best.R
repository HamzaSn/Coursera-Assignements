setwd('C:/Users/Hamza/Desktop/Study/BioStatistics/Coursera/Assignments and quiz/week4 Assignment')
hospital <- read.csv(file = "hospital-data.csv")
outcome <- read.csv(file = "outcome-of-care-measures.csv")

#----------------------------------------------------------------------------------------------------------

Bdata <- outcome[,c(2,7,11,17,23)]
colnames(Bdata) <-  c("Hospital.Name" , "State" , "Heart.Attack" , "Heart.Failure" , "Pneumonia")
Bdata$`Heart.Attack` <- as.numeric(Bdata$`Heart.Attack`)
Bdata$`Heart.Failure` <- as.numeric(Bdata$`Heart.Failure`)
Bdata$Pneumonia <- as.numeric(Bdata$Pneumonia)
Bdata$State <- factor(Bdata$State)

#str(Bdata)------------------------------------------------------------------------------------------------

best <- function(state ,outcome){
        
        D <- NA
        
        filter <- Bdata$State == state
        
        if(!any(state == levels(Bdata$State))) stop("invalid state")
        
        if(outcome == "heart attack") { D <- Bdata[filter,c(1,2,3)] }
        
        else if(outcome == "heart failure") { D <- Bdata[filter,c(1,2,4)] }
        
        else if(outcome == "pneumonia") {
                 D <- Bdata[filter,c(1,2,5)]
        } else {
                
             stop( "invalid outcome")
        } 
        
       index <- which.min(D[,3])
       
       D[index , 1]
}
  



   

        
        