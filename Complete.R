Complete <- function(directory,id){
        
        fileName <- NA
        completeCases <- NA 
        
        for(i in id) {
                path <- paste(directory,"/",formatC(i , width = 3 , flag = "0"),".csv",sep = "")
                data <- read.csv(path)
                fileName[i] = formatC(i , width = 3 , flag = "0")
                completeCases[i] = sum(complete.cases(data) )
        }
        fileName <- fileName[!is.na(fileName)]
        completeCases <- completeCases[!is.na(completeCases)]

         data.frame(fileName,completeCases)
        
}


#test

cc <- Complete(getwd(), c(6, 10, 20, 34, 100, 200, 310))
print(cc$completeCases)

cc1 <- Complete(getwd(), 54)

  
cr <- corr(directory = getwd(), threshold = 2000)                
n <- length(cr)                
cr <- corr(directory = getwd(), threshold = 1000)                
cr <- sort(cr$cor)
print(c(n, round(cr, 4)))

