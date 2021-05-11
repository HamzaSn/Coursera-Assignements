



corr <- function(directory ,  threshold = 500 , id = 1:332 ){
        fileName <- NA
        completeCases <- NA 
        
        for(i in id) {
                path <- paste(directory,"/",formatC(i , width = 3 , flag = "0"),".csv",sep = "")
                data <- read.csv(path)
                fileName[i] = formatC(i , width = 3 , flag = "0")
                completeCases[i] <- sum(complete.cases(data) )
                
                
        }
        
        fileName <- fileName[!is.na(fileName)]
        completeCases <- completeCases[!is.na(completeCases)]
        
        result <- data.frame(fileName,completeCases)
        correlation <- NA
        fileName2 <- NA
        for(j in id ){
                
                path <- paste(directory,"/",formatC(j , width = 3 , flag = "0"),".csv",sep = "")
                data2 <- read.csv(path)
                
                if(result$completeCases[j] > threshold) { 
                        correlation[j] = cor(x= data2$sulfate , y = data2$nitrate , use = "complete.obs")
                        fileName2[j]= formatC(j , width = 3 , flag = "0")
                        
                        
                } else {
                        correlation[j]=NA
                        fileName2[j]=NA
                }
                
                
        } 
        
        cor <- correlation[!is.na(correlation)]
        ID <- fileName2[!is.na(fileName2)]
        data.frame(cor,ID)
}

#test

  
cr <- corr(directory = getwd(), threshold = 2000)                
n <- length(cr)                
cr <- corr(directory = getwd(), threshold = 1000)                
cr <- sort(cr$cor)
print(c(n, round(cr, 4)))


