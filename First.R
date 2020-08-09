setwd("specdata")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## obtaining the required files by storing them into mydata variable
  mydata <- list.files(path = directory)[id]
  
  
  ## reading the .csv files of mydata
  readfiles <- lapply(mydata, read.csv)
  
  ## combine all the files into one file so that we can calculate the mean at once
  combine <- do.call(rbind,readfiles)
  print(head(combine))
  ##calculate the mean without the NA values
  mean(combine[,pollutant], na.rm = TRUE)
  
}


pollutantmean(getwd(),2)

pollutantmean(getwd(),3)


complete <- function(directory, id = 1:332) {
  ## obtaining the required files by storing them into mydata variable
  mydata <- list.files(path = directory)[id]
  
  ## create an empty vector
  frows <- c()
  counter <- 1
  
  for(i in mydata) {
    
    ## storing each .csv file without the NA values into fineobject variable
    fineobject <- na.omit(read.csv(i))
    
    ## store the number of the fineobject rows in the empty f(ine)rows vector
    frows[counter] <- nrow(fineobject)
    counter <- counter + 1
  }
  ## creating and printing the f(ine)list
  flist <- data.frame("id" = id, "nobs" = frows)
  print(flist)
}

complete(getwd())

corr<- function(directory, threshold = 0) {
  ## obtaining the required files by storing them into mydata variable
  mydata <- list.files(path = directory)
  
  ## creating an empty numeric vector which will hold the final result
  result <- vector(mode = "numeric", length = 0) 
  
  for(i in 1:332) 
  {
    ## Reading and storing the required files without NAs into goodfile variable
    goodfile <- na.omit(read.csv(mydata[i]))
    if( nrow(goodfile) > threshold ) {
      
      ## calculating the correlation and combine in the result empty vector
      corvector <- cor(goodfile$sulfate, goodfile$nitrate)
      result <- append(result,corvector)
    }
  }
  ## return the result
  result
}

corr(getwd())


pollutantmean(getwd(),3)

cc <- complete(getwd(), c(6, 10, 20, 34, 100, 200, 310))
print


cc <- complete(getwd(), 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete(getwd(), 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr(getwd())                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr(getwd(), 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr(getwd(), 2000)                
n <- length(cr)                
cr <- corr(getwd(), 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
