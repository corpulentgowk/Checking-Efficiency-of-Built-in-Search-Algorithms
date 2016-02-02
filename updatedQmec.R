#Step 1: Generate uniformly distributed matrix 
#containing values from 0-10 (inclusive)
numRows <- 5;
numCols <- 5;
valBound <- 5;
boundary <- .0000002; #Overall boundary. Needs to be tweaked so we can get that good balance. 
f <- "Screw Git Bruh"
subject <- round(matrix(runif(numRows*numCols, min = 0, max = 10), ncol=numCols)) #Single matrix used through all trials

#Setup variables for storing treatment data
treatment1Results <- c();
treatment2Results <- c();
allData <- c();
treatmentOrder <- c();

#Set a minimum amount of trials for each treatment
minTries <- 25;

#Create two different options for finding all of the values 
#in a matrix that is greater than 5
treatmentFunc1 <- function(subj){
  start <- Sys.time()
  vals <- c()
  for(i in 1:numRows){
    for(j in 1:numCols){
      if(subj[i,j] > valBound){
        vals <- c(vals,subj[i,j])
      }
    }
  }
  end <-Sys.time()
  return(end - start) 
}

treatmentFunc2 <- function(subj){
  start <- Sys.time()
  vals <- c()
  for(i in 1:numCols){
    for(j in 1:numRows){
      if(subj[j,i] > valBound){
        vals <- c(vals,subj[i,j])
      }
    }
  }
  end <-Sys.time()
  return(end - start) 
} 

#Run until there is at least minTries data collected for each treatment
while( length(treatment1Results) < minTries || length(treatment2Results) < minTries ) {
  #Method for choosing random treatment
  treatmentOp <- round(runif(1, min = 0, max = 1))
  
  if (treatmentOp == 0) {
    #Run treatment1 on matrix
      results <- treatmentFunc1(subject)
    
    
      if(results <= boundary){
        allData <-c(allData, 1) 
        treatment1Results <-c(treatment1Results,1) #Row-wise faster than boundary
      }
      else{
        allData <-c(allData, 0) 
        treatment1Results <-c(treatment1Results,0) #Column-wise faster than boundary
      }
  }
  else {
    #Run treatment2 on Matrix 
    results2 <- treatmentFunc2(subject)
    if(results2 <= boundary) {
      allData <-c(allData, 1)
      treatment2Results <-c(treatment2Results,1) #Column-wise is faster than boundary miliseconds
    }
    else {
      allData <-c(allData, 0)
      treatment2Results <-c(treatment2Results,0) #Column-wise is slower than boundary miliseconds
    }
  }
  treatmentOrder <- c(treatmentOrder, treatmentOp) #Keeps track of which hand was used to shoot the ball
}

#Output the data to a CSV
output.data <- data.frame(treatmentOrder, allData)
names(output.data)[1] <- "Treatment Order"
names(output.data)[2] <- "Faster than Boundary"
write.csv(output.data, file = "QMECHW1.csv")

