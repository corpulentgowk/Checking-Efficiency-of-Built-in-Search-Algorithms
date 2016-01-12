#Step 1: Generate uniformly distributed matrix 
#containing values from 0-10 (inclusive)
numRows <- 5;
numCols <- 5;
valBound <- 5;

treatment1 <- round(matrix(runif(numRows*numCols, min = 0, max = 10), ncol=numCols)) #right hand matrix
treatment2 <- round(matrix(runif(numRows*numCols, min = 0, max = 10), ncol=numCols)) #left hand matrix (goes with our analogy)

#Setup variables for storing treatment data
treatment1Results <- c();
treatment2Results <- c();
allData <- c();
treatmentOrder <- c();

#Set a minimum amount of trials for each treatment
minTries <- 25;

#Create two different options for finding all of the values 
#in a matrix that is greater than 5
subjectFunc0 <- function(treat){
  start <- Sys.time()
  vals <- c()
  for(i in 1:numRows){
    for(j in 1:numCols){
      if(treat[i,j] > valBound){
        vals <- c(vals,treat[i,j])
      }
    }
  }
  end <-Sys.time()
  return(end - start) 
}

subjectFunc1 <- function(treat){
  start <- Sys.time()
  vals <- c()
  for(i in 1:numCols){
    for(j in 1:numRows){
      if(treat[j,i] > valBound){
        vals <- c(vals,treat[i,j])
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
  
  #Alternation variable is used to switch the order of the operand
  #in order to help remove bias.
  alternation = length(allData) %% 2
  
  if (treatmentOp == 0) {
    #Run treatment1 Matrix on Functions
    if(alternation == 0) {
      results0 <- subjectFunc0(treatment1)
      results1 <- subjectFunc1(treatment1)
    }
    else {
      results1 <- subjectFunc1(treatment1)
      results0 <- subjectFunc0(treatment1)
    }
    
    if(results0 <= results1){
      allData <-c(allData, 1) 
      treatment1Results <-c(treatment1Results,1) #keeps track of whether row-wise was faster or not on treatment1
    }
    else{
      allData <-c(allData, 0) 
      treatment1Results <-c(treatment1Results,0) #keeps track of whether column-wise was faster or not on treatment1
    }
  }
  else {
    #Run treatment1 Matrix on Functions
    if(alternation == 0) {
      results2 <- subjectFunc0(treatment2)
      results3 <- subjectFunc1(treatment2)
    }
    else {
      results3 <- subjectFunc1(treatment2)
      results2 <- subjectFunc0(treatment2)
    }
    if(results2 <= results3) {
      allData <-c(allData, 1)
      treatment2Results <-c(treatment2Results,1) #keeps track of whether row-wise was faster or not on treatment2
    }
    else {
      allData <-c(allData, 0)
      treatment2Results <-c(treatment2Results,0) #keeps track of whether column-wise was faster or not on treatment2
    }
  }
  treatmentOrder <- c(treatmentOrder, treatmentOp) #Keeps track of which hand was used to shoot the ball
}

#Output the data to a CSV
output.data <- data.frame(treatmentOrder, allData)
names(output.data)[1] <- "Treatment Order"
names(output.data)[2] <- "Time to Complete Operation"
write.csv(output.data, file = "QMECHW1.csv")

