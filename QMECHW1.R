#Step 1: Generate uniformly distributed matrix 
#containing values from 0-10 (inclusive)

numRows <- 5;
numCols <- 5;
valBound <- 5;

rightHand <- round(matrix(runif(numRows*numCols, min = 0, max = 10), ncol=numCols)) #right hand matrix
leftHand <- round(matrix(runif(numRows*numCols, min = 0, max = 10), ncol=numCols)) #left hand matrix (goes with our analogy)

#Setup variables for storing treatment data
treatment0 <- c();
treatment1 <- c();
allData <- c();
treatmentOrder <- c();

#Set a minimum amount of trials for each treatment
minTries <- 25;

#Create two different options for finding all of the values 
#in a matrix that is greater than 5
subjectFunc0 <- function(treat){
  start <- Sys.time()
  vals <- c()
  for(i in 1:numCols){
    for(j in 1:numRows){
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
  vals <- treat[treat > valBound]
  end <-Sys.time()
  return(end - start) 
} 


#Run until there is at least minTries data collected for each treatment
while( length(treatment0) < minTries || length(treatment1) < minTries ) {
  #Method for choosing random treatment
  treatmentOp <- round(runif(1, min = 0, max = 1))
  
  if (treatmentOp == 0) {
    treatment1Test <- "OPN"
    #Run RightHand Matrix on Functions
    results0 <- subjectFunc0(rightHand)
    results1 <- subjectFunc1(rightHand)
    if(results0 <= results1){
      allData <-c(allData, 1) #This means that manual search was faster.
      test <- "wtf"
      treatment0 <-c(treatment0,1) #keeps track of whether manual was faster or not on treatment0
    }
    else{
      allData <-c(allData, 0) #This means that the built in function was faster.
      treatment0 <-c(treatment0,0) #keeps track of whether manual was faster or not on treatment0
    }
  }
  else {
    treatment2Test <- "OPN"
    results2 <- subjectFunc0(leftHand)
    results3 <- subjectFunc1(leftHand)
    if(results2 <= results3){
      allData <-c(allData, 1) #This means that manual search was faster.
      treatment1 <-c(treatment1,1) #keeps track of whether manual was faster or not on treatment1
    }
    else{
      allData <-c(allData, 0) #This means that the built in function was faster.
      treatment1 <-c(treatment1,0) #keeps track of whether manual was faster or not on treatment1
    }
  }
  treatmentOrder <- c(treatmentOrder, treatmentOp) #Keeps track of which hand was used to shoot the ball
}

#Output the data to a CSV
output.data <- data.frame(treatmentOrder, allData)
names(output.data)[1] <- "Treatment Order"
names(output.data)[2] <- "Time to Complete Operation"
write.csv(output.data, file = "QMECHW1.csv")

