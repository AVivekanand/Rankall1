rankhospital1 <-function(state1,outcome1,num1 = "best"){
  infile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #listoutcome <- list("heart attack","heart failure","pneumonia")
  if (!state1 %in% infile[,7]){
    stop("invalid state")
  }
  if (outcome1 == "heart attack"){
    outcome_column_num <- 11
  }
  else if (outcome1 == "heart failure"){
    outcome_column_num <- 17
  }
  else if (outcome1 == "pneumonia"){
    outcome_column_num <- 23
  }
  else{
    stop("invalid outcome")
  }
  if (!num1 == "best"){
    if(!num1 == "worst") {
      if (!is.numeric(num1)) {
        stop("invalid num")
      }
    }
  } 
  #if (!outcome %in% listoutcome){
  #   stop("invalid outcome")
  # } 
  reqdata <- subset (infile,State == state1,select = c(7,2,outcome_column_num))
  #comdata <- reqdata
  colnames(reqdata) <- c("State","Hospital_Name","Outcome")
  
  reqdata <- transform(reqdata,Outcome = as.numeric(Outcome))
  hospitalname <- (with(reqdata,tapply(Hospital_Name,Outcome,sort)))
  hospitalname <- as.data.frame(hospitalname)
  hospitalname <- unlist(hospitalname)
  
  #rOW_no <- which.min(as.double(data.state[,Outcome]))
  #colnames(hospitalname) <- c("Outcome","name")
  #sort(hospitalname[,1])
  if (num1 == "best"){
    return(hospitalname[[1]])
  }
  if(num1 == "worst") {
    worsthospital <- (tail(hospitalname,n=1))
    return (worsthospital[[1]])
  }
  if (num1 > length (hospitalname)){
    return(NA)  
  }
  #print(hospitalname[[num1]])
  return(hospitalname[[num1]])
}