rankall <-function(outcome,num = "best"){
  statefile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  Statevector<- sort(unique(statefile[,"State"]))
  output <- data.frame()
  #colnames(output) <- c("Hospital","state")
  #return(Statevector)
  for (state in Statevector){
    state1 <- state
    num1 <- num
    outcome1 <- outcome
    hospital  <- rankhospital1(state1,outcome1,num1)
    rankout1 <- cbind(hospital,state)
    output <-rbind(output,rankout1)
  }
  return(output)
}