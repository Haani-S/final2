#rank hospital repo 
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  my_data=read.csv("outcome-of-care-measures.csv",na.strings='Not Available',stringsAsFactors=FALSE)
  d=my_data[,c(2,7,11,17,23)]
  colnames(d)=c("Hospital","State","heart attack","heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% d[,"State"]){
      stop('invalid state')
  } else if (!outcome %in% c("heart attack","heart failure","pneumonia")){
      stop('invalid outcome')
  } else if (is.numeric(num)){
    si=filter(d,State==state)
    data=si[,c(1,2,grep(outcome,colnames(si)))]
    #data=data[order(data$outcome,data$Hospital),]
    ts <- data[order(data[, outcome],data[, "Hospital"]), ]
    output <- ts[, "Hospital"][num]
    return(output)
  ## 30-day death rate
  } else if (!is.numeric(num)){
    if(num=="best"){
      output=best(state,outcome)
  } else if(num=="worst"){
      si=filter(d,State==state)
      data=si[,c(1,2,grep(outcome,colnames(si)))]
      #data=data[order(data$outcome,data$Hospital),]
      ts <- data[order(data[, outcome],data[, "Hospital"],decreasing=TRUE), ]
      output <- ts[, "Hospital"][1]
  } else{
      stop('invalid rank')
  }
return(output)
}
      
}
