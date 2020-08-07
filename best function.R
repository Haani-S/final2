#this is my function 
best <- function(state, outcome) {
  ## Read outcome data
  data=read.csv("outcome-of-care-measures.csv",na.strings='Not Available',stringsAsFactor=FALSE)
  d=data[,c(2,7,11,17,23)]
  colnames(d)=c("Hospital","State","heart attack","heart failure","pneumonia" )
  ## Check that state and outcome are valid
  if(!state %in% d[,"State"]) {
      stop('invalid state')
  } else if(!outcome %in% c("heart attack","heart failure","pneumonia")) {
      stop('invalid outcome')
  } else { 
    si=d %>% filter(State== state)
    haani=si[,c(1,2,grep(outcome,colnames(si)))]
    min_data=min(haani[,outcome],na.rm=TRUE)
    result=haani[,"Hospital"][which(haani[,outcome]==min_data)]
    output=result[order(result)]
  }
return(output)  
}
  
