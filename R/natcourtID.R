# Work in progress

natcourtID <- function(court, data=NA, country=NA){
  if(colnames(data)[1] != "courtID"){
    data <- generateID(data)
  }
  if(is.na(country)){
    if(grepl(" - ", court)){
      courtcountry <- unlist(strsplit(court, " - "))
      court <- courtcountry[1]
      country <- courtcountry[2]
    }
  }
  
  if(!is.na(country)){
    data <- data[which(data$States == country),]
  }
  x <- grep(court, data$Courts)
  if(length(x) > 0){
    if(length(x) == 1){
      return(data$courtID[x])
    }
  }
}
