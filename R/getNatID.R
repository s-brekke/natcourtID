getNatID <- function(court, data=NA, country=NA){
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
    if(country %in% data$States){
    data <- data[which(data$States == country),]
    }
  }
  
  if(grepl("\\*[[:upper:]]\\d+\\*", court)){
    court <- gsub("^\\W*|\\W*$", "", unlist(strsplit(court, "\\*[[:upper:]]\\d+\\*")))
    court <- court[which(court != "")]
  }
  output <- lapply(court, function(y) onenatcourtID(y, data))
  return(unlist(output))
}
