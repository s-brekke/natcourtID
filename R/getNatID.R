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
  
  if(TRUE %in% grepl("\\*[[:upper:]]\\d+\\*", court)){
    court <- gsub("^\\W*|\\W*$", "", unlist(strsplit(court, "\\*[[:upper:]]\\d+\\*")))
    court <- court[which(court != "")]
  }
  output <- lapply(court, function(y) onenatcourtID(y, data))
  return(unlist(output))
}

# One Natinal Court ID
# not intended for human use, part of getNatID.

onenatcourtID <- function(court, data){
  input <- court 
  x <- grep(court, data$Courts, fixed = TRUE)
  if(length(x) == 0 & grepl("\\(", court)){
    x <- grep(gsub("\\W*\\(.*$", "", court), data$Courts)
    if(length(x) > 0){
      court <- gsub("\\W*\\(.*$", "", court)
    }
  }
  
  # Improve search if a base and a location is found in the name.
  # Improvise ID code ending in 0 if not found.
  if(length(x) == 0){
    location_words <- c(" d[ieu] ", " of ", ", ", " te ", " u ", " i "," d'")
    location_words2 <- c(" gericht ", " afdeling ", " division ", " sad ", " Landes ")
    location <- gsub(",.*$", "", gsub(paste0("^.*?", c(location_words, location_words2), collapse="|"), "", court))
    
    loc <- location
    # Try to translate names of regions
    if(!location %in% data$court_location){
      location2 <- unique(na.omit(data$court_location[grep(paste0(" ", location, " "), paste0("", data$Courts, " "))]))
      if(length(location2) == 1){
        location <- location2
      }
    }
    
    if(location %in% data$court_location){
      base <- gsub(paste0(loc, ".*$"), "", court)
      if(paste0(base, loc) %in% data$Courts){
        return(data$courtID[which(data$Courts == paste0(base, loc))])
      }
      
      # "Hof van beroep Brussel" is sometimes named "Hof van beroep te Brussel"
      base <- gsub(paste0(location_words, "$", collapse="|"), " ", base)
      
      if(paste0(base, loc) %in% data$Courts){
        return(data$courtID[which(data$Courts == paste0(base, loc))])
      }
      
      if(TRUE %in% grepl(base, data$Courts)){
        if(length(unique(data$States[grep(base, data$Courts)])) == 1){
          ID_base <- gsub("[[:upper:]]{3}", "", data$courtID[grep(base, data$Courts)][1])
          ID_location <- gsub("^.*([[:upper:]]{3}).*$", "\\1", data$courtID[which(data$court_location == location)[1]])
          return(paste0(ID_base, ID_location, 0))
        }
      }
    }
    location <- gsub(paste0("^.*?", location_words, collapse="|"), "", court)
  }
  
  if(length(x) == 0 & grepl(", ", court)){
    x <- grep(gsub(", .*$", "", court), data$Courts)
    if(length(x) > 0){
      court <- gsub(", .*$", "", court)
    }
  }
  
  if(length(x) == 0 & grepl(", ", court)){
    x <- grep(gsub("-.*$", "", court), data$Courts)
    if(length(x) > 0){
      court <- gsub("-.*$", "", court)
    }
  }
  
  if(length(x) > 0){
    if(length(unique(data$courtID[x])) == 1){
      return(unique(data$courtID[x]))
    }
    if(length(unique(data$courtID[which(data$Courts == court)])) == 1){
      return(unique(data$courtID[which(data$Courts == court)]))
    }
    
    # Search for a specific branch
    branch_tags <- c(" division", "chamber", "courts")
    if(grepl(paste0("\\(.*", branch_tags, collapse="|"), input, ignore.case = TRUE)){
      branch_tags <- branch_tags[unlist(lapply(branch_tags, function(y) grep(y, input, ignore.case = TRUE)))[1]]
      branch <- gsub("^\\W*", "", gsub(paste0(".*[,\\(-](.*?", branch_tags, ").*$"), "\\1", input, ignore.case = TRUE))
      if(branch %in% data$Branch[x]){
        x <- x[which(data$Branch[x] == branch)]
      } else {
        if(TRUE %in% grepl(branch, data$Branch[x], ignore.case = TRUE)){
          x <- x[grep(branch, data$Branch[x], ignore.case = TRUE)]
        }
      }
    }
    if(length(unique(data$courtID[x])) == 1){
      return(unique(data$courtID[x]))
    }
  }
}
