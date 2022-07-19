onenatcourtID <- function(court, data){
  input <- court 
  x <- grep(court, data$Courts, fixed = TRUE)
  if(length(x) == 0 & grepl("\\(", court)){
    x <- grep(gsub("\\W*\\(.*$", "", court), data$Courts)
    if(length(x) > 0){
      court <- gsub("\\W*\\(.*$", "", court)
    }
  }
  
  # Improvise ID code if court is not found, but 
  if(length(x) == 0){
    location_words <- c(" d[ieu] ", " of ", "gericht ", ", ", " te ", " afdeling ", " division ", " sad ",
                        " u ", " i "," d'", " Landes ")
    location <- gsub(",.*$", "", gsub(paste0("^.*?", location_words, collapse="|"), "", court))
    if(location %in% data$court_location){
      base <- gsub(paste0(location, ".*$"), "", court)
      if(paste0(base, location) %in% data$Courts){
        return(data$courtID[which(data$Courts == paste0(base, location))])
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
