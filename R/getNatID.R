getNatID <- function(court, country=NA, flatten = TRUE, data=NA){
  if(class(data) == "logical"){
    data <- natcourts
  }
  if(is.na(country)){
    if(TRUE %in% grepl(" - ", court)){
      courtcountry <- unlist(strsplit(court, " - "))
      court <- courtcountry[1]
      country <- courtcountry[2]
    }
  }
  
  if(TRUE %in% grepl("Benelux", court)){
    country <- NA
  }
  
  if(!is.na(country)){
    if(country %in% data$States){
      data <- data[which(data$States == country),]
      court <- gsub(paste0("\\s*-\\s*", country, "$"), "", court)
    }
  }
  
  # Allow for lists of courts where some contain multiple courts 
  converted <- NULL
  split <- "\\*[[:upper:]]\\d+\\*"
  if(TRUE %in% grepl(split, court)){
    for(x in grep(split, court)){
      court_list <- gsub("^\\W*|\\W*$", "", unlist(strsplit(unlist(court[x]), split)))
      court_list <- court_list[which(court_list != "")]
      court_list <- gsub("\\([^\\)]*$", "", court_list)
      court[[x]] <- list(unlist(lapply(court_list, function(y) onenatcourtID(y, data, country))))
      converted <-c(converted, x)
    }
  }
  
  output <- lapply(court, function(y) onenatcourtID(y, data, country))
  output[converted] <- court[converted]
  output <- lapply(output, unlist)
  
  if(flatten){
    output <- lapply(output, function(y) paste(y, collapse="; "))
    return(unlist(output))
  } else {
    return(output)
  }
}

# One Natinal Court ID
# not intended for human use, part of getNatID.

onenatcourtID <- function(court, data, country){
  
  if(grepl("^\\d[[:upper:]][[:upper:]]\\d", court)){
    return(court)
  }
  location <- NA
  division <- NA
  
  if(is.na(country) & grepl(" - ", court)){
    country <- gsub("^.* - ", "", court)
    if(!country %in% data$States){
      country <- NA
    }
  }
  if(!is.na(country)){
    court <- gsub(paste0(" - ", country, "$"), "", court)
  }
  
  if(paste(country) == "Czechia"){
    country <- "Czech Republic"
  }
  
  
  
  input <- court 
  
  x <- grep(tolower(court), tolower(data$Courts), fixed = TRUE)
  if(length(x) == 0){
    x <- grep(iconv(gsub("\\W", ".", court), from="UTF-8",to="ASCII//TRANSLIT"), 
              iconv(data$Courts,from="UTF-8",to="ASCII//TRANSLIT"))
  }
  if(length(x) == 0){
    optional_fillers <- c(" te ", "/", " am ", " \\(", " in ", " per la ", " della ", " de lo ", "nº \\d+", "-", " sitting ")
    
    x <- grep(tolower(iconv(gsub("\\W", ".", gsub(paste(optional_fillers, collapse="|"), ".", court)), from="UTF-8",to="ASCII//TRANSLIT")), 
              tolower(iconv(gsub(paste(optional_fillers, collapse="|"), ".", data$Courts),from="UTF-8",to="ASCII//TRANSLIT")))
    if(length(x) == 0){
      x <- grep(tolower(iconv(gsub("\\W", ".", gsub(paste(optional_fillers, collapse="|"), ".", 
                                                    gsub("\\).*$|,.*$|\\*[[:upper:]]\\d+\\* ", "", court)
      )), from="UTF-8",to="ASCII//TRANSLIT")), 
      tolower(iconv(gsub(paste(optional_fillers, collapse="|"), ".", data$Courts),from="UTF-8",to="ASCII//TRANSLIT")))
    }
  }
  if(length(x) > 1){
    if(length(which(tolower(data$Courts) == tolower(court))) > 0){
      x <- which(tolower(data$Courts) == tolower(court))
    }
  }
  
  if(length(x) == 0 & grepl("\\(", court)){
    x <- grep(gsub("\\W", ".", gsub("\\W*\\(.*$", "", court)), data$Courts)
    if(length(x) == 0){
      x <- grep(iconv(gsub("\\W", ".", gsub("\\W*\\(.*$", "", court)),from="UTF-8",to="ASCII//TRANSLIT"), 
                iconv(data$Courts,from="UTF-8",to="ASCII//TRANSLIT"))
    }
    if(length(x) > 0){
      court <- gsub("\\W*\\(.*$", "", court)
    }
  }
  
  # Improve search if a base and a location is found in the name.
  # Improvise ID code ending in 0 if not found.
  if(length(x) == 0){
    location_words <- c(" d[ieu] ", " of ", ", ", " te ", " u ", " i "," d'", " des ", " at ")
    location_words2 <- c("gericht ", " afdeling ", " division ", " sad ", " Landes ", " Außenstelle ", " soud [[:lower:]]* ")
    not_location <- c("^[EÉ]tat")
    
    location <- gsub(",.*$", "", gsub(paste0("^.*?", c(location_words, location_words2), collapse="|"), "", court))
    if(grepl("instanc|tribunal|court|[ée]tat|urteil ", location, ignore.case = TRUE)){
      location <- gsub(paste0("^.*?", c(location_words, location_words2), collapse="|"), "", location)
    }
    
    location <- gsub("^[^[:upper:]]*([[:upper:]])", "\\1", location)
    location <- gsub("\\W+\\d+[[:lower:]]*$|\\W+$|\\sI+V?$", "", location)
    
    if(grepl(paste(not_location, collapse="|"), location)){
      location <- NA
    }
    
    if(location == ""){
      location <- NA
    }
    
    loc <- location
    if(paste(location) != court & !is.na(location)){
      # Try to translate names of regions
      if(!location %in% data$court_location){
        location2 <- unique(na.omit(data$court_location[grep(paste0(" ", gsub("\\W", ".", location), " "), paste0(" ", data$Courts, " "))]))
        location2 <- location2[which(!location2 %in% 98:99)]
        if(length(location2) == 1){
          location <- location2
        }
      }
      
      if(location %in% data$court_location){
        base <- gsub(paste0(loc, ".*$"), "", court)
        base <- gsub("\\W*$", "", base)
        if(paste0(base, "\\s?", loc) %in% data$Courts){
          return(data$courtID[which(data$Courts == paste0(base, loc))])
        }
        
        # "Hof van beroep Brussel" is sometimes named "Hof van beroep te Brussel"
        base <- gsub(paste0(location_words, "$", collapse="|"), " ", base)
        
        if(length(which(data$Courts == paste0(base, loc))) == 1){
          return(data$courtID[which(data$Courts == paste0(base, loc))])
        }
        
        if(TRUE %in% grepl(base, data$Courts) & !paste0(base, loc) %in% data$Courts){
          if(length(unique(data$States[grep(base, data$Courts)])) == 1){
            ID_base <- gsub("[[:upper:]]{3}.*$", "", data$courtID[grep(base, data$Courts)][1])
            ID_location <- gsub("^.*([[:upper:]]{3}).*$", "\\1", data$courtID[which(data$court_location == location)[1]])
            return(paste0(ID_base, ID_location, 0))
          }
        }
      } else { # Find courts where everything is equal except location
        y <- grep(gsub(location, "", court, fixed = TRUE), data$Courts, fixed = TRUE)
        if(length(y) > 0){
          y <- y[which(unlist(lapply(y, function(y) gsub(data$court_location[y], "", data$Courts[y], fixed=TRUE) == gsub(location, "", court, fixed=TRUE))))]
          if(length(y) > 0){
            code_root <- unique(gsub("[[:upper:]][[:upper:]][[:upper:]].*$", "", data$courtID[y]))
            if(length(code_root) == 1){ # If there is only one such root, return this
              return(code_root)
            }
          }
        }
      }
    }
  }
  
  if(length(x) == 0 & grepl(", ", court)){
    x <- grep(gsub(", .*$", "", court), data$Courts, fixed = TRUE)
    if(length(x) > 0){
      court <- gsub(", .*$", "", court)
    }
  }
  
  if(length(x) == 0 & grepl("-", court)){
    x <- grep(gsub("\\W", ".", gsub("\\W*-.*$", "", court)), data$Courts)
    if(length(x) > 0){
      court <- gsub("\\W*-.*$", "", court)
    }
  }
  
  if(length(x) == 1){
    return(data$courtID[x])
  }
  
  if(length(x)>1){
    branches <- x[which(data$Branch[x] != "")]
    if(length(branches) > 0){
      branches <- branches[which(unlist(lapply(data$Branch[branches], function(y) grepl(y, input))))]
      if(length(branches)== 0){
        y <- x[which(data$Branch[x] == "")]
        if(length(y) == 1){
          return(data$courtID[y])
        }
      }
    } 
    if(length(branches) == 1){
      return(unique(data$courtID[branches]))
    }
    if(length(branches)==0){ # Multiple observations, but same name, same country, and no branch
      if(length(unique(paste(data$Courts[x], data$States[x]))) == 1){
        return(data$courtID[x[1]])
      }
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
    branch_tags <- c(" division", "chamber", "courts", " Außenstelle \\w+", "sezione.*")
    if(grepl(paste0("[\\(,].*", branch_tags, collapse="|"), input, ignore.case = TRUE)){
      branch_tags <- branch_tags[which(unlist(lapply(branch_tags, function(y) grepl(y, input, ignore.case = TRUE))))]
      branch <- gsub("^\\W*", "", gsub(paste0(".*[,\\(-](.*?", branch_tags, ").*?$"), "\\1", input, ignore.case = TRUE))
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
  
  
  # Try English names (somewhat lazy)
  if(length(x) == 0){
    if(TRUE %in% grepl(tolower(court), tolower(data$English.Translation), fixed = TRUE)){
      x <- grep(tolower(court), tolower(data$English.Translation), fixed = TRUE)
    }
    if(length(unique(data$courtID[x])) == 1){
      return(unique(data$courtID[x]))
    }
  }
  
  # multiple courts?
  if(grepl(" et | and ", court)){
    x <- unlist(lapply(unlist(strsplit(court, " et | and ")), function(y) which(data$Courts == y)) )
    if(length(x) > 0){
      return(unique(data$courtID[x]))
    }
  }
  
  # Known type of court in unknown location
  if(paste(location) != court & !is.na(location)){
    base <- gsub(paste0(gsub("\\W", ".", loc), ".*$"), "", court)
    base <- gsub(paste0("\\W*", location_words, "$", collapse="|"), "", base)
    base <- gsub("\\W*$", "", base)
    if(TRUE %in% grepl(paste0("^", gsub("\\W", "", base)), data$Courts)){
      x <- grep(paste0("^", gsub("\\W", ".", base)), data$Courts)
      id_out <- unique(gsub("[[:upper:]]{3}.*$", "", data$courtID[x]))
      if(length(id_out == 1)){
        return(id_out)
      }
    }
  }
  
  # If paranthesis is included in name (England & Wales or Northern Ireland)
  if(length(x) > 1){
    y <- x[grep(gsub(").*$", "", input),
                data$Courts[x], fixed = TRUE)]
    
    if(length(y) == 1){
      return(data$courtID[y])
    }
  }
  # Reverse search:
  if(!is.na(country)){
    y <- which(data$States == country)[which(unlist(lapply(data$Courts[which(data$States == country)], function(y) grepl(y, input))))]
    if(length(y) == 0){
      y <- which(data$States == country)[which(unlist(lapply(gsub("\\W*\\(.*$", "", data$Courts[which(data$States == country)]), 
                                                             function(y) grepl(y, input))))]
    }
    if(length(y) > 1){
      branches <- data$Branch[y]
      
      z <- y[which(branches != "" & unlist(lapply(branches, function(y) grepl(y, input))))]
      
      if(length(z) > 1 | length(z) == 0){
        z <- ifelse(length(which(branches == "")) == 1,
                    y[which(branches == "")],
                    z)
      }
      
      if(length(z) > 0){
        y <- z
      }
      
      if(length(y) == 1){
        return(data$courtID[y])
      }
      
      out <- unique(gsub("[[:upper:]][[:upper:]][[:upper:]].*$", "", data$courtID[y]))
      if(length(out) == 1){
        return(out)
      }
    }
    if(length(y) == 1){
      return(data$courtID[y])
    }
  }
  
  if(length(x) > 1){
    out <- unique(gsub("[[:upper:]][[:upper:]][[:upper:]].*$", "", data$courtID[x]))
    if(length(out) == 1){
      return(out)
    }
  }
  
  return(NA)
}

