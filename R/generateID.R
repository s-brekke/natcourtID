generateID <- function(data, name = "courtID"){

  data_in <- data
  data$in_hierarchy[which(!data$in_hierarchy %in% 0:1)] <- "X"
  data$level <- data$court_level
  data$level[which(data$level == 99)] <- "X"
  data$level[which(data$in_hierarchy == 0)] <- 0
  
  data$country_code <- country_code$code[match(data$States, country_code$country)]

  # Remove coverage information
  data$court_location <- gsub(": covers.*", "", data$court_location)
  data$court_location <- gsub("\\W+and\\W+|, |\\w*\\W+divisions\\W*|\\W*branches in\\W*|\\W*&\\W*", "/", data$court_location)
  data$court_location <- gsub("^/", "", data$court_location)
  data$court_location[grep("not known", data$court_location, ignore.case = TRUE)] <- 99

  data$location <- toupper(gsub("^(...).*$" ,"\\1", gsub("\\W|\\d", "", data$court_location)))

  # Deal with courts with multiple locations separated by a slash
  data$court_location <- gsub("\\W*$", "", data$court_location)
  data$court_location <- gsub("\\W*branches in | or ", "/", data$court_location)
  data$court_location <- gsub("Saint\\W", "S", data$court_location)
  data$court_location <- gsub("L[ea]\\s|The ", "", data$court_location)
  
  natcourts$court_location[grep("London .*PO", natcourts$court_location)] <- "London"

  data$location[grep("/", data$court_location)] <- toupper(gsub("^(...).*$", "\\1",
                                                                  paste0(gsub("/(.)[^/]*", "\\1",
                                                                  paste0("/", data$court_location[grep("/", data$court_location)])),
                                                                  gsub(".*/.(.)[^/]*", "\\1",
                                                                       data$court_location[grep("/", data$court_location)]))))

  data$loc_country <- paste(data$location, data$country_code)
  # Make sure court location is unique:
  locations <- data[, c("location", "court_location", "country_code", "loc_country")]
  locations <- locations[which(locations$location != ""),]

  locations <- locations[which(!duplicated(paste(locations$court_location, locations$country_code))),]

  for(l in locations$loc_country[which(duplicated(locations$loc_country))]){
    t <- table(data$court_location[which(data$loc_country == l)])
    region <- names(t[which(t == min(t))])[1]
    region_fix <- gsub("\\W|\\d", "", region)
    # message(region)
    
    if(tolower(region) %in% tolower(location_codes$location) | 
       tolower(region_fix) %in% tolower(gsub("\\W|\\d", "", location_codes$location)) |
       TRUE %in% grepl(region, location_codes$location)){
      location_new <- location_codes$code[which(tolower(region) == tolower(location_codes$location) | 
        tolower(region_fix) == tolower(gsub("\\W|\\d", "", location_codes$location)) | 
          grepl(region, location_codes$location))]
    } else {

    location_new <- toupper(gsub("^(..).(.).*$", "\\1\\2", region_fix))
    if(gsub("^...", location_new, l) %in% locations$loc_country){
      location_new <- toupper(gsub("^(..)..(.).*$", "\\1\\2", region_fix))
      if(gsub("^...", location_new, l) %in% locations$loc_country){
        location_new <- toupper(gsub("^(.).(..).*$", "\\1\\2", region_fix))
      }
      if(gsub("^...", location_new, l) %in% locations$loc_country){
        location_new <- toupper(gsub("^(..).*(.)$", "\\1\\2", region_fix))
      }
      if(gsub("^...", location_new, l) %in% locations$loc_country){
        stop("Unique region ID could not be constructed. Add more algorithms.")
      }
    }
    }
    locations[which(locations$loc_country == l & locations$court_location == region),"location"] <- location_new
    data[which(data$loc_country == l & data$court_location == region),"location"] <- location_new

    locations[which(locations$loc_country == l & locations$court_location == region),"loc_country"] <- gsub("^...", location_new, l)
    data[which(data$loc_country == l & data$court_location == region),"loc_country"] <- gsub("^...", location_new, l)
  }

  data$competence <- data$court_competence
  data$competence[which(!data$competence %in% 1:4)] <- 0

  # data$competence[grepl("broadcast", data$competence_specific, ignore.case = TRUE)] <- "B"
  # data$competence[grepl("data", data$competence_specific, ignore.case = TRUE)] <- "D"

  data$competence[which(grepl("tax", data$competence_specific, ignore.case = TRUE) & data$competence == 4)] <- 5
  data$competence[which(grepl("criminal", data$competence_specific, ignore.case = TRUE) & data$competence == 4)] <- 6
  data$competence[which(grepl("labou?r", data$competence_specific, ignore.case = TRUE) & data$competence == 4)] <- 7
  data$competence[which(grepl("commercial", data$competence_specific, ignore.case = TRUE) & data$competence == 4)] <- 8
  data$competence[which(grepl("social", data$competence_specific, ignore.case = TRUE) & data$competence == 4)] <- 9

  

  # Roman numerals 
  data$Courts[grep(" I$", data$Courts)] <- paste(data$Courts[grep(" I$", data$Courts)], 1)
  data$Courts[grep(" II$", data$Courts)] <- paste(data$Courts[grep(" II$", data$Courts)], 2)
  data$Courts[grep(" III$", data$Courts)] <- paste(data$Courts[grep(" III$", data$Courts)], 3)
  data$Courts[grep(" IV$", data$Courts)] <- paste(data$Courts[grep(" IV$", data$Courts)], 4)
  
  data$courtID <- paste0(data$level, paste(data$country_code), data$competence, data$location)
  data$courtID <- iconv(data$courtID,from="UTF-8",to="ASCII//TRANSLIT")
  duplicates <- names(which(table(data$courtID[which(is.na(data$Name.change..1.))])>1))
  duplicates <- duplicates[which(nchar(duplicates) == 7)]
  for(d in duplicates){
    x <- which(data$courtID == d & is.na(data$Name.change..1.))
    x <- x[order(data$Courts[x])]

    # Make sure Prague 1 has number one, Tribunal d'instance de Paris 12ème number 12, etc
    numbers <- gsub("^\\D*(\\d+).*?$$", "\\1", data$Courts[x][grep("\\d", data$Courts[x])])
    if(length(numbers) > 0 & length(numbers) == length(unique(numbers))){
      data$courtID[x][grep("\\d", data$Courts[x])] <-
        paste0(data$courtID[x][grep("\\d", data$Courts[x])],
               numbers)
      x <- which(data$courtID == d & is.na(data$Name.change..1.))
    }
    if(length(x) > 1){

      available_numbers <- 1:100
      available_numbers <- available_numbers[which(!available_numbers %in% numbers)]

      x <- x[order(data$Courts[x])]
      data$courtID[x] <-
        paste0(d, available_numbers[1:length(x)])
    }
  }
  courtID <- data$courtID
  return_data <- cbind(courtID, data_in)
  colnames(return_data)[1] <- name
  return(return_data)
}
