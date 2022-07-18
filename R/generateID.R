# data <- read.csv("~/Dokumenter/Tjenester for andre/Anna/National courts/national_courts.csv")

generateID <- function(data, name = "courtID"){

  data_in <- data
  data$in_hierarchy[which(!data$in_hierarchy %in% 0:1)] <- 0
  data$level <- data$court_level
  data$level[which(data$in_hierarchy == 0)] <- 0
  
  
  data$country_code <- country_codes$code[match(data$States, country_codes$country)]

  # Remove coverage information
  data$court_location <- gsub(": covers.*", "", data$court_location)
  data$court_location[grep("not known", data$court_location, ignore.case = TRUE)] <- 99

  data$location <- toupper(gsub("^(...).*$" ,"\\1", gsub("\\W|\\d", "", data$court_location)))

  # Deal with courts with multiple locations separated by a slash
  data$court_location <- gsub("\\W*$", "", data$court_location)
  data$court_location <- gsub("\\W*branches in | or ", "/", data$court_location)
  data$court_location <- gsub("Saint\\W", "S", data$court_location)
  data$court_location <- gsub("L[ea]\\s|The ", "", data$court_location)

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
    locations[which(locations$loc_country == l & locations$court_location == region),"location"] <- location_new
    data[which(data$loc_country == l & data$court_location == region),"location"] <- location_new

    locations[which(locations$loc_country == l & locations$court_location == region),"loc_country"] <- gsub("^...", location_new, l)
    data[which(data$loc_country == l & data$court_location == region),"loc_country"] <- gsub("^...", location_new, l)
  }

  data$competence <- data$court_competence
  data$competence[which(!data$competence %in% 1:4)] <- 0

  # data$competence[grepl("broadcast", data$competence_specific, ignore.case = TRUE)] <- "B"
  # data$competence[grepl("data", data$competence_specific, ignore.case = TRUE)] <- "D"

  data$competence[grepl("custom", data$competence_specific, ignore.case = TRUE)] <- 8
  data$competence[grepl("social", data$competence_specific, ignore.case = TRUE)] <- 9
  data$competence[grepl("tax", data$competence_specific, ignore.case = TRUE)] <- 5
  data$competence[grepl("criminal", data$competence_specific, ignore.case = TRUE)] <- 6
  data$competence[grepl("labou?r", data$competence_specific, ignore.case = TRUE)] <- 7

  # not mutually exclusive: Administrative and labour, labour and social

  data$courtID <- paste0(data$level, paste(data$country_code), data$competence, data$location)
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
