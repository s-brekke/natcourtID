retouch_natcourts <- function(decisions, text, folder=getwd(), data = natcourts){
  
  # Remove unwanted spaces ####
  natcourts$Branch <- gsub("^\\s*|\\s*$", "", natcourts$Branch)
  natcourts$Courts <- gsub("^\\s*|\\s*$", "", natcourts$Courts)
  natcourts$States <- gsub("^\\s*|\\s*$", "", natcourts$States)
  natcourts$court_location <- gsub("^\\s*|\\s*$", "", natcourts$court_location)
  
  # Identify non-courts ####
  not_court_keyword <- "concept.{1,15}court|Notion.{1,15}juridiction|definition.{1,15}court|Definition.{1,20}jurisdiction|National court or tribunal"
  
  decisions <- decisions[which(decisions$preliminary_ruling == 1),]
  inadmissible <- which(decisions$reference_for_preliminary_ruling=="inadmissible")
  admissible <- which(!grepl("inadmissible", decisions$reference_for_preliminary_ruling) & decisions$type == "Judgment")
  inadmissible_courts <- 
    decisions$referring_court_ID[inadmissible][grep(
      not_court_keyword, 
      decisions$keywords[inadmissible], ignore.case = TRUE)]
  
  text <- text[which(text$case %in% decisions$case[inadmissible]),]
  inadmissible_courts_text_cases <- text$case[grep(not_court_keyword, text$keywords, ignore.case = TRUE)]
  inadmissible_courts <- unique(c(inadmissible_courts,
    na.omit(decisions$referring_court_ID[which(decisions$case %in% inadmissible_courts_text_cases)])))
  
  # Whether or not a court is a court can be context dependent. 
  # In C-443/93, the Elegktiko Synedrio is a court. However, in C‑363/11, the Court
  # concludes that "in the circumstances which gave rise to this reference for a 
  # preliminary ruling, the Elegktiko Sinedrio does not constitute a court or tribunal"
  # getPar("ECLI:EU:C:2012:825", 33)
  # What can you do.
  
  
  inadmissible_courts <- inadmissible_courts[which(!inadmissible_courts %in% decisions$referring_court_ID[admissible])]
  natcourts$not_a_court <- natcourts$courtID %in% inadmissible_courts
  
  
  save(natcourts, file=file.path(folder, "natcourts.rda"))
}


# save(natcourts, file="natcourts.rda")
