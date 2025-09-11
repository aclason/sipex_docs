


#Title
#Year

#"ID","Title", "Organization",
#"All orgs", "org1", "org2", "org3","org4",
#"Description"


d1$Title
d1[, (names(d1)) := lapply(.SD, function(x) {
  x <- gsub("\n\\*No link available, see Mendeley","",x)
  x <- trimws(x)                  # Trim leading/trailing spaces and newlines
  x <- gsub("\n+", ", ", x)        # Replace one or more newlines with a single comma + space
  x <- gsub(",\\s*,", ",", x)      # Remove double commas caused by empty newlines
  x <- gsub("\r", "",x)
  x <- gsub("[–—]", "-", x) #change em and en dashes to a hyphen
  x <- gsub(":", "-", x) #change em and en dashes to a hyphen
  return(x)
})]
d1$Title

#Fix parts of the title that are fixable (remove colons, semicolons, periods):
#Title and Descriptions accept parenthesis, punctuations, apostrophes and hyphens
special_chars <- unique(unlist(strsplit(paste(d1$Title, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[:,\'/?\r–.&*—_;]"

d1[, Title := gsub(chars_to_remove, " ", Title)]
d1[, Title := gsub("\\s+", " ", trimws(Title))]
d1[, ID := seq(1,nrow(d1))]
d1$Title



#check for duplicates in the Organization and create new tags for all but one organization
unique(d1$Organization)
d1[, Organization := gsub("(?<=\\s)-(?=\\s)", ",", Organization, perl = TRUE)]
d1[, c("org1", "org2", "org3", "org4") := tstrsplit(Organization, ",", fixed = TRUE)]
d1[,.(Organization, org1, org2, org3, org4)]
unique(d1$org4)
d1[, c("org1", "org2", "org3", "org4") := lapply(.SD, trimws), 
   .SDcols = c("org1", "org2", "org3", "org4")]
#this is good for now, but we'll have to chose an organization as the main one during the proofing
setnames(d1, "Organization","All orgs")
d1[, Organization := NA]

#reorder
setcolorder(d1, c("ID","Title", "Organization", 
                  setdiff(names(d1), c("ID", "Title", "Organization"))))

#seperate to make the resources doc:
d3 <- d1[,.(ID, Title, `Document Name (title_location_year published)`)]

