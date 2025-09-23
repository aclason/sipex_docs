

################################## Time to combine tags ###################################
###########################################################################################
names(d1)
cols_to_combine <- names(d1)[!names(d1) %in% c("ID","Title", "Organization",
                                               "All orgs", "org1", "org2", "org3","org4",
                                               "Description")]
d1[, Tags := do.call(paste, c(.SD, sep = ",")), .SDcols = cols_to_combine]

d2 <- d1[,.(ID,Title,Tags)]
d2[, Tags := gsub(",+", ",", Tags)]  # Replace multiple commas with a single comma
d2[, Tags := gsub("^,|,$", "", Tags)]

#Tags don't accept special characters, but accepts dashes, spaces, 
#underscores and capitalization. No slashes or ampersands

#first, get rid of the parentheses, but also anything inside the paraentheses:
d2[, Tags := gsub("\\(.*?\\)", "", Tags)]

#get rid of other special characters
special_chars <- unique(unlist(strsplit(paste(d2$Tags, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[/&.?]"
d2[, Tags := gsub(chars_to_remove, " ", Tags)]

#clean up the commas and spaces
d2[, Tags := gsub("NA", "", Tags)] # Remove "NA" 
d2[, Tags := gsub(",\\s*NA\\s*,", ",", Tags)] # Remove "NA" surrounded by commas
d2[, Tags := gsub("\\s*,\\s*", ", ", Tags)]   # Ensure a single space after each comma
d2[, Tags := gsub("\\s*,", ",", Tags)]        # Remove any spaces before a comma
d2[, Tags := gsub("\\s+$", "", Tags)]         # Remove trailing spaces

#might need to get rid of parentheses in Organization - not sure yet

# Check for duplicates within the entry -------------------------------------
d2$Tags
remove_dupes_ignore_case <- function(x) {
  terms <- unlist(strsplit(x, ",\\s*"))
  deduped <- terms[!duplicated(tolower(terms))]
  paste(deduped, collapse = ", ")
}
d2[, Tags := remove_dupes_ignore_case(Tags), by = ID]

#now pull out groups and add to a new column:
groups <- c("Variable retention", "Reforestation", "Prescribed fire",
            "Indigenous fire stewardship", "Thinning", "Stand interventions",
            "Fuel management", "Forest products", "Monitoring")

extract_groups <- function(x) {
  matched <- groups[sapply(groups, function(g) grepl(g, x, ignore.case = TRUE))]
  if (length(matched) == 0) return(NA_character_)
  # Format: replace spaces with hyphens and convert to lowercase
  cleaned <- tolower(gsub(" ", "-", matched))
  paste(cleaned, collapse = ", ")
}

# Apply it to your data.table
d2[, Group := sapply(Tags, extract_groups)]
d2$Group

#add a License column:
d2[, License:="Open Data Commons Attribution License"]

d1[,.(org1)]

#reorder:
d2 <- d2[,.(ID,Title,Organization, Tags, License, Group)]

#print out 20 resources
fwrite(d2[20:40], file.path(out_dir,"datasets_010425.csv"))

#create the resource doc:
#need to watch the ids - as we are making them twice.
d3 <- d3[,.(Dataset_ID = ID, Name = Title, 
            Path = `Document Name (title_location_year published)`)]

#append the file type to the end of each path - for now it's pdf, would
# need to update this in the future depending on file types
d3[,Path := paste0(Path,".pdf")]

fwrite(d3[20:40], file.path(out_dir,"resources_010425.csv"), append = FALSE)

