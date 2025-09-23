library(data.table)

in_dir <- "downloads"
out_dir <- "processed"

# Title and Descriptions accept parenthesis, punctuations, apostrophes and hyphens.
# Tags don't accept special characters but accepts dashes, spaces, underscores and capitalization.
#No slashes or ampersands
# License - refer to this for specific names and descriptions https://opendefinition.org/licenses/
# Groups/Categories - these are already added so the value for this is the slug/url 
#of the categories
# comma separated for multiple categories


#d1 <- fread(file.path(in_dir,"Document Collection and Tagging-140425.csv"))
d1 <- fread(file.path(in_dir,"collection test.csv"))

#d1 <- d1[`Upload to SIPex?` == "Yes - upload to SIPex"]
#clean colnames:
colnames(d1)

#ID
#d1[, ID := seq(1,nrow(d1))]
setnames(d1, "doc_id", "ID")
setnames(d1, "Document", "Title")
setnames(d1, "Grouping tags","Group")
setnames(d1, gsub("\n", "", names(d1), fixed = TRUE))  # Replace newlines with spaces
setnames(d1, gsub("/", "", names(d1), fixed = TRUE))
setnames(d1, gsub("\r", "", names(d1), fixed = TRUE))
setnames(d1, trimws(names(d1)))
names(d1)

# Titles ----
d1$Title
#Fix parts of the title that are fixable (remove colons, semicolons, periods):
#Title and Descriptions accept parenthesis, punctuations, apostrophes and hyphens
special_chars <- unique(unlist(strsplit(paste(d1$Title, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[:,/?\r–.&*_;]"

d1[, Title := gsub(chars_to_remove, " ", Title)]
d1[, Title := gsub("\\s+", " ", trimws(Title))]
d1$Title

#Organization - looks like curation now has a single org
unique(d1$Organization)
#d1[, Organization := gsub("(?<=\\s)-(?=\\s)", ",", Organization, perl = TRUE)]
#d1[, c("org1", "org2") := tstrsplit(Organization, ",", fixed = TRUE)]
#d1[,.(Organization, org1, org2)]
#d1[, c("org1", "org2") := lapply(.SD, trimws), 
 #  .SDcols = c("org1", "org2")]
#this is good for now, but we'll have to chose an organization as the main one during the proofing
#setnames(d1, "Organization","All orgs")
#d1[, Organization := NA]

#reorder
setcolorder(d1, c("ID","Title", "Organization", 
                  setdiff(names(d1), c("ID", "Title", "Organization"))))
#seperate to make the resources doc:
d4 <- d1[,.(ID, Title, `Document Name (title_location_year published)`)]
setnames(d4, c("ID","Title","Document Name (title_location_year published)"),
         c("Dataset_ID","Name","Path"))

# JOIN TAGS -----------------
#checking some tags
names(d1)
unique(d1$License)
unique(d1$`Geographical Area -NATIONAL`)



#need to make "All" into a column specific "All"
#d1[, (names(d1)) := lapply(.SD, 
#                            function(x) gsub("All \\(since this can be applied to any forest in BC\\)",
#                                            "All", x))]
cols_to_combine <- names(d1)[!names(d1) %in% c("ID","Title", "Upload to SIPex?", "License",
                                               "Document Name (title_location_year published)",
                                               "Organization", "Year Published",
                                               "Author(s)","Additional organizations",
                                               "Description", "Descriptive location",
                                               "Group",
                                               "DOI", "Name of Journal", #not sure
                                               "Who has copyright?", #not sure
                                               "Notes about Copyright")] #not sure
d1[, Tags := do.call(paste, c(.SD, sep = ",")), .SDcols = cols_to_combine]

d2 <- d1[,.(ID,Title,Organization, `Author(s)`, `Year Published`, Tags,`Descriptive location`,
            Group, License, Description)]
d2[, Tags := gsub(",+", ",", Tags)]  # Replace multiple commas with a single comma
d2[, Tags := gsub("^,|,$", "", Tags)]
#Tags don't accept special characters, but accepts dashes, spaces, 
#underscores and capitalization and periods. No slashes or ampersands

#first, get rid of the parentheses, but also anything inside the paraentheses:
d2[, Tags := gsub("\\(.*?\\)", "", Tags)]

#get rid of other special characters
special_chars <- unique(unlist(strsplit(paste(d2$Tags, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[/&?]"
d2[, Tags := gsub(chars_to_remove, " ", Tags)]
#apostrophe's with no spaces:
chars_to_remove <- "['’]"
d2[, Tags := gsub(chars_to_remove, "", Tags)]


#clean up the commas and spaces
d2[, Tags := gsub("NA", "", Tags)] # Remove "NA" 
d2[, Tags := gsub(",\\s*NA\\s*,", ",", Tags)] # Remove "NA" surrounded by commas
d2[, Tags := gsub("\\s*,\\s*", ", ", Tags)]   # Ensure a single space after each comma
d2[, Tags := gsub("\\s*,", ",", Tags)]        # Remove any spaces before a comma
d2[, Tags := gsub("\\s+$", "", Tags)]         # Remove trailing spaces
d2[, Tags := gsub(",+", ",", Tags)]  # Replace multiple commas with a single comma
d2[, Tags := gsub('["“”‘’]', '', Tags)]
d2[, Tags := gsub(",\\s*$", "", Tags)] 
#might need to get rid of parentheses in Organization - not sure yet
#now pull out groups and add to a new column:

#Clean up descriptions:
#d3 <- d1[,.(ID,Title, Organization,`Author(s)`,`Year Published`,
 #           License, Group, Description)]

d2[, Description := gsub("\\(.*?\\)", "", Description)]

#get rid of other special characters
special_chars <- unique(unlist(strsplit(paste(d3$Description, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[/&?\"]"
d2[, Description := gsub(chars_to_remove, "", Description)]

#clean up the commas and spaces
#d3[, Description := gsub("NA", "", Description)] # Remove "NA" 
#d3[, Description := gsub(",\\s*NA\\s*,", ",", Description)] # Remove "NA" surrounded by commas
d2[, Description := gsub("\\s*,\\s*", ", ", Description)]   # Ensure a single space after each comma
#d3[, Description := gsub("\\s*,", ",", Description)]        # Remove any spaces before a comma
d2[, Description := gsub("\\s*\\.\\s*", ". ", Description)]       # Remove any spaces before a period
#d3[, Description := gsub("\\s+$", "", Description)]         # Remove trailing spaces

setnames(d2, "Descriptive location", "Descriptive Location")

#write out the dataset file:
fwrite(d2, file.path("../sipex_upload/datasets data","datasets_110925_test.csv"))

#write out the resources file:
fwrite(d4, file.path("../sipex_upload/resources data","resources_110925_test.csv"))





