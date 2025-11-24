library(data.table)
library(stringr)

in_dir <- "cleaned"
out_dir <- "../sipex_upload"

# Title and Descriptions accept parenthesis, punctuations, apostrophes and hyphens.
# Tags don't accept special characters but accepts dashes, spaces, underscores and capitalization.
#No slashes or ampersands
# License - refer to this for specific names and descriptions https://opendefinition.org/licenses/
# Groups/Categories - these are already added so the value for this is the slug/url 
#of the categories
# comma separated for multiple categories

# data associated with LTR
# batch uploads at the end of Septemeber:
d1 <- fread(file.path(in_dir,"downloads_250925.csv"))
# update to include additional training entries:
d1 <- fread(file.path(in_dir,"downloads_061025.csv"))

# update to include more from C3:
d1 <- fread(file.path(in_dir,"downloads_071025.csv"))


#d1 <- d1[`Upload to SIPex?` == "Yes - upload to SIPex"]
#clean colnames:
colnames(d1)

#ID
#d1[, ID := seq(1,nrow(d1))]
setnames(d1, "doc_id", "ID")
setnames(d1, "Document", "Title")
setnames(d1, "Featured Topic Tag","Group")
setnames(d1, "License Type", "License")
setnames(d1, gsub("\n", "", names(d1), fixed = TRUE))  # Replace newlines with spaces
setnames(d1, gsub("/", "", names(d1), fixed = TRUE))
setnames(d1, gsub("\r", "", names(d1), fixed = TRUE))
setnames(d1, trimws(names(d1)))
names(d1)

# Titles ---------------------------------
d1$Title
#Fix parts of the title that are fixable (remove colons, semicolons, periods):
#Title and Descriptions accept parenthesis, punctuations, apostrophes and hyphens
special_chars <- unique(unlist(strsplit(paste(d1$Title, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[:,/?\r–&*_;]"

d1[, Title := gsub(chars_to_remove, " ", Title)]
d1[, Title := gsub("\\s+", " ", trimws(Title))]
d1$Title

small_words <- c(
  "a", "an", "and", "as", "at", "but", "by", "for", "from",
  "if", "in", "into", "nor", "of", "on", "or", "over", "per",
  "the", "to", "up", "via", "with","when"
)

d1[, Title := sapply(Title, function(x) {
  words <- str_split(x, " ", simplify = TRUE)
  n <- length(words)
  
  words <- sapply(seq_along(words), function(i) {
    w <- words[i]
    # remove punctuation for logic checks
    w_clean <- str_remove_all(w, "^[^A-Za-z0-9]+|[^A-Za-z0-9]+$")
    
    if (str_detect(w_clean, "^[A-Z0-9\\-]+$") & nchar(w_clean) > 1) {
      # keep acronyms/codes as-is
      w
    } else if (tolower(w_clean) %in% small_words && i != 1 && i != n) {
      # keep small words lowercase unless first/last
      tolower(w)
    } else {
      # capitalize first letter only, preserve punctuation
      str_replace(w, "^[A-Za-z]", toupper)
    }
  })
  
  str_c(words, collapse = " ")
})]
d1$Title


# Organizations --------------------------
#Organization - looks like curation now has a single org
unique(d1$Organization)

#reorder
setcolorder(d1, c("ID","Title", "Organization", 
                  setdiff(names(d1), c("ID", "Title", "Organization"))))
#seperate to make the resources doc:
d4 <- d1[,.(ID, Title, `Document Name (title_location_year published)`)]
setnames(d4, c("ID","Title","Document Name (title_location_year published)"),
         c("Dataset_ID","Name","Path"))
#might need to get rid of parentheses in Organization - not sure yet
#now pull out groups and add to a new column:


# JOIN TAGS -----------------
#checking some tags
names(d1)
cols_to_combine <- names(d1)[!names(d1) %in% c("ID","Title", "Upload to SIPex?", "License",
                                               "Document Name (title_location_year published)",
                                               "Organization", "Year Published",
                                               "Author(s)", "Author contact",
                                               "Additional organizations",
                                               "Description", "Descriptive location",
                                               "Group", "Engagement Type",
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

#check tags ---------------------
sort(unique(trimws(unlist(strsplit(d2$Tags, ",")))))
d2[d2[, grepl("Caribou", Tags, ignore.case = TRUE)]]

#Description ----------------------------
d2[, Description := gsub("\\(.*?\\)", "", Description)]
special_chars <- unique(unlist(strsplit(paste(d2$Description, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[/&?\"]"
d2[, Description := gsub(chars_to_remove, "", Description)]
#clean up the commas and spaces
d2[, Description := gsub("\\s*,\\s*", ", ", Description)]   # Ensure a single space after each comma
d2[, Description := gsub("\\s*\\.\\s*", ". ", Description)]       # Remove any spaces before a period


#Descriptive location --------------------
sort(unique(trimws(unlist(strsplit(d2$`Descriptive location`, ",")))))
setnames(d2, c("Descriptive location"), 
         c("Descriptive Location"))

#cleaning authors names: ----------------------
sort(unique(trimws(unlist(strsplit(d2$`Author(s)`, ",")))))
d2[, `Author(s)` := gsub("([A-Z])\\.\\s+([A-Z])\\.", "\\1.\\2.", `Author(s)`)]   # collapse initials
d2[, `Author(s)` := gsub("([A-Z])\\.\\s+([A-Z])\\.", "\\1.\\2.", `Author(s)`)]   # collapse initials (third initial)
d2[, `Author(s)` := gsub("[\n\r]+", " ", `Author(s)`)]                           # remove newlines
d2[, `Author(s)` := gsub("\\.$", "", `Author(s)`)]                               # strip trailing periods
d2[, `Author(s)` := gsub("’", "'", `Author(s)`)]                                 # curly to straight apostrophe
d2[, `Author(s)` := gsub("O’", "O'", `Author(s)`)]                               # fix O’Neill
d2[, `Author(s)` := trimws(`Author(s)`)]
d2[, `Author(s)` := trimws(`Author(s)`)] 
sort(unique(trimws(unlist(strsplit(d2$`Author(s)`, ",")))))


#check descriptive location --------------------
sort(unique(trimws(unlist(strsplit(d2$`Descriptive Location`, ",")))))


#check groups --------------------
#sort(unique(trimws(unlist(strsplit(d2$Group, ",")))))
#d2[Group == "fire-prescribed-fire", .(ID, Title)]


#check organization --------------------
sort(unique(trimws(unlist(strsplit(d2$Organization, ",")))))
d2[, Organization := gsub("[\n\r]+", " ", Organization)]# remove newlines
d2[, Organization := gsub("\\.$", "", Organization)] # strip trailing periods
d2[, Organization := gsub("’", "'", Organization)] # curly to straight apostrophe
d2[, Organization := gsub('^"+|"+$', '', Organization)]
d2[, Organization := trimws(Organization)]
sort(unique(trimws(unlist(d2$Organization))))


d2$Title
d2$License


#write out the dataset file:
fwrite(d2[22], file.path(out_dir,"datasets data","datasets_071025_1.csv"))

#write out the resources file:
fwrite(d4[22], file.path(out_dir,"resources data","resources_071025_1.csv"))

# write out the ones that failed to upload:
#write out the dataset file:
fwrite(d2[ID %in% c("C2-005", "C3-084", "C3-091", "C3-093", "C3-096", "C3-099", 
                    "C3-100", "C3-101", "C3-105", "C3-106", "C3-107", "C3-108", 
                    "C3-109", "C5-003", "T1-002", "T1-003", "T1-004", "T1-005", 
                    "T1-012", "T1-013", "T1-014", "T1-015", "T1-017", "T1-018", 
                    "COP1-002", "COP1-003", "COP1-004", "COP1-005", "COP1-029", 
                    "COP1-036", "COP1-050")], 
       file.path("../sipex_upload/datasets data","datasets_250925_f.csv"))

#write out the resources file:
fwrite(d4[Dataset_ID %in% c("C2-005", "C3-084", "C3-091", "C3-093", "C3-096", "C3-099", 
                    "C3-100", "C3-101", "C3-105", "C3-106", "C3-107", "C3-108", 
                    "C3-109", "C5-003", "T1-002", "T1-003", "T1-004", "T1-005", 
                    "T1-012", "T1-013", "T1-014", "T1-015", "T1-017", "T1-018", 
                    "COP1-002", "COP1-003", "COP1-004", "COP1-005", "COP1-029", 
                    "COP1-036", "COP1-050")], 
       file.path("../sipex_upload/resources data","resources_250925_f.csv"))


