library(data.table)
library(stringr)
library(ggplot2)

in_dir <- "master-tracking"

#resources in the library(cheating as coll6 not there)
d1 <- fread(file.path(in_dir,"all_resources_280126.csv"))
# read in the docs that have been uploaded:
uploads <- fread(file.path(in_dir,"collection_tracker_280126.csv"))


#clean collection master list
setnames(uploads, c("doc_id", "Document or Training Material",
                    "Document File Name\n \n(title_location_year published)"),
         c("ID","Title", "Path"))
uploads <- uploads[,.(ID, Title, `Uploaded to SIPex?`)]



# clean the resource library
#ID
colnames(d1)
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
d1[, Organization := gsub("[\n\r]+", " ", Organization)]# remove newlines
d1[, Organization := gsub("\\.$", "", Organization)] # strip trailing periods
d1[, Organization := gsub("’", "'", Organization)] # curly to straight apostrophe
d1[, Organization := gsub('^"+|"+$', '', Organization)]
d1[, Organization := trimws(Organization)]
sort(unique(trimws(unlist(d1$Organization))))



## -----------------------------------------------------------------------------
dt_uploaded <- merge(d1, uploads[`Uploaded to SIPex?` == "Yes", 
                                     .(ID, `Uploaded to SIPex?`)], by = "ID")

tag_cols <- names(dt_uploaded)[!names(dt_uploaded) %in% c("ID","Title", "Upload to SIPex?",
                                                          "Uploaded to SIPex?",
                                                          "License",
                                        "Document Name (title_location_year published)",
                                        "Author(s)", "Author contact",
                                        "Additional organizations",
                                        "Description", "Descriptive location",
                                        "Group", "Engagement Type",
                                        "DOI", "Name of Journal", #not sure
                                        "Who has copyright?", #not sure
                                        "Notes about Copyright",
                                        "Associated Working Group  Community of Practice",       
                                        "Name_contributor",                                   
                                        "Date_received",                                     
                                        "Date_digitized" )] 

dt_long <- melt(
  d1,
  id.vars = "ID",
  measure.vars = tag_cols,
  variable.name = "tag_type",
  value.name = "tag"
)
dt_tags <- dt_long[
  !is.na(tag) & tag != "",
  .(tag = trimws(unlist(strsplit(tag, ",")))),
  by = .(ID, tag_type)
]

dt_tags[, tag := gsub(",+", ",", tag)]  # Replace multiple commas with a single comma
dt_tags[, tag := gsub("^,|,$", "", tag)]
dt_tags[, tag := gsub("\\(.*?\\)", "", tag)]

#get rid of other special characters
special_chars <- unique(unlist(strsplit(paste(dt_tags$tag, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[/&?]"
dt_tags[, tag := gsub(chars_to_remove, " ", tag)]
#apostrophe's with no spaces:
chars_to_remove <- "['’]"
dt_tags[, tag := gsub(chars_to_remove, "", tag)]


#clean up the commas and spaces
dt_tags[, tag := gsub("NA", "", tag)] # Remove "NA" 
dt_tags[, tag := gsub(",\\s*NA\\s*,", ",", tag)] # Remove "NA" surrounded by commas
dt_tags[, tag := gsub("\\s*,\\s*", ", ", tag)]   # Ensure a single space after each comma
dt_tags[, tag := gsub("\\s*,", ",", tag)]        # Remove any spaces before a comma
dt_tags[, tag := gsub("\\s+$", "", tag)]         # Remove trailing spaces
dt_tags[, tag := gsub(",+", ",", tag)]  # Replace multiple commas with a single comma
dt_tags[, tag := gsub('["“”‘’]', '', tag)]
dt_tags[, tag := gsub(",\\s*$", "", tag)] 
dt_tags[tag_type == "Resource Type", tag := stringr::str_to_title(tag)]

unique(dt_tags$tag_type)

ggplot(dt_tags[tag_type == "Organization"|
                 tag_type == "Year Published"|
                 tag_type == "Resource Type"|
                 tag_type == "Geographical Area - REGION"]) +
  geom_bar(aes(x= tag))+
  facet_wrap(~tag_type)

ggplot(dt_tags[tag_type == "Organization"]) +
  geom_bar(aes(x= tag))

ggplot(dt_tags[tag_type == "Resource Type"]) +
  geom_bar(aes(x = tag), fill = "#002A26") +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = 20, angle = 45, hjust = 1, face = "bold"),
    axis.text.y  = element_text(size = 20, face = "bold"),
    plot.title   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggplot(dt_tags[tag_type == "Forest Type"]) +
  geom_bar(aes(x = tag), fill = "#E8AA00") +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = 20, angle = 45, hjust = 1, face = "bold"),
    axis.text.y  = element_text(size = 20, face = "bold"),
    plot.title   = element_blank(),
    panel.grid.major.x = element_blank()
  )


ggplot(dt_tags[tag_type == "Technique"]) +
  geom_bar(aes(x = tag), fill = "#002A26") +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_text(size = 20, angle = 45, hjust = 1, face = "bold"),
    axis.text.y  = element_text(size = 20, face = "bold"),
    plot.title   = element_blank(),
    panel.grid.major.x = element_blank()
  )


bec_abbrev <- dt_tags[
  tag_type == "BEC Zone" & grepl("^[A-Z]{2,5}$", tag)
]

ggplot(bec_abbrev[tag_type == "BEC Zone"]) +
  geom_bar(aes(x = tag), fill = "#E8AA00") +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 14, angle = 45, hjust = 1, face = "bold"),
    axis.text.y  = element_text(size = 14),
    plot.title   = element_text(size = 20, face = "bold"),
    panel.grid.major.x = element_blank()
  )

dt_tags[tag == "Commercial thinning"]






