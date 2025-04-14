library(data.table)

in_dir <- "downloads"
out_dir <- "processed"

d1 <- fread(file.path(in_dir,"Document Collection and Tagging-140425.csv"))

d1 <- d1[Upload == "Yes - upload to SIPex"]
#clean colnames:
colnames(d1)
setnames(d1, "Document", "Title")
setnames(d1, gsub("\n", "", names(d1), fixed = TRUE))  # Replace newlines with spaces
setnames(d1, gsub("/", "", names(d1), fixed = TRUE))
setnames(d1, gsub("\r", "", names(d1), fixed = TRUE))
setnames(d1, trimws(names(d1)))
names(d1)

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



# JOIN TAGS -----------------
#checking some tags
names(d1)
unique(d1$`Copyright (Yes - publish; No - need permission)`)
unique(d1$ISSN)
unique(d1$`Geographical Area -NATIONAL`)
unique(d1$`Geographical Area -REGIONAL`)

#clean up the tags a bit:
cols_to_drop <- c("Document Name (title_location_year published)",
                  "Copyright (Yes - publish; No - need permission)",
                  "Upload", "ISSN")
d1 <- d1[, !cols_to_drop, with = FALSE]

#need to make "All" into a column specific "All"
#d1[, (names(d1)) := lapply(.SD, 
 #                            function(x) gsub("All \\(since this can be applied to any forest in BC\\)",
  #                                            "All", x))]
names(d1)

# Tenures ------------------------
unique(d1$Tenure)
d1$Tenure <- gsub("area-based", "area based", d1$Tenure, ignore.case = TRUE)
d1$Tenure <- gsub("volume-based", "volume based", d1$Tenure, ignore.case = TRUE)
d1$Tenure <- gsub("voume based", "volume based", d1$Tenure, ignore.case = TRUE)
d1$Tenure <- gsub("Area-basaed", "area based", d1$Tenure, ignore.case = TRUE)

# Publication year:
unique(d1$`Year Published`)
d1[grep("Publication year is not confirmed", `Year Published`)]
d1[, `Year Published` := gsub(", Publication year is not confirmed", "", `Year Published`)]

#Authors -------------------------
#too many - just need to do this check manually maybe?
unique(d1$`Author(s)`)

#Resource type -------------------
unique(d1$`Resource Type`)
d1[, `Resource Type` := gsub("Case Study", "Case study", `Resource Type`)]
d1[, `Resource Type` := gsub("Decision Aid", "Decision aid", `Resource Type`)]

# National ---------------------
unique(d1$`Geographical Area -NATIONAL`)
standardize_geography <- function(x) {
  # Split by comma, trim whitespace, sort, and rejoin
  return(paste(sort(trimws(unlist(strsplit(x, ",")))) , collapse = ", "))
}
d1$`Geographical Area -NATIONAL` <- sapply(d1$`Geographical Area -NATIONAL`, 
                                           standardize_geography)

# Provincial --------------------
unique(d1$`Geographical Area -PROVINCIAL  STATE`)
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("Britsh", 
                                                    "British", 
                                                    `Geographical Area -PROVINCIAL  STATE`)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("itnerior", "Interior", 
                                                    `Geographical Area -PROVINCIAL  STATE`)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("central Interior", 
                                                    "Central Interior", 
                                                    `Geographical Area -PROVINCIAL  STATE`,
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("south Interior",
                                                    "South Interior",
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("coastal BC", 
                                                    "Coastal BC", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("cenral", 
                                                    "Central", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("north Coast BC", 
                                                    "North Coast BC", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("north Interior BC",
                                                    "North Interior BC", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("south coast BC", 
                                                    "South Coast BC", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("coast", 
                                                    "Coast", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("island", 
                                                    "Island", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("interior", 
                                                    "Interior", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("lower mainland BC",
                                                    "Lower Mainland BC", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("(?<!BC)\\s*central Interior",
                                                    "Central Interior BC", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    perl = TRUE, ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("(?<!BC)\\s*southern Interior",
                                                    "Southern Interior BC", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    perl = TRUE, ignore.case = TRUE)]
d1[, `Geographical Area -PROVINCIAL  STATE` := gsub("(?<!BC)\\s*south interior", 
                                                    "Southern Interior BC", 
                                                    `Geographical Area -PROVINCIAL  STATE`, 
                                                    perl = TRUE, ignore.case = TRUE)]

# Regional --------------------
# mostly leave them as is
unique(d1$`Geographical Area -REGIONAL`)
d1[grep("Chilcotin", `Geographical Area -REGIONAL`), 
   .(ID,Title,`Geographical Area -REGIONAL`)]
#change Chilcotin (western) to Chilcotin:
d1$`Geographical Area -REGIONAL` <- gsub("Chilcotin \\(western\\)", 
                                         "Chilcotin", d1$`Geographical Area -REGIONAL`)
d1[grep("Cariboo", `Geographical Area -REGIONAL`), 
   .(ID,`Geographical Area -REGIONAL`)]

# Forest type ------------------
unique(d1$`Forest Type`)
d1[, `Forest Type` := gsub("interiior", "interior", `Forest Type`, ignore.case = TRUE)]
d1[, `Forest Type` := gsub("\\bSubalpine forests\\b", "Montane and subalpine forests", `Forest Type`)]


# BEC zones ------------------
unique(d1$`BEC Zone`)
d1[, `BEC Zone` := gsub("Engalmann", "Engelmann", `BEC Zone`, ignore.case = TRUE)]

unique(d1$BECZone_Subzone_variant)
expand_bec_variants <- function(bec_strings) {
  # Split by commas first
  out <- lapply(strsplit(bec_strings, ","), function(parts) {
    parts <- trimws(parts)
    expanded <- c()
    
    for (i in seq_along(parts)) {
      part <- parts[i]
      
      # If it looks like a BEC variant with suffix number (e.g., "SBS_dw_1")
      if (grepl("^[A-Z]+_[a-z]+(?:_[0-9]+)?$", part)) {
        # Try to extract prefix before last underscore
        prefix <- sub("^(.*?_[a-z]+)_?[0-9]*$", "\\1", part)
        
        # Check for sequences like "1,2,3"
        while (i + 1 <= length(parts) && grepl("^[0-9]+$", trimws(parts[i + 1]))) {
          expanded <- c(expanded, paste0(prefix, "_", trimws(parts[i + 1])))
          i <- i + 1
        }
        
        # Add the original part (could be with _number or just the base)
        if (!grepl("^[0-9]+$", part)) {
          expanded <- c(expanded, part)
        }
      } else if (!grepl("^[0-9]+$", part)) {
        # Just add it as-is (e.g., "IDF_xm" or "CWH_mm")
        expanded <- c(expanded, part)
      }
    }
    
    unique(trimws(expanded))
  })
  
  # Return as comma-separated strings again
  sapply(out, function(x) paste(unique(x), collapse = ", "))
}
d1[, BECZone_Subzone_variant := expand_bec_variants(BECZone_Subzone_variant)]
# BECZone_Subzone_variant - remove underscores:
d1[, BECZone_Subzone_variant := gsub("_", "", BECZone_Subzone_variant)]


# Practice -----------------------------------
unique(d1$Practice)
d1[, Practice := gsub("Intervention", "Stand intervention", Practice)]
d1[, Practice := gsub("Waste/biomass/fuel management", 
                      "Waste management, Biomass management, Fuel management", Practice)]

# Value ------------------------------
unique(d1$Value)
d1[, Value := gsub("heatlh", "health", Value)]
d1[, Value := gsub("health", "health", Value)]

# Technique --------------------------
unique(d1$Technique)
d1[, Value := gsub("Varibale", "Variable", Value)]
d1[, Value := gsub("Regenration", "Regeneration", Value)]
d1[, Value := gsub("Regenreation", "Regeneration", Value)]
unique(d1$`Technique 2`)

# Research Forest ------------------
unique(d1$`Research Forest`)
d1[, `Research Forest` := gsub("research forest", "Research Forest", `Research Forest`)]

# Tree species --------------------
unique(d1$`Tree Species`)
d1[, `Tree Species` := gsub("balsmifera", "balsamifera", `Tree Species`)]
d1[, `Tree Species` := gsub("heterophyllaBa", "heterophylla", `Tree Species`)]

# wildlife ---------------
unique(d1$`Wildlife Species`)

#
unique(d1$Habitat)

unique(d1$Equipment)

unique(d1$`Forest Products`)

# Zonation -------------------------
unique(d1$Zonation)
d1[, Zonation := gsub("Widlife", "Wildlife", Zonation)]
d1[, Zonation := gsub("managemetn", "management", Zonation)]

# Stand development ---------------
unique(d1$`Stand Development`)
d1[, `Stand Development` := gsub("disurbances", "disturbances", `Stand Development`)]
d1[, `Stand Development` := gsub("Comeptition", "Competition", `Stand Development`)]
d1[, `Stand Development` := gsub("distrubances", "disturbances", `Stand Development`)]
d1[, `Stand Development` := gsub("disturbancces", "disturbances", `Stand Development`)]

# Disturbances --------------------
unique(d1$`Disturbance Type`)
d1[, `Disturbance Type` := gsub("wevil", "weevil", `Disturbance Type`)]


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




