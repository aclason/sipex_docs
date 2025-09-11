


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
