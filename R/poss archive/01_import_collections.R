library(data.table)

in_dir <- "downloads"
out_dir <- "processed"

#d1 <- fread(file.path(in_dir,"Document Collection and Tagging-140425.csv"))
d1 <- fread(file.path(in_dir,"Collection 1_ Document Collection and Tagging (Working Doc) - EDITED - Ready for Upload.csv"))
d2 <- fread(file.path(in_dir,"Collection 2_ Document Collection and Tagging (Working Doc) - SIPex - Ready for Upload.csv"))
d3 <- fread(file.path(in_dir,"Collection 3_ Document Collection and Tagging (Working Doc) - SIPex - Ready for Upload.csv"))

#check for colnames consistency
cols1 <- colnames(d1)
cols2 <- colnames(d2)
cols3 <- colnames(d3)
all_cols <- unique(c(cols1, cols2, cols3))
col_check <- data.table(
  column = all_cols,
  in_d1 = all_cols %in% cols1,
  in_d2 = all_cols %in% cols2,
  in_d3 = all_cols %in% cols3
)
col_check

fix_cols <- data.table(
  current = c("stand_age", "sppcomp", "basal"),
  correct = c("StandAge", "SpeciesComposition", "BasalArea")
)
#clean document names
setnames(d1,
         old = "Document Name (title_location_year published)", 
         new = "Document_name")
setnames(d2,
         old = "Document Name\n \n(title_location_year published)", 
         new = "Document_name")
setnames(d3,
         old = "Document Name\n \n(title_location_year published)", 
         new = "Document_name")

#notes about copyright
setnames(d3,
         old = "Notes about Copyright", 
         new = "Notes about copyright")

#geographical zone:
setnames(d1,
         old = "Geographical Area - \nZONE", 
         new = "Geographic-Zone")
setnames(d2,
         old = "Geographical Area - \nZONE\n", 
         new = "Geographic-Zone")
setnames(d3,
         old = "Geographical Area - \nZONE\n", 
         new = "Geographic-Zone")
dlist <- list(d1, d2, d3)
#merge
d1 <- rbindlist(dlist, fill = TRUE)

#clean colnames:
colnames(d1)
setnames(d1, "Document", "Title")
setnames(d1, gsub("\n", "", names(d1), fixed = TRUE))  # Replace newlines with spaces
setnames(d1, gsub("/", "", names(d1), fixed = TRUE))
setnames(d1, gsub("\r", "", names(d1), fixed = TRUE))
setnames(d1, trimws(names(d1)))
setnames(d1, "Author(s)", "Authors")
names(d1)

d2 <- d1[`Upload to SIPex?` == "Yes - upload to SIPex" | 
           `Upload to SIPex?` == ""]


#quick tag look:
cols_to_combine <- c("Technique")
d1[, Tags := do.call(paste, c(.SD, sep = ",")), .SDcols = cols_to_combine]
d2 <- d1[,.(doc_id,Tags)]
d2_long <- d2[, .(tag = unlist(strsplit(Tags, "\\n\\n|,"))), by = doc_id]
d2_long <- d2_long[, .(tag = unlist(strsplit(tag, "\\n"))), by = doc_id]
d2_long[, tag := trimws(tag)]
d2_long <- d2_long[tag != ""]

unique(d2_long$tag)

ggplot(data = d2_long[tag == "Fuel management"|tag == "Waste management"|tag == "Biomass product"]) +
  geom_histogram(aes(x = tag), stat = "count")

d2_long[tag == "Fuel management"]


#quick tag look:
cols_to_combine <- c("Resource Type")
d1[, Tags := do.call(paste, c(.SD, sep = ",")), .SDcols = cols_to_combine]
d2 <- d1[,.(doc_id,Tags)]
d2_long <- d2[, .(tag = unlist(strsplit(Tags, "\\n\\n|,"))), by = doc_id]
d2_long <- d2_long[, .(tag = unlist(strsplit(tag, "\\n"))), by = doc_id]
d2_long[, tag := trimws(tag)]
d2_long <- d2_long[tag != ""]

unique(d2_long$tag)

ggplot(data = d2_long) +
  geom_histogram(aes(x = tag), stat = "count")

d2_long[tag == "Fuel management"]

#quick tag look:
cols_to_combine <- c("Zonation")
d1[, Tags := do.call(paste, c(.SD, sep = ",")), .SDcols = cols_to_combine]
d2 <- d1[,.(doc_id,Tags)]
d2_long <- d2[, .(tag = unlist(strsplit(Tags, "\\n\\n|,"))), by = doc_id]
d2_long <- d2_long[, .(tag = unlist(strsplit(tag, "\\n"))), by = doc_id]
d2_long[, tag := trimws(tag)]
d2_long <- d2_long[tag != ""]
d2_long[, tag := gsub('^"+|"+$', '', tag)]
d2_long[, tag := gsub("(^\"\"|\"\"$)", "", tag)]

frequency(d2_long$tag)

unique(d2_long$tag)

ggplot(data = d2_long) +
  geom_histogram(aes(x = tag), stat = "count")

d2_long[tag == "Fuel management"]
