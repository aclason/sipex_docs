library(data.table)

in_dir <- "downloads"
out_dir <- "processed"

d1 <- fread(file.path(in_dir,"Collection 4_ Document Collection and Tagging (Working Doc) - Tag List.csv"),
            skip = 1)
colnames(d1)

cols_to_keep_1 <- c("HABITAT", "FOREST PRODUCTS", "TENURE", "ZONATION",
                    "STAND DEVELOPMENT", "TECHNIQUE 2", "BEC ZONE","PRACTICE",
                    "VALUE", "TECHNIQUE")

cols_to_split_all <- c("TREE SPECIES", "BEC Zone_Subzone_Variant",
                       "GEOGRAPHICAL AREA - PROVINCIAL/STATE",
                       "GEOGRAPHICAL AREA - REGIONAL")

cols_already_split <- c("TREE TYPE", "WILDLIFE SPECIES", "EQUIPMENT",
                        "DISTURBANCE TYPE", "TECHNIQUE 2","Research Forest",
                        "GEOGRAPHICAL AREA - NATIONAL", "FOREST TYPE",
                        "DATE PUBLISHED", "RESOURCE TYPE","ORGANIZATION")

# keep first element in tags with description etc.
d2 <- copy(d1)
for (col in cols_to_keep_1) {
  d2[, (col) := trimws(strsplit(as.character(get(col)), "[\r\n]+")[[1]])[1], by = seq_len(.N)]
}

d2[, row_id := .I]




cleaned <- d1[, .(TECHNIQUE, `TREE SPECIES`)]

cleaned[, c("Technique", "Description") := tstrsplit(TECHNIQUE, "\n\n", fixed = TRUE)]

d2 <- d1[, c("Tree_species_1", "Tree_species_2", "Tree_species_3") := tstrsplit(`TREE SPECIES`,
                                                                                "\n", fixed = TRUE, keep = 1:3)]


cleaned <- data.table(
  Technique   = tstrsplit(d1$TECHNIQUE, "\n\n", fixed = TRUE)[[1]],
  Description = tstrsplit(d1$TECHNIQUE, "\n\n", fixed = TRUE)[[2]],
  SpeciesCode = tstrsplit(d1$`TREE SPECIES`, "\n", fixed = TRUE)[[1]],
  CommonName  = tstrsplit(d1$`TREE SPECIES`, "\n", fixed = TRUE)[[2]],
  LatinName   = tstrsplit(d1$`TREE SPECIES`, "\n", fixed = TRUE)[[3]]
)

cols_to_clean <- c("TECHNIQUE", "ANOTHER_COL", "YET_ANOTHER_COL")  # up to 20 columns
