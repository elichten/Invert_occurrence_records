# This script is for testing out the BeeBDC package and developing our workflow 
# for assessing TX native bee conservation status. It should be kept as a 
# working document (without deleting old code and comments), and not converted 
# into a clean workflow.

# Last updated: "2025-01-31 14:33:39 CST" (RLW)
# Authors: Elinor M. Lichtenberg, Avery Pearson, Robert Whyle

#************************************
#************************************


# Project Setup -----------------------------------------------------------


#****************
## Install packages----

# NOTE: We've only provided code for the installation of BeeBDC-related packages below, but you will also need to install packages "countrycode", "rgbif","spocc","taxize", and "tidyverse" to run this script. For more information on how to install R packages, see this guide: https://cran.r-project.org/doc/manuals/r-patched/R-admin.html
# Once you've installed these packages, you won't need to run this section of code again. Instead, jump right to the 'Load libraries' section and start from there.

# First, install BiocManager, devtools, ComplexHeatmap, and rnaturalearthhires. Complete these installations prior to installing BeeBDC
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager", repos = "http://cran.us.r-project.org")
BiocManager::install("ComplexHeatmap")

# Install remotes if needed
if (!require("remotes", quietly = TRUE)) install.packages("remotes", repos = "http://cran.us.r-project.org")

# Download and load rnaturalearthhires
# NOTE: The order of installation matters here. Make sure this is done BEFORE installing BeeBDC. If the below code isn't working, then reference the github page (https://github.com/ropensci/rnaturalearthhires) for download instructions
remotes::install_github("ropensci/rnaturalearthhires")
install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev",type = "source") 

# Install BeeBDC
install.packages("BeeBDC",
                 dependencies = T)

# Install gnparser and rgnparser
  # gnparser needs to be installed and up to date before you can install/run rgnparser. Make sure you have latest R, R studio, and packages. Then, follow this link for instructions on how to install gnparser on your device (windows/linux/mac): https://github.com/gnames/gnparser#installation
  # When you get to the step where you need to move the file to a specific location on your computer, we recommend putting it in the same location as your R packages. 
  # After you've successfully installed gnparser, then go ahead and install rgnparser, like you would a normal R package (e.g., 'install.packages('rgnparser')')
  
#****************
### Load libraries----

# NOTE: Jump here if all above packages have already been installed
library(rgbif)
library(ritis)
library(rgnparser)
library(spocc)
library(tidyverse)
library(rnaturalearthhires) ## Ensure that this is loaded BEFORE BeeBDC
library(BeeBDC)
library(ComplexHeatmap)
library(magrittr)
library(sf)
library(tigris)
library(raster)
library(red)
library(purrr)

#****************
## Set up environment----

# Set root folder - these file paths may need to be adjusted based on your file path location
# Default root is the directory containing this .Rproj file
RootPath <- here::here()

# Create the working directory in the RootPath if it doesn't exist already
if (!dir.exists(file.path(RootPath, "Data_acquisition_workflow"))) {
  dir.create(file.path(RootPath, "Data_acquisition_workflow"), recursive = TRUE)
}

# Set the working directory (BeeBDC uses this for file output)
setwd(file.path(RootPath, "Data_acquisition_workflow"))

# Set up directories — Run each time you use the script.
BeeBDC::dirMaker(RootPath = RootPath, 
                 RDoc = "Lichtenberg_invert_occurrence_records.R") %>%
  # Adds paths created by this function to the environment()
  list2env(envir = parent.env(environment()))

# Add Additional_Datasets path and directory if not found
if (!dir.exists(file.path(DataPath, "Additional_Datasets"))) {
  dir.create(file.path(DataPath, "Additional_Datasets"), recursive = TRUE)
}
AddlDataPath <- file.path(DataPath, "Additional_Datasets")

# Check if the Cleaned directory exists, and if not, create it
dir.create(file.path(OutPath, "Cleaned"), showWarnings = FALSE)

# Store the default par settings so they can be restored
.pardefaults <- par(no.readonly = T)

# Load custom scripts needed for this project
R.utils::sourceDirectory(file.path(DataPath, "Scripts"))

#************************************
#************************************


# Create Species List -----------------------------------------------------
# if you do not have/do not want to use a species list, simply skip this section and proceed to the create taxonomyfile section. Forgoing a species list is useful when you want to generate a species list for a given area.

## Option 1: read in text file with species list ------------------------
# file should be a .csv and simply list out all the species names in one column (do not include column names)
# make sure to adjust the line of code below to reflect the name of your species list file
sppList <- read_csv('your_species_list.csv', show_col_types = FALSE)[[1]] 

## Option 2: Create a list of accepted canonical (no authorship included) names of your species of interest. -------------------------
# This list will be used to filter and reduce the size of backbones. 
# It can also be used for querying occurrence records in later steps.
sppList <- c("Apis mellifera",
             "Bombus pensylvanicus",
             "Halictus ligatus",
             "Xylocopa virginica")

# If the user is unsure of the taxonomic status of listed names, they can be 
# harmonized with the backbones available through APIs.
sppList <- c("Apis mellifica",  # Using outdated synonym of Apis mellifera
             "Bombus pensylvanicus",
             "Halictus ligatus",
             "Xylocopa virginica")

# Optional: Use GBIF backbone to convert names on sppList to accepted names
sppListHarmonized <- rgbif::name_backbone_checklist(sppList) #Checking names using GBIF taxonomic backbone
sppList <- sppListHarmonized$species # Output list of accepted names

#************************************
#************************************


# Create taxonomyfile -----------------------------------------------------


#****************
## Taxonomic backbone from a single source----

# These key columns are required for taxonomyFile:
# Enter "?BeeBDC::testTaxonomy" into the console for more information

      # taxonomic_status (e.g., "invalid", "accepted", "synonym")
      # source           (e.g., "DiscoverLife", "MyDataset")
      # accid            (e.g., "10094", "0")
      # id               (e.g., "10094", "291844")
      # canonical        (e.g., "Osmia bicolor")
      # authorship       (e.g., "(Schrank, 1781)")
      # taxon_rank       (e.g., "Species", "Subspecies")
      # valid            ("TRUE" if this is an accepted taxon, "FALSE" if not)
      # kingdom
      # phylum
      # class
      # order
      # family
      # genus
      # species

#******
### Option 1: read in local taxonomic backbone----
# If users have a backbone made prior to this workflow, it can be read directly into taxonomyFile for use in later steps. Ensure that column names and types match BeeBDC format (for reference, download a small test backbone using taxadbToBeeBDC()). 
taxonomyFile <- read_delim(file.path(DataPath,"your_taxon_filename.csv")) %>%
                check_taxon_table() # Verify that table has required columns

#******
### Option 2: BeeBDC download of DiscoverLife's Backbone----
# Discover Life's backbone for Anthophila comes with the BeeBDC download via beesTaxonomy()
taxonomyFile <- beesTaxonomy_fixed()

#******
### Option 3: download taxonomic backbone from API----
# It's best to download backbones at higher taxonomic level encompassing all species of interest to capture as many synonyms as possible. The backbone can then be filtered using sppList. 
# taxadbToBeeBDC() can download backbones from several sources in BeeBDC format. 
# Check the details of ?taxadb::td_create() for a list of provider options
taxonomyFile <- taxadbToBeeBDC(name = "Hymenoptera",
                               rank = "Order",
                               ignore_case = FALSE, # ignore_case = FALSE needs to be specified when querying Order, otherwise there is SQL error
                               provider = "gbif")

#****************
## Taxonomic backbone from multiple sources----

### Read in source backbones----
# This process is different from above since the backbones need to be joined together, requiring a new join column and distinct column names among data frames. You do not need to use all four backbone options provided here (Discover Life, GBIF, ITIS, personal backbone), simply run the sections below with the backbones you are planning to use. 

#******
#### Discover Life----
# Read in Discover Life taxonomy from BeeBDC. See option 2 from the single-source backbone instructions above for troubleshooting.
taxDL <- beesTaxonomy_fixed() %>%
         format_provider_data(., 'dl') # Format table for joining
  
#******
#### GBIF----
taxGBIF <- taxadbToBeeBDC(name = "Hymenoptera",
                          rank = "Order",
                          ignore_case = FALSE, # ignore_case = FALSE needs to be specified when querying Order, otherwise there is SQL error
                          provider = "gbif") %>%
           format_provider_data(., 'gbif') # Format table for joining

#******
#### ITIS----
taxITIS <- taxadbToBeeBDC(name = "Hymenoptera",
                          rank = "Order",
                          ignore_case = FALSE,
                          provider = "itis") %>%
            # This step uses ritis::taxon_authorship() to fill the authorship column, which can take a long time for large datasets
            # Set getAuthorship=FALSE if you want to skip this step.
            format_provider_data(., "itis", getAuthorship=FALSE) # Format table for joining

#******
#### Local taxonomic backbone----
# TODO: Test this more thoroughly
taxCustom <- read_delim(file.path(DataPath,"your_file_name.csv")) %>%
                        check_taxon_table(.) %>% # Verify that table has required columns
                        format_provider_data(., "custom")

#******
### Merge backbones with priority----

#### Join taxonomic backbones

# Take possible tables from above. Modify the possibleTables list based on the backbones you are using. 
possibleTables <- c("taxDL", "taxGBIF", "taxITIS", "taxCustom")
# Join tables that are currently being used in the session
joinedTaxonomy <- possibleTables %>%
        lapply(function(x) if (exists(x)) get(x) else NULL) %>%
        purrr::compact() %>%
        reduce(~ full_join(.y, .x, by="joinCol"))

# Coalesce each column in the three tables into a single column, using the first non-NA value encountered.
# Table priority is determined by the order in which the table names are provided to the mergeTaxonomy() function.
# Again, if you are not using all four backbone options, please modify the argument in the merge_taxonomy() function below. 
mergedTaxonomy <- merge_taxonomy(joinedTaxonomy, c('dl', 'gbif', 'itis', 'custom'))

#### Correct orphaned accids

# Create a reference table for instances of orphaned accids. 
# This happens when a unique synonym occurs in a lower-priority taxonomic 
# backbone but refers to an accepted name shared by a higher priority backbone.
acceptedIDs <- mergedTaxonomy %>%
  filter(taxonomic_status == "accepted") %>%
  dplyr::select(id, canonical) %>%
  rename(accid = id)

# Use reference table to reassign accids based on current accepted ids.
taxonomyFile <- mergedTaxonomy %>%
  left_join(
    acceptedIDs,
    by = join_by(canonical_accepted == canonical),
    suffix = c("", "_replacement"),
    na_matches="never" # Prevents explosion of NA matches
  ) %>%
  mutate(accid = if_else(taxonomic_status != "accepted", 
                         accid_replacement, 
                         accid)) %>%
  dplyr::select(-canonical_accepted,-authorship_accepted,-accid_replacement)


#### Remove spent databases

# Determine which variables are being used in environment
existingVariables <- c("acceptedIDs","joinedTaxonomy","mergedTaxonomy","taxDL",
                       "taxGBIF","taxITIS", "taxCustom") %>%
                     get_existing_variables(.) # Get currently in-use variables

rm(list = existingVariables, existingVariables) # Remove existing variables


#****************
## Filter taxonomyFile to species of interest and their synonyms----

# This function accepts a list of scientific names from a single taxonomic level
taxonomyFile <- filter_by_taxon(table = taxonomyFile, 
                                rank = "species", 
                                names = sppList)

#************************************
#************************************


# Import Occurrence Records -----------------------------------------------


#****************
## Query occurrence records from GBIF----

# Users have the option of using rgbif::occ_search or rgbif::occ_download. occ_search() is only meant for smaller requests and generally not recommended for research. For larger requests, use rgbif::occ_download.
# See https://docs.ropensci.org/rgbif/articles/downloading_a_long_species_list.html for another example and further explanation.#

# Compile list of all species names from taxonomyFile 
# (filter to sppList in previous step if desired. Having a pared down list will help speed up run times as you proceed in the code.)
fullSppList <- taxonomyFile$canonical %>%
               unique()

# Get the taxon keys for each species in fullSppList 
# This can take a long time for long lists
gbifTaxonKeys <- fullSppList %>% 
  name_backbone_checklist(strict = T, kingdom = "Animalia") %>% # Change kingdom to your use-case
  filter(matchType == "EXACT") %>% # get matched names
  pull(usageKey) %>%
  unique() # Filter out duplicates caused by spelling differences

# Set up your environment with your GBIF credentials. If you do not have a GBIF account, please create one before running this section!
check_gbif_creds() # Securely input your GBIF credentials into local environment

# Intialize download of occurrence data, providing search parameters as needed
# This search uses the gbifTaxonKey values generated in the previous step
# Read more about adding custom predicates here: https://www.rdocumentation.org/packages/rgbif/versions/3.8.0/topics/download_predicate_dsl
occ_download(
  pred_in("taxonKey", gbifTaxonKeys),  # Search for all taxonKeys in the list
  pred("occurrenceStatus", "PRESENT"),
  pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN", "MATERIAL_SAMPLE"))), #Excluding these types bases of record
  pred_or(
    pred_lte("coordinateUncertaintyInMeters", 2000), #Uncertainty less than or equal to 2000 m.
    pred_isnull("coordinateUncertaintyInMeters")
  ),
  pred_or(
    pred_gte("year", 1960),
    pred_isnull("year")
  ),
  pred("hasCoordinate", TRUE),
  ## Uncomment the section below to define a bounding box. Otherwise global occurrence data will be downloaded.
  # pred_or(
  #   pred_in("stateProvince",c("Texas","Arkansas","Louisiana","Oklahoma","New Mexico","Tamaulipas", "Nuevo Leon", "Coahuila","Chihuahua")),
  #   pred_within("POLYGON ((-107.24 25.6, -107.24 36.88, -92.52 36.88, -92.52 25.6, -107.24 25.6))")
  #),
  format = "DWCA"  # DarwinCore (DWCA) format preserves helpful fields like "verbatimEventDate"
)

# After executing the download, a message will be output in the console with
# various instructions for viewing your download, as well as a Download Key.
# Copy and paste your resulting Download Key into the function below.

# Load the downloaded dataset and save to Data_acquisition_workflow/Additional_Datasets
occDatasets <- get_typed_gbif_download("download_key") %>%
               format_gbif_import(.) %>% # Format table to fit BeeBDC template
               add_occ_records(table = .,
                               mainList = occDatasets,
                               datasetName = "gbifOccs")

# Or load a previously downloaded GBIF database
# This step can also handle occurrence.txt files from a DWCA download
occDatasets <- read_delim(file.path(AddlDataPath, "your_file_name.csv"), 
                          delim = '\t', # GBIF's downloads are tab-delimited — change the delim argument if needed
                          col_types = (c(BeeBDC::ColTypeR()$cols))) %>%
               format_gbif_import(.) %>% # Format table to fit BeeBDC template
               add_occ_records(table = .,
                               mainList = occDatasets,
                               datasetName = "gbifOccs_local")


#****************
## Import local geographically-relevant datasets (BeeBDC examples)----


# These key columns are required for occurrence data: 

  ## These columns are from the BeeBDC workflow
      # dataSource        (e.g., "GBIF", "BMin_Anthophila", "My_dataSource")
      # database_id       (e.g., "GBIF_22", "BMin_data_1093")

  ## These columns follow DarwinCore format (https://dwc.tdwg.org/terms/)
      # scientificName
      # decimalLatitude
      # decimalLongitude
      # basisOfRecord
      # countryCode
      # country 
      # locality
      # occurrenceStatus
      # catalogNumber
      # occurrenceID
      # eventDate
      # recordedBy
      # datasetName
      # coordinateUncertaintyInMeters
      # year
      # month
      # day

#******
# TODO: Test more thoroughly
### Import a custom dataset
# Provide a filename and a name for your dataset and make sure your dataset includes all the necessary columns (listed above)
occDatasets <- add_occ_records(filepath = file.path(AddlDataPath,
                                                    "your_file_name.csv"),
                               datasetName = "my_dataset",
                               mainList = occDatasets)

#******

### Import Bee data provided in BeeBDC package that are relevant to Texas (if desired)
# NOTE: These will spit out a lot of unimportant warnings about column names

# Create an empty occDatasets list to collect imported datasets (if needed)
if (!exists("occDatasets")) {
  occDatasets <- list()
}

# Ballare et al., 2019 (Bal)
occDatasets$Bal_std <- readr::read_csv(file.path(AddlDataPath,
                                                 "jbd_Bal_data.csv"),
                                       col_types = BeeBDC::ColTypeR()) %>%
                       mutate(city = word(locality, 1, sep = ","),
                              county = word(locality, 2, sep = ","),
                              state = word(locality, 3, sep = ","),
                              country = "USA", # Assuming all localities are in the USA
                              countryCode = "US", # Assuming country code for the USA is "US"
                              basisOfRecord = "HUMAN_OBSERVATION")

# Bob Minckley data (BMin)
occDatasets$BMin_std <- readr::read_csv(file.path(AddlDataPath,
                                                  "jbd_BMin_data.csv"),
                                        col_types = BeeBDC::ColTypeR())

# Ecdysis data (ECD)
occDatasets$Ecd_std <- readr::read_csv(file.path(AddlDataPath,
                                                 "jbd_Ecd_data.csv"),
                                       col_types = BeeBDC::ColTypeR())

# Florida State Collection of Arthropods (FSCA)
occDatasets$FSCA_std <- readr::read_csv(file.path(AddlDataPath,
                                                  "jbd_FSCA_data.csv"),
                                        col_types = BeeBDC::ColTypeR())

# US Geological Survey data (Geol)
occDatasets$GeoL_std <- readr::read_csv(file.path(AddlDataPath,
                                                  "jbd_GeoL_data.csv"),
                                        col_types = BeeBDC::ColTypeR())

# Illinois Natural History Survey data (INHS)
occDatasets$INHS_std <- readr::read_csv(file.path(AddlDataPath,
                                                  "jbd_INHS_data.csv"),
                                        col_types = BeeBDC::ColTypeR())

# K.A. Parys 2019 (KP)
occDatasets$KP_std <- readr::read_csv(file.path(AddlDataPath,
                                                "jbd_KP_data.csv"),
                                      col_types = BeeBDC::ColTypeR())

# Shannon Collins data set (SMC)
occDatasets$SMC_std <- readr::read_csv(file.path(AddlDataPath,
                                                 "jbd_SMC_Data.csv"),
                                       col_types = BeeBDC::ColTypeR()) %>%
                       mutate(eventDate = as.character(eventDate))

# Paige Chesshire's data (PaigeNAm)
occDatasets$PaigeNAm <- readr::read_csv(file.path(AddlDataPath,
                                                  "Paige_data", 
                                                  "NorAmer_highQual_only_ALLfamilies.csv"), 
                                        col_types = BeeBDC::ColTypeR()) %>%
                        # Change the column name from Source to dataSource to match the rest of the data.
                        dplyr::rename(dataSource = Source) %>%
                        # add a NEW database_id column
                        dplyr::mutate(
                          database_id = paste0("Paige_data_", 1:nrow(.)),
                          .before = scientificName)

# Other datasets from Dorey 2023 can be loaded in here, if needed
## (MABC)
# occDatasets$MABC_std <- readr::read_csv(file.path(AddlDataPath,
#                                                  "jbd_MABC_data.csv"),
#                                         col_types = BeeBDC::ColTypeR())

## EcoS
# occDatasets$EcoS_std <- readr::read_csv(file.path(AddlDataPath,
#                                                  "jbd_EcoS_data.csv"),
#                                         col_types = BeeBDC::ColTypeR())


## Lichtenberg data
# occDatasets$Lic_std <- readr::read_csv(file.path(AddlDataPath,
#                                                  "jbd_Lic_Data.csv"))%>%
#                        mutate(eventDate = as.character(eventDate))


#****************
## Merge all occurrence data----

# Merge datasets
db_standardized <- bind_rows(occDatasets) %>%
                   format_occ_table(.) # Fill missing columns

# Remove spent datasets
rm(occDatasets)

# Save the dataset
readr::write_excel_csv(db_standardized, file.path(OutPath_Intermediate, 
                                                  "00_prefilter_database.csv"))


#****************
# If db_standardized has been previously created and saved, read in here:
if(!exists("db_standardized")){
  db_standardized <- readr::read_csv(file.path(OutPath_Intermediate, 
                                               "00_prefilter_database.csv"), 
                                     col_types = BeeBDC::ColTypeR())
}

#****************
## (Optional) 
# Load sample datasets provided by BeeBDC package for working through the code. 
# Only do this if you want to quickly run through the workflow with a dataset 
# small enough to run quickly even in RAM-heavy operations

data("bees3sp", package = "BeeBDC")
data("beesRaw", package = "BeeBDC")

db_standardized <- 
  dplyr::bind_rows(beesRaw, 
                   bees3sp %>% # Only keep a subset of columns from bees3sp
                     dplyr::select(all_of(colnames(beesRaw)), countryCode))

#************************************
#************************************


# Flag Data ---------------------------------------------------------------


# NOTE: These flags can be counter intuitive. 
#   A value of FALSE in the ".coordinates_empty" columns indicates that the row 
#   was flagged for empty coordinates (and vice versa)


#************************************

## Taxonomy flags ----

#****************  
### Harmonize and flag taxonomy----

#### Prep data names
# Make sure names_clean column doesn't exist before starting harmonization
db_standardized <- dplyr::select(db_standardized, 
                                 !tidyselect::any_of("names_clean"))

# Clean the scientificName column
parse_names <- db_standardized %>%
               dplyr::select(., !tidyselect::any_of("names_clean")) %>%
               {bdc::bdc_clean_names(sci_names = .$scientificName)}

# Keep only the .uncer_terms and names_clean columns
parse_names <- dplyr::select(parse_names, .uncer_terms, names_clean)

# Merge names with complete dataset
check_taxa <- dplyr::bind_cols(db_standardized, parse_names)

rm(parse_names, db_standardized)


# To re-start in the middle of the flagging and harmonization process, read in the most recent intermediate file you outputted, replacing "03_space_inter_database.csv" with that file's name.
if (!exists("check_taxa")) {
  check_taxa <- readr::write_excel_csv(check_taxa, file.path(DataPath, 
                                                             "Output",
                                                             "Intermediate",
                                                             "02_taxonomy_database.csv"))
}


# Harmonize occurrence data and flag those that do not match the taxonomyFile
# Occurrence records not found in the taxonomy file will be flagged with ".invalidName' column.
# You can also use multiple cores to achieve this. See ‘?harmoniseR()’ for details.
check_taxa <- prepare_harmonise(taxonomyFile) %>% # Add empty "flags" and "canonical_withFlags" columns if missing
              BeeBDC::harmoniseR(path = DataPath, #The path to a folder that the output can be saved
                                 taxonomy = ., # The formatted taxonomy file
                                 data = check_taxa,
                                 mc.cores = 1)

# Remove taxonomy file
rm(taxonomyFile)

# Save the harmonized file
readr::write_excel_csv(check_taxa, file.path(DataPath, 
                                             "Output",
                                             "Intermediate",
                                             "02_taxonomy_database.csv"))

#****************  
### Filter taxonomy----

# Optional (But recommended)
# Filter occurrence records to those which relate to your taxonomyFile
# This can help to reduce the workload on some of the upcoming functions
check_taxa <- subset(check_taxa, .invalidName == TRUE)

#************************************

## Initial misc flags ----

#****************
### Flag occurrences without scientificName present----

# Adds column ".scientificName_empty"
check_pf <- bdc::bdc_scientificName_empty(data = check_taxa,
                                          sci_names = "scientificName")

#****************
### Flag occurrences with missing decimalLatitude and decimalLongitude----

# Adds column ".coordinates_empty"
check_pf <- bdc::bdc_coordinates_empty(data = check_pf, 
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude")

#****************
### Flag occurrences that are not on Earth----

# Adds column ".coordinates_outOfRange"
check_pf <- bdc::bdc_coordinates_outOfRange(data = check_pf, 
                                            lat = "decimalLatitude",
                                            lon = "decimalLongitude")

#****************
### Flag occurrences that don't match basisOfRecord types ----  

# user decides what the acceptable forms of data are here - modify as necessary
# Adds column ".basisOfRecords_notStandard"
check_pf <- bdc::bdc_basisOfRecords_notStandard(
  data = check_pf,
  basisOfRecord = "basisOfRecord",
  names_to_keep = c(
    ### Keep all plus some at the bottom.
    "Event",
    "HUMAN_OBSERVATION",
    "HumanObservation",
    "LIVING_SPECIMEN",
    "LivingSpecimen",
    "MACHINE_OBSERVATION",
    "MachineObservation",
    "MATERIAL_SAMPLE",
    "O",
    "Occurrence",
    "MaterialSample",
    "OBSERVATION",
    "Preserved Specimen",
    "PRESERVED_SPECIMEN",
    "preservedspecimen Specimen",
    "Preservedspecimen",
    "PreservedSpecimen",
    "preservedspecimen",
    "S",
    "Specimen",
    "Taxon",
    "UNKNOWN",
    "",
    NA,
    "NA",
    "LITERATURE", 
    "None", "Pinned Specimen", "Voucher reared", "Emerged specimen"
  ))

#****************
### Harmonize country names----

# Create a separate dataset with harmonized country names and associated info
check_pf_noNa <- BeeBDC::countryNameCleanR(
  data = check_pf,
  # Create a Tibble of common issues in country names and their replacements
  commonProblems = dplyr::tibble(problem = c('U.S.A.','US','USA','usa',
                                             'UNITED STATES','United States',
                                             'U.S.A',"México"),
                                 fix = c('United States of America','United States of America',
                                         'United States of America','United States of America',
                                         'United States of America','United States of America',
                                         'United States of America',"Mexico")))

# Get country name from coordinates using a wrapper around the jbd_country_from_coordinates() function. Because our dataset is much larger than those used to design bdc, we have made it so that you can analyse data in smaller pieces. Additionally, like some other functions in BeeBDC, we have implemented parallel operations (using mc.cores = #cores in stepSize = #rowsPerOperation); see ‘?jbd_CfC_chunker()’ for details. NOTE: In an actual run you should use scale = “large”
# Creates another dataset containing just the databaseID and the country name based on coordinates
suppressWarnings(
  countryOutput <- BeeBDC::jbd_CfC_chunker(data = check_pf_noNa,
                                           lat = "decimalLatitude",
                                           lon = "decimalLongitude",
                                           country = "country",
                                           # How many rows to process at a time
                                           stepSize = 10000, # Decrease if you're running out of RAM, increase for speed
                                           # Start row
                                           chunkStart = 1,
                                           path = OutPath_Intermediate,
                                           # Normally, please use scale = "large"
                                           scale = "large",
                                           mc.cores = 1),
                   classes = "warning")

# Re-join these datasets
check_pf <- dplyr::left_join(check_pf, 
                             countryOutput, 
                             by = "database_id", 
                             suffix = c("", "CO")) %>%
            # Take the new country name if the original is NA
            dplyr::mutate(country = dplyr::if_else(is.na(country), 
                                                   countryCO,
                                                   country)) %>%
            # Remove duplicates if they arose from left_join!
            dplyr::distinct()

# Save the dataset
readr::write_excel_csv(check_pf, file.path(OutPath_Intermediate, 
                                           "01_prefilter_database.csv"))

# Remove these interim datasets
rm(check_pf_noNa, countryOutput)

# Read in previously saved dataset if necessary
if (!exists("check_pf")) {
  check_pf <- readr::read_csv(file.path(DataPath, 
                                        "Output", 
                                        "Intermediate", 
                                        "01_prefilter_database.csv"), 
                              col_types = BeeBDC::ColTypeR())
}

#****************
### Standardize country names and add ISO2 codes, if needed----

# Adds two columns: "country_suggested" and "countryCode", removes these columns prior to running in an instance when the dataset contains these columns from previous runs. 
# Standardise country names and add ISO2 codes if needed
check_pf <- bdc::bdc_country_standardized(
  # Remove the countryCode and country_suggested columns if they were already
  # created in a previous run
  data = check_pf %>% dplyr::select(!tidyselect::any_of(c("countryCode", "country_suggested"))),
  country = "country"
) 

#****************
### Correct transposed coordinates----

# Flag and correct records when decimalLatitude and decimalLongitude appear to be transposed. We created this chunked version of bdc::bdc_coordinates_transposed() because it is very RAM-heavy  using our large bee dataset. Like many of our other ‘jbd_…’ functions there are other improvement - e.g., parallel running.
# NOTE: Usually you would use scale = “large”, which requires rnaturalearthhires
# Adds column "coordinates_transposed" if transpositions are made
# USE SAMPLE DATASET IF TESTING THIS FUNCTION (TAKES LONG AND USES A LOT RAM)
check_pf <- BeeBDC::jbd_Ctrans_chunker(
  # bdc_coordinates_transposed inputs
  data = check_pf,
  idcol = "database_id",
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country",
  countryCode = "countryCode",
  border_buffer = 0.2, # in decimal degrees (~22 km at the equator)
  save_outputs = TRUE,
  sci_names = "scientificName",
  # chunker inputs
  stepSize = 1000000,  # How many rows to process at a time
  chunkStart = 1,  # Start row
  append = FALSE,  # If FALSE it may overwrite existing dataset
  progressiveSave = FALSE,
  # In a normal run, please use scale = "large"
  scale = "large",
  path = OutPath_Check,
  mc.cores = 1
) 

# If transpositions were made, use this to view how many were corrected
# Will output a warning if there are no transposed occurrences
table(check_pf$coordinates_transposed, useNA = "always") # NOTE: F = transposed/corrected records, T = already correct records 

# Save the dataset
readr::write_excel_csv(check_pf, file.path(OutPath_Intermediate, 
                                           "01_prefilter_database.csv"))

# Read in the data again if needed
if (!exists("check_pf")) {
  check_pf <- readr::read_csv(file.path(OutPath_Intermediate, 
                                    "01_prefilter_database.csv"), 
                              col_types = BeeBDC::ColTypeR())
}

#****************
### Flag coordinates inconsistent with country names----

# Collect all country names in the country_suggested column. 
# We rebuilt a BDC function to flag occurrences where the coordinates are inconsistent with the provided country name.
# NOTE: A "no simple features geometry column present" error will show if ALL 
#   observations are flagged for .coordinates_empty OR .coordinates_outOfRange
check_pf <- BeeBDC::jbd_coordCountryInconsistent(data = check_pf, 
                                                 lon = "decimalLongitude",
                                                 lat = "decimalLatitude", 
                                                 scale = 50, 
                                                 pointBuffer = 0.01) # Note: Non-breaking database_id warning caused by issues with code

# Save dataset
readr::write_excel_csv(check_pf, file.path(OutPath_Intermediate, 
                                           "01_prefilter_database.csv"))

#****************
### Flag GeoRef Issues----

# This function identifies records whose coordinates can potentially be 
# extracted from locality information, which must be manually checked later.
xyFromLocality <- bdc::bdc_coordinates_from_locality(data = check_pf, 
                                                     locality = "locality",
                                                     lon = "decimalLongitude", 
                                                     lat = "decimalLatitude", 
                                                     save_outputs = FALSE) # Files will save at top-level directory

# Save resultant data
readr::write_excel_csv(xyFromLocality, 
                       file.path(OutPath_Check, 
                                 "01_coordinates_from_locality.csv"))
#************************************

# Optional: correct Georeferencing issues ----

# Using the exported xyFromLocality file (01_coordinates_from_locality.csv) created above, georeference these records using your choice of tools. There are many options to do georeferencing with, here are a few examples:
  # 1. ArcGIS. Learn more about their georeferencing tools here: https://pro.arcgis.com/en/pro-app/latest/help/data/imagery/georeferencing-tools.htm 
  # 2. Google Earth
# take the xyFromLocality file, do georeferencing with that
  # add instructions for the columns that you need to fill in as you georef
  # add resources for doing georeferencing
  # read back in file with georeferenced records
  # join with check_pf db (that has the xyFromLocality records removed) and save as new 02_taxonomy_database file
  # instruct users to restart at top of flagging section
# add option to georeference based on county centroid (be sure to add in coordUncertainty and caveat on precision)
# thinking only based on county for now?? But we'll see


#************************************

# Remove spent data
rm(xyFromLocality)

#****************
### Flag records marked as "absent"----

# You can skip this step if the table is missing occurrenceStatus column
check_pf <- BeeBDC::flagAbsent(data = check_pf,PresAbs = "occurrenceStatus")

#****************
### Flag License----

# Flag the records that may not be used according to their license information.
check_pf <- BeeBDC::flagLicense(data = check_pf,
                                strings_to_restrict = "all",
                                # DON'T flag if in the following dataSource(s)
                                excludeDataSource = NULL)

#****************
### Flag GBIF issues----

# NOTE: Skip this step if you're not using GBIF data

# Flag select issues that are flagged by GBIF
check_pf <- BeeBDC::GBIFissues(data = check_pf, 
                               issueColumn = "issue", 
                               GBIFflags = c("COORDINATE_INVALID",
                                             "ZERO_COORDINATE")
                               )

#****************
### Flag reports----

# Save the flags so far. This function will create a copy of everything that 
# has been flagged up until now. This will be updated throughout the script 
# and can accessed at the end, so be wary of moving files around manually. 
# However, these data  will also still be maintained in the main running file, 
# so this is an optional fail-safe.
flagFile <- BeeBDC::flagRecorder(
  data = check_pf,
  outPath = OutPath_Report,
  fileName = paste0("flagsRecorded_", Sys.Date(), ".csv"),
  # These are the columns that will be kept along with the flags
  idColumns = c("database_id",
                "id",
                "catalogNumber",
                "occurrenceID",
                "dataSource"),
  # TRUE if you want to find a file from a previous part of the script to append to
  append = FALSE)

# Update the .summary column
check_pf <- BeeBDC::summaryFun(
  data = check_pf,
  # Don't filter these columns (or NULL)
  dontFilterThese = NULL,
  # Remove the filtering columns?
  removeFilterColumns = FALSE,
  # Filter to ONLY cleaned data?
  filterClean = FALSE)

# Use bdc to generate reports
# NOTE: Function will error if no observations are flagged
(report <- bdc::bdc_create_report(data = check_pf,
                                  database_id = "database_id",
                                  workflow_step = "prefilter"))

# Save report to Data_acquisition_workflow/Output/Report
readr::write_csv(report$x$data, file.path(OutPath_Report, 
                                          "01_Report_Prefilter.csv"))

#****************
### Save----

# Save the intermediate dataset
readr::write_excel_csv(check_pf, file.path(OutPath_Intermediate,
                                           "01_prefilter_output.csv"))
 
### Save flags----
flagFile <- BeeBDC::flagRecorder(data = check_pf, 
                                 outPath = OutPath_Report, 
                                 fileName = paste0("flagsRecorded_", 
                                                   Sys.Date(),
                                                   ".csv"), 
                                 idColumns = c("database_id",
                                               "id", "catalogNumber", 
                                               "occurrenceID", 
                                               "dataSource"), 
                                 append = TRUE, 
                                 printSummary = TRUE)


#************************************

## Space Flags ---------------------------------------------------------------

#****************
# Read in latest database if needed
if (!exists("check_pf")) {
  check_pf <- readr::read_csv(file.path(OutPath_Intermediate,
                                        "01_prefilter_output.csv"), 
                              col_types = BeeBDC::ColTypeR())
}

#****************
### Coordinate precision----

# Below function identifies records with a coordinate precision below a specified number of decimal places. The major difference between bdc and BeeBDC functions is that jbd_coordinates_precision() will only flag occurences if BOTH latitude and longitude are rounded, while BeeBDC::jbd_coordinates_precision flags if only one is rounded.
check_space <- BeeBDC::jbd_coordinates_precision(data = check_pf, 
                                                 lon = "decimalLongitude",
                                                 lat = "decimalLatitude", 
                                                 ndec = 2)  # number of decimals to be tested

# Remove spent database
rm(check_pf)

# Save resulting file
readr::write_excel_csv(check_space, file.path(OutPath_Intermediate,
                                              "03_space_inter_database.csv"))

#****************
### Common spatial issues----

# Only run for occurrences through clean_coordinates() that are spatially "valid"
tempSpace <- check_space %>%
  dplyr::filter(!.coordinates_empty == FALSE) %>%
  dplyr::filter(!.coordinates_outOfRange == FALSE)

# Flag common spatial issues using functions from the package 
# "CoordinateCleaner", which addresses common issues in biodiversity datasets
tempSpace <-
  CoordinateCleaner::clean_coordinates(
    x =  tempSpace,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    countries = NULL, # Tests if coords are from x country. This is not needed.
    tests = c(
      "capitals",     # records within 0.5 km of capitals centroids
      "centroids",    # records within 1 km around country and province centroids
      "equal",      # records with equal coordinates
      "gbif",         # records within 1 km of GBIF headquarters. (says 1 degree in package, but code says 1000 m)
      "institutions", # records within 100m of zoo and herbaria
      "zeros"       # records with coordinates 0,0
      # "seas"        # Not flagged as this should be flagged by coordinate country inconsistent
    ),
    capitals_rad = 1000,
    centroids_rad = 500,
    centroids_detail = "both", # test both country and province centroids
    inst_rad = 100, # remove zoo and herbaria within 100m
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_scale = 50,
    value = "spatialvalid" # result of tests are appended in separate columns
  ) %>%
  # Remove duplicate .summary column that can be replaced later and turn into a tibble
  dplyr::select(!tidyselect::starts_with(".summary")) %>%
  dplyr::tibble()

# Re-merge the datasets
check_space <- tempSpace %>%
  # Re-bind with the records that were removed earlier
  dplyr::bind_rows(check_space %>%
                     dplyr::filter(.coordinates_empty == FALSE | .coordinates_outOfRange == FALSE))

# Remove temporary dataset
rm(tempSpace)

# Save the intermediate dataset
readr::write_excel_csv(check_space, file.path(OutPath_Intermediate,
                                              "03_space_inter_database.csv"))

#****************
# Diagonal and Grid Issues----

# Load most recent database, if necessary
if (!exists("check_space")) {
  check_space <- readr::read_csv(file.path(OutPath_Intermediate,
                                           "03_space_inter_database.csv"), 
                                 col_types = BeeBDC::ColTypeR())
}

# Finds sequential numbers that could be fill-down errors in lat and long, and groups by the ‘groupingColumns'. This is accomplished by using a sliding window with the length determined by minRepeats. Only coordinates of precision ‘ndec’ (number of decimals in decimal degree format) will be examined. Note, that this function is very RAM-intensive and so the use of multiple threads should be approached with caution depending on your data set. However, the option is provided.
check_space <- BeeBDC::diagonAlley(
  data = check_space,
  # The minimum number of repeats needed to find a sequence in for flagging
  minRepeats = 6,
  ndec = 3,
  groupingColumns = c("eventDate",
                      "recordedBy",
                      "datasetName"),
  mc.cores = 1)

# Spatial gridding from rasterization: Select only records with more than X occurrences
griddingDF <- check_space %>%
                # Exclude NA lat and lon values
                tidyr::drop_na(c("decimalLatitude",
                                 "decimalLongitude")) %>%
                # Group by the dataset name
                dplyr::group_by(datasetName) %>%
                # Remove rows that aren't unique for lat and long
                dplyr::distinct(decimalLongitude, 
                                decimalLatitude,
                                .keep_all = TRUE) %>%
                # Find the groups with 4 or more occurrence records
                dplyr::filter(dplyr::n() >= 4) %>%
                dplyr::ungroup()

# Run the gridding analysis to find datasets that might be gridded.
#***NOTE: for longer datasets, this can take a while to run (>20 min.)
gridded_datasets <- CoordinateCleaner::cd_round(x = griddingDF, 
                                                lon = "decimalLongitude",
                                                lat = "decimalLatitude",
                                                ds = "datasetName",
                                                T1 = 7,
                                                min_unique_ds_size = 4,
                                                test = "both",
                                                value = "dataset",
                                                graphs = TRUE,
                                                verbose = TRUE,
                                                reg_out_thresh = 2,
                                                # Minimum gridding distance to be flagged (in degrees)
                                                reg_dist_min = 0.1,
                                                # Maximum gridding distance to be flagged (in degrees)
                                                reg_dist_max = 2) %>%
                    dplyr::tibble()

# The griddingDF is no longer needed. remove it.
rm(griddingDF)

# Integrate results with main dataset
check_space <- check_space %>%
  # Join the datasets
  dplyr::left_join(
    # Select the columns of interest
    dplyr::select(gridded_datasets,
                  dataset,
                  lon.flag,
                  lat.flag,
                  summary),
    by = c("datasetName" = "dataset")) %>%
  # Make new columns with more-consistent naming and change the NA vlaues to = TRUE (not flagged)
  dplyr::mutate(.lonFlag = tidyr::replace_na(lon.flag, TRUE),
                .latFlag = tidyr::replace_na(lat.flag, TRUE),
                .gridSummary = tidyr::replace_na(summary, TRUE)) %>%
  # Remove old columns
  dplyr::select(!c(lon.flag,
                   lat.flag, summary))

# Save the gridded_datasets file for later examination and remove.
readr::write_excel_csv(gridded_datasets, file.path(OutPath_Intermediate,
                                                   "03_space_griddedDatasets.csv"))

# Remove old variable
rm(gridded_datasets)

#****************
### Uncertainty----

# Flag records that exceed a coordinateUncertaintyInMeters threshold (currently set to 1000m)
check_space <- BeeBDC::coordUncerFlagR(data = check_space, 
                                       uncerColumn = "coordinateUncertaintyInMeters",
                                       threshold = 1000)

#****************
### Country Checklist----


# This step identifies mismatches between the Discover Life country checklist 
# and your occurrence dataset, identifying potential misidentifications, outliers, etc.

## Required Checklist Columns

    # validNAme = Same as taxonomyFile$validName: "Scientific name + authorship"  (e.g., "Bremus americanorum (Fabricius, 1775)")
    # iso_a3_eh = Three-letter country codes in the ISO 3166 alpha-3 standard (e.g., "LBN", "MEX", "USA")
    # continent = Continent name (e.g., "North America", "Asia", "Europe")

    # For more information on the above columns, download the provided bee checklist
    # using (checklist <- beesChecklist_fixed())


# Import your own country-level checklist
checklist <- readr::read_delim(file.path(AddlDataPath, "test_checklist.csv")) %>%
             check_checklist(.)

# Or, if working with bee data, import the provided country-level bees checklist
checklist <- beesChecklist_fixed()

# Use this to check for species in observations that aren't present in checklist at all
view(check_space %>%
     filter(!(scientificName %in% checklist$validName)) %>%
     count(scientificName, name = "count"))

check_space <- BeeBDC::countryOutlieRs(checklist = checklist,
                                       data = check_space,
                                       keepAdjacentCountry = TRUE,
                                       pointBuffer = 0.05,
                                       # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
                                       # Smaller numbers will result in much longer calculation times. 
                                       scale = 50,
                                       mc.cores = 1)

# Since version 1.1.2 a new function, BeeBDC::continentOutlieRs(), has been released which is conceptually the same as BeeBDC::countryOutlieRs() except it works on the level of continents. Users might appreciate that generating a continent-level checklist is much easier than a country-level one and so might actually be of more wide-spread use (beyond bees).
check_space <- BeeBDC::continentOutlieRs(checklist = checklist,
                                         data = check_space,
                                         keepAdjacentContinent = FALSE,
                                         pointBuffer = 0.05,
                                         # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
                                         # Smaller numbers will result in much longer calculation times. 
                                         scale = 50,
                                         mc.cores = 1)

# A list of failed species-country combinations and their numbers can be output here
# Use this to investigate any possible issues in your observation data or checklist
view(check_space %>%
       dplyr::filter(.countryOutlier == FALSE) %>%
       dplyr::select(database_id, scientificName, country) %>%
       dplyr::group_by(scientificName) %>%
       dplyr::mutate(count_scientificName = n()) %>%
       dplyr::distinct(scientificName, country, .keep_all = TRUE) %>%
       readr::write_excel_csv(file.path(OutPath_Intermediate, "03_space_failedCountryChecklist.csv"))
     )

#****************
### Map Spatial Errors----

# Assemble maps of potential spatial errors and outliers, either one flag at a time or using the .summary column. 
# First, you need to rebuild the .summary column.
check_space <- BeeBDC::summaryFun(data = check_space,
                                  dontFilterThese = NULL,
                                  removeFilterColumns = FALSE,
                                  filterClean = FALSE)

# Use col_to_map in order to map ONE spatial flag at a time or map the .summary column for all flags.
check_space %>%
  dplyr::filter(.summary == FALSE) %>% # map only records flagged as FALSE
  bdc::bdc_quickmap(
    data = .,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    col_to_map = ".summary",
    size = 0.9
  )

#****************
### Space Report----

# Create the report using bdc
(report <- bdc::bdc_create_report(data = dplyr::tibble(check_space %>%
                                                         dplyr::select(!.uncer_terms)), 
                                  database_id = "database_id",
                                  workflow_step = "space"))

# Save report to Data_acquisition_workflow/Output/Report
readr::write_csv(report$x$data, file.path(OutPath_Report, 
                                          "03_Report_space.csv"))

#****************
### Space Figures----

# Create figures for the spatial data filtering results
# NOTE: If there are no flagged observations to visualize, you will receive a 'dplyr::select()' error
(figures <- BeeBDC::jbd_create_figures(data = dplyr::tibble(check_space %>%
                                                            dplyr::select(!.uncer_terms)), 
                                       path = OutPath_Figures, 
                                       database_id = "database_id",
                                       workflow_step = "space", 
                                       save_figures = TRUE))

### For examining the figures, the options are:

    # .cap = Records around country capital centroid
    # .cen = Records around country or province centroids
    # .dbl = Duplicated coordinates per species
    # .equ = Identical coordinates
    # .otl = Geographical outliers
    # .gbf = Records around the GBIF headquarters
    # .inst = Records around biodiversity institutions
    # .rou = Rounded (probably imprecise) coordinates
    # .urb = Records within urban areas — (Likely not relevant for bees.)

# You can examine the figures. For example, run figures$.rou
figures$summary_all_tests

# Save interim dataset
readr::write_excel_csv(check_space, file.path(OutPath_Intermediate,
                                              "03_space_inter_database.csv"))

# Save the flags so far
BeeBDC::flagRecorder(data = check_space, 
                     outPath = OutPath_Report,
                     fileName = paste0("flagsRecorded_", Sys.Date(), ".csv"), 
                     idColumns = c("database_id",
                                   "id",
                                   "catalogNumber",
                                   "occurrenceID", 
                                   "dataSource"), 
                     append = TRUE, 
                     printSummary = TRUE)

# Save the intermediate dataset
readr::write_excel_csv(check_space, file.path(OutPath_Intermediate,
                                              "03_space_database.csv"))

#************************************

## Time Flags -------------------------------------------------------------

#****************
# Read in the last database, if needed
if (!exists("check_space")) {
  check_time <- readr::read_csv(file.path(OutPath_Intermediate,
                                          "03_space_database.csv"),
                                col_types = BeeBDC::ColTypeR())
} else {
  check_time <- check_space
  # Remove the spent file
  rm(check_space)
}

### Plot histogram of dates

# Define range of dates to filter out those that were parsed incorrectly
earliest_date <- as.POSIXct("1863-01-01")
latest_date <- Sys.time()

# Create histogram
hist(lubridate::ymd_hms(check_time$eventDate, truncated = 5), 
     breaks = 365*10, 
     main = "Histogram of eventDates",
     xlim = c(earliest_date,
              latest_date))

# Further filters to remove unreasonable dates
check_time$year <- ifelse(check_time$year > lubridate::year(Sys.Date()) | check_time$year <
                            1600, NA, check_time$year)

check_time$month <- ifelse(check_time$month > 12 | check_time$month < 1, NA, check_time$month)

check_time$day <- ifelse(check_time$day > 31 | check_time$day < 1, NA, check_time$day)

#****************
### Recover dates----

# The dateFindR() function will search through some other columns in order to find and rescue dates that may not have made it into the correct columns. It will further update the eventDate, day, month, and year columns where these data were a) missing and b) located in one of the searched columns.
# NOTE: This update to the original BeeBDC function is more conservative with date assignments
check_time <- dateFindR_fixed(data = check_time,
                              # Years above this are removed (from the recovered dates only)
                              maxYear = lubridate::year(Sys.Date()),
                              # Years below this are removed (from the recovered dates only)
                              minYear = 1700)

#****************
### No eventDate----

# Flag records missing eventDate
check_time <- bdc::bdc_eventDate_empty(data = check_time, eventDate = "eventDate")

#****************
### Old records----

# This will flag records prior to the date selected. 1970 is frequently chosen for SDM work. You may not need to filter old records at all, so think critically about your use. We have chosen 1950 as a lower extreme.
check_time <- bdc::bdc_year_outOfRange(data = check_time, eventDate = "year", year_threshold = 1950)

#****************
### Time report----

# Create a time report that updates the .summary column
check_time <- BeeBDC::summaryFun(
  data = check_time,
  # Don't filter these columns (or NULL)
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms"),
  # Remove the filtering columns?
  removeFilterColumns = FALSE,
  # Filter to ONLY cleaned data?
  filterClean = FALSE)

#****************
### Time figures----

# Create time results figures
# TODO: Error "object 'i' not found". This is most likely a bug in the create_figures code. (AEP note: this ran without error for me...)
figures <- BeeBDC::jbd_create_figures(data = check_time,
                                      path = DataPath,
                                      database_id = "database_id",
                                      workflow_step = "time",
                                      save_figures = TRUE)

# You can check figures. For example, run figures$year

#****************
### Save flags----

# Save all flags so far
BeeBDC::flagRecorder(data = check_time,
                     outPath = OutPath_Report,
                     fileName = paste0("flagsRecorded_", Sys.Date(), ".csv"),
                     idColumns = c("database_id",
                                   "catalogNumber",
                                   "dataSource"),
                     append = TRUE, 
                     printSummary = TRUE)

#************************************
#************************************


# De-duplication ----------------------------------------------------------


# We will FLAG duplicates here. These input columns can be hacked to de-duplicate as you wish. 
# This function uses user-specified inputs and columns to identify duplicate occurrence records.
# Duplicates are identified iteratively and will be tallied up, duplicate pairs clustered, and sorted at the end of the function. 
# The function is designed to work with Darwin Core data with a database_id column, but it is also modifiable to work with other columns. 
# I would encourage you to see ‘?dupeSummary()’ for more details as this function is quite modifiable to user needs.
check_time <- BeeBDC::dupeSummary(
  data = check_time,
  path = OutPath_Report,
  # options are "ID","collectionInfo", or "both"
  duplicatedBy = "collectionInfo", 
  # The columns to generate completeness info from (and to sort by completeness)
  completeness_cols = c("decimalLatitude",
                        "decimalLongitude",
                        "scientificName",
                        "eventDate"),
  # The columns to ADDITIONALLY consider when finding duplicates in collectionInfo
  collectionCols = c("decimalLatitude",
                     "decimalLongitude",
                     "scientificName",
                     "eventDate", 
                     "recordedBy"),
  # The columns to combine, one-by-one with the collectionCols
  collectInfoColumns = c("catalogNumber",
                         "otherCatalogNumbers"),
  # Custom comparisons — as a list of columns to compare
  # RAW custom comparisons do not use the character and number thresholds
  CustomComparisonsRAW = dplyr::lst(c("catalogNumber",
                                      "institutionCode",
                                      "scientificName")),
  # Other custom comparisons use the character and number thresholds
  CustomComparisons = dplyr::lst(c("occurrenceID", "scientificName"),
                                 c("recordId", "scientificName"),
                                 c("id", "scientificName")),
  # The order in which you want to KEEP duplicated based on data source
  sourceOrder = unique(check_time$dataSource),
  # Paige ordering is done using the database_id prefix, not the dataSource prefix.
  prefixOrder = c("BMin",
                  "INHS",
                  "CAES",
                  "GBIF",
                  "Ecd",
                  "KP",
                  "SCAN",
                  "SMC",
                  "Bal",
                  "MABC",
                  "FSCA",
                  "EcoS"),
  # Set the complexity threshold for id letter and number length
  # minimum number of characters when WITH the numberThreshold
  characterThreshold = 2,
  # minimum number of numbers when WITH the characterThreshold
  numberThreshold = 3,
  # Minimum number of numbers WITHOUT any characters
  numberOnlyThreshold = 5)%>% # END dupeSummary
  dplyr::as_tibble(col_types = BeeBDC::ColTypeR())

# Save the dataset into the intermediate folder
readr::write_excel_csv(check_time, file.path(OutPath_Intermediate, 
                                             "04_2_dup_database.csv"))

# Save flags so far
BeeBDC::flagRecorder(data = check_time,
                     outPath = OutPath_Report,
                     fileName = paste0("flagsRecorded_", Sys.Date(), ".csv"),
                     idColumns = c("database_id",
                                   "id", 
                                   "catalogNumber", 
                                   "occurrenceID", 
                                   "dataSource"), 
                     append = TRUE, 
                     printSummary = TRUE)


#************************************
#************************************


# Plot and Review Duplicates ----------------------------------------------


# Read in most recent file of flagged duplicates, if necessary
if (!exists("duplicates")) {
  duplicates <- BeeBDC::fileFinder(path = DataPath, fileName = "duplicateRun_") %>%
    readr::read_csv()
}

# Set global figure parameters
par(mar = c(2, 2, 2, 2)/2, mfrow = c(1, 1))

# Create chordDiagram
# Many values can be left out depending on desired output, but the below code provides the default values. Since there are no duplicates present in the current dataset, BeeBDC will give an error message
BeeBDC::chordDiagramR(
  # The duplicate data from the dupeSummary function output  
  dupeData = duplicates,
  outPath = OutPath_Figures,
  fileName = "ChordDiagram.pdf",
  # These can be modified to help fit the final pdf that's exported.
  width = 9,
  height = 7.5,
  bg = "white",
  # How few distinct dataSources should a group have to be listed as "other"
  smallGrpThreshold = 3,
  title = "Duplicated record sources",
  # The default list of colour palettes to choose from usign the paleteer package
  palettes = c("cartography::blue.pal", "cartography::multi.pal", 
               "cartography::sand.pal", "cartography::orange.pal", "cartography::red.pal",
               "cartography::purple.pal", "cartography::brown.pal"),
  canvas.ylim = c(-1.0,1.0), 
  canvas.xlim = c(-0.6, 0.25),
  text.col = "black",
  legendX = grid::unit(6, "mm"),
  legendY = grid::unit(18, "mm"),
  legendJustify = c("left", "bottom"),
  niceFacing = TRUE)

# Duplicate histogram

# Read in uncleaned data if necessary
if (!exists("check_time")) {
  beeData <- readr::read_csv(file.path(OutPath_Intermediate, 
                                       "05_unCleaned_database.csv"), 
                             col_types = BeeBDC::ColTypeR())
} else {
  beeData <- check_time
  rm(check_time)
}

# Create a plot with two bar graphs
# One bar shows the absolute number of duplicate records for each source, while the other shows the proportion of records that are duplicated within each data source.
BeeBDC::dupePlotR(
  data = beeData,
  # The outPath to save the plot as
  outPath = OutPath_Figures,
  fileName = "duplicatePlot.pdf",
  # Colours in order: duplicate, kept duplicate, unique
  dupeColours = c("#F2D2A2","#B9D6BC", "#349B90"),
  # Plot size and height
  base_height = 7, base_width = 7,
  legend.position = c(0.85, 0.8),
  # Extra variables can be fed into forcats::fct_recode() to change names on plot
  GBIF = "GBIF", SCAN = "SCAN", iDigBio = "iDigBio", USGS = "USGS", ALA = "ALA", 
  ASP = "ASP", CAES = "CAES", Ecd = "Ecd",
  returnPlot = TRUE
)

#************************************
#************************************


# Georeference records ----------------------------------------------
# add option to georeference based on county centroid (be sure to add in coordUncertainty and caveat on precision)
# thinking only based on county for now?? But we'll see


#************************************
#************************************


# Data Filtering ----------------------------------------------------------


## Identify the outliers and get a list of their database_ids. This would require the source outlier files provided with the BeeBDC paper. These files can further be modified to include more outliers.
# check_time <- BeeBDC::manualOutlierFindeR(
#   data = check_time,
#   DataPath = DataPath,
#   PaigeOutliersName = "removedBecauseDeterminedOutlier.csv",
#   newOutliersName = "^All_outliers_ANB_14March.xlsx",
#   ColombiaOutliers_all = "All_Colombian_OutlierIDs.csv",
#   # A .csv with manual outlier records that are too close to otherwise TRUE records
#   NearTRUE = "nearTRUE.csv",
#   duplicates = duplicates)

# Save uncleaned dataset
beeData <- BeeBDC::summaryFun(data = beeData, 
                                 dontFilterThese = c(".gridSummary",
                                                     ".lonFlag",
                                                     ".latFlag",
                                                     ".uncer_terms",
                                                     ".uncertaintyThreshold"),
                                 removeFilterColumns = FALSE,
                                 filterClean = FALSE) %>%
  readr::write_excel_csv(file.path(OutPath_Intermediate,
                                             "05_unCleaned_database.csv"))
  

#****************
## Filter

# There are several options for filtering here. You do not need to use all of them - select the ones that are most applicable for you!

# read in unCleaned database (if necessary)
if (!exists("beeData")) {
  beeData <- readr::read_csv(file.path(OutPath_Intermediate, 
                                       "05_unCleaned_database.csv"), 
                             col_types = BeeBDC::ColTypeR())
} else {
  beeData <- beeData
}

## Optional filter 1: Clean the check_time dataset by removing flagged occurrences (recommended - gets rid of now-unnecessary flagging columns) ----
cleanData <- BeeBDC::summaryFun(
  data = beeData,
  # Add flags to ignore here
  dontFilterThese = c(".gridSummary",
                      ".coordinates_empty",
                      ".lonFlag",
                      ".latFlag",
                      ".uncer_terms",
                      ".uncertaintyThreshold"),
  # Remove the filtering columns
  removeFilterColumns = TRUE,
  # Filter to ONLY cleaned data?
  filterClean = TRUE)

## Optional filter 2: Filter out iNaturalist records ----
# Since iNaturalist is a community platform, anyone from amateurs to experts may be identifying specimens. Since this can therefore lead to identification uncertainty, you can optionally filter out iNaturalist records or manually select the iNaturalist records you trust.

### iNaturalist option 1: remove all iNaturalist records (more conservative option) ----
cleanData <- cleanData %>%
  filter(institutionCode != 'iNaturalist')

### iNaturalist option 2: Subset iNaturalist records and manually select the records you'd like to keep ----
# create object only including the iNaturalist records
iNatOnly <- cleanData %>%
  filter(institutionCode == 'iNaturalist')

# create a csv file for this object
readr::write_excel_csv(iNatOnly, file.path(OutPath_Check,
                                           "iNaturalist_review_records.csv"))

# Open this file in excel. Manually filter down records by deleting uncertain records/only retaining the records you trust. A few options for filtering could include only retaining records identified by an iNaturalist user(s) that you trust, or only retaining species that are easily identifiable (and are thus less likely to be mis-identified). Once you have deleted all the uncertain records, save the updated file!

# Read back in the updated iNaturalist records file
cleaned_iNatOnly <- readr::read_csv(file.path(OutPath_Check,
                                              "iNaturalist_review_records.csv"))

# created filtered version of cleanData so records aren't duplicated in the merge
cleanData <- cleanData %>%
  filter(institutionCode != 'iNaturalist')

# Merge trusted iNaturalist records with the rest of the cleaned dataset
cleanData <- cleanData %>%
  rbind(cleaned_iNatOnly)

## Optional filter 3: Filter by area of interest ----

##create object of area of interest boundary. Modify to your region of interest. Pick *ONE* of these options to run based on what your geographic range of interest is
  
#filter by COUNTRY
  # adjust code below to your country of interest
boundary <- nation() %>%
  filter(NAME == 'United States') %>%
  st_transform(boundary, crs = 4326) %>%
  dplyr::select(geometry)

#filter by STATE 
  # adjust code below to your state of interest
boundary <- states(cb = TRUE) %>%
  filter(NAME == 'Texas') %>%
  st_transform(boundary, crs = 4326) %>%
  dplyr::select(geometry)

#filter by COUNTY
  # adjust code below to your state/county of interest
boundary <- counties(state = 'Texas') %>%
  filter(NAME == 'Denton') %>%
  st_transform(boundary, crs = 4326) %>%
  dplyr::select(geometry)

#filter by YOUR OWN UNIQUE GEOGRAPHIC REGION
  # EITHER: provides coordinates of your region
  #provide the coordinates of the corners of your region (if a normal polygon)
coords <- matrix(c(-107.24, 25.6,
                   -107.24, 36.88,
                   -92.52, 36.88,
                   -92.52, 25.6,
                   -107.24, 25.6), 
                 ncol = 2, byrow = TRUE) 
  # transform those coordinates into a spatial object (sf) and set the coordinate system (crs = 4326)
polygon <- st_sfc(st_polygon(list(coords)))
boundary <- st_sf(geometry = polygon) 
st_crs(boundary) <- 4326

  # OR: read in your own unique shape file
fname <- system.file(paste0(DataPath, '/your file name.shp'), package="sf")
boundary <- st_read(fname)

# Perform the spatial filter! Filters to only include observations within your specified boundary. Make sure you have created a boundary object using *ONE* of the above options. This will return all records that fall within or on your specified spatial boundary.
spFilter <- cleanData %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  st_join(boundary, 
          left = FALSE,
          join = st_intersects) %>%
  mutate(
    decimalLongitude = st_coordinates(.)[, 1],
    decimalLatitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() 

## Optional filter 4: filter using dplyr instead of spatial intersections ----
cleanData <- iNatFilter %>%
  #filter(country == '')
  filter(stateProvince == 'Texas')
  #filter(county == '')

## Optional filter 5: Filter records based on coordinateUncertainty ----
 # currently set to filter out records with greater than 5km coordUncertainty - adjust as needed
cleanData <- cleanData %>%
  filter(coordinateUncertaintyInMeters <= 5000)

## Save and export the cleaned dataset ----
readr::write_excel_csv(spFilter, file.path(OutPath, "Cleaned",
                                                     "05_cleaned_database.csv"))

#************************************
#************************************


#----Prepping data for NatureServe analysis----

#TODO: ask Hannah if we want to build in the option to filter out records based on coordinate uncertainty 

# using test file - will need to fix this when we have the real dataset

test <- readr::read_csv(file.path(OutPath, "./Cleaned/05_cleaned_database.csv"))

test_group <- test %>%
  group_by(scientificName)

#***NOTE: If you have not yet created the 'boundary' object, you need to do so before running this function. Please go back up to 'create object area of interest boundary' earlier in the 'Data Filtering' section. 

# CAVEAT: this function calculates range extent, but does not remove extreme disjuncts and vagrancies(as recommended by NatureServe standards). If you suspect there are some spatial outliers, please remove before running these calculations for a NatureServe analysis. 

get_NatureServe_values <- function(x, boundary) {
  # Load necessary packages
  require(tidyverse)
  require(tigris)
  require(sf)
  require(red)
  require(lwgeom)
  
  x <- x %>%
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))
  # Check if the dataframe is grouped
  is_grouped <- dplyr::is.grouped_df(x)
  
  # If grouped, split by group. Otherwise, treat the whole dataframe as one group
  grouped_data <- if (is_grouped) {
    dplyr::group_split(x)
  } else {
    list(x)
  }
  
  # Retrieve group keys if available
  group_keys_df <- if (is_grouped) {
    dplyr::group_keys(x)
  } else {
    NULL
  }
  
  # Process each group
  results <- map2(grouped_data, seq_along(grouped_data), function(species_data, idx) {
    # Create sf object
    species_sf <- species_data %>%
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    # Calculate number of observations (# of unique specimen records)
    observations <- species_sf %>%
      st_join(boundary, join = st_intersects) %>%
      filter(!is.na(NAME)) %>%
      nrow()
    
    # Calculate number of occurrences (# of unique specimen records with distinct locations)
    occurrences <- species_sf %>%
      st_join(boundary, join = st_intersects) %>%
      filter(!is.na(NAME)) %>%
      distinct(geometry) %>% 
      nrow()
    
    # Calculate range extent (Convex Hull Intersected with Boundary) in km^2
    if (occurrences <= 2) {
      range_extent <- NA

    } else {
      range_extent <- species_sf %>%
        st_union() %>%
        st_convex_hull() %>%
        st_intersection(boundary) %>%
        st_area() %>%
        units::set_units(km^2)
    }
    
    # Calculate area of occupancy (AOO) in km^2
    coords <- species_sf %>%
      st_join(boundary, join = st_intersects) %>%
      filter(!is.na(NAME)) %>%
      st_coordinates() %>%
      as.data.frame()
    
    if (nrow(coords) == 0) {
      area_of_occupancy <- NA
    } else {
      area_of_occupancy <- red::aoo(coords) %>%
        units::set_units(km^2)
    }
    
    # Create result tibble
    result <- tibble(
      observations = observations,
      occurrences = occurrences,
      range_extent = as.numeric(range_extent),
      area_of_occupancy = as.numeric(area_of_occupancy)
    )
    
    # If grouped, add group keys to result
    if (!is.null(group_keys_df)) {
      result <- bind_cols(group_keys_df[idx, ], result)
    }
    
    return(result)
  })
  
  # Combine results
  results_df <- bind_rows(results)
  
  # provide warning about NatureServe calculations if there are any NA values for range extent
  if(any(is.na(results_df$range_extent)))
  {
     warning('NA range extent values present for some taxa - not enough information to complete NatureServe analyses for these taxa')
  }
    
  return(results_df)
  
}

out <- get_NatureServe_values(test_group, boundary = boundary)
out2 <- get_NatureServe_values(test, boundary = boundary)

