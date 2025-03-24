# This script is for testing out the BeeBDC package and developing our workflow for assessing TX native bee conservation status. It should be kept as a working document (without deleting old code and comments), and not converted into a clean workflow.
# Last updated: "2025-01-31 14:33:39 CST" (RLW)
# Authors: Elinor M. Lichtenberg, Avery Pearson, Robert Whyle

#************************************
#************************************


# Project Setup -----------------------------------------------------------


#****************
## Install packages----

### NOTE: Only code for installation of BeeBDC-related packages shown below, but also need to install/use packages "countrycode", "rgbif","spocc","taxize", and "tidyverse". 
### NOTE: Remove ### to install packages as needed.

# First, install BiocManager, devtools, ComplexHeatmap, and rnaturalearthhires. Complete these installations prior to installing BeeBDC
## NOTE: Dorey provides code for managing packages for with "renv", but we don't use it here.
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager", repos = "http://cran.us.r-project.org")
BiocManager::install("ComplexHeatmap")

# Install remotes if needed
if (!require("remotes", quietly = TRUE)) install.packages("remotes", repos = "http://cran.us.r-project.org")

# Download and load rnaturalearthhires
## NOTE: The order of installation matters here. Make sure this is done before installing BeeBDC
## if the below code isn't working, then reference the github page for download instructions
remotes::install_github("ropensci/rnaturalearthhires")
install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev",type = "source") 

# Now install and load BeeBDC
install.packages("BeeBDC",
                 dependencies = T)

# Loads the remaining relevant BeeBDC packages
lapply(c("ComplexHeatmap", "magrittr"), library, character.only = TRUE)

# Install rgnparser
## For Macs
### You may need to install gnparser through terminal
## For Windows
### Make sure you have latest R, R studio, and packages. 'Check that package 'rgnparser' is installed.'rgnparser' package can be installed, but will not run unless the latest version of 'gnparser' (a component function of rgnparser package) is downloaded and placed anywhere in your PC's environmental PATH variables.
### You can download latest version of 'gnparser' from https://github.com/gnames/gnparser/releases/tag/v1.6.9
### If you'd like to place 'gnparser' in a folder outside of Widows default PATH variables, you'll have to edit your PC's PATH variables to include the folder you choose. This is difficult to do if working without administrator access on your PC.

### See https://github.com/gnames/gnparser#installation

#****************
### Load libraries----

# NOTE: Jump here if all above packages have been installed previously
library(rgbif)
library(ritis)
library(rgnparser)
library(spocc)
library(tidyverse)
library(rnaturalearthhires) ## Ensure that "rnaturalearthhires" is loaded before loading "BeeBDC"
library(BeeBDC)
lapply(c("ComplexHeatmap", "magrittr"), library, character.only = TRUE)
library(sf)
library(tigris)
library(raster)
library(red)
library(purrr)

#****************
## Set up environment----

## Set root folder - these file paths may need to be adjusted based on your file path location
## Default root is the directory containing this .Rproj file
RootPath <- here::here()

## Create the working directory in the RootPath if it doesn't exist already
if (!dir.exists(file.path(RootPath, "Data_acquisition_workflow"))) {
  dir.create(file.path(RootPath, "Data_acquisition_workflow"), recursive = TRUE)
}

# Set the working directory (BeeBDC uses this for file output)
setwd(file.path(RootPath, "Data_acquisition_workflow"))

# Set up directories — Run each time you use the script.
BeeBDC::dirMaker(RootPath = RootPath, RDoc = "Lichtenberg_invert_occurrence_records.R")%>%
  ## Adds paths created by this function to the environment()
  list2env(envir = parent.env(environment()))

## Store the default par settings so they can be restored
.pardefaults <- par(no.readonly = T)

# Load custom scripts needed for this project
R.utils::sourceDirectory(file.path(DataPath, "Scripts"))

#************************************
#************************************


# Create Species List -----------------------------------------------------


## Create a list of accepted canonical (no authorship included) names of your species of interest. This list will be used to filter and reduce the size of backbones. It can also be used for querying occurrence records in later steps.
sppList <- c("Apis mellifera",
             "Bombus pensylvanicus",
             "Halictus ligatus",
             "Xylocopa virginica")

## If the user is unsure of the taxonomic status of listed names, they can be harmonized with the backbones available through APIs.
sppList <- c("Apis mellifica",  # Using outdated synonym of Apis mellifera
             "Bombus pensylvanicus",
             "Halictus ligatus",
             "Xylocopa virginica")

sppListHarmonized <- rgbif::name_backbone_checklist(sppList) #Checking names using GBIF taxonomic backbone
sppList <- sppListHarmonized$species # Output list of accepted names

#************************************
#************************************


# Create taxonomyfile -----------------------------------------------------


#****************
## Taxonomic backbone from a single source----

### Option 1: read in local taxonomic backbone----
### If users have a backbone made prior to this workflow, it can be read directly into taxonomyFile for use in later steps. Ensure that column names and types match BeeBDC format (for reference, download a small test backbone using taxadbToBeeBDC()). 
### Key columns required for workflow: taxonomic_status, source, accid, id, [all taxonomic levels], validName, canonical, authorship, taxon_rank, valid.
taxonomyFile<-read_csv(file.path(DataPath,"your_taxonomic_backbone.csv"))


### Option 2: BeeBDC download of DiscoverLife's Backbone----
### Discover Life's backbone for Anthophila comes with the BeeBDC download via beesTaxonomy()
taxonomyFile <- beesTaxonomy_fixed() # TODO: This is a fixed version of BeeBDC::BeesTaxonomy, which should reliably work now. Still needs to be tested on macOS


### Option 3: download taxonomic backbone from API----
### It's best to download backbones at higher taxonomic level encompassing all species of interest to capture as many synonyms as possible. The backbone can then be filtered using sppList. 
### taxadbToBeeBDC() will download backbones from several sources in BeeBDC format. 
#! add in comment about provider options and how to choose them
taxonomyFile <- taxadbToBeeBDC(name = "Hymenoptera",
                             rank = "Order",
                             ignore_case = FALSE, # ignore_case = FALSE needs to be specified when querying Order, otherwise there is SQL error
                             provider = "gbif")


#****************
## Taxonomic backbone from multiple sources----

### Read in source backbones----
### This process is different from above since the backbones need to be joined together, requiring a new join column and distinct column names among data frames

#******
### Discover Life
#### Read in Discover Life taxonomy from BeeBDC. See option 2 from the single-source backbone instructions above for troubleshooting.
taxDLog <- base::readRDS(file.path(DiscLifePath,"beesTaxonomy_2024-06-17.Rda"))
taxDL <- format_provider_data(taxDLog, 'dl') # Format using custom script
  
#******
### GBIF
#### GBIF API query
taxGBIFog <- taxadbToBeeBDC(name = "Hymenoptera",
                            rank = "Order",
                            ignore_case = FALSE, # ignore_case = FALSE needs to be specified when querying Order, otherwise there is SQL error
                            provider = "gbif")
taxGBIF <- format_provider_data(taxGBIFog, 'gbif') # Format using custom script

#******
### ITIS
#### ITIS API query
taxITISog <- taxadbToBeeBDC(name = "Hymenoptera",
                            rank = "Order",
                            ignore_case = FALSE,
                            provider = "itis")
# ITIS authorship data is not returned through the above query
# For ITIS, this step uses ritis::taxon_authorship() to fill authorship column
# Warning: this can take prohibitively long for large datasets—try filtering to sppList first if needed
taxITIS <- format_provider_data(taxITISog, 'itis') # Format using custom script

#******
### Merge backbones with priority----
### Join taxonomic backbones

# Take possible tables from above
possibleTables <- c("taxDL", "taxGBIF", "taxITIS")
# Join tables that are currently being used in the session
joinedTaxonomy <- possibleTables %>%
        lapply(function(x) if (exists(x)) get(x) else NULL) %>%
        purrr::compact() %>%
        reduce(~ full_join(.y, .x, by="joinCol"))

### Coalesce each column in the three tables into a single column, using the first non-NA value encountered.
### Table priority is determined by the order in which the table names are provided to the mergeTaxonomy() function.
mergedTaxonomy <- merge_taxonomy(joinedTaxonomy, c('dl', 'gbif', 'itis')) # Custom script

### Correct orphaned accids
#### Create a reference table for instances of orphaned accids. This happens when a unique synonym occurs in a lower-priority taxonomic backbone but refers to an accepted name shared by a higher priority backbone.
acceptedIDs <- mergedTaxonomy %>%
  filter(taxonomic_status == "accepted") %>%
  dplyr::select(id, canonical) %>%
  mutate(accid = id) %>%
  dplyr::select(accid, canonical)

#### Use reference table to reassign accids based on current accepted ids.
taxonomyFile <- mergedTaxonomy %>%
  left_join(
    acceptedIDs,
    by = c("canonical_accepted" = "canonical"),
    suffix = c("", "_replacement")
  ) %>%
  mutate(accid = if_else(taxonomic_status != "accepted", accid_replacement, accid))%>%
  dplyr::select(-canonical_accepted,-authorship_accepted,-accid_replacement)

#### Remove spent databases
# Determine which variables are being used in environment
existingVariables <- c("acceptedIDs","joinedTaxonomy","mergedTaxonomy","taxDL",
                       "taxDLog","taxGBIF","taxGBIFog","taxITIS","taxITISog") %>%
                     get_existing_variables(.) # Custom script to get currently in-use variables
                     
rm(list = existingVariables, existingVariables) # Remove existing variables

#******
## Filter taxonomyFile to species of interest and synonyms----

filteredTaxonomyFile <- taxonomyFile %>%
  left_join(
    taxonomyFile %>%
      dplyr::select(id, canonical, authorship),
    by = c("accid" = "id"),
    suffix = c("", "_accepted")
  ) %>%
  filter((canonical %in% sppList) |
           (canonical_accepted %in% sppList)) # filter to rows related to those in sppList

#************************************
#************************************


# Import Occurrence Records -----------------------------------------------


#****************
## Query occurrence records from APIs----

## GBIF
### Users have the option of using rgbif::occ_search or rgbif::occ_download. occ_search() is only meant for smaller requests and generally not recommended for research. For larger requests, use rgbif::occ_download.
### See https://docs.ropensci.org/rgbif/articles/downloading_a_long_species_list.html for another example and further explanation.

### Collect and add synonyms to sppList from current backbone
synSppList <- filteredTaxonomyFile$canonical

### Get the taxon keys for each species in list. gbifTaxonKeys should be a long vector like this c(2977832,2977901,2977966,2977835,2977863)
gbifTaxonKeys <- synSppList %>% 
  name_backbone_checklist(strict = T) %>% # match to backbone 
  filter(matchType == "EXACT") %>% # get matched names
  pull(usageKey)

### Set up your environment with your GBIF credentials
# TODO: Can we combine this step with the occ_download step to avoid confusion?
check_gbif_creds() # Custom script

## Intialize download of occurrence data, providing search parameters as needed
### This search uses the gbifTaxonKey values generated in the previous step
### For help with constructing queries, see https://docs.ropensci.org/rgbif/index.html.
occ_download(
  pred_in("taxonKey", gbifTaxonKeys),  # Search for all taxonKeys in the list
  pred("occurrenceStatus", "PRESENT"),
  pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN", "MATERIAL_SAMPLE"))), #Excluding these types bases of record
  pred_lte("coordinateUncertaintyInMeters", 2000), #Uncertainty less than or equal to 2000 m.
  pred("hasCoordinate", TRUE),
  pred_gte("year", 1960),  # Include only records from 1960 onwards
  ## Uncomment the section below to define a bounding box. Otherwise global occurrence data will be downloaded.
  ## Read more about adding predicates here: https://www.rdocumentation.org/packages/rgbif/versions/3.8.0/topics/download_predicate_dsl
  # pred_or(
  #   pred_in("stateProvince",c("Texas","Arkansas","Louisiana","Oklahoma","New Mexico","Tamaulipas", "Nuevo Leon", "Coahuila","Chihuahua")),
  #   pred_within("POLYGON ((-107.24 25.6, -107.24 36.88, -92.52 36.88, -92.52 25.6, -107.24 25.6))")
  #),
  format = "SIMPLE_CSV"  # Download format as CSV for easier handling
)

## Check download progress

### After executing the download, a message will be output in the console with instructions for checking download progress, retrieving data, citation info, and other associated metadata for the download.
### Copy and paste your resulting Download Key in the code below
occ_download_wait('your_download_key')

## Load the downloaded dataset and save (this saves to the working directory unless specified with path = )
gbifOccs <- occ_download_get('your_download_key') %>%
  occ_download_import()

## reformat to BeeBDC::ColTypeR() standards
gbifOccs <- gbifOccs %>%
  mutate(dateIdentified = as.character(dateIdentified),
         recordNumber = as.character(recordNumber),
         database_id = paste0("GBIF_4bees_",seq_len(nrow(gbifOccs))),
         datasetID = "GBIF_4bees",
         dataSource = "GBIF",
         country = ""
  )

# Or load previously downloaded database

# GBIF simple-CSV downloads default to a tab-delimited format. Change the delim argument if needed
gbifOccs <- read_delim(file.path(DataPath,"Additional_Datasets", "your_file_name.csv"), delim='\t')%>%
  mutate(dateIdentified = as.character(dateIdentified),
         recordNumber = as.character(recordNumber),
         database_id = paste0("GBIF_4bees_",seq_len(nrow(.))),
         datasetID = "GBIF_4bees",
         dataSource = "GBIF",
         country = ""
  )

#****************
## Import local geographically-relevant data sets (BeeBDC examples)----

## The datasets provided by Dorey that are relevant to Texas include
### Ballare et al., 2019 (Bal)
#### Read in standardized data set
# TODO: Create note about which columns are necessary for code to run

# TODO: Make sure this isq the cleaned dataset
Bal_std <- readr::read_csv(file.path(DataPath,"Additional_Datasets",
                                              "jbd_Bal_data.csv"),
                           col_types = BeeBDC::ColTypeR()) %>%
  mutate(city = word(locality, 1, sep = ","),
         county = word(locality, 2, sep = ","),
         state = word(locality, 3, sep = ","),
         country = "USA", # Assuming all localities are in the USA
         countryCode = "US", # Assuming country code for the USA is "US"
         basisOfRecord = "HUMAN_OBSERVATION")

# TODO: Remove this code?
#### For comparison, read in the unstandardized version of Ballare data
# Bal_Data <- BeeBDC::readr_BeeBDC(dataset = "Bal",
#                                  path = file.path(DataPath, "Additional_Datasets"),
#                                  inFile = file.path("InputDatasets", "Beedata_ballare.xlsx"),
#                                 outFile = "jbd_Bal_Data.csv",
#                                sheet = "animal_data",
#                                dataLicense = "https://creativecommons.org/licenses/by-nc-sa/4.0/")


### Bob Minckley data (BMin)
BMin_std <- readr::read_csv(file.path(DataPath,
                                      "Additional_Datasets",
                                      "jbd_BMin_data.csv"),
                            col_types = BeeBDC::ColTypeR()) %>%
            as.data.frame()

### Ecdysis data (ECD)
Ecd_std <- readr::read_csv(file.path(DataPath,
                                     "Additional_Datasets",
                                     "jbd_Ecd_data.csv"),
                           col_types = BeeBDC::ColTypeR())

### Florida State Collection of Arthropods (FSCA)
FSCA_std <- readr::read_csv(file.path(DataPath,
                                      "Additional_Datasets",
                                      "jbd_FSCA_data.csv"),
                            col_types = BeeBDC::ColTypeR())

### US Geological Survey data (Geol)
GeoL_std <- readr::read_csv(file.path(DataPath,
                                      "Additional_Datasets",
                                      "jbd_GeoL_data.csv"),
                            col_types = BeeBDC::ColTypeR())

### Illinois Natural History Survey data (INHS)
INHS_std <- readr::read_csv(file.path(DataPath,
                                      "Additional_Datasets",
                                      "jbd_INHS_data.csv"),
                            col_types = BeeBDC::ColTypeR())

### K.A. Parys 2019 (KP)
KP_std <- readr::read_csv(file.path(DataPath,
                                    "Additional_Datasets",
                                    "jbd_KP_data.csv"),
                          col_types = BeeBDC::ColTypeR())

### Shannon Collins data set (SMC)
SMC_std <- readr::read_csv(file.path(DataPath,
                                     "Additional_Datasets",
                                     "jbd_SMC_Data.csv"))%>%
  mutate(eventDate = as.character(eventDate))

### Paige Chesshire's data (PaigeNAm)
PaigeNAm <- readr::read_csv(file.path(DataPath,
                                      "Additional_Datasets",
                                      "Paige_data", 
                                      "NorAmer_highQual_only_ALLfamilies.csv"), 
                            col_types = BeeBDC::ColTypeR()) %>%
            # Change the column name from Source to dataSource to match the rest of the data.
            dplyr::rename(dataSource = Source) %>%
            # add a NEW database_id column
            dplyr::mutate(
              database_id = paste0("Paige_data_", 1:nrow(.)),
              .before = scientificName)

## Other datasets from Dorey 2023 can be loaded in here, if needed
### (MABC)
# MABC_std <- readr::read_csv(file.path(DataPath, 
#                                      "Additional_Datasets",
#                                      "jbd_MABC_data.csv"),
#                             col_types = BeeBDC::ColTypeR())

## EcoS
# EcoS_std <- readr::read_csv(file.path(DataPath,
#                                      "Additional_Datasets",
#                                      "jbd_EcoS_data.csv"),
#                             col_types = BeeBDC::ColTypeR())


## Lichtenberg data
# Lic_std <- readr::read_csv(file.path(DataPath,
#                                     "Additional_Datasets",
#                                     "jbd_Lic_Data.csv"))%>%
#   mutate(eventDate = as.character(eventDate))

#****************
## Merge all occurrence data----

# Take possible variable names from above
existingDatasets <- c("Bal_std", "BMin_std", "Ecd_std", "FSCA_std", 
                      "gbifOccs", "GeoL_std", "INHS_std", "KP_std", 
                      "PaigeNAm", "SMC_std") %>%
                    get_existing_variables() # Custom script to filter to used variables only

# Merge datasets in filtered list of names
db_standardized <- lapply(existingDatasets, function(name) get(name, envir = .GlobalEnv)) %>%
                   bind_rows(.)

## Remove spent databases
rm(list = existingDatasets, existingDatasets)

### Save the dataset
db_standardized %>%
  readr::write_excel_csv(., file.path(OutPath_Intermediate, 
                                      "00_prefilter_database.csv")
                         )

# If db_standardized has been previously created and saved, read in here:
if(!exists("db_standardized")){
  db_standardized <- readr::read_csv(file.path(OutPath_Intermediate, 
                                               "00_prefilter_database.csv"), 
                                     col_types = BeeBDC::ColTypeR())
  }

# To re-start in the middle of the flagging and harmonization process, read in the most recent intermediate file you outputted, replacing "03_space_inter_database.csv" with that file's name.
if (!exists("check_space")) {
  check_space <- readr::read_csv(file.path(OutPath_Intermediate, 
                                           "03_space_inter_database.csv"), 
                                 col_types = ColTypeR())
}

#****************
# (Optional) Load sample datasets provided by BeeBDC package for working through the code. Only do this if you want to quickly run through the workflow with a dataset small enough to run quickly even in RAM-heavy operations

data("bees3sp",
     package = "BeeBDC")
data("beesRaw",
     package = "BeeBDC")
db_standardized <- dplyr::bind_rows(beesRaw, 
                                # Only keep a subset of columns from bees3sp
                                bees3sp %>%
                                  dplyr::select(tidyselect::all_of(colnames(beesRaw)),
                                                countryCode))

#****************
# Import proprietary data - make sure it is in DarwinCore format

#************************************
#************************************


# Flag Data ---------------------------------------------------------------


#****************
## Flag occurrences without scientificName present----

### Adds column ".scientificName_empty"
check_pf <- bdc::bdc_scientificName_empty(data = db_standardized,
                                          sci_names = "scientificName")

# Remove db_standardized dataset to reduce data storage space
rm(db_standardized) 

#****************
## Flag occurrences with missing decimalLatitude and decimalLongitude----

### Adds column ".coordinates_empty"
check_pf <- bdc::bdc_coordinates_empty(data = check_pf, lat = "decimalLatitude",
                                       lon = "decimalLongitude")

#****************
## Flag occurences that are not on Earth----

### Adds column ".coordinates_outOfRange"
check_pf <- bdc::bdc_coordinates_outOfRange(data = check_pf, lat = "decimalLatitude",
                                            lon = "decimalLongitude")

#****************
## Flag occurrences that don't match basisOfRecord types ----  

### user decides what the acceptable forms of data are here - modify as necessary
### Adds column ".basisOfRecords_notStandard"
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
## Harmonize country names----

### Create a separate dataset with harmonized country names and associated info
# This requires the table to have country and countryCode columns
check_pf_noNa <- BeeBDC::countryNameCleanR(
  data = check_pf,
  ### Create a Tibble of common issues in country names and their replacements
  commonProblems = dplyr::tibble(problem = c('U.S.A.','US','USA','usa',
                                             'UNITED STATES','United States',
                                             'U.S.A',"México"),
                                 fix = c('United States of America','United States of America',
                                         'United States of America','United States of America',
                                         'United States of America','United States of America',
                                         'United States of America',"Mexico")))

### Get country name from coordinates using a wrapper around the jbd_country_from_coordinates() function. Because our dataset is much larger than those used to design bdc, we have made it so that you can analyse data in smaller pieces. Additionally, like some other functions in BeeBDC, we have implemented parallel operations (using mc.cores = #cores in stepSize = #rowsPerOperation); see ‘?jbd_CfC_chunker()’ for details. NOTE: In an actual run you should use scale = “large”
### Creates another dataset containing just the databaseID and the country name based on coordinates
suppressWarnings(
  countryOutput <- BeeBDC::jbd_CfC_chunker(data = check_pf_noNa,
                                           lat = "decimalLatitude",
                                           lon = "decimalLongitude",
                                           country = "country",
                                           # How many rows to process at a time
                                           stepSize = 10000,
                                           # Start row
                                           chunkStart = 1,
                                           path = OutPath_Intermediate,
                                           # Normally, please use scale = "large"
                                           scale = "large",
                                           mc.cores = 1),
  classes = "warning")

### Re-join these datasets
check_pf <- dplyr::left_join(check_pf, 
                             countryOutput, 
                             by = "database_id", 
                             suffix = c("", "CO")) %>%
  # Take the new country name if the original is NA
  dplyr::mutate(country = dplyr::if_else(is.na(country), countryCO, country)) %>%
  # Remove duplicates if they arose from left_join!
  dplyr::distinct()

## Save the dataset
check_pf %>%
  readr::write_excel_csv(., file.path(OutPath_Intermediate, 
                                      "01_prefilter_database.csv")
                         )

## Read in previously saved dataset if necessary
if (!exists("check_pf")) {
  check_pf <- readr::read_csv(file.path(DataPath, 
                                        "Output", 
                                        "Intermediate", 
                                        "01_prefilter_database.csv"), 
                              col_types = BeeBDC::ColTypeR())
}

## Remove these interim datasets
rm(check_pf_noNa, countryOutput)

#****************
## Standardize country names and add ISO2 codes, if needed----

### Adds two columns: "country_suggested" and "countryCode", removes these columns prior to running in an instance when the dataset contains these columns from previous runs. 
### Standardise country names and add ISO2 codes if needed
check_pf <- bdc::bdc_country_standardized(
  # Remove the countryCode and country_suggested columns to avoid an error with 
  # where two "countryCode" and "country_suggested" columns exist (i.e. if the dataset has been  
  # run before)
  data = check_pf %>% dplyr::select(!tidyselect::any_of(c("countryCode", "country_suggested"))),
  country = "country"
) 

#****************
## Correct transposed coordinates----

### Flag and correct records when decimalLatitude and decimalLongitude appear to be transposed. We created this chunked version of bdc::bdc_coordinates_transposed() because it is very RAM-heavy  using our large bee dataset. Like many of our other ‘jbd_…’ functions there are other improvement - e.g., parallel running.
### NOTE: Usually you would use scale = “large”, which requires rnaturalearthhires
### Adds column "coordinates_transposed"
### USE SAMPLE DATASET IF TESTING THIS FUNCTION (TAKES LONG AND USES A LOT RAM)
check_pf <- BeeBDC::jbd_Ctrans_chunker(
  # bdc_coordinates_transposed inputs
  data = check_pf,
  id = "database_id",
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

table(check_pf$coordinates_transposed, useNA = "always") # NOTE: F = transposed/corrected records, T = already correct records 

### Save the dataset

check_pf %>%
  readr::write_excel_csv(., file.path(OutPath_Intermediate, 
                                      "01_prefilter_database.csv")
                         )

### Read in the data again if needed

if (!exists("check_pf")) {
  check_pf <- readr::read_csv(file.path(OutPath_Intermediate, 
                                    "01_prefilter_database.csv"), 
                              col_types = BeeBDC::ColTypeR())
}

#****************
## Flag coordinates inconsistent with country names----

### Collect all country names in the country_suggested column. We rebuilt a bdc function to flag occurrences where the coordinates are inconsistent with the provided country name.
check_pf <- BeeBDC::jbd_coordCountryInconsistent(data = check_pf, lon = "decimalLongitude",
                                                 lat = "decimalLatitude", scale = 50, pointBuffer = 0.01)

### Save dataset

check_pf %>%
  readr::write_excel_csv(., file.path(OutPath_Intermediate, 
                                      "01_prefilter_database.csv")
                         )

#****************
## Flag GeoRef Issues----
### This function identifies records whose coordinates can potentially be extracted from locality
### information, which must be manually checked later.

xyFromLocality <- bdc::bdc_coordinates_from_locality(data = check_pf, locality = "locality",
                                                     lon = "decimalLongitude", lat = "decimalLatitude", save_outputs = FALSE)

### Save resultant data

xyFromLocality %>%
  readr::write_excel_csv(file.path(OutPath_Check, 
                                   "01_coordinates_from_locality.csv")
                         )

### Remove spent data

rm(xyFromLocality)

#****************
## Flag records marked as "absent"----
### Run function

check_pf <- BeeBDC::flagAbsent(data = check_pf,PresAbs = "occurrenceStatus") #if occurrenceStatus is absent, skip this step

#****************
## Flag License----
### Flag the records that may not be used according to their license information.

check_pf <- BeeBDC::flagLicense(data = check_pf,
                                strings_to_restrict = "all",
                                # DON'T flag if in the following dataSource(s)
                                excludeDataSource = NULL)

#****************
## Flag GBIF issues----
### Flag select issues that are flagged by GBIF

check_pf <- BeeBDC::GBIFissues(data = check_pf, issueColumn = "issue", GBIFflags = c("COORDINATE_INVALID",
                                                                                     "ZERO_COORDINATE"))

#****************
## Flag reports----
### Save flags
#### Save the flags so far. This function will make sure that you keep a copy of everything that has been flagged up until now. This will be updated throughout the script and can accessed at the end, so be wary of moving files around manually. However, these data  will also still be maintained in the main running file, so this is an optional fail-safe.

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

### Update the .summary column

check_pf <- BeeBDC::summaryFun(
  data = check_pf,
  # Don't filter these columns (or NULL)
  dontFilterThese = NULL,
  # Remove the filtering columns?
  removeFilterColumns = FALSE,
  # Filter to ONLY cleaned data?
  filterClean = FALSE)

### Reporting
#### Use bdc to generate reports

(report <- bdc::bdc_create_report(data = check_pf,
                                  database_id = "database_id",
                                  workflow_step = "prefilter",
                                  save_report = TRUE))

#****************
## Save----
### Save the intermediate dataset

check_pf %>%
  readr::write_excel_csv(., file.path(OutPath_Intermediate,
                                      "01_prefilter_output.csv")
                         )

#************************************
#************************************


# Taxonomy ----------------------------------------------------------------


## Read in the filtered dataset or rename the 3.x dataset for 4.0 (refers to vignette section)

if (!exists("check_pf")) {
  database <- readr::read_csv(file.path(OutPath_Intermediate, 
                                        "01_prefilter_output.csv"), 
                              col_types = BeeBDC::ColTypeR())
} else {
  # OR rename and remove
  database <- check_pf
  # Remove spent dataset
  rm(check_pf)
}

## Remove *names_clean* if you've already run the following functions before on this dataset before

database <- database %>%
  dplyr::select(!tidyselect::any_of("names_clean"))

#****************             
## Prep data names----
### This step cleans the database's scientificName column

parse_names <- bdc::bdc_clean_names(sci_names = database$scientificName,
                                    save_outputs = FALSE)

### Keep only the .uncer_terms and names_clean columns

parse_names <- parse_names %>%
  dplyr::select(.uncer_terms,
                names_clean)

### Merge names with complete dataset

database <- dplyr::bind_cols(database,
                             parse_names)
rm(parse_names)

#****************
## Harmonize taxonomy----
### Download custom taxonomy file from the BeeBDC package and DiscoverLife website
### Harmonize the names in the occurrence tibble.This flags the occurrences without a matched name and matches names to their correct name according to Discover Life. You can also use multiple cores to achieve this. See ‘?harmoniseR()’ for details.

database <- BeeBDC::harmoniseR(path = DataPath, #The path to a folder that the output can be saved
                               taxonomy = filteredTaxonomyFile, # The formatted taxonomy file
                               data = database,
                               mc.cores = 1)

### Remove taxonomy file

rm(filteredTaxonomyFile)

### Save the harmonized file

database %>%
  readr::write_excel_csv(., file.path(DataPath, 
                                      "Output",
                                      "Intermediate",
                                      "02_taxonomy_database.csv")
                         )

## Save flags----

flagFile <- BeeBDC::flagRecorder(data = database, 
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
#************************************


# Space -------------------------------------------------------------------


## Read in latest database

if (!exists("database")) {
  database <- readr::read_csv(file.path(OutPath_Intermediate,
                                        "02_taxonomy_database.csv"), 
                              col_types = BeeBDC::ColTypeR())
}


## Coordinate precision----
### Below function identifies records with a coordinate precision below a specified number of decimal places. The major difference between bdc and BeeBDC functions is that jbd_coordinates_precision() will only flag occurences if BOTH latitude and longitude are rounded, while BeeBDC::jbd_coordinates_precision flags if only one is rounded.

check_space <- BeeBDC::jbd_coordinates_precision(data = database, 
                                                 lon = "decimalLongitude",
                                                 lat = "decimalLatitude", 
                                                 ndec = 2)  # number of decimals to be tested

### Remove spent database

rm(database)

### Save resulting file

check_space %>%
  readr::write_excel_csv(., file.path(OutPath_Intermediate,
                                      "03_space_inter_database.csv")
                         )

#****************
## Common spatial issues----

### Only run for occurrences through clean_coordinates() that are spatially "valid"

tempSpace <- check_space %>%
  dplyr::filter(!.coordinates_empty == FALSE) %>%
  dplyr::filter(!.coordinates_outOfRange == FALSE)

### Flag common spatial issues using functions from the package "CoordinateCleaner", which addresses common issues in biodiversity datasets

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

### Re-merge the datasets

check_space <- tempSpace %>%
  # Re-bind with the records that were removed earlier
  dplyr::bind_rows(check_space %>%
                     dplyr::filter(.coordinates_empty == FALSE | .coordinates_outOfRange == FALSE))

### Remove temporary dataset

rm(tempSpace)

### Save the intermediate dataset

check_space %>%
  readr::write_excel_csv(file.path(OutPath_Intermediate,
                               "03_space_inter_database.csv")
                         )

#****************
## Diagonal and Grid Issues----

### Load most recent database, if necessary

if (!exists("check_space")) {
  check_space <- readr::read_csv(file.path(OutPath_Intermediate,
                                           "03_space_inter_database.csv"), 
                                 col_types = BeeBDC::ColTypeR())
}

### Finds sequential numbers that could be fill-down errors in lat and long, and groups by the ‘groupingColumns'. This is accomplished by using a sliding window with the length determined by minRepeats. Only coordinates of precision ‘ndec’ (number of decimals in decimal degree format) will be examined. Note, that this function is very RAM-intensive and so the use of multiple threads should be approached with caution depending on your data set. However, the option is provided.

check_space <- BeeBDC::diagonAlley(
  data = check_space,
  # The minimum number of repeats needed to find a sequence in for flagging
  minRepeats = 6,
  ndec = 3,
  groupingColumns = c("eventDate",
                      "recordedBy",
                      "datasetName"),
  mc.cores = 1)

### Spatial gridding from rasterization: Select only records with more than X occurrences

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

### Run the gridding analysis to find datasets that might be gridded.

gridded_datasets <- CoordinateCleaner::cd_round(x = griddingDF, 
                                                lon = "decimalLongitude",
                                                lat = "decimalLatitude", 
                                                ds = "datasetName", 
                                                T1 = 7, 
                                                min_unique_ds_size = 4,
                                                test = "both", 
                                                value = "dataset", 
                                                graphs = FALSE, 
                                                verbose = TRUE, 
                                                reg_out_thresh = 2,
                                                reg_dist_min = 0.1, 
                                                reg_dist_max = 2) %>%
  dplyr::tibble()
### The griddingDF is no longer needed. remove it.

rm(griddingDF)

### Integrate results with main dataset

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

### Save the gridded_datasets file for later examination and remove.

gridded_datasets %>%
  readr::write_excel_csv(file.path(OutPath_Intermediate,
                                   "03_space_griddedDatasets.csv")
                         )
rm(gridded_datasets)

#****************
## Uncertainty----

### Flag records that exceed a coordinateUncertaintyInMeters threshold

check_space <- BeeBDC::coordUncerFlagR(data = check_space, 
                                       uncerColumn = "coordinateUncertaintyInMeters",
                                       threshold = 1000)
#****************
## Country Checklist----

### This step identifies mismatches between the Discover Life country checklist (beesChecklist) and your occurrence dataset for bee species, identifying potential misidentifications, outliers, etc.’
### Download the country-level checklist.

bee.checklist <- read_csv(file.path(RootPath,
                                    "BDC_repo", 
                                    "DiscoverLife_Data", 
                                    "beesChecklist_2024-06-17.csv"),
                          col_types = cols(
                            subgenus = col_character(),
                            infraspecies = col_character()
                          ))

check_space <- BeeBDC::countryOutlieRs(checklist = bee.checklist,
                                       data = check_space,
                                       keepAdjacentCountry = TRUE,
                                       pointBuffer = 0.05,
                                       # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
                                       # Smaller numbers will result in much longer calculation times. 
                                       scale = 50,
                                       mc.cores = 1)

### Since version 1.1.2 a new function, BeeBDC::continentOutlieRs(), has been released which is conceptually the same as BeeBDC::countryOutlieRs() except it works on the level of continents. Users might appreciate that generating a continent-level checklist is much easier than a country-level one and so might actually be of more wide-spread use (beyond bees).

check_space <- BeeBDC::continentOutlieRs(checklist = bee.checklist,
                                         data = check_space,
                                         keepAdjacentContinent = FALSE,
                                         pointBuffer = 0.05,
                                         # Scale of map to return, one of 110, 50, 10 OR 'small', 'medium', 'large'
                                         # Smaller numbers will result in much longer calculation times. 
                                         scale = 50,
                                         mc.cores = 1)

### A list of failed species-country combinations and their numbers can be output here

view(check_space %>%
       dplyr::filter(.countryOutlier == FALSE) %>%
       dplyr::select(database_id, scientificName, country) %>%
       dplyr::group_by(scientificName) %>%
       dplyr::mutate(count_scientificName = n()) %>%
       dplyr::distinct(scientificName, country, .keep_all = TRUE) %>%
       readr::write_excel_csv(file.path(OutPath_Intermediate, "03_space_failedCountryChecklist.csv"))
     )

#****************
## Map Spatial Errors----

### Assemble maps of potential spatial errors and outliers, either one flag at a time or using the .summary column. 
### First, you need to rebuild the .summary column.

check_space <- BeeBDC::summaryFun(data = check_space,
                                  dontFilterThese = NULL,
                                  removeFilterColumns = FALSE,
                                  filterClean = FALSE)

### Use col_to_map in order to map ONE spatial flag at a time or map the .summary column for all flags.

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
## Space Report----

### Create the report using bdc

(report <- bdc::bdc_create_report(data = dplyr::tibble(check_space %>%
                                                         dplyr::select(!.uncer_terms)), 
                                  database_id = "database_id",
                                  workflow_step = "space",
                                  save_report = TRUE))

#****************
## Space Figures----

### Create figures for the spatial data filtering results

(figures <- BeeBDC::jbd_create_figures(data = dplyr::tibble(check_space %>%
                                                              dplyr::select(!.uncer_terms)), 
                                       path = DataPath, 
                                       database_id = "database_id",
                                       workflow_step = "space", 
                                       save_figures = TRUE))


### For examining the figures, the options are:

#### .cap = Records around country capital centroid
#### .cen = Records around country or province centroids
#### .dbl = Duplicated coordinates per species
#### .equ = Identical coordinates
#### .otl = Geographical outliers
#### .gbf = Records around the GBIF headquarters
#### .inst = Records around biodiversity institutions
#### .rou = Rounded (probably imprecise) coordinates
#### .urb = Records within urban areas — (Likely not relevant for bees.)

### You can examine the figures. For example, run figures$.rou

figures$summary_all_tests

### Save interim dataset

check_space %>%
  readr::write_excel_csv(file.path(OutPath_Intermediate,
                                   "03_space_inter_database.csv")
                         )

### Save the flags so far

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

### Save the intermediate dataset

check_space %>%
  readr::write_excel_csv(., file.path(OutPath_Intermediate,
                                      "03_space_database.csv")
                         )

#************************************
#************************************


# Time Issues -------------------------------------------------------------


## Read in the last database, if needed

if (!exists("check_space")) {
  check_time <- readr::read_csv(file.path(OutPath_Intermediate,
                                          "03_space_database.csv"),
                                col_types = BeeBDC::ColTypeR())
} else {
  check_time <- check_space
  # Remove the spent file
  rm(check_space)
}

## Plot histogram of dates

### Parse dates with lubridate

check_time$eventDate<-lubridate::ymd_hms(check_time$eventDate, truncated = 5)

### Define range of dates to filter out those that were parsed incorrectly

earliest_date <- as.POSIXct("1863-01-01")
latest_date <- Sys.time()

### Create histogram

hist(check_time$eventDate, 
     breaks = 365*10, 
     main = "Histogram of eventDates",
     xlim = c(earliest_date,
              latest_date))

### Further filters to remove unreasonable dates

check_time$year <- ifelse(check_time$year > lubridate::year(Sys.Date()) | check_time$year <
                            1600, NA, check_time$year)
check_time$month <- ifelse(check_time$month > 12 | check_time$month < 1, NA, check_time$month)
check_time$day <- ifelse(check_time$day > 31 | check_time$day < 1, NA, check_time$day)

#****************
## Recover dates----

### The dateFindR() function will search through some other columns in order to find and rescue dates that may not have made it into the correct columns. It will further update the eventDate, day, month, and year columns where these data were a) missing and b) located in one of the searched columns.

check_time <- BeeBDC::dateFindR(data = check_time,
                                # Years above this are removed (from the recovered dates only)
                                maxYear = lubridate::year(Sys.Date()),
                                # Years below this are removed (from the recovered dates only)
                                minYear = 1700)

#****************
## No eventDate----

### Flag records missing eventDate

check_time <- bdc::bdc_eventDate_empty(data = check_time, eventDate = "eventDate")

#****************
## Old records----

### This will flag records prior to the date selected. 1970 is frequently chosen for SDM work. You may not need to filter old records at all, so think critically about your use. We have chosen 1950 as a lower extreme.

check_time <- bdc::bdc_year_outOfRange(data = check_time, eventDate = "year", year_threshold = 1950)

#****************
## Time report----

### Create a time report that updates the .summary column

check_time <- BeeBDC::summaryFun(
  data = check_time,
  # Don't filter these columns (or NULL)
  dontFilterThese = c(".gridSummary", ".lonFlag", ".latFlag", ".uncer_terms"),
  # Remove the filtering columns?
  removeFilterColumns = FALSE,
  # Filter to ONLY cleaned data?
  filterClean = FALSE)

#****************
## Time figures----

### Create time results figures

figures <- BeeBDC::jbd_create_figures(data = check_time,
                                      path = DataPath,
                                      database_id = "database_id",
                                      workflow_step = "time",
                                      save_figures = TRUE)

### You can check figures. For example, run figures$year

#****************
## Save flags----

### Save all flags so far

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


## We will FLAG duplicates here. These input columns can be hacked to de-duplicate as you wish. 
## This function uses user-specified inputs and columns to identify duplicate occurrence records.
## Duplicates are identified iteratively and will be tallied up, duplicate pairs clustered, and sorted at the end of the function. 
## The function is designed to work with Darwin Core data with a database_id column, but it is also modifiable to work with other columns. 
## I would encourage you to see ‘?dupeSummary()’ for more details as this function is quite modifiable to user needs.

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
  c("occurrenceID",
    "scientificName"),
  c("recordId",
    "scientificName"),
  c("id",
    "scientificName"),
  # The order in which you want to KEEP duplicated based on data source
  # try unique(check_time$dataSource)
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

## Save the dataset into the intermediate folder

check_time %>%
  readr::write_excel_csv(., file.path(OutPath_Intermediate, 
                                      "04_2_dup_database.csv")
                         )

## Save flags so far

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


# Data Filtering ----------------------------------------------------------


# Read in the most-recent duplicates file (generated by dupeSummary()) in order to identify the duplicates of the expert outliers

if (!exists("duplicates")) {
  duplicates <- BeeBDC::fileFinder(path = DataPath, fileName = "duplicateRun_")%>%
    readr::read_csv()
}

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

### Save uncleaned dataset

check_time <- BeeBDC::summaryFun(data = check_time, 
                                 dontFilterThese = c(".gridSummary",
                                                     ".lonFlag",
                                                     ".latFlag",
                                                     ".uncer_terms",
                                                     ".uncertaintyThreshold"),
                                 removeFilterColumns = FALSE,
                                 filterClean = FALSE)


#****************
## Filter

cleanData <- BeeBDC::summaryFun(
  data = check_time,
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

## TODO: We need to be able to filter by defined geographic range

## Save the cleaned dataset
# TODO: Check if this makes a directory
cleanData %>%
  readr::write_excel_csv(., file.path(OutPath,
                                      "Cleaned",
                                      "05_cleaned_database.csv")
                         )

#************************************
#************************************


# Plots and Figures -------------------------------------------------------


## Read in most recent file of flagged duplicates, if necessary
if (!exists("duplicates")) {
  duplicates <- BeeBDC::fileFinder(path = DataPath, fileName = "duplicateRun_") %>%
    readr::read_csv()
}

## Set global figure parameters

par(mar = c(2, 2, 2, 2)/2, mfrow = c(1, 1))

## Create chordDiagram
### Many values can be left out depending on desired output, but the below code provides the default values. Since there are no duplicates present in the current dataset, BeeBDC will give an error message

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

## Duplicate histogram
### Read in uncleaned data if necessary

if (!exists("check_time")) {
  beeData <- readr::read_csv(file.path(OutPath_Intermediate, 
                                       "05_unCleaned_database.csv"), 
                             col_types = BeeBDC::ColTypeR())
} else {
  beeData <- check_time
  rm(check_time)
}

### Create a plot with two bar graphs
#### One bar shows the absolute number of duplicate records for each source, while the other shows the proportion of records that are duplicated within each data source.

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

#----Prepping data for NatureServe analysis----

# using test file - will need to fix this when we have the real dataset

test <- read_csv("test_file_path.csv") %>%
  mutate(decimalLatitude = as.integer(decimalLatitude)) %>%
  mutate(decimalLongitude = as.integer(decimalLongitude))

#### restrict records to state of interest (TX in this case) ####

TX_only <- test %>%
  filter(stateProvince == 'Texas') %>%
  filter(!is.na(species))

#create object of area of interest boundary, which we will need when calculating aoc. Modify to your region of interest
boundary <- states(cb = TRUE) %>%
  filter(NAME == 'Texas') %>%
  st_transform(boundary, crs = 4326)

get_NatureServe_values <- function(x, boundary)
{
  #load necessary packages
  require(tidyverse)
  require(tigris)
  require(sf)
  require(red)
  require(lwgeom)
  
  results <- list()
  
  for(i in unique(x$species)) {
    
    species_data <- x %>%
      filter(species == i)
    
    # Calculate number of observations of species of interest
    observations <- nrow(species_data)
    
    #create sf object to be used for area of occupancy and range extent calculations
    species_sf <- species_data %>%
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    # Calculate area of occupancy for species of interest
    if (length(unique(species_data$decimalLatitude)) <= 2 & length(unique(species_data$decimalLongitude)) <= 2) {
      range_extent <- NA # Too few observations to calculate area
    } else {
      range_extent <- species_sf %>%
        st_union() %>%
        st_convex_hull() %>%
        st_intersection(boundary) %>%
        st_area() %>%
        units::set_units(km^2)
    }
    
    area_of_occupancy <- species_sf %>%
      st_coordinates() %>%
      red::aoo() %>%  # Use red::aoo() to calculate the Area of Occupancy (AOO)
      units::set_units(km^2)
    
    # Store the results for this species
    results[[i]] <- tibble(
      species = i, 
      observations = observations, 
      range_extent = as.numeric(range_extent), 
      area_of_occupancy = as.numeric(area_of_occupancy))
    
    
  }
  #combine all species results into df
  results_df <- bind_rows(results)
  
  #return results
  return(results_df)
}


#run NatureServe calculations on your dataframe
natureServe_numbers <- TX_only %>%
  get_NatureServe_values(boundary = boundary)

