#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  Manifesto Project  >>>>>>>>>>>>>>>>>
#
#
#           --------------Data Loading and processing---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) Retrieve latest country level data from original sources (sourcing country R files)
#                  3) Load and process data into one master file
#                  4) Upload to DARWIN
#                  5) commented out template for downloading data to Excel to create Excel based charts
# 
# 
# 
#
# Author: Kai Foerster, ID: 214288
#
# Version date:	  15/11/2023
#=======================================================================================================


# ===========================================================================
#   1) Settings on the loading and processing of data
# ===========================================================================


# Housekeeping ------------------------------------------------------------
rm(list = ls())

# Controls ----------------------------------------------------------------

col_date <- c("ref_date", "min_date", "update_date")
today <- Sys.Date()

# Default settings and connections 
update_raw_data <- 1
update_vintage <- 0
load_raw_data   <- 1

# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("data.table", "openxlsx","zoo", "readr", "manifestoR", "stringr")

# Load packages silently
suppressPackageStartupMessages(
  invisible(lapply(pckg_to_load, library, character.only = TRUE))
)
