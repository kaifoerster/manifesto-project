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

# Default settings and connections 
key <- "manifesto_apikey.txt"


# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("data.table", "openxlsx","zoo", "readr", "manifestoR", "stringr", "dplyr", "tidytext", "stopwords", "ggplot2", "purrr", "lubridate")

# Load packages silently
suppressPackageStartupMessages(
  invisible(lapply(pckg_to_load, library, character.only = TRUE))
)

# Setting directories ----------------------------------------------------
wd <- getwd() # "your path"
setwd(wd)

# Login to manifestR -----------------------------------------------------
mp_setapikey("manifesto_apikey.txt")

# ===========================================================================
#   2) Loading data
# ===========================================================================

# Look up table
# Labour Party: 51320
# Torries: 51620
# Liberal Democrats: 51421
# SNP: 51902
# DUP: 51903
# UUP: 51621
# UKIP: 51951
# Green Party: 51110
# We Ourselves: 51210
# SDLP: 51340
# Party of Wales: 51901
# per108: European Union: positive
# per110: European Union: negative

# Load main dataset-------------------------------

mpds <- as.data.table(mp_maindataset())
mpds_UK <- mpds[countryname=="United Kingdom" & corpusversion!="NA",]

# Check data availability of corpus-------------------
available_docs <- mp_availability(countryname == "United Kingdom" & date > 200000)

# Download corpus------
my_corpus <- mp_corpus(countryname == "United Kingdom" & date > 200000 )

# Subsetting the corpus by party-------------------------------

# Define the party codes
party_codes <- c("51320", "51620", "51421", "51951", "51110")

# Define the party names
party_names <- c("Labour_Party", "Conservative", "Liberal_Democrats", "UKIP", "Green_Party")

# Create a named vector to make it easier to access the results
names(party_codes) <- party_names

# Use lapply to loop over the party codes and extract documents for each
party_documents <- lapply(party_codes, function(code) {
  matches <- grepl(paste0("^", code), names(my_corpus))
  return(my_corpus[matches])
})

# Name the list elements with the party names
names(party_documents) <- party_names

tidied_corpora <- lapply(party_documents, function(documents) {
  as.data.frame(documents, with.meta = TRUE)
})

big_corpus <- bind_rows(tidied_corpora, .id = "party")

# ===========================================================================
#   2) Exploratory data analysis 
# ===========================================================================

# Assuming 'big_corpus' is the combined dataframe for all parties
# Unnest tokens to separate words
all_words <- big_corpus %>% 
  unnest_tokens(word, text)

# Filter for EU positive words and select relevant columns
eu_positive <- all_words %>%
  select(party, date, word, pos, cmp_code) %>%
  filter(cmp_code == 108)

# Compute TF-IDF while excluding stopwords and certain cmp_code values
tfidf_codes <- eu_positive %>%
  anti_join(get_stopwords(), by = "word") %>%
  #filter(is.na(as.numeric(word))) %>%
  #filter(!(cmp_code %in% c("H", "", "0", "000", NA))) %>%
  #mutate(cmp_code = recode_v5_to_v4(cmp_code)) %>%
  count(party, date, word) %>% # Include party in the grouping
  bind_tf_idf(word, party, n)

# Plot the top 10 words by TF-IDF for each party and year
tfidf_codes %>%
  mutate(year = substr(date,1,4)) %>% # Extract year from date
  group_by(party, year) %>%
  filter(party == "Labour_Party")%>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = as.factor(year))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~party + year, ncol = 2, scales = "free_y") + # Facet by party and year
  coord_flip()


