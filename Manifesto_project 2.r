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
#   0) Settings on the loading and processing of data
# ===========================================================================


# Housekeeping ------------------------------------------------------------
rm(list = ls())

# Controls ----------------------------------------------------------------

# Default settings and connections 
key <- "manifesto_apikey.txt"


# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("data.table", "openxlsx","zoo", "readr", "manifestoR", "stringr", "dplyr", "tidytext", "stopwords", "ggplot2", "purrr", "lubridate", "tidyr", "topicmodels", "tm", "quanteda", "forcats")

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
#   1) Data acquisition, description, and preparation
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
mpds_UK <- mpds[countryname=="United Kingdom",]

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
big_corpus <- data.table(big_corpus)
big_corpus[,party_date := paste(party, date, sep = "_")]
big_corpus[,uniqueID:= paste(manifesto_id, pos, sep = "_")]
big_corpus[,text_count := .N, by = c("party", "date")]
big_corpus[cmp_code == 108 | cmp_code == 110,text_count_EU := .N, by = c("party", "date")]

#Creating a subset of corpus with only texts related to EU
big_corpus_EU <- big_corpus[cmp_code == 108 | cmp_code == 110,]

#Creating Document-Feature-Matrix of the texts

# Define the additional words to remove
additional_stopwords <- c("eu", "uk", "european", "europe", "britain", "british", "union")


dfmat <- big_corpus_EU$text %>%
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = c(stopwords("en"), additional_stopwords)) %>%
  tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5)

rownames(dfmat) <- big_corpus_EU$uniqueID

# Remove rows with only zero entries
dfmat <- dfmat[rowSums(dfmat != 0) > 0, ]
dfmat

###Redoing the excercise to create different dfmat for each party-year

# Assuming you have a 'party' and 'year' column in big_corpus_EU
party_names <- unique(big_corpus_EU$party)
years <- unique(big_corpus_EU$date)

# Initialize an empty list to store dfmats
list_of_dfmats <- list()

library(quanteda)
library(dplyr)

# Assuming you have a 'party' and 'year' column in big_corpus_EU
party_names <- unique(big_corpus_EU$party)
years <- unique(big_corpus_EU$date)

# Initialize an empty list to store dfmats
list_of_dfmats <- list()

for (party in party_names) {
  for (year in years) {
    # Filter the data for the current party and year
    filtered_corpus <- subset(big_corpus_EU, party == party & date == year)
    
    # Check if the filtered corpus is not empty
    if (nrow(filtered_corpus) > 0) {
      # Process the text and create dfmat
      dfmat <- filtered_corpus$text %>%
        tokens(remove_punct = TRUE) %>%
        tokens_remove(pattern = c(stopwords("en"), additional_stopwords)) %>%
        tokens_replace(pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) %>%
        dfm() %>%
        dfm_trim(min_termfreq = 5)
      
      rownames(dfmat) <- filtered_corpus$uniqueID
      
      # Remove rows with only zero entries
      dfmat <- dfmat[rowSums(dfmat != 0) > 0, ]
      
      # Store the dfmat in the list with a unique name
      list_of_dfmats[[paste(party, year, sep = "_")]] <- dfmat
    } else {
      # Optionally, add a NULL or similar marker for empty datasets
      list_of_dfmats[[paste(party, year, sep = "_")]] <- NULL
    }
  }
}



#Descriptive statistics

party_desc <- unique(big_corpus$party)
years_desc <- unique(big_corpus$date)
hist(big_corpus$text_count)
hist(big_corpus$text_count_EU)
summary(big_corpus$text_count)
summary(big_corpus$text_count_EU)

library(knitr)

# Assuming 'big_corpus' is your data and 'text_count' is the column you want to summarize
text_count_summary <- summary(big_corpus$text_count)

# Create a data frame from the summary for better formatting
text_count_summary_df <- data.frame(Statistic = names(text_count_summary), 
                                    Value = as.vector(text_count_summary))

# Use kable to create a Markdown table
kable_text_count_summary <- kable(text_count_summary_df, format = "markdown", 
                                  col.names = c("Statistic", "Value"),
                                  caption = "Summary of Text Count in Big Corpus")

# Print the table (or use cat to display it in the console)
cat(kable_text_count_summary)

"""
In this research, political manifestos from key United Kingdom parties – specifically the Conservatives, Liberal Democrats, Labour, Green Party, and UKIP – have been methodically downloaded and analyzed. The focus was on documents published since the year 2000, resulting in a comprehensive collection encompassing the years 2001, 2015, 2017, and 2019. This corpus provides a substantial dataset for examining political discourse across different party lines and electoral cycles.

Quantitative analysis of the text count for each party-year pair reveals a diverse range of manifesto lengths. The data, as summarized in the following table, indicates a minimum text count of 1, extending to a maximum of 2,377 texts within a single manifesto. On average, the manifestos comprised approximately 1,650 texts, with the median value marginally lower at 1,612 texts. The distribution of text counts exhibits considerable variability, as evidenced by the first quartile at 1,396 texts and the third quartile at 1,867 texts.

Furthermore, a focused examination of the European Union (EU) related content within these manifestos uncovers varying degrees of engagement with EU topics. The analysis shows a minimum of 6 EU-related texts in certain party-year pairs, whilst the maximum observed is 231 texts, indicating heightened attention to EU matters in specific instances. The median number of EU-related texts stands at 66, with an average of 103.5, suggesting a moderate yet significant presence of EU discourse across the corpus. It is noteworthy that there were 21,812 instances where EU-related content was absent, reflecting the diverse priorities and strategic emphases of the parties in their manifesto compositions over the years.

"""
# ===========================================================================
#   2) Research Question
# ===========================================================================

"""
This research aims to explore the evolution of political discourse surrounding the European Union (EU) across major UK political parties, focusing on how topics and specific terminologies related to the EU have transformed over time, with particular emphasis on the period following the Brexit referendum. The primary research question is: How have UK political parties discussions and thematic focus on the European Union evolved in their manifestos from 2000 onwards, and what notable shifts in discourse can be observed in the aftermath of the Brexit vote? This investigation seeks to identify key themes, recurrent words, and any significant changes in the narrative surrounding the EU, thereby shedding light on the political and ideological shifts within these parties.

The feasibility of addressing this research question with the available data and methods is promising. The comprehensive collection of party manifestos from 2000 onwards, including pivotal years like 2015, 2017, and 2019, provides a rich textual corpus for longitudinal analysis. The use of topic modeling, a machine learning technique adept at uncovering latent thematic structures within large text corpora, is particularly well-suited for this analysis. It can effectively reveal dominant topics and track their evolution across different time periods. Moreover, the dataset s granuality, with detailed text counts and a focus on EU-related content, allows for a nuanced examination of shifts in political rhetoric, especially in response to a significant event like Brexit.

However, it is important to note that topic modeling, while powerful, has limitations. It can identify patterns and topics but does not inherently capture the context or the sentiment behind the text. Additional qualitative analysis or sentiment analysis might be required to fully understand the implications of the shifts in discourse. Furthermore, the absence of texts related to the EU in certain party-year pairs indicates varying levels of emphasis on the EU topic, which could impact the completeness of the analysis for those specific instances. Despite these limitations, the methods and data at hand provide a robust foundation for investigating the proposed research question.

"""
# ===========================================================================
#   3) Topic model development
# ===========================================================================
#### Over the whole corpus
# Choose the number of topics
num_topics <- 10  # This is a hyperparameter you need to choose

# Create the LDA topic model
lda_model <- LDA(dfmat, k = num_topics, control = list(seed = 1234))

#Dimensions of LDA
print(dim(lda_model@gamma))

# Print topic words
topic_words <- tidy(lda_model, matrix="beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)
topic_words

# Graph
topic_words %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


####By country and year

num_topics <- 10  # Number of topics
list_of_lda_models <- list()
list_of_topic_words <- list()

for (name in names(list_of_dfmats)) {
  dfmat <- list_of_dfmats[[name]]
  
  # Check if dfmat has enough documents and terms for LDA
  if (nrow(dfmat) > num_topics && ncol(dfmat) > num_topics) {
    lda_model <- LDA(dfmat, k = num_topics, control = list(seed = 1234))
    list_of_lda_models[[name]] <- lda_model
    
    # Extracting and storing topic words
    topic_words <- tidy(lda_model, matrix = "beta") %>%
      group_by(topic) %>%
      slice_max(beta, n = 10) %>%
      ungroup() %>%
      arrange(topic, -beta)
    list_of_topic_words[[name]] <- topic_words
  }
}


# Loop over each party-year pair in list_of_topic_words
for (name in names(list_of_topic_words)) {
  topic_words <- list_of_topic_words[[name]]
  
  # Create the plot
  plot <- topic_words %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() +
    labs(title = paste("Topics for", name), x = "Beta", y = "Terms") +
    theme_minimal()
  
  # Display or save the plot
  print(plot)  # use print() to display the plot in a loop
  
  # Optionally, save each plot as an image file
  ggsave(filename = paste0("Topic_Model_", name, ".png"), plot = plot, width = 10, height = 6)
}



# ===========================================================================
#   5) Exploratory data analysis 
# ===========================================================================

# Assuming 'big_corpus' is the combined dataframe for all parties
# Unnest tokens to separate words
all_words <- big_corpus %>% 
  unnest_tokens(word, text)

# Filter for EU positive words and select relevant columns
eu_positive <- all_words %>%
  select(party, date, party_date, word, pos, cmp_code) %>%
  filter(cmp_code == 108)


# Compute TF-IDF while excluding stopwords and certain cmp_code values
tfidf_codes <- eu_positive %>%
  # Remove stopwords
  anti_join(get_stopwords(), by = "word") %>%
  # Count occurrences of each word for each party-date combination
  count(party, date, party_date, word) %>%
  # Calculate TF-IDF
  bind_tf_idf(word, party_date, n)%>%
  mutate(year = substr(date, 1, 4))

# Plot the top 10 words by TF-IDF for each party and year

for (p in party_names) {
  plot <- tfidf_codes %>%
    group_by(party, year) %>%
    filter(party == p) %>%
    arrange(desc(tf_idf)) %>%  # Sort by tf_idf in descending order
    slice(1:10) %>%  # Select the top 10 observations
    ungroup() %>%
    ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = as.factor(year))) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf", title = paste("TF-IDF Scores for", p)) +
    facet_wrap(~party + year, ncol = 2, scales = "free_y") +
    coord_flip()
  
  # Save the plot
  ggsave(filename = paste0("TFIDF_", p, ".png"), plot = plot, width = 10, height = 6)
}

unique_years <- unique(tfidf_codes$year)

for (y in unique_years) {
  plot <- tfidf_codes %>%
    group_by(party, year) %>%
    filter(year == y) %>%
    arrange(desc(tf_idf)) %>%  # Sort by tf_idf in descending order
    slice(1:10) %>%  # Select the top 10 observations
    ungroup() %>%
    ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = as.factor(party))) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf", title = paste("TF-IDF Scores for Year", y)) +
    facet_wrap(~party, ncol = 2, scales = "free_y") +
    coord_flip()
  
  # Save the plot
  ggsave(filename = paste0("TFIDF_Year_", y, ".png"), plot = plot, width = 10, height = 6)
}
