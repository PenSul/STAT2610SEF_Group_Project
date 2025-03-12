# STAT2610SEF_Group_Project/R/main.R
# Main script for Music & Emotion Analysis project
# STAT 2610SEF Course Project - Spring 2025


library(tidyverse)
library(tidytext)
library(tuber)
library(rvest)
library(httr)
library(jsonlite)
library(textdata)
library(sentimentr)
library(wordcloud)
library(plotly)
library(knitr)
library(rmarkdown)
library(dplyr)


source("R/00_config.R")
source("R/01_setup.R")
source("R/02_data_collection.R")
source("R/03_text_processing.R")
source("R/04_sentiment_analysis.R")
source("R/05_visualization.R")
source("R/lyrics_scraper.R")
source("R/youtube_scraper.R")

# Pre-Steo: Create necessary directories and check packages
cat("Setting up project environment...\n")
CreateDirectories()
CheckAndInstallPackages()

# Step 1: Read song list from CSV
cat("Reading song data...\n")
songData <- ReadSongList()

# Step 2: Setup API authentication
cat("Setting up API authentication...\n")
apiTokens <- SetupAPITokens()

# Step 3: Collect lyrics data (trying multiple methods)
cat("Collecting lyrics data...\n")
lyricsData <- CollectLyricsDirectly(songData)

# Still Step 3 after collect: If no lyrics found with scraping, use manual sample lyrics for testing
if (nrow(lyricsData) == 0 || !any(lyricsData$LyricsStatus == "found")) {
    cat("No lyrics found via scraping. Using sample lyrics for testing purposes...\n")
    lyricsData <- AddManualLyrics(songData)
}

# Step 4: Collect YouTube comments (using direct API key calls)
cat("Collecting YouTube comments...\n")
commentsData <- CollectYouTubeCommentsWithAPIKey(songData, PROJECT_SETTINGS$max_comments)

# Only continue if have some lyrics
if (nrow(lyricsData) > 0 && any(lyricsData$LyricsStatus == "found")) {
    # Step 5: Process text data
    cat("Processing lyrics text...\n")
    stopwords <- GetStopwords()
    lyrics_tokens <- ProcessLyrics(lyricsData, stopwords)
    
    if (!is.null(commentsData) && nrow(commentsData) > 0) {
        cat("Processing comments text...\n")
        comment_tokens <- ProcessComments(commentsData, stopwords)
    } else {
        comment_tokens <- NULL
    }
    
    # Step 6: Extract key words by genre
    cat("Calculating TF-IDF by genre...\n")
    genre_tf_idf <- CalculateGenreTFIDF(lyrics_tokens)
    
    # Step 7: Extract n-grams (this is optional, may not use depend on time)
    cat("Extracting bigrams from lyrics...\n")
    lyrics_bigrams <- ExtractNgrams(lyricsData, n = 2, stopwords)
    
    # Step 8: Perform sentiment analysis
    cat("Analyzing lyrics sentiment...\n")
    lyrics_sentiment <- AnalyzeLyricsSentiment(lyrics_tokens)
    
    if (!is.null(comment_tokens) && nrow(comment_tokens) > 0) {
        cat("Analyzing comments sentiment...\n")
        comments_sentiment <- AnalyzeCommentsSentiment(comment_tokens)
    } else {
        comments_sentiment <- NULL
    }
    
    # Step 9: Compare sentiment across genres
    cat("Comparing sentiment across genres...\n")
    genre_sentiment <- CompareSentimentByGenre(lyrics_sentiment)
    
    # Step 10: Compare lyrics and comments sentiment
    if (!is.null(comments_sentiment)) {
        cat("Comparing lyrics and comments sentiment...\n")
        lyrics_comments_comparison <- CompareLyricsAndComments(lyrics_sentiment, comments_sentiment)
    } else {
        lyrics_comments_comparison <- NULL
    }
    
    # Step 11: Generate visualizations
    cat("Generating visualizations...\n")
    
    # Word clouds by genre
    CreateGenreWordClouds(lyrics_tokens)
    
    # Sentiment distribution plots
    CreateSentimentPlots(lyrics_sentiment, "Lyrics")
    if (!is.null(comments_sentiment)) {
        CreateSentimentPlots(comments_sentiment, "Comments")
    }
    
    # Genre comparison plots
    CreateGenreComparisonPlots(genre_sentiment)
    
    # Lyrics vs comments comparison
    if (!is.null(lyrics_comments_comparison)) {
        CreateLyricsCommentsComparisonPlot(lyrics_comments_comparison)
    }
    
    cat("Analysis complete!\n")
    
    # Step 12: Generate report
    # cat("Generating final report...\n")
    # Only include if have the report generation function (IDK will write or not)
    # GenerateReport()
    
} else {
    stop("No lyrics were found. Cannot continue with analysis.")
}