# STAT2610SEF_Group_Project/R/01_setup.R
# Setup functions


#' Create necessary directories for the project
#'
#' Creates base directories for data storage if they don't exist
#'
#' @return TRUE if successful
CreateDirectories <- function() {
    # Create main data directory
    if (!dir.exists(DATA_DIR)) {
        dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Create lyrics directory
    if (!dir.exists(LYRICS_DIR)) {
        dir.create(LYRICS_DIR, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Create comments directory
    if (!dir.exists(COMMENTS_DIR)) {
        dir.create(COMMENTS_DIR, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Create output directory
    if (!dir.exists(OUTPUT_DIR)) {
        dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Create visualizations directory
    if (!dir.exists(VISUALS_DIR)) {
        dir.create(VISUALS_DIR, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Log
    cat("Project directories created or verified:\n")
    cat("- Data directory:", DATA_DIR, "\n")
    cat("- Lyrics directory:", LYRICS_DIR, "\n")
    cat("- Comments directory:", COMMENTS_DIR, "\n")
    cat("- Output directory:", OUTPUT_DIR, "\n")
    cat("- Visualizations directory:", VISUALS_DIR, "\n")
    
    return(TRUE)
}

#' Check required packages and install if missing
#'
#' Verifies that all required packages are installed
#'
#' @return TRUE if all packages are installed successfully
CheckAndInstallPackages <- function() {
    requiredPackages <- c(
        "tidyverse", "tidytext", "tuber", "rvest", "httr", "jsonlite",
        "textdata", "sentimentr", "wordcloud", "plotly", "knitr",
        "rmarkdown", "dplyr", "stringr", "ggplot2", "scales",
        "ggrepel", "htmlwidgets"
    )
    
    # Check and install missing packages
    newPackages <- requiredPackages[!requiredPackages %in% installed.packages()[,"Package"]]
    if (length(newPackages) > 0) {
        cat("Installing missing packages:", paste(newPackages, collapse = ", "), "\n")
        install.packages(newPackages, dependencies = TRUE)
    } else {
        cat("All required packages are installed.\n")
    }
    
    # Load all packages
    for (pkg in requiredPackages) {
        library(pkg, character.only = TRUE)
    }
    return(TRUE)
}

#' Download and prepare sentiment lexicons
#'
#' Downloads sentiment lexicons for text analysis
#'
#' @return list of loaded lexicons
PrepareTextData <- function() {
    # Initialize list to store lexicons
    lexicons <- list()
    
    # Get NRC lexicon
    tryCatch({
        lexicons$nrc <- get_sentiments("nrc")
        cat("NRC sentiment lexicon loaded.\n")
    }, error = function(e) {
        warning("Failed to load NRC lexicon: ", e$message)
    })
    
    # Get AFINN lexicon
    tryCatch({
        lexicons$afinn <- get_sentiments("afinn")
        cat("AFINN sentiment lexicon loaded.\n")
    }, error = function(e) {
        warning("Failed to load AFINN lexicon: ", e$message)
    })
    
    # Get Bing lexicon
    tryCatch({
        lexicons$bing <- get_sentiments("bing")
        cat("Bing sentiment lexicon loaded.\n")
    }, error = function(e) {
        warning("Failed to load Bing lexicon: ", e$message)
    })
    
    # Get Loughran lexicon
    tryCatch({
        lexicons$loughran <- get_sentiments("loughran")
        cat("Loughran sentiment lexicon loaded.\n")
    }, error = function(e) {
        warning("Failed to load Loughran lexicon: ", e$message)
    })
    return(lexicons)
}

#' Get stopwords list
#'
#' Combines standard stopwords with music-related stopwords
#'
#' @return character vector of stopwords
GetStopwords <- function() {
    # Start with basic stopwords from tidytext
    stopwords <- tidytext::stop_words$word
    
    # Add stopwords
    stopwords <- unique(c(stopwords, CUSTOM_STOPWORDS))
    
    return(stopwords)
}
