# STAT2610SEF_Group_Project/R/00_config.R
# Configuration file for Music & Emotion Analysis project
# STAT 2610SEF Course Project - Spring 2025

# :) better not steal the API, I will add license later.


# API credentials
API_CREDENTIALS <- list(
    genius = list(
        # Fill in your genius API
        client_id = "", 
        client_secret = "",
        redirect_uri = "http://localhost:1410/"
    ),
    youtube = list(
        # Fill in your Google Cloud API
        app_name = "STAT2610SEF-YouTube-Client",
        client_id = "",
        client_secret = "",
        api_key = ""
    )
)

# Project settings
PROJECT_SETTINGS <- list(
    csv_file_path = "song_list.csv",
    max_comments = 100,               # Maximum number of comments to collect per song
    sentiment_lexicon = "bing",
    min_word_freq = 3,                # Minimum word frequency for word clouds
    max_words = 100                   # Maximum number of words in word clouds
)

# Define global file paths
BASE_DIR <- "C:/Projects/R_GP/STAT2610SEF_Group_Project"
DATA_DIR <- file.path(BASE_DIR, "Data")
LYRICS_DIR <- file.path(DATA_DIR, "Lyrics")
COMMENTS_DIR <- file.path(DATA_DIR, "Comments")
OUTPUT_DIR <- file.path(DATA_DIR, "Output")
VISUALS_DIR <- file.path(BASE_DIR, "Visualizations")

# Emotion categories and colors for visualization
EMOTION_COLORS <- list(
    joy = "#FFD700",         # Gold
    trust = "#32CD32",       # Lime Green
    fear = "#800080",        # Purple
    surprise = "#FF8C00",    # Dark Orange
    sadness = "#4169E1",     # Royal Blue
    disgust = "#006400",     # Dark Green
    anger = "#FF0000",       # Red
    anticipation = "#FF69B4" # Hot Pink
)

# Sentiment colors for visualization
SENTIMENT_COLORS <- list(
    positive = "#4CAF50",   # Green
    negative = "#F44336",   # Red
    neutral = "#9E9E9E"     # Grey
)

# Genre colors for visualization
GENRE_COLORS <- list(
    "Pop" = "#E91E63",        # Pink
    "Rock" = "#2196F3",       # Blue
    "Hip Hop" = "#FF9800",    # Orange
    "R&B" = "#9C27B0",        # Purple
    "Country" = "#8BC34A",    # Light Green
    "Electronic" = "#00BCD4", # Cyan
    "Jazz" = "#FFC107",       # Amber
    "Classical" = "#607D8B",  # Blue Grey
    "Folk" = "#795548",       # Brown
    "Metal" = "#000000",      # Black
    "Reggae" = "#CDDC39",     # Lime
    "Blues" = "#3F51B5",      # Indigo
    "Other" = "#9E9E9E"       # Grey
)

# StopWords to exclude from text analysis
CUSTOM_STOPWORDS <- c(
    "feat", "ft", "chorus", "verse", "bridge", "intro", "outro", "pre-chorus",
    "hook", "refrain", "interlude", "remix", "edit", "version", "remaster",
    "explicit", "clean", "instrumental", "acoustic", "live", "studio",
    "yeah", "hey", "oh", "uh", "mm", "hmm", "ah", "la", "na", "da", "oo", "woah",
    "gonna", "gotta", "wanna", "cuz", "cause", "ain't", "y'all"
)

# Create paths object for easier reference
PATHS <- list(
    base = BASE_DIR,
    data = DATA_DIR,
    lyrics = LYRICS_DIR,
    comments = COMMENTS_DIR,
    output = OUTPUT_DIR,
    visuals = VISUALS_DIR
)