# Music Sentiment Analysis Project

This tool analyzes sentiment patterns in song lyrics and YouTube comments across different 
music genres. It extracts lyrics, collects YouTube comments, performs sentiment analysis, 
and generates visualizations of the results.

## Prerequisites

- R (version 4.0.0 or higher recommended)
- RStudio (recommended for easier execution)
- Internet connection
- Google account (provided below)
- Access to Genius API (via the Google account)

## Setup Instructions

### Step 1: Clone or Download the Repository

Download the entire project folder to your local machine.

### Step 2: Set Up API Access

Before running the analysis, you need to set up API access for Google/YouTube and Genius:

#### Google/YouTube API Setup

1. Sign in to the provided Google account:
   - Email: Your Google Account
   - Password: *[Use the password provided to you separately]*

2. Visit the [Google Cloud Console](https://console.cloud.google.com/)
   - Make sure you're signed in with the your Google account
   - The YouTube API should already be configured for this account

#### Genius API Setup

1. Visit [Genius API Clients](https://genius.com/api-clients) 
   - Sign in using the same Google account
   - The API client should already be set up

### Step 3: Install R Dependencies

Open RStudio and run the following command to install all required packages:

```r
install.packages(c(
  "tidyverse", "tidytext", "tuber", "rvest", "httr", "jsonlite",
  "textdata", "sentimentr", "wordcloud", "plotly", "knitr",
  "rmarkdown", "dplyr", "stringr", "ggplot2", "scales", 
  "ggrepel", "htmlwidgets", "RColorBrewer"
))
```

*Note: This step may take several minutes. Some packages might prompt you to select a CRAN mirror - choose one close to your location.*

### Step 4: Prepare Input Data

1. Verify that the `song_list.csv` file exists in the project root directory
   - This file should contain the songs to analyze with columns:
     - MajorityGenre
     - MinorityGenre
     - SongName
     - ArtistName
     - SongLink (YouTube URL)

### Step 5: Run the Analysis

1. Open the project in RStudio

2. Set the working directory to the project root folder

3. Open the `main.R` file

4. Before running, make sure the API credentials in `00_config.R` match those from the provided account
   - API keys should already be set up correctly in the script

5. Run the `main.R` script:

6. When prompted for authentication:
   - For YouTube API: A browser window will open asking you to allow the application to access YouTube data. Sign in with the provided Google account and grant permissions.
   - For Genius API: A similar authentication flow may occur.

7. Wait for the analysis to complete (this may take several minutes depending on the number of songs)

### Step 6: View Results

The script will create several directories with the results:

- `Data/`: Contains raw data and processed text files
  - `Lyrics/`: Individual lyrics files
  - `Comments/`: YouTube comments data
  - `Output/`: Processed data files

- `Visualizations/`: Contains all generated plots and visualizations
  - PNG files for static visualizations
  - HTML files for interactive visualizations

Open the visualizations in your browser to explore the analysis results interactively.

## Troubleshooting

### API Authentication Issues

If you encounter authentication problems:

1. Verify you're using the correct Google account
2. Check the API keys in `00_config.R` match those from the provided account
3. If authentication windows don't appear, try running these commands directly:
   ```r
   library(tuber)
   yt_oauth(API_CREDENTIALS$youtube$client_id, API_CREDENTIALS$youtube$client_secret)
   ```

### Package Loading Errors

If you get errors about missing packages:

1. Run the `CheckAndInstallPackages()` function manually:
   ```r
   source("R/01_setup.R")
   CheckAndInstallPackages()
   ```

2. Install problematic packages manually:
   ```r
   install.packages("package_name")
   ```

### Data Collection Issues

If lyrics or comments aren't being collected:

1. Check your internet connection
2. Verify the YouTube URLs in your song list are valid
3. Some songs may not have lyrics available in Genius - the script will use fallback mechanisms