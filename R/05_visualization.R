# STAT2610SEF_Group_Project/R/05_visualization.R
# Visualization functions for Music & Emotion Analysis project
# STAT 2610SEF Course Project - Spring 2025

#' Create word clouds for each genre
#'
#' Generates word cloud visualizations for the most common words in each genre.
#'
#' @param lyrics_tokens data.frame with processed lyrics tokens
#' @param min_freq minimum frequency for words to include
#' @param max_words maximum number of words to display
#' @return NULL (saves plots to files)
CreateGenreWordClouds <- function(lyrics_tokens, 
                                  min_freq = PROJECT_SETTINGS$min_word_freq, 
                                  max_words = PROJECT_SETTINGS$max_words) {
    # Get list of genres
    genres <- unique(lyrics_tokens$MajorityGenre)
    
    # Create a word cloud for each genre
    for (genre in genres) {
        # Filter for current genre and count words
        genre_words <- lyrics_tokens %>%
            filter(MajorityGenre == genre) %>%
            count(word, sort = TRUE)
        
        # Skip if not enough words
        if (nrow(genre_words) < 10) {
            warning("Not enough words for genre: ", genre)
            next
        }
        
        # Set up output file
        filename <- file.path(VISUALS_DIR, paste0("wordcloud_", gsub(" ", "_", genre), ".png"))
        png(filename, width = 800, height = 600, res = 100)
        
        # Generate word cloud
        wordcloud(
            words = genre_words$word,
            freq = genre_words$n,
            min.freq = min_freq,
            max.words = max_words,
            random.order = FALSE,
            rot.per = 0.35,
            colors = brewer.pal(8, "Dark2")
        )
        
        dev.off()
        cat("Created word cloud for", genre, "at", filename, "\n")
    }
    
    return(invisible(NULL))
}

#' Create sentiment distribution plots
#'
#' Generates visualizations of sentiment distributions.
#'
#' @param sentiment_data result from sentiment analysis functions
#' @param data_type character: "Lyrics" or "Comments"
#' @return NULL (saves plots to files)
CreateSentimentPlots <- function(sentiment_data, data_type = "Lyrics") {
    # Extract song sentiment scores
    if (data_type == "Lyrics") {
        sentiment_scores <- sentiment_data$song_sentiment
    } else {
        sentiment_scores <- sentiment_data$song_sentiment
    }
    
    # Check if we have NRC sentiment categories
    has_emotions <- all(c("joy", "sadness", "anger", "fear") %in% colnames(sentiment_scores))
    
    # Create positivity ratio plot
    if ("positivity_ratio" %in% colnames(sentiment_scores)) {
        p1 <- ggplot(sentiment_scores, aes(x = positivity_ratio)) +
            geom_histogram(binwidth = 0.05, fill = "#1976D2", color = "white", alpha = 0.8) +
            geom_vline(xintercept = 0.5, linetype = "dashed", color = "#616161") +
            labs(
                title = paste(data_type, "Positivity Ratio Distribution"),
                x = "Positivity Ratio (Positive / (Positive + Negative))",
                y = "Number of Songs"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10)
            )
        
        # Save plot
        filename <- file.path(VISUALS_DIR, paste0(tolower(data_type), "_positivity_dist.png"))
        ggsave(filename, p1, width = 8, height = 6, dpi = 300)
        cat("Created positivity distribution plot at", filename, "\n")
        
        # Create interactive version
        p1_interactive <- ggplotly(p1)
        htmlwidgets::saveWidget(
            p1_interactive, 
            file.path(VISUALS_DIR, paste0(tolower(data_type), "_positivity_dist_interactive.html")),
            selfcontained = TRUE
        )
    }
    
    # Create emotion proportion plot if using NRC lexicon
    if (has_emotions) {
        # Reshape data for plotting
        emotions <- c("joy", "trust", "fear", "surprise", "sadness", "disgust", "anger", "anticipation")
        emotions_present <- emotions[emotions %in% colnames(sentiment_scores)]
        
        emotion_data <- sentiment_scores %>%
            select(SongID, SongName, MajorityGenre, all_of(emotions_present)) %>%
            pivot_longer(
                cols = all_of(emotions_present),
                names_to = "emotion",
                values_to = "count"
            ) %>%
            # Calculate proportion for each song
            group_by(SongID) %>%
            mutate(
                total = sum(count),
                proportion = count / total
            ) %>%
            ungroup()
        
        # Create plot
        p2 <- ggplot(emotion_data, aes(x = emotion, y = proportion, fill = emotion)) +
            geom_boxplot(alpha = 0.7) +
            scale_fill_manual(values = unlist(EMOTION_COLORS[emotions_present])) +
            labs(
                title = paste("Emotion Distribution in", data_type),
                x = "Emotion",
                y = "Proportion within Song"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none"
            )
        
        # Save plot
        filename <- file.path(VISUALS_DIR, paste0(tolower(data_type), "_emotion_dist.png"))
        ggsave(filename, p2, width = 10, height = 6, dpi = 300)
        cat("Created emotion distribution plot at", filename, "\n")
        
        # Create interactive version
        p2_interactive <- ggplotly(p2)
        htmlwidgets::saveWidget(
            p2_interactive, 
            file.path(VISUALS_DIR, paste0(tolower(data_type), "_emotion_dist_interactive.html")),
            selfcontained = TRUE
        )
    }
    
    return(invisible(NULL))
}

#' Create genre comparison plots
#'
#' Generates visualizations comparing sentiment across genres.
#'
#' @param genre_sentiment result from CompareSentimentByGenre()
#' @return NULL (saves plots to files)
CreateGenreComparisonPlots <- function(genre_sentiment) {
    # Check if we have NRC sentiment categories
    has_emotions <- all(c("joy", "sadness", "anger", "fear") %in% colnames(genre_sentiment))
    
    # Create positivity ratio by genre plot
    if ("positivity_ratio" %in% colnames(genre_sentiment)) {
        # Sort genres by positivity ratio
        genre_ordered <- genre_sentiment %>%
            arrange(positivity_ratio) %>%
            pull(MajorityGenre)
        
        p1 <- ggplot(genre_sentiment, aes(x = factor(MajorityGenre, levels = genre_ordered), 
                                          y = positivity_ratio, 
                                          fill = MajorityGenre)) +
            geom_bar(stat = "identity", alpha = 0.8) +
            geom_hline(yintercept = 0.5, linetype = "dashed", color = "#616161") +
            coord_flip() +
            scale_fill_manual(values = unlist(GENRE_COLORS)) +
            labs(
                title = "Positivity Ratio by Genre",
                x = "Genre",
                y = "Positivity Ratio (Positive / (Positive + Negative))"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 10),
                legend.position = "none"
            )
        
        # Save plot
        filename <- file.path(VISUALS_DIR, "genre_positivity_comparison.png")
        ggsave(filename, p1, width = 10, height = 6, dpi = 300)
        cat("Created genre positivity comparison plot at", filename, "\n")
        
        # Create interactive version
        p1_interactive <- ggplotly(p1)
        htmlwidgets::saveWidget(
            p1_interactive, 
            file.path(VISUALS_DIR, "genre_positivity_comparison_interactive.html"),
            selfcontained = TRUE
        )
    }
    
    # Create emotion profiles by genre if using NRC lexicon
    if (has_emotions) {
        # Reshape data for plotting
        emotions <- c("joy", "trust", "fear", "surprise", "sadness", "disgust", "anger", "anticipation")
        emotions_present <- emotions[emotions %in% colnames(genre_sentiment)]
        
        emotion_data <- genre_sentiment %>%
            select(MajorityGenre, all_of(emotions_present)) %>%
            pivot_longer(
                cols = all_of(emotions_present),
                names_to = "emotion",
                values_to = "count"
            )
        
        # Calculate proportions
        emotion_proportions <- emotion_data %>%
            group_by(MajorityGenre) %>%
            mutate(
                total = sum(count),
                proportion = count / total
            ) %>%
            ungroup()
        
        # Create plot
        p2 <- ggplot(emotion_proportions, 
                     aes(x = emotion, y = proportion, fill = emotion, group = MajorityGenre)) +
            geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
            facet_wrap(~ MajorityGenre) +
            scale_fill_manual(values = unlist(EMOTION_COLORS[emotions_present])) +
            labs(
                title = "Emotion Profiles by Genre",
                x = "Emotion",
                y = "Proportion"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 12),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                strip.text = element_text(size = 10, face = "bold"),
                legend.position = "none"
            )
        
        # Save plot
        filename <- file.path(VISUALS_DIR, "genre_emotion_profiles.png")
        ggsave(filename, p2, width = 12, height = 8, dpi = 300)
        cat("Created genre emotion profiles plot at", filename, "\n")
        
        # Create interactive version
        p2_interactive <- ggplotly(p2)
        htmlwidgets::saveWidget(
            p2_interactive, 
            file.path(VISUALS_DIR, "genre_emotion_profiles_interactive.html"),
            selfcontained = TRUE
        )
    }
    
    return(invisible(NULL))
}

#' Create a comparison plot of lyrics vs comments sentiment
#'
#' @param comparison_data result from CompareLyricsAndComments()
#' @return NULL (saves plot to file)
CreateLyricsCommentsComparisonPlot <- function(comparison_data) {
    # Check if comparison data exists
    if (is.null(comparison_data)) {
        warning("No comparison data available for visualization.")
        return(NULL)
    }
    
    # Filter out NA values before plotting
    comparison_data <- comparison_data %>%
        filter(!is.na(lyrics_positivity) & !is.na(comments_positivity))
    
    # Create scatter plot
    p <- ggplot(comparison_data, aes(x = lyrics_positivity, y = comments_positivity, 
                                     color = MajorityGenre)) +
        geom_point(alpha = 0.7, size = 3) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#616161") +
        scale_color_manual(values = unlist(GENRE_COLORS)) +
        labs(
            title = "Lyrics vs. Comments Sentiment",
            subtitle = "Comparison of positivity ratio in lyrics and YouTube comments",
            x = "Lyrics Positivity Ratio",
            y = "Comments Positivity Ratio",
            color = "Genre"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10)
        )
    
    # Save plot
    filename <- file.path(VISUALS_DIR, "lyrics_comments_comparison.png")
    ggsave(filename, p, width = 10, height = 8, dpi = 300)
    cat("Created lyrics vs comments comparison plot at", filename, "\n")
    
    # Create interactive version with tooltips
    p_interactive <- ggplotly(
        p + geom_text(aes(label = SongName), vjust = -1, hjust = 0.5, size = 3, show.legend = FALSE)
    )
    
    htmlwidgets::saveWidget(
        p_interactive, 
        file.path(VISUALS_DIR, "lyrics_comments_comparison_interactive.html"),
        selfcontained = TRUE
    )
    
    return(invisible(NULL))
}