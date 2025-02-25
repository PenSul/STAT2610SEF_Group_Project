# **🎵 Music & Emotion: Does Song Genre Affect Listener Sentiment?**  
## **📌 1. Proposal Plan (One-Page Poster)**  

### **1️⃣ Title & Subtitle**  
**Title:** 🎵 _"Music & Emotion: Does Song Genre Affect Listener Sentiment?"_  
**Subtitle:** 🎤 _"An Analysis of Song Lyrics & Listener Comments Using Text Mining in R"_  

📌 *Make it bold & eye-catching!*  
📌 Add **music-related icons** (🎶, 🎧, 🎤)  

### **2️⃣ Background (Problem Statement)**  
- Music **affects human emotions**, but **do different genres impact listeners differently?**  
- We analyze **song lyrics & listener comments** to explore **sentiment variations across genres.**  
- Our study helps **understand emotional trends in music consumption.**  

### **3️⃣ Features (Methodology & Approach)**  
✅ **Data Collection:**  
- Scrape song lyrics from **Genius API** (`geniusr` package)  
- Collect listener comments from **YouTube, Reddit, or Spotify**  

✅ **Text Processing in R:**  
- Tokenization & stopword removal (`tidytext`, `tm`)  
- Sentiment analysis (`syuzhet`)  

✅ **Genre-Wise Sentiment Comparison:**  
- Compare **Rock, Pop, Hip-Hop, Classical, Jazz, EDM**  
- Determine **which genre has the most positive, negative, or neutral sentiment**  

### **4️⃣ Expected Findings & Impact**  
✅ **Identify emotional trends in music genres**  
✅ **Visualize how different genres affect listener mood**  
✅ **Help music producers and listeners understand the emotional impact of music**  

### **5️⃣ Visual Elements (Make It Stand Out!)**  
📌 **Infographic icons** (🎶, 🎧, mood faces)  
📌 **Color scheme:** Blue 🎵, Yellow 🌞, and Dark Grey 🎼  
📌 **Data visuals (word cloud, sentiment graph, pie chart)**  

---

# **📍 2. Data Collection**  

The dataset will consist of:  
1️⃣ **Song Lyrics** (To analyze the emotion in the lyrics)  
2️⃣ **Listener Comments** (To understand how listeners feel about the song)  

### **🔹 Step 1: Collect Song Lyrics (Using Genius API in R)**  
```r
install.packages("geniusr")
library(geniusr)

genius_token() # Authenticate Genius API

# Example: Fetch lyrics for a specific song
lyrics <- get_lyrics_search(artist_name = "Coldplay", song_title = "Yellow")
writeLines(lyrics$line, "coldplay_yellow.txt")
```  

📌 **Collect multiple songs across genres:**  
- **Rock 🎸** → Coldplay, Queen, Linkin Park  
- **Pop 🎤** → Taylor Swift, Ed Sheeran, Ariana Grande  
- **Hip-Hop 🎙️** → Drake, Kanye West, Eminem  
- **Classical 🎼** → Beethoven, Mozart, Bach (vocal pieces)  
- **Jazz 🎷** → Miles Davis, Louis Armstrong  
- **EDM 🎧** → Marshmello, Avicii, Calvin Harris  

---

### **🔹 Step 2: Collect Listener Comments (YouTube & Reddit Scraping)**  
```r
install.packages("tuber")
library(tuber)

yt_oauth("your_api_key", "your_api_secret")

# Fetch comments from a YouTube music video
comments <- get_comment_threads(video_id="dQw4w9WgXcQ", max_results=100)
write.csv(comments, "youtube_comments.csv", row.names=FALSE)
```
```r
install.packages("redditExtractoR")
library(redditExtractoR)

# Fetch comments from a music discussion thread
reddit_data <- get_reddit(search_terms = "best rock songs", subreddit = "Music", sort_by = "new", wait_time = 5)
write.csv(reddit_data, "reddit_comments.csv", row.names=FALSE)
```  

📌 **Ensure at least 10 articles (each 2000+ words) are collected.**  

---

# **📍 3. Text Processing & Sentiment Analysis**  

### **🔹 Step 1: Preprocess Text Data**  
```r
library(tidyverse)
library(tidytext)

lyrics <- readLines("coldplay_yellow.txt")
df <- data.frame(text = lyrics, stringsAsFactors=FALSE)

# Tokenization & Stopword Removal
tokens <- df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

print(tokens)
```  

### **🔹 Step 2: Sentiment Analysis with Syuzhet**  
```r
install.packages("syuzhet")
library(syuzhet)

sentiments <- get_nrc_sentiment(df$text)

# Summarize emotions
emotion_counts <- colSums(sentiments)
print(emotion_counts)
```  

📌 **Compare Sentiment Across Genres:**  
```r
avg_sentiment <- df %>%
  group_by(genre) %>%
  summarise(positive = mean(sentiments$positive),
            negative = mean(sentiments$negative))

print(avg_sentiment)
```  

---

# **📍 4. Data Visualization (For Poster & Report)**  

### **🔹 Word Cloud of Lyrics**  
```r
install.packages("wordcloud")
library(wordcloud)

wordcloud(df$text, min.freq = 5, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
```  

### **🔹 Sentiment Comparison Chart**  
```r
library(ggplot2)

ggplot(avg_sentiment, aes(x=genre, y=positive, fill=genre)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Positive Sentiment Across Music Genres", x="Genre", y="Average Sentiment Score")
```  

### **🔹 Emotion Heatmap**  
```r
library(reshape2)

emotion_matrix <- as.matrix(sentiments)

heatmap(emotion_matrix, col=colorRampPalette(c("red", "white", "blue"))(50))
```  

---

# **📍 5. Poster Design in Canva**  

✅ **Choose an Infographic Template** → Use **modern, music-themed visuals**  
✅ **Use Icons & Colors** → 🎵🎧🎼, Blue & Yellow for contrast  
✅ **Include Data Visuals** → Add **word cloud, sentiment graph, genre comparison chart**  
✅ **Organize Sections Clearly** → **Background, Features, Findings, Impact**  

---