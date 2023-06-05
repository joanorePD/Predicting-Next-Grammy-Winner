load("final_df_n.RData")

final_df_n$IsWinnerNumber <- ifelse(final_df_n$IsWinner == 'Nothing', 3, ifelse(final_df_n$IsWinner == 'Nominee', 2, 1))

# Group by all columns except 'salary'
grouped_df <- final_df_n %>% group_by_at(vars(-c(IsWinnerNumber,IsWinner)))

# Summarize or perform other operations on grouped data
summarized_df <- grouped_df %>% summarise(IsWinnerNumber = min(IsWinnerNumber))

summarized_df$IsWinner <- ifelse(summarized_df$IsWinnerNumber == 3, 'Nothing', ifelse(summarized_df$IsWinnerNumber == 2, 'Nominee', 'Winner'))

summarized_df <- summarized_df[summarized_df$year >= 2000, ]

# Function to check if string contains a word from the vector and return the matching word
findMatchingWord <- function(word_vector, input_string) {
  pattern <- paste(word_vector, collapse = "|")
  matches <- regmatches(input_string, regexpr(pattern, input_string))
  if (length(matches) > 0) {
    return(na.omit(matches)[[1]])
  } else {
    return("")  # Return an empty string if no match found
  }
}

# Apply the get_track_name
summarized_df_genre <- summarized_df %>%
  rowwise() %>%
  mutate(genre = list(map2(c("soul", "hip hop", "pop", "rock", "blues", 
                       "rap", "jazz", "metal", "funk", "country", 
                       "r&b", "folk", "indie", "house", "electro",
                       "disco", 'reggaeton', 'trap', 'latino',
                       'edm', 'adult standards'), genres, findMatchingWord)))

summarized_df_genre <- summarized_df %>%
  rowwise() %>%
  mutate(genre = findMatchingWord(c("soul", "hip hop", "pop", "rock", "blues", 
                                    "rap", "jazz", "metal", "funk", "country", 
                                    "r&b", "folk", "indie", "house", "electro",
                                    "disco", 'reggaeton', 'trap', 'latino',
                                    'edm', 'adult standards'), genres))


# Count the occurrences of each genre
genre_counts <- table(na.omit(summarized_df_genre$genre))

# Get the top 10 genres
top_10_genres <- head(sort(genre_counts, decreasing = TRUE), 10)

# Create a bar plot of the top 10 genres
barplot(top_10_genres, main = "Top 10 Genres", xlab = "Genre", ylab = "Count")

# Plot bar chart
barplot(top_10_genres, main = "Top 10 Genres", xlab = "Genres", ylab = "Count", las = 2)

# Count the occurrences of each artist
artist_counts <- table(summarized_df_genre$artist_name[summarized_df_genre$IsWinner == 'Nominee' | summarized_df_genre$IsWinner == 'Winner'])

# Get the top 10 genres
top_10_artists <- head(sort(artist_counts, decreasing = TRUE), 10)

# Plot bar chart
barplot(top_10_artists, main = "Top 10 Artists", xlab = "Artists", ylab = "Count", las = 2)
