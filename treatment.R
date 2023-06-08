load("final_df_n.RData")

final_df_n$IsWinnerNumber <- ifelse(final_df_n$IsWinner == 'Nothing', 3, ifelse(final_df_n$IsWinner == 'Nominee', 2, 1))

# Group by all columns except 'salary'
grouped_df <- final_df_n %>% group_by_at(vars(track_id))

# Summarize or perform other operations on grouped data
summarized_df <- grouped_df %>% summarise(IsWinnerNumber = min(IsWinnerNumber))

grouped_df_str <- final_df_n %>% group_by_at(vars(-c(IsWinner, IsWinnerNumber)))
summarized_df_str <- grouped_df_str %>% summarise(IsWinnerNumber = min(IsWinnerNumber))

final_df_n_str <- final_df_n
final_df_n_str$str <- paste0(final_df_n_str$IsWinnerNumber, "-", final_df_n_str$track_id)
summarized_df_str$str <- paste0(summarized_df_str$IsWinnerNumber, "-", summarized_df_str$track_id)

final_df_n_str <- filter(summarized_df_str, str %in% summarized_df$str)
final_df_n_str$IsWinner <- ifelse(final_df_n_str$IsWinnerNumber == 3, 'Nothing', ifelse(final_df_n_str$IsWinnerNumber == 2, 'Nominee', 'Winner'))

final_df_n_str <- final_df_n_str[,!names(final_df_n_str) %in% c("track_id", "artist_id", "str", "IsWinnerNumber", "genres")]

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
summarized_df_genre <- final_df_n_str %>%
  rowwise() %>%
  mutate(genre = list(map2(c("soul", "hip hop", "pop", "rock", "blues", 
                       "rap", "jazz", "metal", "funk", "country", 
                       "r&b", "folk", "indie", "house", "electro",
                       "disco", 'reggaeton', 'trap', 'latino',
                       'edm', 'adult standards'), genres, findMatchingWord)))


summarized_df_genre <- final_df_n_str %>%
  rowwise() %>%
  mutate(genre = findMatchingWord(c("soul", "hip hop", "pop", "rock", "blues", 
                                    "rap", "jazz", "metal", "funk", "country", 
                                    "r&b", "folk", "indie", "house", "electro",
                                    "disco", 'reggaeton', 'trap', 'latino',
                                    'edm', 'adult standards'), genres))


save(final_df_n_str, file = "final_df_n_str.RData")

load("final_df_n_str.RData")
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
artist_counts <- table(summarized_df_genre$artist_name[summarized_df_genre$IsWinner == 'Winner'])

# Get the top 10 genres
top_10_artists <- head(sort(artist_counts, decreasing = TRUE), 10)

# Plot bar chart
barplot(top_10_artists, main = "Top 10 Artists", xlab = "Artists", ylab = "Count", las = 2)
