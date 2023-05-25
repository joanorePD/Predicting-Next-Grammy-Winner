load("final_df_n.RData")

# Sort genre frequencies in descending order and take top 10
top_10_genres <- head(sort(genre_counts, decreasing = TRUE), 10)

# Plot bar chart
barplot(top_10_genres, main = "Top 10 Genres", xlab = "Genres", ylab = "Count", las = 2)
