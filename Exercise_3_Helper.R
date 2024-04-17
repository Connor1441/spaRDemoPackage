library(ggplot2)

create_sports_plot <- function(data) {
  # Check if win_ratio column exists, and if not, create it
  if (!"win_ratio" %in% names(data)) {
    data$win_ratio <- with(data, ifelse(wins + losses == 0, NA, wins / (wins + losses)))
  }

  # Create the plot
  p <- ggplot(data, aes(x = team, y = win_ratio, fill = team)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(
      title = "Win Ratio of Sports Teams",
      x = "Team",
      y = "Win Ratio"
    ) +
    scale_fill_brewer(palette = "Pastel1") +
    theme(legend.position = "none") # Remove legend for aesthetics

  # Return the plot
  return(p)
}

# Now let's test the function with the sports_records dataframe
sports_plot <- create_sports_plot(sports_records)

# Print the plot
print(sports_plot)
