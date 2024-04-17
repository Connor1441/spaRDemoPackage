#' create_sports_plot
#'
#' @param data Give a dataframe with team names, with wins and losses
#'
#' @return A ggplot
#' @export
#'
#' @examples
#' create_sports_plot(data)

create_sports_plot <- function(data) {
  # Check if win_ratio column exists, and if not, create it
  if (!"win_ratio" %in% names(data)) {
    data$win_ratio <- with(data, ifelse(wins + losses == 0, NA, wins / (wins + losses)))
  }

  # Create the plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = team, y = win_ratio, fill = team)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Win Ratio of Sports Teams",
      x = "Team",
      y = "Win Ratio"
    ) +
    ggplot2::scale_fill_brewer(palette = "Pastel1") +
    ggplot2::theme(legend.position = "none") # Remove legend for aesthetics

  # Return the plot
  return(p)
}


