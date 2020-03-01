
#' Oldest NBA Player
#'
#' This function gets the oldest NBA player in a given year.
#' In the event of a tie, it defaults to the player with most points.
#' @param df This is your data
#' @param Year what year do you want? Defaults to 1986
#' @keywords age, points, NBA
#' @export
#' @examples
#' get_oldest_player()



get_oldest_player <- function(df , year = 1986) {

  # read the data
  bulk_data = df
  # select the year.
  filter(bulk_data , Year == year)  %>%               # filter by the  indicated year
    select(Player, Age , PTS) %>%                     # select just the three columns
    filter(Age == max(Age)) %>%                     # filter to the oldest age
    arrange(desc(PTS) ) %>%                       # order them from most PTS to least
    head(1) -> oldest_scorer                    # take the top one

  return(oldest_scorer[['Player']])

}
