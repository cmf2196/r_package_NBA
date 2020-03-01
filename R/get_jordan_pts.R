

#' Michael Jordan's Points
#'
#' This function provides you MJ's total points for a given year
#' @param df The data you will be using for this function.
#' @param Year what year do you want? Defaults to 1986
#' @keywords mj , pts
#' @export
#' @examples
#' get_jordan_pts()


# this function takes in a year (integer)as an argument
# it will return the amount of points michael jordan had that year
# if mj did not play that year it will return please choose another year.
get_jordan_pts <- function(df , year = 1986) {

  # read the data
  bulk_data = df
  # select the year.
  filter(bulk_data , Year == year)  %>%
    select(Player, PTS) %>%
    filter(Player == 'Michael Jordan' | Player == 'Michael Jordan*' ) %>%
    select(PTS) -> jordan_pts

  if (nrow(jordan_pts) == 0) {
    print(" Jordan did not play this year")
    return(0)
  }
  else {
    pt_val <- unlist(jordan_pts)
    return(pt_val[1])
  }

}
