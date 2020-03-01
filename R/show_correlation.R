
#' NBA Correlation
#'
#' This function provides you with the correlation between statistics for a given year.
#' @param df The data you will be using
#' @param Year what year do you want? Defaults to 1986
#' @keywords correlation , NBA
#' @export
#' @examples
#' show_correlation()


# this function takes in a year (integer)as an argument
# it will return the correlation matrix for the stats that season


show_correlation<- function(df , year = 1986) {

  # read the data
  bulk_data = df
  # select the year.
  filter(bulk_data , Year == year)  %>%      # filter by the  indicated year
    keep(is.numeric ) -> numeric_cols         # only keep numeric columns

  within(numeric_cols, rm(Year)) -> for_cor # this is done to remove NA from cor  (0 variance )
  c = cor(for_cor , use = "complete.obs" )
  corrplot(c, method="circle")

}
