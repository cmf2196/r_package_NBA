% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_oldest_player.R
\name{get_oldest_player}
\alias{get_oldest_player}
\title{Oldest NBA Player}
\usage{
get_oldest_player(df, year = 1986)
}
\arguments{
\item{df}{This is your data}

\item{Year}{what year do you want? Defaults to 1986}
}
\description{
This function gets the oldest NBA player in a given year.
In the event of a tie, it defaults to the player with most points.
}
\examples{
get_oldest_player()
}
\keyword{NBA}
\keyword{age,}
\keyword{points,}
