% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stv.R
\name{stv}
\alias{stv}
\title{Implement STV Counting Systems}
\usage{
stv(
  x,
  seats = 1,
  file = "",
  surplusMethod = "Cambridge",
  quotaMethod = "Droop",
  quotaFloor = TRUE
)
}
\arguments{
\item{x}{a data frame with rows of ballots and columns for each
candidate. \code{x} must pass all checks from
\code{\link{validateBallots}}.}

\item{seats}{an integer (default = 1) indicating candidates to elect.}

\item{file}{a character string naming a file; \code{""} (default) indicates 
output to the console only (default), otherwise a CSV file is created
(and thus \code{file} should end with \code{".csv"}).}

\item{surplusMethod}{a character string indicating which method to use for
surplus allocation. Currently supports \code{"Cambridge"} (default) and
\code{"Fractional"}.}

\item{quotaMethod}{a character string indicating which method to use for
calculation of quota. Currently supports \code{"Droop"} (default) and 
\code{"Hare"}.}

\item{quotaFloor}{a logical value indicating that candidates must reach
the integer part of the quota in order to be elected if \code{"TRUE"} 
(default) or that they must reach the exact quota which may be 
fractional if \code{"FALSE"}.}
}
\value{
The object returned is a list consisting of two components: a
    vector of the elected candidates, and a data frame with rows containing
    detailed results from each round of STV counting.
    
    For every round of vote counting, a row of the detailed information
    contains: number of active ballots, seats to remaining to be filled,
    the current quota, the maximum and minimum votes obtained by each
    candidate, who was eliminated (if applicable), whether there was a
    tie for elimination (indicated by how many tied), who was elected
    (if applicable), the surplus if elected (or multiple surpluses if
    multiple candidates were elected), and each candidate's votes tally
    for that round.
}
\description{
Analyze a data frame of STV election ballot rankings and return
the elected voters and details of the steps in the election counting.
}
\details{
Revise the details:
\code{stv()} first validates \code{x} by running the \code{validateBallots()} function.
Once validation is complete, it implements the selected single transferable vote
counting method. Each round of counting starts with idetification of active
ballots. Then a quota is calculated (currently only supports Droop method 
\code{floor(votes/(seats + 1)) + 1} and Hare method \code{floor(votes/seats)})
with or without the \code{floor()} rounding. A tally of each candidate's vote share 
is obtained using top choices of active ballots, where a ballot stays active until it 
runs out of marked choices or gets removed during surplus reallocation. If a candidate 
reaches the quota, she/he is elected and associated surplus ballots are reallocated 
(currently only supports Cambridge and Fractional methods). If multiple candidates reach 
the quota, all of them are elected and their surpluses are all reallocated. If no candidate 
reaches the quota, then the candidate with the minimum number of votes is eliminated. 
If multiple candidates tie for minimum number of votes, one of them is selected at random 
and eliminated. The process is repeated until all of the seats are filled or the number of 
candidates still in race equals the number of unfilled seats. In the later case, 
all of the active candidates are elected.
}
\examples{
data(ballots)
cballots <- cleanBallots(ballots)

set.seed(1)
result1 <- stv(cballots, seats = 4)
names(result1)
result1$elected

set.seed(4)
result2 <- stv(cballots, seats = 4)
result2$elected

\dontrun{
result3 <- stv(cballots, seats = 4, surplusMethod = "Fractional")
result3$elected

result4 <- stv(cballots, seats = 4, surplusMethod = "Fractional",
               quotaMethod = "Hare")
result4$elected

result5 <- stv(cballots, seats = 4, surplusMethod = "Cambridge",
               quotaMethod = "Hare")
result5$elected
}

}
