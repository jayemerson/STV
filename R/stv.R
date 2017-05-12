#' Implement STV counting system.
#'
#' \code{stv} returns a data.frame with rows containing detailed results
#' from each round of STV counting.
#'
#' \code{stv()} first validates \code{x} by running \code{validateBallots()} function.
#' Once validation is complete, it implements Single Transferable Vote
#' counting method. Each round of counting starts with idetification of active
#' ballots. Then quota is calculated (currently only supports Droop method \code{INCLUDE FORMULA?}).
#' Tally of candidates is obtained using top choices of active ballots. If a
#' candidate reaches quota, she/he is elected and associated surplus ballots
#' are reallocated (currently only supports Cambridge method \code{ONE LINE DETAIL?}). If multiple candidates
#' reach quota, all of them are elected and their surplus are allocated in that
#' round itself. If nobody reaches quota then candidate with minimum number of votes
#' is eliminated. If multiple candidates tie for minimum number of votes, one of
#' them is selected at random and eliminated. The process is repeated till all
#' of the seats are filled or number of candidates still in race equals number
#' of unfilled seats. In later case, all of the active candidates are elected.
#' Note: a ballot stays active till either it runs out of marked choices or gets
#' removed during surplus allocation.
#'
#' @param x a data.frame with rows as ballots and columns as candidates. \code{x}
#'     must pass all checks from \code{validateBallots()}.
#' @param seats a number indicating candidates to elect. (default = 1)
#' @param file a character string naming file. "" indicates output to the console only (default).
#'     Saves a CSV file. So, name should contain ".csv" at the end.
#' @param surplusMethod a character string indicating which method to use for
#'     surplus allocation. Currently supports only one option "Cambridge" (default)
#' @param quotaMethod a character string indicating which method to use for
#'     calculation of quota. Currently support only one option "Droop" (default)
#'
#' @return a data.frame with rows containing detailed results from each round of STV counting.
#'     For any given round of counting, a row contains: number of active ballots, seats to fill,
#'     quota, maximum and minimum votes obtained by any candidate, who was eliminated (if any),
#'     if there was a tie for elimination (if yes, how many tied), who was elected (if any),
#'     surplus if elected and each candidate's votes tally for that round.
#'
#' @examples
#' data(ballots)
#' cballots <- cleanBallots(ballots)
#' result <- stv(cballots, seats = 4)
#' unique(result$elect.cand)
#' @export
stv <- function(x, seats = 1, file = "", surplusMethod = "Cambridge", quotaMethod = "Droop") {

  # Ensure supported surpluse and quota methods are selected
  if(surplusMethod != "Cambridge") stop("Please set surplusMethod = 'Cambridge'. This is currently the only supported method.")
  if(quotaMethod != "Droop") stop("Please set quotaMethod = 'Droop'. This is currently the only supported method.")

  junk <- validateBallots(x)

  if(seats > ncol(x)) stop("Number of seats must be less than or equal to the number of candidates.")

  # Initialize various things:
  elim <- c()                                  # Store names of eliminated candidates
  elect <- c()                                 # Store names of elected candidates
  included.ballots <- rep(TRUE, nrow(x))       # Ballots included at a given round
  unfilled <- seats                            # Vacant seats at a given round
  Nround <- 0                                  # Current round number

  res <- data.frame(matrix(NA, ncol = 9 + ncol(x), nrow = 0)) # rename as res
  names(res) <- c("ballots", "seats.to.fill", "quota", "max.vote.count", "min.vote.count",
                  "elim.cand", "tied.for.elim", "elect.cand", "surplus", names(x))

  # Iteratively elect/eliminate candidates till all seats are filled:
  while (unfilled > 0) {
    Nround <- Nround + 1
    res[Nround, ] <- rep(NA, ncol(res))
    res$seats.to.fill[Nround] <- unfilled
    curr.candidates <- setdiff(names(x), c(elim, elect))

    # Check if #remaining candidates already equal #unfilled seats:
    if (unfilled == length(curr.candidates)) {
      elect <- c(elect, curr.candidates)
      res$elect.cand[Nround] <- paste(curr.candidates, collapse = "; ")
      res$seats.to.fill[Nround] <- 0
      res[Nround, elect] <- "Elected"
      res[Nround, elim] <- "Eliminated"
      res$quota[Nround] <- "Won by elim"

      if (file != "") {
        if (substr(file, nchar(file)-3, nchar(file)) != ".csv") warning("File name provided does not include .csv extention")
        write.table(res, file = file, sep = ",", row.names = FALSE)
      }

      return(res)
      # return(list("elected" = elect, "detailed.info" = res))
    }

    # For remaining candidates, get valid ballots (1. not empty 2. not excluded in prior round)
    curr.ballots <- (rowSums(!is.na(x[ ,curr.candidates])) > 0) & included.ballots
    ballot.size <- sum(curr.ballots)
    res$ballots[Nround] <- ballot.size

    # Calculate Quota: Manually add 1 instead of using "ceiling()" to address whole numbers
    if (quotaMethod == "Droop") quota <- floor(ballot.size/(unfilled + 1)) + 1
    res$quota[Nround] <- quota

    # Get top choice for each valid ballot then tabulate it (i.e. get vote count for each candidate):
    top.choice <- apply(x[ ,curr.candidates], 1, function(i.row) names(x[ ,curr.candidates])[which.min(i.row)]) # try using curr.candidates instead of names(x[ ,curr.cand])
    top.choice[!curr.ballots] <- NA
    vote.counts <- table(factor(top.choice, levels = curr.candidates))

    res[Nround, names(vote.counts)] <- vote.counts
    res$max.vote.count[Nround] <- max(vote.counts)
    res$min.vote.count[Nround] <- min(vote.counts)
    res[Nround, elect] <- "Elected"
    res[Nround, elim] <- "Eliminated"

    # Check for elected candidate(s): If quota crossed, surplus distribution by Cambridge method.
    #   else: eliminate a candidate and redistribute votes
    if (any(vote.counts >= quota)) {
      curr.elected <- vote.counts[vote.counts >= quota]
      elect <- c(elect, names(curr.elected))
      unfilled <- seats - length(elect)
      res$elect.cand[Nround] <- paste(names(curr.elected), collapse = "; ")
      res$surplus[Nround] <- paste(curr.elected - quota, collapse = "; ")

      # Redistribute surplus votes (Cambridge method only for now)
      for (i in names(curr.elected)) {
        cand.ballots <- which(top.choice == i)
        if (surplusMethod == "Cambridge") {
          included.ballots[sample(cand.ballots, quota)] <- FALSE
        }
      } # CLOSE surplus allocation
    } else {
      curr.elim <- names(which(vote.counts == min(vote.counts)))
      if (length(curr.elim) > 1) res$tied.for.elim[Nround] <- paste("Yes: ", length(curr.elim), sep = "")
      elim <- c(elim, sample(curr.elim, 1))
      res$elim.cand[Nround] <- tail(elim, 1)
    } # CLOSE elim/elect if-else function

  } # CLOSE while-loop

  if (file != "") {
    if (substr(file, nchar(file)-3, nchar(file)) != ".csv") warning("File name provided does not include .csv extention")
    write.table(res, file = file, sep = ",", row.names = FALSE)
  }

  return(res)
  # return(list("elected" = elect, "detailed.info" = res))
}


