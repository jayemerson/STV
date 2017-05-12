#' Validates Input Data Format for \code{stv()}.
#'
#' Validates data format for \code{stv()} input. Assumes each row and column
#' corresponds to a ballot and candidate respectively. See details for the
#' tests run by this function. If input data is in correct format, returns
#' string: "All tests passed. Please feel free to run \code{stv()} function.".
#' Otherwise, prints corresponding warning message.
#'
#' Checks if input data is in acceptable format for \code{stv()}. User must run
#' this function before calling \code{stv()}. Before any calculation, \code{stv()}
#' calls this function and proceeds only if \code{x} passes all of the following tests
#' (run in given order).
#' \enumerate{
#'   \item If \code{x} is a \code{data.frame}.
#'   \item If \code{x} has unique column names.
#'   \item If \code{x} has numeric entries.
#'   \item If \code{x} doesn't have any blank column.
#'   \item If \code{x} doesn't have any blank row or a row with non-sequential ranks.
#' }
#'
#' @param x a \code{data.frame} with rows as ballots and columns as candidates.
#'
#' @return string: "All tests passed. Please feel free to run stv() function." if \code{x}
#'   passes all tests. Otherwise, corresponding warning message. NOTE: Some of the
#'   warnings can be fixed using \code{cleanBallots()} function. Other issues must be
#'   fixed by the user.
#'
#' @examples
#' data(ballots)
#' result <- try(validateBallots(ballots), silent=TRUE)
#' print(result)
#' @export
validateBallots <- function(x) {

  # 1. Allowed Class: data.frame
  if (class(x) != "data.frame") stop("\nPlease enter object of class data.frame, or use cleanBallots().")

  # 2. Check sanity of column names:
  if (any(is.na(names(x)))) stop("\nPlease provide each candidate's name/identifier as column names.")
  if (length(unique(names(x))) != ncol(x)) stop("\nPlease provide unique column names.")

  # 3. Check if x is numeric:
  if (any(!sapply(x, is.numeric))) {
    print(paste("Column(s):",
                paste(which(!sapply(x, is.numeric)), collapse = ", "),
                "contain non-numeric data."))
    stop("\nPlease provide numeric input.")
  }

  # 4. Check for blank cols:
  if (any(colSums(is.na(x)) == nrow(x))) {
    print(paste("Column(s):",
                paste(which(colSums(is.na(x)) == nrow(x)), collapse = ","),
                "do not contain any ranks."))
    stop("\nPlease remove column(s) for candidate(s) not ranked, or use cleanBallots()")
  }

  # 5. Check for blank rows and rows w/ non-sequencial ranks
  if (any(rowSums(is.na(x)) == ncol(x))) {
    print(paste("Row(s):",
                paste(which(rowSums(is.na(x)) == ncol(x)), collapse = ", "),
                "do not contain any ranks."))
    stop("\nPlease remove blank row(s), or use cleanBallots()")
  }

  valid <- rep(FALSE, nrow(x))
  for (i in 1:nrow(x)) {
    if (identical(as.numeric(sort(x[i,])), as.numeric(1:max(x[i,], na.rm = TRUE)))) {
      valid[i] <- TRUE
    }
  }

  if (any(!valid)) {
    print(paste("Row(s):",
                paste(which(!valid), collapse = ", "),
                "contain non-sequencial (missing or duplicated) ranks."))
    stop("\nPlease remove row(s) with non-sequencial ranks, or use cleanBallots()")
  }

  return("All tests passed. Please feel free to run stv() function.")
}

#' Tries to Address \code{validateBallots()} Warning(s).
#'
#' Tries to clean data for \code{stv()}. Some of the warnings from \code{validateBallots()}
#' have to addressed by the user (see Details).
#'
#' Assumes \code{x} contains rows and columns corresponding to ballots and
#' candidates respectively. Tries to address issues raised by \code{validateBallots()}
#' in the following order:
#' \enumerate{
#'   \item If \code{x} is a \code{matrix} then converts to \code{data.frame}.
#'     Otherwise, user has to convert \code{x} into \code{data.frame}.
#'   \item Checks if \code{x} has numeric entries. If not, checks if numeric
#'     data was passed as character. If this also fails, then user has to
#'     convert data into numeric type.
#'   \item If column names of \code{x} are missing assigns \code{cand.names} as
#'     column names. If
#'   \code{x} already has valid column names, no need to specify \code{cand.names}.
#'   If column names of \code{x} missing and \code{cand.names} not specified, returns
#'   error message.
#'   \item Removes blank columns.
#'   \item Removes blank and/or non-sequentially ranked rows.
#' }
#'
#' @param x a \code{data.frame} with rows as ballots and columns as candidates.
#' @param cand.names character vector of length equal to number of candidates
#'   (needed when column names of \code{x} are missing or not unique).
#'
#' @return a \code{data.frame} compatible for \code{stv()} function.
#'
#' @examples
#' data(ballots)
#' cballots <- cleanBallots(ballots)
#' validateBallots(cballots)
#' @export
cleanBallots <- function(x, cand.names = NA) {

  # 1. Check if input: matrix or data.frame, convert matrix into data.frame
  if (!(class(x) %in% c("data.frame", "matrix"))) stop("\nPlease enter object of class either data frame or matrix.")
  if (class(x) == "matrix") x <- as.data.frame(x, stringsAsFactors = FALSE)

  # 2. Check if x is numeric:
  cols.non.numeric <- !sapply(x, is.numeric)
  if (any(cols.non.numeric)) {
    for (i in which(cols.non.numeric)) {
      if (is.factor(x[,i])) x[,i] <- as.character(x[,i])
      temp <- x[!is.na(x[,i]), i]
      if (anyNA(suppressWarnings(as.numeric(temp)))) {
        stop(paste("\nPlease check data type in column", i))
      }
      x[,i] <- as.numeric(x[,i])
    }
  }

  # 3. Provide column names:
  #--- CHECK ---: If x already has valid column names, is this code
  #-------------: going to remove those names?
  if (!is.na(cand.names)) {
    if (length(cand.names) != ncol(x)) stop ("Please provide exactly one candidate name for each column.")
    names(x) <- cand.names
  }

  # 4. Remove blank cols:
  x <- x[, colSums(!is.na(x)) > 0]

  # 5. Remove blank and/or non-sequentially ranked rows:
  x <- x[rowSums(is.na(x)) != ncol(x), ]

  valid <- rep(FALSE, nrow(x))
  for(i in 1:nrow(x)) {
    valid[i] <- identical(as.numeric(sort(x[i,])), as.numeric(1:max(x[i,], na.rm = TRUE)))
  }
  x <- x[valid, ]

  if (class(try(validateBallots(x))) == "try-error") warning("Validation failed the validateBallots() check")
  return(x)
}

