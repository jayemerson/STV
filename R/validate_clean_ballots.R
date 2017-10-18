#' Validates Input Data Format for \code{stv()}.
#'
#' The function \code{validateBallots} validates data format for \code{stv()} input.  
#' It assumes that each row corresponds to a ballot and each column corresponds to a 
#' candidate. If input data is in the correct format, \code{validateBallots} returns 
#' string: "All tests passed. Please feel free to run \code{stv()} function." 
#' Otherwise, it prints a message corresponding to the formatting error.
#'
#' The \code{validateBallots} function should be run before \code{stv()} is called. 
#' Before any calculation, \code{stv()} calls this function and proceeds only 
#' if \code{x} passes all of the following tests.
#' \enumerate{
#'   \item If \code{x} is a \code{data.frame}.
#'   \item If \code{x} has unique column names.
#'   \item If \code{x} has numeric entries.
#'   \item If \code{x} doesn't have any blank columns.
#'   \item If \code{x} doesn't have any blank rows.
#'   \item If \code{x} doesn't have any rows with non-sequential ranks.
#' }
#'
#' @param x a \code{data.frame} with rows as ballots and columns as candidates.
#'
#' @return string: "All tests passed. Please feel free to run stv() function." if \code{x}
#'   passes all tests. Otherwise, a message corresponding to the problem is retunred. 
#'   NOTE: Some of the warnings can be fixed using \code{cleanBallots()} function. 
#'   Other issues must be fixed by the user.
#'
#' @examples
#' data(ballots)
#' result <- try(validateBallots(ballots), silent=TRUE)
#' print(result)
#' @export
validateBallots <- function(x) {

  # 1. Allowed Class: data.frame
  if (class(x) != "data.frame") 
    stop("\nPlease enter object of class data.frame, or use cleanBallots().")

  # 2. Check sanity of column names:
  if(is.null(names(x))) 
    stop("\nPlease provide each candidate's name/identifier as column names.")
  if (any(is.na(names(x)))) 
    stop("\nPlease provide each candidate's name/identifier as column names.")
  if (length(unique(names(x))) != ncol(x)) 
    stop("\nPlease provide unique column names.")

  # 3. Check for blank cols:
  if (any(colSums(is.na(x)) == nrow(x))) {
    print(paste("Column(s):",
                paste(which(colSums(is.na(x)) == nrow(x)), collapse = ","),
                "do not contain any ranks."))
    stop("\nPlease remove column(s) for candidate(s) not ranked, or use cleanBallots()")
  }
  
  # 4. Check if x is numeric:
  if (any(!sapply(x, is.numeric))) {
    print(paste("Column(s):",
                paste(which(!sapply(x, is.numeric)), collapse = ", "),
                "contain non-numeric data."))
    stop("\nPlease provide numeric input.")
  }

  # 5. Check for blank rows and rows w/ non-sequencial ranks
  if (any(rowSums(is.na(x)) == ncol(x))) {
    print(paste("Row(s):",
                paste(which(rowSums(is.na(x)) == ncol(x)), collapse = ", "),
                "do not contain any ranks."))
    stop("\nPlease remove blank row(s), or use cleanBallots()")
  }

  valid <- rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    valid[i] <- identical(as.numeric(sort(x[i,])),
                          as.numeric(1:max(x[i,], na.rm = TRUE)))
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
#' The \code{cleanBallots()} function tries to clean data for use in \code{stv()}. 
#' Some warnings from \code{validateBallots()} can only be addressed by the user.
#'
#' The \code{cleanBallots()} function assumes \code{x} contains rows and columns 
#' corresponding to ballots and candidates respectively. Formatting corrections 
#' are attempted in the following order:
#' \enumerate{
#'   \item If \code{x} is a \code{matrix} it is convered to a \code{data.frame}.
#'     Otherwise, user must provide a \code{data.frame}.
#'   \item If \code{x} has non-numeric entries, user must
#'     convert data to numeric type.
#'   \item If \code{cand.names} is specified, it is assigned as column names of \code{x}. 
#'   All collumn names must be uniquely specified.
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
cleanBallots <- function(x, cand.names = NULL) {

  # 1. Check if input: matrix or data.frame, convert matrix into data.frame
  if (!(class(x) %in% c("data.frame", "matrix"))) {
    stop("\nPlease enter object of class either data frame or matrix.")
  }
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
  #-------------: going to remove those names?   JWE May 16, could be
  # improved to check existing column names?
  if (!is.null(cand.names)) {
    if (length(cand.names) != ncol(x)) {
      stop ("Please provide exactly one candidate name for each column.")
    }
    names(x) <- cand.names
  }

  # 4. Remove blank cols: 
  x <- x[, colSums(!is.na(x)) > 0]

  # 5. Remove blank rows: 
  x <- x[rowSums(!is.na(x)) > 0, ]

  # 6. Remove ballots with duplicated ranks:
  x <- x[!apply(x, 1, function(y) any(duplicated(unlist(y), incomparables = NA))),]

  # 7. Reformat votes to follow rank convention
  for (i in 1:nrow(x)) {
    x[i,] <- rank(x[i,], na.last="keep")
  }

  if (class(try(validateBallots(x))) == "try-error")
    warning("Validation failed the validateBallots() check")
  return(x)
}

