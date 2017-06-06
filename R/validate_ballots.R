#' Validates Input Data Format for \code{stv()}.
#'
#' Validates data format for \code{stv()} input. Assumes each row and column
#' corresponds to a ballot and candidate respectively. See details for the
#' tests run by this function. If input data is in correct format, returns
#' string: "All tests passed. Please feel free to run \code{stv()} function.".
#' Otherwise, prints corresponding warning message.
#'
#' Checks if input data is in acceptable format for \code{stv()}. User must 
#' run this function before calling \code{stv()}. Before any calculation, 
#' \code{stv()} calls this function and proceeds only if \code{x} passes all 
#' of the following tests (run in given order).
#' \enumerate{
#'   \item If \code{x} is a \code{data.frame} or \code{matrix}. Converts
#'     \code{matrix} into \code{data.frame}.
#'   \item Checks if \code{x} has numeric entries. If not, checks if numeric
#'     data was passed as character. If this also fails, then user has to
#'     convert data into numeric type.
#'   \item If column names of \code{x} are invalid or missing reports/assigns 
#'     \code{cand.names} as column names. If \code{cand.names} itself is invalid
#'     and user asked \code{x} to be cleaned, provides column names as
#'     \code{cand.icol}.
#'   \item Reports/Removes blank columns.
#'   \item Reports/Removes blank rows.
#'   \item Reports/Removes rows with duplicated rankings.
#'   \item Reports/Cleans non-sequentially ranked rows.
#' }
#' 
#' @param x a \code{data.frame} with rows as ballots and columns as candidates.
#' @param clean logical. Should data be cleaned?
#' @param cand.names character vector of length equal to number of candidates
#'   (needed when column names of \code{x} are missing or not unique).
#'   
#' @return string: "All tests passed. Please feel free to run stv() function." 
#' if \code{x} passes all tests. If \code{clean == FALSE} then reports and stops
#' at the first failed test. NOTE: Some of the warnings can be fixed using
#' \code{clean == TRUE} option. Other issues must be fixed by the user.
#'
#' @examples
#' data(ballots)
#' result <- try(validateBallots(ballots), silent=TRUE)
#' print(result)
#' @export

#=========#=========#=========#=========#=========#=======60
#=========#=========#=========#=========#=========#=========#=========#=======80

validateBallots2 <- function(x, clean = FALSE, cand.names = NA) {
  
  # 1. Check if input: matrix or data.frame, convert matrix into data.frame
  if (!(class(x) %in% c("data.frame", "matrix"))) {
    stop("\nPlease enter object of class either data frame or matrix.")
  }
  if (class(x) == "matrix") x <- as.data.frame(x, stringsAsFactors = FALSE)
  
  # 2. Check if x is numeric:
  cols.non.numeric <- !sapply(x, is.numeric)
  if (any(cols.non.numeric)) {
    if (clean) {                # Check if numeric data passed as character
      for (i in which(cols.non.numeric)) {
        if (is.factor(x[,i])) x[,i] <- as.character(x[,i])
        temp <- x[!is.na(x[,i]), i]
        if (anyNA(suppressWarnings(as.numeric(temp)))) {
          stop(paste("\nPlease check data type in column", i))
        }
        x[,i] <- as.numeric(x[,i])
      }
    } else { # Reporting mode
      print(paste("Columns contain non-numeric data. E.g. columns:", 
                  paste(head(which(cols.non.numeric)), collapse = ", ")))
      stop("\nPlease provide numeric input.")
    }
  }
  
  # 3. Check sanity of column names:
  if (any(is.na(names(x))) | length(unique(names(x))) != ncol(x)) {
    if (clean) {
      if (!is.na(cand.names)) {
        if (length(unique(cand.names)) != ncol(x)) {
          stop ("Please provide unique candidate identifiers to each column.")
        }
        names(x) <- cand.names
      } else { # If cand.names not provided but still want us to clean
        names(x) <- paste("cand", ncol(x), sep = ".")
      }
    } else {  # Reporting mode
      stop("\nPlease provide unique candidate identifiers as column names.")
    }
  } 

  # 4. Check for blank cols:
  if (any(colSums(!is.na(x)) == 0)) {
    if (clean) {
      x <- x[, colSums(!is.na(x)) > 0]
    } else { # Reporting mode
      print(paste("Column(s): contain non-numeric data. E.g. columns:",
                  paste(head(which(colSums(!is.na(x)) == 0)), collapse = ","),
                  "do not contain any ranks."))
      stop("\nPlease remove candidate(s) w/o any rankings.")
    }
  }
  
  # 5. Remove blank rows: 
  if (any(rowSums(!is.na(x)) == 0)) {
    if (clean) {
      x <- x[rowSums(!is.na(x)) > 0, ]
    } else { # Reporting mode
      print(paste("Row(s) contain non-numeric data. E.g. rows:",
                  paste(head(which(rowSums(!is.na(x)) == 0)), collapse = ","),
                  "do not contain any ranks."))
      stop("\nPlease remove ballot(s) w/o any rankings.")
    }
  }
  
  # 6. Remove ballots with duplicated ranks:
  valid.rows <- sapply(1:nrow(x), 
                       function(i) { # Need to coerce rows of data.frame into vector
                         anyDuplicated(x[i, ][!is.na(x[i, ])]) == 0 
                       })
  if (!all(valid.rows)) {
    if (clean) {
      x <- x[valid.rows, ]
    } else { # Reporting mode:
      print(paste("Row(s) contain duplicated ranks. E.g. rows:",
                  paste(head(which(!valid.rows)), collapse = ",")))
      stop("\nPlease remove ballot(s) with duplicated rankings.")
    }
  }
  
  # 7. Re-rank ballots with non-sequential ranks:
  valid.rows <- sapply(1:nrow(x), 
                       function(i) all(1:sum(!is.na(x[i, ])) %in% x[i, ]))
  if (!all(valid.rows)) {
    if (clean) {
      for (i in 1:nrow(x)) {
        x[i,] <- rank(x[i,], na.last="keep")
      }
    } else { # Reporting mode:
      print(paste("Row(s) contain non-sequential ranks. E.g. rows:",
                  paste(head(which(!valid.rows)), collapse = ",")))
      stop("\nPlease remove ballot(s) with non-sequential rankings.")
    }
  }
  
  return("All tests passed. Please feel free to run stv() function.")
}



