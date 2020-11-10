#' Sleepstaging from one participant.
#'
#' A dataset containing sleep staging result for one participant. In this case, no NREM period exceeds 120min (excl. wake), wherefore no splitting is initiated.
#' The variables are as follows:
#'
#' @format A vmrk file (can be read as a text file) with 954 rows and 2 variables:
#' \describe{
#'   \item{Type}{Describes what column 'Description' shows}
#'   \item{Description}{Gives the sleep stage according to AASM criteria. 0 = wake, 1 = N1, 2 = N2, 3 = N3, 5 = W}
#' }
"sleepstages"

#' Sleepstaging from one participant.
#'
#' A dataset containing sleep staging result for one participant. In this case, a NREM period exceeds 120min (excl. wake), wherefore splitting is initiated.
#' The variables are as follows:
#'
#' @format A vmrk file (can be read as a text file) with 959 rows (including one header row) and 2 variables:
#' \describe{
#'   \item{Type}{Describes what column 'Description' shows}
#'   \item{Description}{Gives the sleep stage according to AASM criteria. 0 = wake, 1 = N1, 2 = N2, 3 = N3, 5 = W}
#' }
"sleepstages2"
NULL
