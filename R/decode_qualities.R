#' Decodes ASCII encoded Phred score values
#'
#' @param qualities A single elements  character vector of encoded qualities
#' @param offset The phred offset used for
#'
#' @return A numeric vector of Phred scores
#' @export
#'
#' @examples decode_qualities("???#;ABAAAHHHHGHFGDHEG@GG@GDGGB>DDDGBDD=")
decode_qualities <- function(qualities, offset=33) {

  assertthat::assert_that(assertthat::is.scalar(offset))
  assertthat::assert_that(assertthat::is.number(offset))

  if(!(offset == 33|offset == 64)) {
    stop("Offset is not equal to 33 or 64")
  }

  phred_score <- as.integer(charToRaw(qualities)) - offset

  if(!any(phred_score>0)) {
    base::stop("Phred score is less than 0")
  }

  return(phred_score)
}
