#' Parses a fastq file into tibble
#'
#' @param file A fastq file location
#'
#' @return A tibble of the sections of the entries in the fastq file
#' @export
#'
#' @examples read_fastq(system.file("good.fq", package="fastq1R"))
read_fastq <- function(file) {

  assertthat::assert_that(assertthat::is.readable(file))
  assertthat::assert_that(assertthat::has_extension(file,"fq"))

  lines <- base::scan(file,character())

  ids <- lines[c(TRUE,FALSE,FALSE,FALSE)]
  bases <- lines[c(FALSE,TRUE,FALSE,FALSE)]
  qualities <- lines[c(FALSE,FALSE,FALSE,TRUE)]


  if(!all(base::startsWith(ids,"@"))) {
    base::stop("Not all IDs starts with @")
  }

  ids <- stringr::str_sub(ids, start=2)

  if(any(base::duplicated(ids))) {
    base::stop("Some IDs are duplicated")
  }

  if(!all(base::nchar(bases) == base::nchar(qualities))) {
    base::stop("the length of the bases doesn't match the length of the qualities")
  }

  tibble::tibble(ID=ids, Bases=bases, Qualities=qualities, GC=gc_content(bases)) %>%
    return()

}

gc_content <- function(seq) {

  assertthat::assert_that(is.character(seq))
  if(any(stringr::str_detect(seq,"[^GATC]"))) {
    base::warning("Non GATC characters found in sequences")

  }

  seq <- base::toupper(seq)

  GC_percent <- (base::nchar(stringr::str_replace_all(seq,"[^GC]",""))/nchar(seq))*100

  return(GC_percent)
}

